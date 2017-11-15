{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics

import Prelude hiding (readFile, putStr)
--import System.IO
import System.Environment
import Data.Char
import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Encoding
import qualified Data.Attoparsec.Text as A
import Data.Aeson
import Data.List.Split

--
-- utils
notNull :: T.Text -> Bool
notNull = not . T.null

--
-- parsers
spaces :: A.Parser ()
spaces = do A.skipWhile isSpace
            return ()

lexeme :: A.Parser a -> A.Parser a
lexeme p = do spaces
              v <- p
              spaces
              return v

natural :: A.Parser Int
natural = do ns <- lexeme $ A.decimal
             return ns

word :: A.Parser T.Text
-- if newline scheme is \r\n, won't work. if \n \n instead of \n\n,
-- won't work.
-- maybe should catch only \n\n, which are used as paragraph separator
word = do spaces
          w  <- A.takeWhile1 (\c -> not $ isSpace c)
          ns <- A.takeWhile (\c -> c == '\n')
          spaces
          return (w `T.append` ns)

symbol :: T.Text -> A.Parser T.Text
symbol xs = lexeme (A.string xs)

--
-- data types
data Exam = Exam { year :: Int,
                   edition :: String,
                   questions :: [Question]
                 } deriving (Generic,Show)

instance ToJSON Exam where
  toEncoding = genericToEncoding defaultOptions

data Question = Question { number :: Int,
                           valid :: Bool,
                           enum :: T.Text,
                           items :: [Item]
                         } deriving (Generic,Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

data Item = Item { letter :: Letter,
                   correct :: Bool,
                   text :: T.Text
                 } deriving (Generic,Show)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

data Letter = A | B | C | D deriving (Generic,Show,Read,Enum)

instance ToJSON Letter where
    toEncoding = genericToEncoding defaultOptions

--
-- OAB parsers
examQuestions :: A.Parser [Question]
examQuestions = do qs <- A.manyTill (lexeme question) A.endOfInput
                   return qs

question :: A.Parser Question
question = do symbol "ENUM"
              valid <- A.option T.empty (symbol "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- A.manyTill word (symbol "OPTIONS")
              is <- itemsP
              spaces
              return Question {number=number, valid=(notNull valid),
                               enum=(T.unwords enumWords), items=is}

itemsP :: A.Parser [Item]
itemsP = do
  ia <- itemA
  ib <- itemBC B
  ic <- itemBC C
  id <- itemD
  return [ia,ib,ic,id]

itemBody :: A.Parser () -> A.Parser (Bool, T.Text)
itemBody p = do
  correct <- itemCorrect
  itemWords <- A.manyTill word p
  return (correct, (T.unwords itemWords))

itemLetter :: Letter -> A.Parser ()
itemLetter l = do
  symbol $ T.pack (show l)
  A.choice [A.char ')', A.char ':']
  return ()

itemCorrect :: A.Parser Bool
itemCorrect = do
  correct <- A.option T.empty (symbol "CORRECT)")
  return (notNull correct)

itemA :: A.Parser Item
itemA = do
  itemLetter A
  (correct, itemWords) <- itemBody $ itemLetter B
  return Item {letter = A, correct = correct, text = itemWords}

itemBC :: Letter -> A.Parser Item
itemBC l = do
  (correct, itemWords) <- itemBody $ itemLetter (succ l)
  return Item {letter = l, correct = correct, text = itemWords}

itemD :: A.Parser Item
itemD = do
  (correct, itemWords) <- itemBody $ ((symbol "---") >> return ())
  return Item {letter = D, correct = correct, text = itemWords}

onlyParse :: A.Parser a -> T.Text -> a
onlyParse p t =
  let r = A.parseOnly p t
  in case r of
       Left _ -> T.empty
       Right res -> res

--
-- main

main :: IO [Question]
main = do
  [filename] <- getArgs
  contents <- readFile filename
  return $ onlyParse examQuestions contents
  {-
  questions <- A.parse examQuestions contents
  let yearEdition = splitOn "-" filename
      (year, edition) = (yearEdition !! 0, yearEdition !! 1)
  case questions of
    A.Fail _ _ err -> print err
    A.Done _ qs ->
      putStr
        (encode . encodeUtf8 $
         Exam {year = (read year :: Int), edition = edition, questions = qs})

example = "ENUM Questão 1\n\nJúlio e Lauro constituíram o mesmo advogado para,\njuntos, ajuizarem ação de interesse comum. No curso do processo,\nsobrevieram conflitos de interesse entre os constituintes, tendo\nJúlio deixado de concordar com Lauro com relação aos pedidos.\n\nNessa situação hipotética, deve o advogado\n\nOPTIONS\n\nA:CORRECT) optar, com prudência e discernimento, por um dos mandatos,\ne renunciar ao outro, resguardando o sigilo profissional.\n\nB) manter com os constituintes contrato de prestação de serviços\njurídicos no interesse da causa, resguardando o sigilo\nprofissional.\n\nC) assumir, com a cautela que lhe é peculiar, o patrocínio de\nambos, em ações individuais.\n\nD) designar, com prudência e cautela, por substabelecimento com\nreservas, um advogado de sua confiança.\n\n---\nENUM Questão 2\n\nMário, advogado regularmente inscrito na OAB, foi\ncondenado pela prática de crime hediondo e, após a sentença penal\ntransitada em julgado, respondeu a processo disciplinar, tendo\nsofrido, como consequência, penalidade de exclusão da Ordem. \n\nConsiderando a situação hipotética apresentada e o Estatuto da\nAdvocacia e da OAB, assinale a opção correta.\n\nOPTIONS\n\nA) Ainda que se reabilite criminalmente, Mário não poderá mais\nse inscrever na OAB, visto que não preenche o requisito de\nidoneidade moral.\n\nB) Serão considerados inexistentes os atos privativos de\nadvogado praticados por Mário após a exclusão, dado o\nimpedimento do exercício do mandato em razão da sanção\ndisciplinar aplicada.\n\nC) A penalidade de exclusão somente poderia ter sido aplicada\ncaso Mário tivesse recebido três suspensões.\n\nD:CORRECT) Supondo-se que o processo disciplinar tenha ficado paralisado\npor mais de três anos, aguardando o julgamento, a pretensão\nà punibilidade de Mário estaria prescrita e ele não poderia ser\nexcluído da Ordem.\n\n---"
---}
