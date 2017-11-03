{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

import System.IO
import System.Environment
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Aeson
import Data.List.Split
import qualified Data.ByteString.Lazy as B

--
-- utils
notNull :: String -> Bool
notNull = not . null

--
-- parsers
natural :: ReadP Int
natural = do skipSpaces
             ns <- munch1 isDigit
             skipSpaces
             return (read ns :: Int)

token :: ReadP a -> ReadP a
token p = do skipSpaces
             v <- p
             skipSpaces
             return v

word :: ReadP String
-- if newline scheme is \r\n, won't work. if \n \n instead of \n\n,
-- won't work.
-- maybe should catch only \n\n, which are used as paragraph separator
word = do skipSpaces
          w <- munch1 (\c -> not $ isSpace c)
          ns <- munch (\c -> c == '\n')
          skipSpaces
          return (w ++ ns)

symbol :: String -> ReadP String
symbol xs = token (string xs)

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
                           enum :: String,
                           items :: [Item]
                         } deriving (Generic,Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

data Item = Item { letter :: Letter,
                   correct :: Bool,
                   text :: String
                 } deriving (Generic,Show)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

data Letter = A | B | C | D deriving (Generic,Show,Read,Enum)

instance ToJSON Letter where
    toEncoding = genericToEncoding defaultOptions

--
-- OAB parsers
examQuestions :: ReadP [Question]
examQuestions = do qs <- manyTill (token question) (token eof)
                   return qs

question :: ReadP Question
question = do symbol "ENUM"
              valid <- option [] (symbol "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- manyTill word (symbol "OPTIONS")
              is <- itemsP
              skipSpaces
              return Question {number=number, valid=(notNull valid),
                               enum=(unwords enumWords), items=is}

itemsP :: ReadP [Item]
itemsP = do
  ia <- itemA
  ib <- itemBC B
  ic <- itemBC C
  id <- itemD
  return [ia,ib,ic,id]

itemBody :: ReadP () -> ReadP (Bool, String)
itemBody p = do
  correct <- itemCorrect
  itemWords <- manyTill word p
  return (correct, (unwords itemWords))

itemLetter :: Letter -> ReadP ()
itemLetter l = do
  symbol (show l)
  choice [char ')', char ':']
  return ()

itemCorrect :: ReadP Bool
itemCorrect = do
  correct <-  (symbol "CORRECT)") <++ (return []) --option [] (symbol "CORRECT)")
  return (notNull correct)

itemA :: ReadP Item
itemA = do
  itemLetter A
  (correct, itemWords) <- itemBody $ itemLetter B
  return Item {letter = A, correct = correct, text = itemWords}

itemBC :: Letter -> ReadP Item
itemBC l = do
  (correct, itemWords) <- itemBody $ itemLetter (succ l)
  return Item {letter = l, correct = correct, text = itemWords}

itemD :: ReadP Item
itemD = do
  (correct, itemWords) <- itemBody $ ((symbol "---") >> return ())
  return Item {letter = D, correct = correct, text = itemWords}

--
-- main

main :: IO ()
main = do
  args <- getArgs
  let filename = (head args)
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  let questions = fst $ head $ readP_to_S examQuestions contents
      yearEdition = splitOn "-" filename
      (year, edition) = (yearEdition !! 0, yearEdition !! 1)
  B.putStr
    (encode $ Exam {year = (read year :: Int), edition = edition, questions = questions})
  hClose handle

example = "ENUM Questão 1\n\nJúlio e Lauro constituíram o mesmo advogado para,\njuntos, ajuizarem ação de interesse comum. No curso do processo,\nsobrevieram conflitos de interesse entre os constituintes, tendo\nJúlio deixado de concordar com Lauro com relação aos pedidos.\n\nNessa situação hipotética, deve o advogado\n\nOPTIONS\n\nA:CORRECT) optar, com prudência e discernimento, por um dos mandatos,\ne renunciar ao outro, resguardando o sigilo profissional.\n\nB) manter com os constituintes contrato de prestação de serviços\njurídicos no interesse da causa, resguardando o sigilo\nprofissional.\n\nC) assumir, com a cautela que lhe é peculiar, o patrocínio de\nambos, em ações individuais.\n\nD) designar, com prudência e cautela, por substabelecimento com\nreservas, um advogado de sua confiança.\n\n---\nENUM Questão 2\n\nMário, advogado regularmente inscrito na OAB, foi\ncondenado pela prática de crime hediondo e, após a sentença penal\ntransitada em julgado, respondeu a processo disciplinar, tendo\nsofrido, como consequência, penalidade de exclusão da Ordem. \n\nConsiderando a situação hipotética apresentada e o Estatuto da\nAdvocacia e da OAB, assinale a opção correta.\n\nOPTIONS\n\nA) Ainda que se reabilite criminalmente, Mário não poderá mais\nse inscrever na OAB, visto que não preenche o requisito de\nidoneidade moral.\n\nB) Serão considerados inexistentes os atos privativos de\nadvogado praticados por Mário após a exclusão, dado o\nimpedimento do exercício do mandato em razão da sanção\ndisciplinar aplicada.\n\nC) A penalidade de exclusão somente poderia ter sido aplicada\ncaso Mário tivesse recebido três suspensões.\n\nD:CORRECT) Supondo-se que o processo disciplinar tenha ficado paralisado\npor mais de três anos, aguardando o julgamento, a pretensão\nà punibilidade de Mário estaria prescrita e ele não poderia ser\nexcluído da Ordem.\n\n---"
