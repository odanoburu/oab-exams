{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics

import System.IO
import System.Environment
import Data.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char hiding (letter)
import Text.ParserCombinators.Parsec.Prim
--import Control.Applicative hiding (many, optional)
--import Control.Monad
import Data.Aeson
import Data.List.Split

--
-- utils
notNull :: String -> Bool
notNull = not . null

--
-- parsers
natural :: Parser Int
natural = do spaces
             ns <- many1 digit
             spaces
             return (read ns :: Int)

lexeme :: Parser a -> Parser a
lexeme p = do spaces
              v <- p
              spaces
              return v

word :: Parser String
-- if newline scheme is \r\n, won't work. if \n \n instead of \n\n,
-- won't work.
-- maybe should catch only \n\n, which are used as paragraph separator
word = do spaces
          w <- many1 $ satisfy (\c -> not $ isSpace c)
          ns <- many $ satisfy (\c -> c == '\n')
          spaces
          return (w ++ ns)

symbol :: String -> Parser String
symbol xs = lexeme (string xs)

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
examQuestions :: Parser [Question]
examQuestions = do qs <- manyTill (lexeme question) (lexeme eof)
                   return qs

question :: Parser Question
question = do symbol "ENUM"
              valid <- option [] (symbol "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- manyTill word (symbol "OPTIONS")
              is <- itemsP
              spaces
              return Question {number=number, valid=(notNull valid),
                               enum=(unwords enumWords), items=is}

itemsP :: Parser [Item]
itemsP = do
  ia <- itemA
  ib <- itemBC B
  ic <- itemBC C
  id <- itemD
  return [ia,ib,ic,id]

itemBody :: Parser () -> Parser (Bool, String)
itemBody p = do
  correct <- itemCorrect
  itemWords <- manyTill word p
  return (correct, (unwords itemWords))

itemLetter :: Letter -> Parser ()
itemLetter l = do
  symbol (show l)
  choice [char ')', char ':']
  return ()

itemCorrect :: Parser Bool
itemCorrect = do
  correct <-  choice [(symbol "CORRECT)"), (return [])]
  return (notNull correct)

itemA :: Parser Item
itemA = do
  itemLetter A
  (correct, itemWords) <- itemBody $ itemLetter B
  return Item {letter = A, correct = correct, text = itemWords}

itemBC :: Letter -> Parser Item
itemBC l = do
  (correct, itemWords) <- itemBody $ itemLetter (succ l)
  return Item {letter = l, correct = correct, text = itemWords}

itemD :: Parser Item
itemD = do
  (correct, itemWords) <- itemBody $ ((symbol "---") >> return ())
  return Item {letter = D, correct = correct, text = itemWords}

--
-- main

main :: IO ()
main = do
  args <- getArgs
  let filename = (head args)
  questions <- parseFromFile examQuestions filename
  let yearEdition = splitOn "-" filename
      (year, edition) = (yearEdition !! 0, yearEdition !! 1)
  putStr
    (encode $ Exam {year = (read year :: Int), edition = edition, questions = questions})

example = "ENUM Questão 1\n\nJúlio e Lauro constituíram o mesmo advogado para,\njuntos, ajuizarem ação de interesse comum. No curso do processo,\nsobrevieram conflitos de interesse entre os constituintes, tendo\nJúlio deixado de concordar com Lauro com relação aos pedidos.\n\nNessa situação hipotética, deve o advogado\n\nOPTIONS\n\nA:CORRECT) optar, com prudência e discernimento, por um dos mandatos,\ne renunciar ao outro, resguardando o sigilo profissional.\n\nB) manter com os constituintes contrato de prestação de serviços\njurídicos no interesse da causa, resguardando o sigilo\nprofissional.\n\nC) assumir, com a cautela que lhe é peculiar, o patrocínio de\nambos, em ações individuais.\n\nD) designar, com prudência e cautela, por substabelecimento com\nreservas, um advogado de sua confiança.\n\n---\nENUM Questão 2\n\nMário, advogado regularmente inscrito na OAB, foi\ncondenado pela prática de crime hediondo e, após a sentença penal\ntransitada em julgado, respondeu a processo disciplinar, tendo\nsofrido, como consequência, penalidade de exclusão da Ordem. \n\nConsiderando a situação hipotética apresentada e o Estatuto da\nAdvocacia e da OAB, assinale a opção correta.\n\nOPTIONS\n\nA) Ainda que se reabilite criminalmente, Mário não poderá mais\nse inscrever na OAB, visto que não preenche o requisito de\nidoneidade moral.\n\nB) Serão considerados inexistentes os atos privativos de\nadvogado praticados por Mário após a exclusão, dado o\nimpedimento do exercício do mandato em razão da sanção\ndisciplinar aplicada.\n\nC) A penalidade de exclusão somente poderia ter sido aplicada\ncaso Mário tivesse recebido três suspensões.\n\nD:CORRECT) Supondo-se que o processo disciplinar tenha ficado paralisado\npor mais de três anos, aguardando o julgamento, a pretensão\nà punibilidade de Mário estaria prescrita e ele não poderia ser\nexcluído da Ordem.\n\n---"
