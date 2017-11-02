{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

import System.IO
import System.Environment
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Aeson
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
                   edition :: Int,
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
{-
examQuestions :: ReadP [Question]
examQuestions = do qs <- manyTill (token question) (token eof)
                   return qs

question :: ReadP Question
question = do symbol "---"
              symbol "ENUM"
              valid <- option [] (symbol "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- manyTill word (symbol "OPTIONS")
              ia <- item A
              ib <- item B
              ic <- item C
              id <- item D
              skipSpaces
              return Question {number=number, valid=(notNull valid),
                               enum=(unwords enumWords), items=[ia,ib,ic,id]}
-}

example = "A:CORRECT) optar, com prudência e discernimento, por um dos mandatos,\ne renunciar ao outro, resguardando o sigilo profissional.\n\nB) manter com os constituintes contrato de prestação de serviços\njurídicos no interesse da causa, resguardando o sigilo\nprofissional.\n\nC) assumir, com a cautela que lhe é peculiar, o patrocínio de\nambos, em ações individuais.\n\nD) designar, com prudência e cautela, por substabelecimento com\nreservas, um advogado de sua confiança.\n\n---"

itemsP :: ReadP [Item]
itemsP = do
  ia <- itemA
  ib <- itemBC B
  ic <- itemBC C
  id <- itemD
  return [ia,ib,ic,id]

itemLetter :: Letter -> ReadP ()
itemLetter l = do
  symbol (show l)
  choice [char ')', char ':']
  return ()

itemCorrect :: ReadP Bool
itemCorrect = do
  correct <- option [] (symbol "CORRECT)")
  return (notNull correct)

itemA :: ReadP Item
itemA = do
  itemLetter A
  (correct, itemWords) <- itemBody $ itemLetter B
  return Item {letter = A, correct = correct, text = itemWords}

itemBody :: ReadP () -> ReadP (Bool, String)
itemBody p = do
  correct <- itemCorrect
  itemWords <- manyTill word p
  return (correct, (unwords itemWords))

itemBC :: Letter -> ReadP Item
itemBC l = do
  (correct, itemWords) <- itemBody $ itemLetter (succ l)
  return Item {letter = l, correct = correct, text = itemWords}

itemD :: ReadP Item
itemD = do
  (correct, itemWords) <- itemBody $ ((symbol "---") >> return ())
  return Item {letter = D, correct = correct, text = itemWords}

{-
question :: ReadP Question
question = do symbol "---"
              symbol "ENUM"
              valid <- option [] (symbol "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- manyTill word (symbol "OPTIONS")
              ia <- item A
              ib <- item B
              ic <- item C
              id <- item D
              skipSpaces
              return Question {number=number, valid=(notNull valid),
                               enum=(unwords enumWords), items=[ia,ib,ic,id]}

itemHeader :: Letter -> ReadP (Letter, Bool)
itemHeader l = do letter <- symbol (show l)
                  correct <- option [] (symbol ":CORRECT")
                  symbol ")"
                  return (l, (notNull correct))

item :: Letter -> ReadP Item
item l = do (letter,correct) <- itemHeader l
            itemWords <- manyTill word (symbol "/ITEM")
            return Item {letter=letter, correct=correct, text=(unwords itemWords)}

--
-- main

main :: IO ()
main = do args <- getArgs
          handle <- openFile (head args) ReadMode
          contents <- hGetContents handle
          B.putStr (encode $ fst $ head $ readP_to_S examQuestions contents)
          hClose handle
-}
