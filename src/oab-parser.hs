import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Control.Monad

--
-- utils
keywords = ["ENUM", "AREA", "OPTIONS"]

isKeyword :: String -> Bool
isKeyword = (\s -> elem s keywords)

--
-- parsers
space :: ReadP ()
space = do many (satisfy isSpace)
           return ()

token :: ReadP a -> ReadP a
token p = do skipSpaces
             v <- p
             skipSpaces
             return v           

nonwhitespace :: ReadP Char
nonwhitespace = satisfy $ (\c -> not $ isSpace c)

word :: ReadP String
word = do skipSpaces
          w <- munch1 (not . isSpace)
          skipSpaces
          return w

symbol :: String -> ReadP String
symbol xs = token (string xs)

keyword :: ReadP String
keyword = do
  space
  symbol "ENUM" <|> symbol "AREA" <|> symbol "OPTIONS"

{-
keyword :: ReadP String
keyword = do
  tk <- token word
  if (isKeyword tk) then do return tk else return []
-}

--
-- oab parser
data Exam = Exam { year :: Int,
                   edition :: Int,
                   questions :: [Question]
                 } deriving Show

data Question = Question { number :: Int,
                           valid :: Bool,
                           enum :: String,
                           items :: [Item]
                         } deriving Show

data Item = Item { letter :: Letter,
                   correct :: Bool,
                   text :: String
                 } deriving Show

data Letter = A | B | C | D deriving (Show, Enum)

{-
questions :: Parser [Question]
questions = do qs <- many (token question)

question :: Parser Question
-- not handling valid yet
question = do symbol "ENUM"
              symbol "Questão"
              number <- integer
              enumWords <- symbol "OPTIONS" <|> many (token nonwhitespace)
              questionItems <- items
              return Question {number=number, valid=True, enum=(unwords enumWords), items=questionItems}
--}


exampleItem = "\nA) Laila deverá executar os honorários em face de Bob em processo\nautônomo, sendo vedado o pagamento nos mesmos autos, por se tratar de \nhonorários contratuais e não sucumbenciais.\nB:CORRECT) o juiz deverá determinar que os valores acordados a título\nde honorários sejam pagos diretamente a Laila, por dedução da quantia\na ser recebida por Rita, independentemente de concordância desta nos\nautos, salvo se Rita provar que já os pagou."

itemHeader :: Letter -> ReadP (Letter, Bool)
itemHeader l = do letter <- token $ symbol (show l)
                  correct <- option [] (symbol ":CORRECT")
                  symbol ")"
                  return (l, not . null $ correct)

item :: Letter -> ReadP Item
item l = do (letter,correct) <- itemHeader
            itemWords <- manyTill word (symbol "/ITEM")
            return Item {letter=letter, correct=correct, text=(unwords itemWords)}
--}
