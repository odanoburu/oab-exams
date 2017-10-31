import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Control.Monad

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
-- won't work
word = do skipSpaces
          w <- munch1 (\c -> not $ isSpace c)
          ns <- munch (\c -> c == '\n')
          skipSpaces
          return (w ++ ns)

symbol :: String -> ReadP String
symbol xs = token (string xs)

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

exampleQuestion = "---\nENUM Questão 1\n\nJúlio e Lauro constituíram o mesmo advogado para,\njuntos, ajuizarem ação de interesse comum. No curso do processo,\nsobrevieram conflitos de interesse entre os constituintes, tendo\nJúlio deixado de concordar com Lauro com relação aos pedidos.\n\nNessa situação hipotética, deve o advogado\n\nOPTIONS\n\nA:CORRECT) optar, com prudência e discernimento, por um dos mandatos,\ne renunciar ao outro, resguardando o sigilo profissional.\n/ITEM\nB) manter com os constituintes contrato de prestação de serviços\njurídicos no interesse da causa, resguardando o sigilo\nprofissional.\n/ITEM\nC) assumir, com a cautela que lhe é peculiar, o patrocínio de\nambos, em ações individuais.\n/ITEM\nD) designar, com prudência e cautela, por substabelecimento com\nreservas, um advogado de sua confiança.\n/ITEM\n---\nENUM Questão 2\n\nMário, advogado regularmente inscrito na OAB, foi\ncondenado pela prática de crime hediondo e, após a sentença penal\ntransitada em julgado, respondeu a processo disciplinar, tendo\nsofrido, como consequência, penalidade de exclusão da Ordem. \n\nConsiderando a situação hipotética apresentada e o Estatuto da\nAdvocacia e da OAB, assinale a opção correta.\n\nOPTIONS\n\nA) Ainda que se reabilite criminalmente, Mário não poderá mais\nse inscrever na OAB, visto que não preenche o requisito de\nidoneidade moral.\n/ITEM\nB) Serão considerados inexistentes os atos privativos de\nadvogado praticados por Mário após a exclusão, dado o\nimpedimento do exercício do mandato em razão da sanção\ndisciplinar aplicada.\n/ITEM\nC) A penalidade de exclusão somente poderia ter sido aplicada\ncaso Mário tivesse recebido três suspensões.\n/ITEM\nD:CORRECT) Supondo-se que o processo disciplinar tenha ficado paralisado\npor mais de três anos, aguardando o julgamento, a pretensão\nà punibilidade de Mário estaria prescrita e ele não poderia ser\nexcluído da Ordem.\n/ITEM\n---\n"

examQuestions :: ReadP [Question]
-- use manyTill instead of many1
examQuestions = do qs <- many1 (token question)
                   return qs

question :: ReadP Question
-- not handling valid yet
question = do symbol "---"
              symbol "ENUM"
              symbol "Questão"
              number <- natural
              enumWords <- manyTill word (symbol "OPTIONS")
              ia <- item A
              ib <- item B
              ic <- item C
              id <- item D
              skipSpaces
              return Question {number=number, valid=True, enum=(unwords enumWords), items=(ia:ib:ic:id:[])}
--


exampleItem = ""

itemHeader :: Letter -> ReadP (Letter, Bool)
itemHeader l = do letter <- token $ symbol (show l)
                  correct <- option [] (symbol ":CORRECT")
                  symbol ")"
                  return (l, not . null $ correct)

item :: Letter -> ReadP Item
item l = do (letter,correct) <- itemHeader l
            itemWords <- manyTill word (symbol "/ITEM")
            return Item {letter=letter, correct=correct, text=(unwords itemWords)}
--}
