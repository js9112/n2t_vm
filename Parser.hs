module Parser where
import Text.Parsec
import Text.Parsec.Char
import Data.Maybe

data Command = CArith Operation
             | CPush Segment Int
             | CPop Segment Int
             deriving (Show)

data Segment = Argument
             | Local
             | Static
             | Constant
             | This
             | That
             | Pointer
             | Temp
             deriving (Enum, Eq)

instance Show Segment where
  show Argument = "argument"
  show Local = "local"
  show Static = "static"
  show Constant = "constant"
  show This = "this"
  show That = "that"
  show Pointer = "pointer"
  show Temp = "temp"

-- data Expr = Val Int
--           | Add Expr Expr
--           | Minus Expr Expr
--           | Neg Expr
--
-- instance Show Expr where
--   show (Val x) = show x
--   show (Add x y) = show x ++ "+" ++ show y
--   show (Minus x y) = show x ++ "-" ++ show y
--   show (Neg x) = "-" ++ show x

data Operation = Add
               | Sub
               | Neg
               | Eq
               | Gt
               | Lt
               | And
               | Or
               | Not
               deriving (Show)

type Parser = Parsec String ()

parseFile :: Parser [Command]
parseFile = do
  ls <- (parseCommand <|> parseComment) `endBy` (many1 crlf)
  eof
  return (catMaybes ls)

parseCommand :: Parser (Maybe Command)
parseCommand = fmap Just (try parseArith
              <|> try parsePush
              <|> try parsePop)
              <* parseCommentOrNothing

parseArith :: Parser Command
parseArith = try (string "add" >> return (CArith Add))
         <|> try (string "sub" >> return (CArith Sub))
         <|> try (string "neg" >> return (CArith Neg))
         <|> try (string "eq" >> return (CArith Eq))
         <|> try (string "gt" >> return (CArith Gt))
         <|> try (string "lt" >> return (CArith Lt))
         <|> try (string "and" >> return (CArith And))
         <|> try (string "or" >> return (CArith Or))
         <|> try (string "not" >> return (CArith Not))

parsePop :: Parser Command
parsePop = do
  string "pop"
  char ' '
  segment <- parseSegment
  char ' '
  loc <- many1 digit
  return (CPop segment (read loc))

parsePush :: Parser Command
parsePush = do
    string "push"
    char ' '
    segment <- parseSegment
    char ' '
    loc <- many1 digit
    if segment==Temp && notElem (read loc) [0..7]
      then fail "Temp has only 8 slots"
      else return (CPush segment (read loc))


parseSegment :: Parser Segment
parseSegment = try (string "argument" >> return Argument)
           <|> try (string "local" >> return Local)
           <|> try (string "static" >> return  Static)
           <|> try (string "constant" >> return Constant)
           <|> try (string "this" >> return This)
           <|> try (string "that" >> return That)
           <|> try (string "pointer" >> return Pointer)
           <|> try (string "temp" >> return Temp)

parseSegment' :: Parser Segment
parseSegment' = choice
  [ try $ string (show x) >> return x
  | x <- map toEnum [0..7]
  ]

parseComment :: Parser (Maybe a)
parseComment = do
  many (oneOf " \t")
  string "//"
  many1 (noneOf "\n\r")
  return Nothing

parseCommentOrNothing :: Parser (Maybe a)
parseCommentOrNothing = parseComment <|> (many (oneOf " \t") >> pure Nothing)
