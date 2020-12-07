module Parser where
import Text.Parsec
import Text.Parsec.Char
import Data.Maybe
import Data.Char(toLower)

data Command = CArith Operation
             | CPush Segment Int
             | CPop Segment Int
             | CFlow Flow
             | CFctFlow FctFlow

instance Show Command where
  show (CArith x) = map toLower $ show x
  show (CPush x y) = "push " ++ show x ++ " " ++ show y
  show (CPop x y) = "pop " ++ show x ++ " " ++ show y
  show (CFlow x) = show x
  show (CFctFlow x) = show x

data Flow = Label String
           | GoTo String
           | IfGoTo String

instance Show Flow where
  show (Label x) = "label " ++ x
  show (GoTo x) = "goto " ++ x
  show (IfGoTo x) = "if-goto " ++ x

data FctFlow = FDef String Int
             | FCall String Int
             | FReturn

instance Show FctFlow where
  show (FDef x y) = "function " ++ x ++ " " ++ show y
  show (FCall x y) = "call " ++ x ++ " " ++ show y
  show FReturn = "return"

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
              <|> try parsePop
              <|> try parseFlow
              <|> try parseFctFlow)
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
  if segment==Temp && notElem (read loc) [0..7]
    then fail "Temp has only 8 slots"
    else return (CPop segment (read loc))

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
  many (noneOf "\n\r")
  return Nothing

parseCommentOrNothing :: Parser (Maybe a)
parseCommentOrNothing = (try parseComment) <|> (many (oneOf " \t") >> pure Nothing)

-- Parsing flow
parseIdentifier :: Parser String
parseIdentifier = do
  firstChar <- nonDigitChar
  rest <- maybeDigitCharList
  return (firstChar:rest)
  where
    nonDigitChar = oneOf "_.:" <|> letter
    maybeDigitCharList = many (nonDigitChar <|> digit)

parseFlow :: Parser Command
parseFlow = do
          c <- parseFlowCommand
          char ' '
          l <- parseIdentifier
          return (CFlow (c l))

parseFlowCommand :: Parser (String -> Flow)
parseFlowCommand = try (string "label" >> return Label)
               <|> try (string "goto" >> return GoTo)
               <|> try (string "if-goto" >> return IfGoTo)

parseFctFlow :: Parser Command
parseFctFlow = try (string "return" >> return (CFctFlow FReturn))
            <|> parseFctFlowWithArgs


parseFctFlowWithArgs :: Parser Command
parseFctFlowWithArgs = do
  c <- parseCmd
  char ' '
  l <- parseIdentifier
  char ' '
  n <- many1 digit
  return (CFctFlow (c l (read n)))
  where parseCmd = try (string "function" >> return FDef)
                   <|> try (string "call" >> return FCall)
