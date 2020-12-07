module Main where


import System.Environment
import System.FilePath.Posix
import Text.Parsec(parse)
import Parser
import CodeWriter

main :: IO ()
main = do
  [src] <- getArgs
  contents <- readFile src
  let filename = takeBaseName src
  let filenameWithPath = dropExtension src
  let x = parse parseFile filename contents
  case x of
    Left err -> print err
    Right ls -> do
      case buildFile ls filename of
        Left err -> putStrLn $ "ERROR: "++ err
        Right f -> do
          writeFile (filenameWithPath ++ ".asm") f
          putStrLn $ "Succesfully translated to " ++ filenameWithPath
