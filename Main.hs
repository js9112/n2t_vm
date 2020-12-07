module Main where

import Control.Monad
import System.Environment
import System.FilePath
import System.Directory
import Text.Parsec(parse)
import Parser
import CodeWriter
import Control.Monad.Trans.State

main :: IO ()
main = do
  [src] <- getArgs
  allFiles <- listDirectory src
  let vmFiles = filter (isExtensionOf ".vm") allFiles
  let path = splitDirectories src
  let programName = last path
  let programPath =  src ++ programName
  case evalStateT buildBootstrap (initialState "Bootstrap") of
    Left err -> putStrLn $ "Something went seriously wrong when writing bootstrap"
    Right boot -> do
      writeFile (programPath ++ ".asm") boot
      forM_ vmFiles (\file -> do
        contents <- readFile (src++file)
        let filename = takeBaseName file
        let x = parse parseFile filename contents
        case x of
          Left err -> print err
          Right ls -> do
            case buildFile ls filename of
              Left err -> putStrLn $ "ERROR: "++ err
              Right f -> do
                appendFile (programPath ++ ".asm") f
                putStrLn $ "Succesfully translated to " ++ programPath)
