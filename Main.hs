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
  let x = parse parseFile "" contents
  case x of
    Left err -> print err
    Right ls -> do
      let x = buildFile ls filename
      writeFile ("test_outs/" ++ filename ++ ".asm") x
