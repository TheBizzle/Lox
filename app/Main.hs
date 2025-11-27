module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import qualified Data.Text.IO as TIO

main :: IO ()
main = getArgs >>= ((map asText) &> processArgs)
  where
    processArgs :: [Text] -> IO ()
    processArgs           []  = runPrompt
    processArgs (fileName:[]) = runFile fileName
    processArgs             _ = (TIO.putStrLn "Usage: jlox [script]") >> (exitWith $ ExitFailure 64)

runPrompt :: IO ()
runPrompt = return ()

runFile :: Text -> IO ()
runFile _ = return ()
