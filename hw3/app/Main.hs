module Main where

import System.Console.Haskeline
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           line <- getInputLine "hi> "
           case line of
               Nothing -> return ()
               Just input -> do
                 let parsed = parse input
--                 outputStrLn $ "DEBUG> " ++ (show parsed)
                 case parsed of
                   Left err -> do outputStrLn $ errorBundlePretty err
                   Right hiExpr -> do
                    evaluated <- eval hiExpr
                    case evaluated of
                      Left err -> outputStrLn $ show err
                      Right hiValue -> outputStrLn $ show (prettyValue hiValue)
                    return ()
                 loop