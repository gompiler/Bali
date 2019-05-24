module Main where

import qualified Options.Applicative as Op
import           ParseCLI

main :: IO ()
main = do
  (CI cmd f) <-
    Op.customExecParser (Op.prefs Op.showHelpOnEmpty) cmdParser
  readFile f >>=
    case cmd of
      _ -> putStrLn
