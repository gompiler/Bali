module ParseCLI
  ( main
  ) where

import qualified Data.ByteString.Lazy as L
import           Disassembler
import           Options.Applicative
import           ShowJ
import           System.Exit
import           System.IO

-- | putExit: function to output to stderr and exit with return code 1
putExit :: String -> IO ()
putExit err = hPutStrLn stderr err >> exitFailure

main :: IO ()
main = do
  (CI cmd f) <- customExecParser (prefs showHelpOnEmpty) cmdParser
  L.readFile f >>=
    case cmd of
      Disassembler -> either putExit (printJ showJDefaultConfigs) . disassemble

-- | Cmd: type specifying mode
data Cmd =
  Disassembler

-- | CmdI: Cmd + Inp
data CmdI =
  CI Cmd
     FilePath

file :: Parser FilePath
file =
  argument str (metavar "FILEPATH" <> help "Read input from file at FILEPATH")

dParser :: ParserInfo CmdI
dParser =
  info
    (CI Disassembler <$> file)
    (fullDesc <> progDesc "Disassembles .class to .j" <> header "Disassembler")

-- Combine all mode parsers into one
cmdParser :: ParserInfo CmdI
cmdParser =
  info
    (hsubparser (command "disassemble" dParser) <**> helper)
    (fullDesc <> progDesc "JVM Optimizer" <>
     header "bali - a JVM bytecode optimizer")
