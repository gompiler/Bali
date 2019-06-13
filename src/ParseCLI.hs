module ParseCLI
  ( Cmd(..)
  , CmdI(..)
  , cmdParser
  , main
  ) where

import           Options.Applicative

main :: IO ()
main = do
  (CI cmd f) <- customExecParser (prefs showHelpOnEmpty) cmdParser
  readFile f >>=
    case cmd of
      _ -> putStrLn

-- | Cmd: type specifying mode
data Cmd =
  Foo

-- | CmdI: Cmd + Inp
data CmdI =
  CI Cmd
     FilePath

file :: Parser FilePath
file =
  argument
    str
    (metavar "FILEPATH" <>
     help "Read input (source to be evaluated/optimized) from file at FILEPATH")

fooParser :: ParserInfo CmdI
fooParser =
  info
    (CI Foo <$> file)
    (fullDesc <> progDesc "foo, placeholder command" <> header "foo")

-- Combine all mode parsers into one
cmdParser :: ParserInfo CmdI
cmdParser =
  info
    (hsubparser (commandGroup "MODE FILEPATH" <> command "foo" fooParser) <**>
     helper)
    (fullDesc <> progDesc "Compiler for goLite" <>
     header "glc - a compiler for goLite")
