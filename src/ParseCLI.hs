module ParseCLI
  ( Cmd(..)
  , CmdI(..)
  , cmdParser
  ) where

import           Options.Applicative

-- | Cmd: type specifying mode
data Cmd
  = Foo

-- | CmdI: Cmd + Inp
data CmdI =
  CI Cmd FilePath

file :: Parser FilePath
file =
  argument str
    (metavar "FILEPATH" <>
     help "Read input (source to be evaluated/optimized) from file at FILEPATH")

fooParser :: ParserInfo CmdI
fooParser =
  info
    (CI Foo <$> file)
    (fullDesc <>
     progDesc "foo, placeholder command" <>
     header "foo")

-- Combine all mode parsers into one
cmdParser :: ParserInfo CmdI
cmdParser =
  info
  (hsubparser
    (commandGroup "MODE FILEPATH" <>
     command "foo" fooParser) <**>
    helper)
  (fullDesc <>
    progDesc "Compiler for goLite" <> header "glc - a compiler for goLite")
