module Disassembler
  ( disassemble
  ) where

import           Base
import           D2Data
import           DConv           (dconv)
import           DParse          (DParser, dparse)
import           Text.Megaparsec

disassemble :: ByteString -> Either String ClassFile
disassemble contents =
  case parse (dparse :: DParser) "" contents of
    Left e -> throwError $ errorBundlePretty e
    Right x ->
      case dconv x of
        Left e  -> throwError $ show e
        Right o -> return o
