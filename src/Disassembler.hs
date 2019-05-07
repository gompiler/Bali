module Disassembler
  ( disassemble
  ) where

import           Base
import           D2Data
import           DConv           (dconv)
import           DParse          (dparser)
import           Text.Megaparsec

disassemble :: ByteString -> Either String ClassFile
disassemble contents =
  case parse dparser "" contents of
    Left e -> throwError $ errorBundlePretty e
    Right x ->
      case dconv x of
        Left e  -> throwError $ show e
        Right o -> return o
