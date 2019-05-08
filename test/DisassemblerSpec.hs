module DisassemblerSpec
  ( spec
  ) where

import           Disassembler
import           TestBase

spec :: Spec
spec =
  describe "Disassemble class" $
  testEntrySpec
    "DisassembleSpec files"
    (either expectationFailure print . disassemble . javac)
