module DisassemblerSpec
  ( spec
  ) where

import           Disassembler
import           TestBase

spec :: Spec
spec =
  describe "Disassemble class" $
  it "Test" $ do
    file <- resourceFile "test/resources/Test.class"
    case file of
      Just f  -> either expectationFailure print $ disassemble f
      Nothing -> print "Skipping Test.class"
