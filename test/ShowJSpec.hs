module ShowJSpec
  ( spec
  ) where

import           Data.List    (intercalate)
import           Disassembler
import           IRData
import           ShowJ
import           TestBase

spec :: Spec
spec =
  describe "" $ do
    it "Instructions" $ do
      let i = Instructions [Astore0, Dcmpg, Dstore (IRIndex 128), Iconst0]
      stringJ i `shouldBe`
        intercalate "\n" ["astore_0", "dcmpg", "dstore 128", "iconst_0"]
    it "Test" $ do
      file <- resourceFile "test/resources/Test.class"
      case file of
        Just f  -> either expectationFailure (printJ' 2) $ disassemble f
        Nothing -> print "Skipping Test.class"
