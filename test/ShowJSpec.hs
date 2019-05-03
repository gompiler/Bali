module ShowJSpec
  ( spec
  ) where

import           Instructions
import           ShowJ
import           TestBase
import Data.List (intercalate)

spec :: Spec
spec =
  describe "" $
  it "Instructions" $ do
    let i = Instructions [Astore0, Dcmpg, Dstore (IRIndex 128), Iconst0]
    stringJ i `shouldBe` intercalate "\n" ["astore_0", "dcmpg", "dstore 128", "iconst_0"]
