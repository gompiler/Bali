module DDataSpec
  ( spec
  ) where

import           DData
import           TestBase

spec :: Spec
spec =
  testAccessInfo
    0x0019
    AccessInfoTest
      { tFieldAccess = FPublic
      , tIsStatic = True
      , tIsFinal = True
      , tIsVolatile = False
      , tIsTransient = False
      , tIsSynthetic = False
      , tIsEnum = False
      }

data AccessInfoTest = AccessInfoTest
  { tFieldAccess :: FieldAccess
  , tIsStatic    :: Bool
  , tIsFinal     :: Bool
  , tIsVolatile  :: Bool
  , tIsTransient :: Bool
  , tIsSynthetic :: Bool
  , tIsEnum      :: Bool
  }

testAccessInfo :: Word16 -> AccessInfoTest -> Spec
testAccessInfo flag t =
  describe ("AccessInfo test " ++ hexString flag) $ do
    let a = AccessFlag flag
    it "field access" $ fieldAccess a `shouldBe` tFieldAccess t
    it "static" $ isStatic a `shouldBe` tIsStatic t
    it "final" $ isFinal a `shouldBe` tIsFinal t
    it "volatile" $ isVolatile a `shouldBe` tIsVolatile t
    it "transient" $ isTransient a `shouldBe` tIsTransient t
    it "synthetic" $ isSynthetic a `shouldBe` tIsSynthetic t
    it "enum" $ isEnum a `shouldBe` tIsEnum t
