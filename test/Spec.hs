--{-# LANGUAGE AllowAmbiguousTypes #-}

import           Data.Binary          (encode)
import qualified Data.ByteString.Lazy as LB
import           Data.Word            (Word32)
import           DParse
import           Text.Megaparsec

main :: IO ()
main = do
  f <- LB.readFile "test/resources/Test.class"
  parseTest (dparse' :: Parser ClassFile) f
