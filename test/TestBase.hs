module TestBase
  ( unless
  , resourceFile
  , module Base
  , module Test.Hspec
  ) where

import           Base
import           Control.Monad        (unless)
import qualified Data.ByteString.Lazy as LB
import           System.Directory
import           Test.Hspec

resourceFile :: String -> IO (Maybe ByteString)
resourceFile path = do
  hasFile <- doesFileExist path
  if not hasFile
    then return Nothing
    else Just <$> LB.readFile path
