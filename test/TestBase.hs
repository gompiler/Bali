{-# LANGUAGE ScopedTypeVariables #-}

module TestBase
  ( unless
  , resourceFile
  , module Base
  , module Test.Hspec
  ) where

import           Base
import           Control.Monad        (filterM, unless)
import qualified Data.ByteString.Lazy as LB
import           System.Directory
import           System.FilePath      (joinPath)
import           Test.Hspec

resourceFile :: FilePath -> IO (Maybe ByteString)
resourceFile resourcePath = do
  let path = joinPath ["test/resources", resourcePath]
  hasFile <- doesFileExist path
  if not hasFile
    then Nothing <$ putStrLn ("Could not find path " ++ path)
    else Just <$> LB.readFile path
