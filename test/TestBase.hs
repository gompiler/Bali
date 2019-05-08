{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module TestBase
  ( unless
  , testEntrySpec
  , TestEntry(..)
  , module Base
  , module Test.Hspec
  ) where

import           Base
import           Control.Monad (unless)
import           Test.Hspec
import           TestFileEmbed

_testEntries :: [TestEntry]
_testEntries = $(testEntries "files")

testEntrySpec :: String -> (TestEntry -> Expectation) -> Spec
testEntrySpec description verify =
  describe description $ mapM_ testEntrySpec' _testEntries
  where
    testEntrySpec' :: TestEntry -> SpecWith ()
    testEntrySpec' entry@TestEntry {name} = it name $ verify entry
