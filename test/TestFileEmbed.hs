{-# LANGUAGE TemplateHaskell       #-}

-- | Allows file embedding for test resources
-- Based on https://github.com/snoyberg/file-embed/blob/master/Data/FileEmbed.hs
module TestFileEmbed
  ( testEntries
  , TestEntry(..)
  ) where

import           Control.Applicative        ((<$>))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB (fromStrict, length, readFile,
                                                   unpack)
import           Data.ByteString.Unsafe     (unsafePackAddressLen)
import           Data.List                  (foldl1', intersect, isSuffixOf)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (qAddDependentFile)
import           System.Directory           (listDirectory, makeAbsolute)
import           System.FilePath            (joinPath)
import           System.IO.Unsafe           (unsafePerformIO)

bsToExp :: ByteString -> Exp
bsToExp bs =
  VarE 'LB.fromStrict `AppE`
  (VarE 'unsafePerformIO `AppE`
   (VarE 'unsafePackAddressLen `AppE`
    LitE (IntegerL $ fromIntegral $ LB.length bs) `AppE`
    LitE (StringPrimL $ LB.unpack bs)))

data TestEntry = TestEntry
  { name     :: String
  , java     :: ByteString
  , javac    :: ByteString
  , javap    :: ByteString
  , krakatau :: ByteString
  }

type TestEntryFields = [TestEntryField]

type FileExtension = String

type TestEntryField = (String, FilePath, FileExtension)

-- | Needs to match field names from TestEntry
-- Name is not included as it isn't a file type
testEntryFields :: TestEntryFields
testEntryFields =
  [ ("java", "java", "java")
  , ("javac", "gen/javac", "class")
  , ("javap", "gen/javap", "j")
  , ("krakatau", "gen/krakatau", "j")
  ]

testEntries :: FilePath -> ExpQ
testEntries root =
  runIO (candidates root testEntryFields) >>= makeEntries root testEntryFields

makeEntries :: FilePath -> TestEntryFields -> [String] -> ExpQ
makeEntries _ _ [] = fail "No entries found"
makeEntries root fields files = do
  typ <- [t|[TestEntry]|]
  e <- ListE <$> mapM makeEntry files
  return $ SigE e typ
  where
    entryContent :: String -> Q [(Name, Exp)]
    entryContent fileName = mapM (entryContent' fileName) fields
    entryContent' :: String -> TestEntryField -> Q (Name, Exp)
    entryContent' fileName (fieldName, folder, ext) = do
      (path, bs) <-
        runIO $ do
          path <- makeAbsolute $ joinPath [root, folder, fileName ++ "." ++ ext]
          content <- LB.readFile path
          return (path, content)
      qAddDependentFile path
      return (mkName fieldName, bsToExp bs)
    makeEntry :: String -> ExpQ
    makeEntry fileName = do
      files' <- entryContent fileName
      return $
        RecConE 'TestEntry $ (mkName "name", LitE $ StringL fileName) : files'

candidates :: FilePath -> TestEntryFields -> IO [String]
candidates root fields = do
  c <- mapM candidates' fields
  return $
    case length c of
      0 -> []
      1 -> head c
      _ -> foldl1' intersect c
  where
    candidates' :: TestEntryField -> IO [String]
    candidates' (_, folder, ext) = do
      path <- makeAbsolute $ joinPath [root, folder]
      map (\c -> take (length c - length ext - 1) c) . filter (isSuffixOf ext) <$>
        listDirectory path
