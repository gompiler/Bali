module DisassemblerSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy as LB
import           Disassembler
import           System.Directory
import           TestBase

spec :: Spec
spec =
  describe "Disassemble class" $
  it "Test" $ do
    let filePath = "test/resources/Test.class"
    hasFile <- doesFileExist filePath
    if not hasFile
      then print "Skipping test as class file is not found"
      else either putStrLn print . disassemble =<<
           LB.readFile "test/resources/Test.class"
