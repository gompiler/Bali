import qualified Data.ByteString.Lazy as LB
import           DConv
import           DParse
import           System.Directory
import           Text.Megaparsec

main :: IO ()
main = do
  let filePath = "test/resources/Test.class"
  hasFile <- doesFileExist filePath
  if not hasFile
    then print "Skipping test as class file is not found"
    else do
      f <- LB.readFile "test/resources/Test.class"
      case parse (dparse :: DParser) "" f of
        Left e -> putStr (errorBundlePretty e)
        Right x ->
          case dconv x of
            Left e  -> putStr $ show e
            Right o -> print o
