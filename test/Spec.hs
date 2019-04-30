import qualified Data.ByteString.Lazy as LB
import           DConv
import           DParse
import           Text.Megaparsec

main :: IO ()
main = do
  f <- LB.readFile "test/resources/Test.class"
  case parse (dparse :: DParser) "" f of
    Left e -> putStr (errorBundlePretty e)
    Right x ->
      case dconv x of
        Left e  -> putStr $ show e
        Right o -> print o
