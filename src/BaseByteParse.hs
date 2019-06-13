{-|
Module      : BaseByteParse
Description : Base helper functions to parse bytes
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BaseByteParse
  ( parseFailure
  , genericFailure
  , nestedParse
  , Parser'
  , Parse(..)
  ) where

import           Base
import qualified Data.Binary.Get as G
import           Text.Megaparsec

-- | Megaparsec parser type alias, with error e and target parse data a
type Parser' e a = Parsec (ParseErr e) ByteString a

data ParseErr a
  = ParseError a
  | GenericParseError String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent a => ShowErrorComponent (ParseErr a) where
  showErrorComponent (ParseError a)        = showErrorComponent a
  showErrorComponent (GenericParseError s) = show s

-- | Show the error component of a given Megaparsec parser
class ShowErrorComponent e =>
      Parse e a
  where
  p :: Parser' e a
  p = p' <* eof
  p' :: Parser' e a

-- TODO see if there is a better way of handling nested instructions
-- | Further parse contents, i.e. nested instructions
nestedParse ::
     (MonadParsec (ParseErr e2) s1 m, ShowErrorComponent e1)
  => Parser' e1 a
  -> ByteString
  -> m a
nestedParse parser content =
  either (genericFailure . errorBundlePretty) return $ parse parser "" content

-- | Report a Megaparsec parse error using given error
parseFailure :: MonadParsec (ParseErr e) s m => e -> m a
parseFailure = customFailure . ParseError

-- | Report a Megaparsec generic parse error using a string
genericFailure :: MonadParsec (ParseErr e) s m => String -> m a
genericFailure = customFailure . GenericParseError

instance ShowErrorComponent e => Parse e Word8 where
  p' = anySingle

instance ShowErrorComponent e => Parse e Word16 where
  p' = G.runGet G.getWord16be <$> takeP Nothing 2

instance ShowErrorComponent e => Parse e Word32 where
  p' = G.runGet G.getWord32be <$> takeP Nothing 4

instance ShowErrorComponent e => Parse e Word64 where
  p' = G.runGet G.getWord64be <$> takeP Nothing 8

instance ShowErrorComponent e => Parse e Int8 where
  p' = G.runGet G.getInt8 <$> takeP Nothing 1

instance ShowErrorComponent e => Parse e Int16 where
  p' = G.runGet G.getInt16be <$> takeP Nothing 2

instance ShowErrorComponent e => Parse e Int32 where
  p' = G.runGet G.getInt32be <$> takeP Nothing 4

instance ShowErrorComponent e => Parse e Int64 where
  p' = G.runGet G.getInt64be <$> takeP Nothing 8
