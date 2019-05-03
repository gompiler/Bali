module Base
  ( (<&>)
  , ($>)
  , (<$$>)
  , (<*->)
  , (<$->)
  , mapS
  , hexString
  , throwError
  , ByteString
  , Word8
  , Word16
  , Word32
  , Word64
  , Int8
  , Int16
  , Int32
  , Int64
  ) where

import           Control.Monad.Except (throwError)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor         (($>), (<&>))
import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.Word            (Word16, Word32, Word64, Word8)
import           Numeric              (showHex)

infixl 4 <$->, <*->, <$$>

-- | Handles nested monads
{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
h <$$> m = fmap h <$> m

-- | Variant of <$> where the application is pure
{-# INLINE (<$->) #-}
(<$->) :: Applicative f => (a -> b) -> a -> f b
f <$-> x = f <$> pure x

-- | Variant of <*> where the application is pure
{-# INLINE (<*->) #-}
(<*->) :: Applicative f => f (a -> b) -> a -> f b
f <*-> x = f <*> pure x

-- | Applies map to a traversable monad
mapS ::
     (Traversable t, Monad m, Monad f) => (a -> f (m b)) -> t a -> f (m (t b))
mapS f x = sequence <$> mapM f x

hexString :: (Integral a, Show a) => a -> String
hexString n = "0x" ++ showHex n ""
