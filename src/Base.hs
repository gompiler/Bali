{-|
Module      : Base
Description : Base helper function module for use in many other modules
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3
Base helper function module for use in many other modules
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Base
  ( (<&>)
  , ($>)
  , (<$$>)
  , (<*->)
  , (<$->)
  , mapS
  , hexString
  , ByteString
  , Word8
  , Word16
  , Word32
  , Word64
  , Int8
  , Int16
  , Int32
  , Int64
  , Convertible(..)
  , MonadError(..)
  ) where

import           Control.Monad.Except (MonadError (..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor         (($>), (<&>))
import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.Word            (Word16, Word32, Word64, Word8)
import           Numeric              (showHex)

-- | Given context c and var a, convert to type b under monad error with e
class Convertible c e a b
  -- convert :: MonadError e m => c -> a -> m b
  where
  convert :: c -> a -> Either e b -- todo see if it's worth loosening to MonadError

instance Convertible c e a a where
  convert _ = pure

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

-- | Convert a number into a hex string, e.g. 0xFFFFFFFF
hexString :: (Integral a, Show a) => a -> String
hexString n = "0x" ++ showHex n ""
