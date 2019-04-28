module Base
  ( (<&>)
  , ($>)
  , (<$$>)
  , (<*->)
  , (<$->)
  , mapS
  ) where

import           Data.Functor (($>), (<&>))

import qualified Data.Maybe   as Maybe

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
