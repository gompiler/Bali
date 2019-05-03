module Sample where

data Sample a b c d e =
  Sample a
         b
         c
         d
         e

class InstFunctor f where
  instMap ::
       (a -> a')
    -> (b -> b')
    -> (c -> c')
    -> (d -> d')
    -> (e -> e')
    -> f a b c d e
    -> f a' b' c' d' e'

data F a b c d e a' b' c' d' e' = F
  { a :: a -> a'
  , b :: b -> b'
  }
