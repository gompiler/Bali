{-|
Module      : Peephole
Description : Peephole optimization definitions
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3
-}
module Peephole where

import           Base
import qualified Data.HashMap.Lazy as M
import           IRData

type LabelMap = M.HashMap ByteString LabelInfo

data LabelInfo =
  LabelInfo
    { position :: Integer
    , useCount :: Integer
    }

data PeepholeContext index indexw label labelw intByte intShort arrayType count =
  PeepholeContext
    { instructions :: Instructions' (Instruction' index indexw label labelw intByte intShort arrayType count)
    , _labelTable :: Int
    }

instance ( Eq index
         , Eq indexw
         , Eq label
         , Eq labelw
         , Eq intByte
         , Eq intShort
         , Eq arrayType
         , Eq count
         ) =>
         Eq (PeepholeContext index indexw label labelw intByte intShort arrayType count)
  -- We define our own equality to check for length first, given that instructions can be quite long
                                                                                                     where
  PeepholeContext {instructions = i1} == PeepholeContext {instructions = i2} =
    (length i1 == length i2) && (i1 == i2)
