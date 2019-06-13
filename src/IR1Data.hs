{-|
Module      : IR1Data
Description : Helper for instruction set for JVM operations
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3
-}
module IR1Data where

import           Base
import           IRData

newtype IRIndex =
  IRIndex Word8
  deriving (Show, Eq)

newtype IRIndexw =
  IRIndexw Word16
  deriving (Show, Eq)

newtype IRLabel =
  IRLabel Word16
  deriving (Show, Eq)

newtype IRLabelw =
  IRLabelw Word32
  deriving (Show, Eq)

newtype IntByte =
  IntByte Int8
  deriving (Show, Eq)

newtype IntShort =
  IntShort Int16
  deriving (Show, Eq)

newtype Count =
  Count Word8
  deriving (Show, Eq)

-- | Different array types
data ArrayType
  = AtBool
  | AtChar
  | AtFloat
  | AtDouble
  | AtByte
  | AtShort
  | AtInt
  | AtLong
  deriving (Show, Eq)

-- | Instruction list wrapper
type Instructions = Instructions' Instruction

-- | Instruction and its different components (for binary representation)
type Instruction
   = Instruction' IRIndex IRIndexw IRLabel IRLabelw IntByte IntShort ArrayType Count
