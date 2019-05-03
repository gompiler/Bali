{-
Instruction set for jvm operations
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IR1Data where

import           IRData

import           Base
import           Data.List (intercalate)

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

type Instructions = Instructions' Instruction

data InstructionFunctor index index' indexw indexw' label label' labelw labelw' intByte intByte' intShort intShort' arrayType arrayType' = InstructionFunctor
  { mapIndex     :: index -> index'
  , mapIndexw    :: indexw -> indexw'
  , mapLabel     :: label -> label'
  , mapLabelw    :: labelw -> labelw'
  , mapIntByte   :: intByte -> intByte'
  , mapIntShort  :: intShort -> intShort'
  , mapArrayType :: arrayType -> arrayType'
  }

type Instruction
   = Instruction' IRIndex IRIndexw IRLabel IRLabelw IntByte IntShort ArrayType
