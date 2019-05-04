{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module D2Data
  ( NameAndTypeInfo(..)
  , RefInfo(..)
  , AttributeInfoKind(..)
  , StackLimit(..)
  , LocalLimit(..)
  , ClassFile
  , ConstantPool
  , ConstantPoolInfo
  , Interfaces
  , InterfaceInfo
  , Fields
  , FieldInfo
  , Methods
  , MethodInfo
  , Attributes
  , AttributeInfo(..)
  , module DData
  ) where

import           Base
import           Data.Int (Int64)
import           DData
import           GHC.Int  (Int32)
import           IR1Data
import           IRData

type TODO = ()

type ClassFile
   = ClassFile' ByteString ByteString ByteString NameAndTypeInfo ByteString RefInfo AttributeInfo

type ConstantPool = ConstantPool' ConstantPoolInfo

type ConstantPoolInfo
   = ConstantPoolInfo' ByteString ByteString ByteString NameAndTypeInfo ByteString RefInfo

type Interfaces = Interfaces' InterfaceInfo

type InterfaceInfo = InterfaceInfo' ByteString

type Fields = Fields' FieldInfo

type FieldInfo = FieldInfo' ByteString ByteString AttributeInfo

type Methods = Methods' MethodInfo

type MethodInfo = MethodInfo' ByteString ByteString AttributeInfo

type Attributes = Attributes' AttributeInfo

data NameAndTypeInfo = NameAndTypeInfo
  { ntName :: ByteString
  , ntType :: ByteString
  } deriving (Show, Eq)

data RefInfo = RefInfo
  { rClass :: ByteString
  , rName  :: ByteString
  , rType  :: ByteString
  } deriving (Show, Eq)

data AttributeInfoKind
  = ACode'
  | AConst'

newtype StackLimit =
  StackLimit Word16
  deriving (Show, Eq)

newtype LocalLimit =
  LocalLimit Word16
  deriving (Show, Eq)

data AttributeInfo
  = ACode { stackLimit      :: StackLimit
          , localLimit      :: LocalLimit
          , code            :: Instructions
          , exceptionTables :: ExceptionTables
          , cAttrs          :: Attributes }
  | AConst ByteString
  deriving (Show, Eq)
