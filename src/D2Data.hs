{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module D2Data
  ( FieldAccess(..)
  , FieldAccessInfo(..)
  , ClassFile(..)
  , ConstantPool
  , ConstantPool'(..)
  , ConstantPoolInfo(..)
  , CpMethodHandle(..)
  , Interfaces
  , Interfaces'(..)
  , AccessFlag(..)
  , FieldDescriptor(..)
  , MethodDescriptor(..)
  , Fields
  , Fields'(..)
  , FieldInfo
  , FieldInfo'(..)
  , Methods
  , Methods'(..)
  , MethodInfo
  , MethodInfo'(..)
  , Attributes
  , Attributes'(..)
  , AttributeInfo(..)
  , ExceptionTables
  , ExceptionTables'(..)
  , ExceptionTable(..)
  , RefInfo(..)
  ) where

import           Base
import           Data.Int     (Int64)
import           DData        (AccessFlag (..), Attributes' (..),
                               ConstantPool' (..), CpMethodHandle (..),
                               ExceptionTable (..), ExceptionTables,
                               ExceptionTables' (..), FieldAccess (..),
                               FieldAccessInfo (..), FieldDescriptor (..),
                               FieldInfo' (..), Fields' (..), Interfaces' (..),
                               MethodDescriptor (..), MethodInfo' (..),
                               Methods' (..))
import           GHC.Int      (Int32)
import           Instructions

type TODO = ()

data ClassFile = ClassFile
  { minorVersion :: Word16
  , majorVersion :: Word16
  , constantPool :: ConstantPool
  , accessFlags  :: AccessFlag
  , thisClass    :: Word16
  , superClass   :: Word16
  , interfaces   :: Interfaces
  , fields       :: Fields
  , methods      :: Methods
  , attrs        :: Attributes
  } deriving (Show, Eq)

type ConstantPool = ConstantPool' ConstantPoolInfo

data RefInfo = RefInfo
  { rClass :: ByteString
  , rName  :: ByteString
  , rInfo  :: ByteString
  } deriving (Show, Eq)

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo
  -- class_index
  = CpClass ByteString
  -- class_index, name_and_type_index
  | CpFieldRef RefInfo
  | CpMethodRef RefInfo
  | CpInterfaceMethodRef RefInfo
  | CpString ByteString
  -- bytes
  | CpInteger Int32
  | CpFloat Float
  -- high_bytes, low_bytes
  | CpLong Int64
  | CpDouble Double
  -- name_index, descriptor_index
  | CpNameAndType ByteString
                  ByteString
  | CpInfo ByteString
  | CpMethodHandle CpMethodHandle
                   RefInfo
  -- descriptor_index
  | CpMethodType ByteString
  -- bootstrap_method_attr_index, name_and_type_index
  | CpInvokeDynamic TODO
                    RefInfo
  deriving (Show, Eq)

type Interfaces = Interfaces' ByteString

type Fields = Fields' FieldInfo

type FieldInfo = FieldInfo' Attributes

type Methods = Methods' MethodInfo

type MethodInfo = MethodInfo' Attributes

type Attributes = Attributes' AttributeInfo

data AttributeInfo
  = ACode { stackLimit      :: Word16
          , localLimit      :: Word16
          , code            :: Instructions
          , exceptionTables :: ExceptionTables
          , cAttrs          :: Attributes }
  | AConst ByteString
  deriving (Show, Eq)
