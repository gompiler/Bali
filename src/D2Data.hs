{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
-}
module D2Data
  ( FieldAccess(..)
  , FieldAccessInfo(..)
  , ClassFile(..)
  , ConstantPool(..)
  , ConstantPoolInfo(..)
  , CpMethodHandle(..)
  , Interfaces(..)
  , AccessFlag(..)
  , FieldDescriptor(..)
  , MethodDescriptor(..)
  , Fields(..)
  , FieldInfo(..)
  , Methods(..)
  , MethodInfo(..)
  , Attributes(..)
  , AttributeInfo(..)
  , ExceptionTables(..)
  , ExceptionTable(..)
  , RefInfo(..)
  ) where

import           Base
import           DData        (AccessFlag (..), CpMethodHandle (..),
                               ExceptionTable (..), ExceptionTables (..),
                               FieldAccess (..), FieldAccessInfo (..),
                               FieldDescriptor (..), Interfaces (..),
                               MethodDescriptor (..))
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

newtype ConstantPool =
  ConstantPool [ConstantPoolInfo]
  deriving (Eq)

instance Show ConstantPool where
  show (ConstantPool inf) =
    "ConstantPool\n" ++ concatMap showInfo (zip [1 ..] inf)
    where
      showInfo :: (Integer, ConstantPoolInfo) -> String
      showInfo (i, info) = "\t" ++ show i ++ ": " ++ show info ++ "\n"

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
  | CpInteger Word32
  | CpFloat Word32
  -- high_bytes, low_bytes
  | CpLong Word32
           Word32
  | CpDouble Word32
             Word32
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

newtype Fields =
  Fields [FieldInfo]
  deriving (Show, Eq)

data FieldInfo = FieldInfo
  { fAccessFlags :: AccessFlag
  , fNameIndex   :: Word16
  , fDescIndex   :: Word16
  , fAttrs       :: Attributes
  } deriving (Show, Eq)

newtype Methods =
  Methods [MethodInfo]
  deriving (Show, Eq)

data MethodInfo = MethodInfo
  { mAccessFlags :: AccessFlag
  , mNameIndex   :: Word16
  , mDescIndex   :: Word16
  , mAttrs       :: Attributes
  } deriving (Show, Eq)

newtype Attributes =
  Attributes [AttributeInfo]
  deriving (Show, Eq)

data AttributeInfo = ACode
  { stackLimit      :: Word16
  , localLimit      :: Word16
  , code            :: Instructions
  , exceptionTables :: ExceptionTables
  , cAttrs          :: Attributes
  } deriving (Show, Eq)
