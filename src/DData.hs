{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html

Note that this represents the first iteration, where we focus on parsing.
All instances of indices are left as is, and no verification is made
with regards to the indices.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards    #-}

module DData
  ( Index
  , FieldAccess(..)
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
  ) where

import           Base
import           Control.Monad (liftM)
import           Data.Int      (Int64)
import           Data.List     (intercalate)
import           GHC.Int       (Int32)
import           Prelude       hiding (showList)

showList :: Show a => Maybe Integer -> Maybe String -> [a] -> String
showList startIndex tag items =
  case tag of
    Just t ->
      t ++
      if null items
        then " []"
        else "\n\t" ++ intercalate "\t\n" listString
    _ -> intercalate "\n" listString
  where
    listString :: [String]
    listString =
      case startIndex of
        Just i -> zipWith indexedItem [i ..] items
        _      -> map show items
    indexedItem :: Show a => Integer -> a -> String
    indexedItem i v = show i ++ ": " ++ show v

type Index = Word16

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  | FPackagePrivate
  deriving (Eq)

class FieldAccessInfo a where
  fieldAccess :: a -> FieldAccess
  isStatic :: a -> Bool
  isFinal :: a -> Bool

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

newtype ConstantPool' a =
  ConstantPool [a]
  deriving (Eq, Ord)

instance Show a => Show (ConstantPool' a) where
  show (ConstantPool l) = showList (Just 1) (Just "ConstantPool") l

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo
  -- class_index
  = CpClass Index
  -- class_index, name_and_type_index
  | CpFieldRef Index
               Index
  | CpMethodRef Index
                Index
  | CpInterfaceMethodRef Index
                         Index
  | CpString Index
  -- bytes
  | CpInteger Int32
  | CpFloat Float
  -- high_bytes, low_bytes
  | CpLong Int64
  | CpDouble Double
  -- name_index, descriptor_index
  | CpNameAndType Index
                  Index
  | CpInfo ByteString
  | CpMethodHandle CpMethodHandle
                   Index
  -- descriptor_index
  | CpMethodType Index
  -- bootstrap_method_attr_index, name_and_type_index
  | CpInvokeDynamic Index
                    Index
  deriving (Show, Eq)

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-5.html#jvms-5.4.3.5
data CpMethodHandle
  -- getfield C.f:T
  = CpmGetField
  -- getstatic C.f:T
  | CpmGetStatic
  -- putfield C.f:T
  | CpmPutField
  -- putstatic C.f:T
  | CpmPutStatic
  -- invokevirtual C.m:(A*)T
  | CpmInvokeVirtual
  -- invokestatic C.m:(A*)T
  | CpmInvokeStatic
  -- invokespecial C.m:(A*)T
  | CpmInvokeSpecial
  -- new C; dup; invokespecial C.<init>:(A*)V
  | CpmNewInvokeSpecial
  -- invokeinterface C.m:(A*)T
  | CpmInvokeInterface
  deriving (Show, Eq)

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.2
newtype AccessFlag =
  AccessFlag Word16
  deriving (Show, Eq)

type Interfaces = Interfaces' Index

newtype Interfaces' l =
  Interfaces [l]
  deriving (Eq)

instance Show a => Show (Interfaces' a) where
  show (Interfaces l) = showList (Just 0) (Just "Interfaces") l

type Fields = Fields' FieldInfo

newtype Fields' l =
  Fields [l]
  deriving (Eq)

instance Show a => Show (Fields' a) where
  show (Fields l) = showList (Just 0) (Just "Fields") l

data FieldInfo' attr = FieldInfo
  { fAccessFlags :: AccessFlag
  , fNameIndex   :: Word16
  , fDescIndex   :: Word16
  , fAttrs       :: attr
  } deriving (Show, Eq)

instance Functor FieldInfo' where
  fmap f FieldInfo {..} =
    FieldInfo
      { fAccessFlags = fAccessFlags
      , fNameIndex = fNameIndex
      , fDescIndex = fDescIndex
      , fAttrs = f fAttrs
      }

type FieldInfo = FieldInfo' Attributes

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.2-200
-- Aka type
data FieldDescriptor
  = TByte
  | TChar
  | TDouble
  | TFloat
  | TInt
  | TLong
  | TRef ByteString
  | TShort
  | TBool
  | TArray FieldDescriptor
  deriving (Show, Eq)

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.3
-- Aka signature
data MethodDescriptor =
  MethodDescriptor [FieldDescriptor]
                   (Maybe FieldDescriptor)

type Methods = Methods' MethodInfo

newtype Methods' l =
  Methods [l]
  deriving (Eq)

instance Show a => Show (Methods' a) where
  show (Methods l) = showList (Just 0) (Just "Methods") l

data MethodInfo' attr = MethodInfo
  { mAccessFlags :: AccessFlag
  , mNameIndex   :: Word16
  , mDescIndex   :: Word16
  , mAttrs       :: attr
  } deriving (Show, Eq)

instance Functor MethodInfo' where
  fmap f MethodInfo {..} =
    MethodInfo
      { mAccessFlags = mAccessFlags
      , mNameIndex = mNameIndex
      , mDescIndex = mDescIndex
      , mAttrs = f mAttrs
      }

type MethodInfo = MethodInfo' Attributes

type Attributes = Attributes' AttributeInfo

newtype Attributes' l =
  Attributes [l]
  deriving (Eq)

instance Show a => Show (Attributes' a) where
  show (Attributes l) = showList (Just 0) (Just "Attributes") l

data AttributeInfo =
  AttributeInfo Index
                ByteString
  deriving (Show, Eq)

type ExceptionTables = ExceptionTables' ExceptionTable

newtype ExceptionTables' l =
  ExceptionTables [l]
  deriving (Eq)

instance Show a => Show (ExceptionTables' a) where
  show (ExceptionTables l) = showList (Just 0) (Just "ExceptionTables") l

data ExceptionTable = ExceptionTable
  { eStartPc   :: Word16
  , eEndPc     :: Word16
  , eHandlerPc :: Word16
  , eCatchType :: Word16
  } deriving (Show, Eq)
