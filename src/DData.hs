{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html

Note that this represents the first iteration, where we focus on parsing.
All instances of indices are left as is, and no verification is made
with regards to the indices.
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DData
  ( Index
  , FieldAccess(..)
  , AccessInfo(..)
  , ClassFile(..)
  , ConstantPool
  , ConstantPool'(..)
  , ConstantPoolInfo(..)
  , CpMethodHandle(..)
  , Interfaces
  , Interfaces'(..)
  , InterfaceInfo(..)
  , AccessFlag(..)
  , FieldDescriptor(..)
  , MethodDescriptor(..)
  , Fields
  , Fields'(..)
  , FieldInfo(..)
  , Methods
  , Methods'(..)
  , MethodInfo(..)
  , Attributes
  , Attributes'(..)
  , AttributeInfo(..)
  , ExceptionTables
  , ExceptionTables'(..)
  , ExceptionTable(..)
  , NameIndex(..)
  , ClassIndex(..)
  , DescIndex(..)
  , NameAndTypeIndex(..)
  , StringIndex(..)
  , RefIndex(..)
  ) where

import           Base
import           Data.Bits (Bits, (.&.))
import           Data.List (intercalate)
import           Prelude   hiding (showList)

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

-- | Points to CONSTANT_Utf8_info
newtype NameIndex =
  NameIndex Word16
  deriving (Show, Eq)

-- | Points to CONSTANT_Class_info
newtype ClassIndex =
  ClassIndex Word16
  deriving (Show, Eq)

-- | Points to CONSTANT_NameAndType_info
newtype NameAndTypeIndex =
  NameAndTypeIndex Word16
  deriving (Show, Eq)

-- | Points to CONSTANT_Utf8_info
newtype DescIndex =
  DescIndex Word16
  deriving (Show, Eq)

-- | Points to CONSTANT_Utf8_info
newtype StringIndex =
  StringIndex Word16
  deriving (Show, Eq)

-- | Points to one of:
-- * CONSTANT_Fieldref_info
-- * CONSTANT_Methodref_info
-- * CONSTANT_InterfaceMethodref_info
newtype RefIndex =
  RefIndex Word16
  deriving (Show, Eq)

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  | FPackagePrivate
  deriving (Show, Eq)

class HasAccessFlag a where
  _getAccessFlag :: a -> AccessFlag

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1-200-E.1
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.5-200-A.1
class AccessInfo a where
  fieldAccess :: a -> FieldAccess
  isStatic :: a -> Bool
  isFinal :: a -> Bool
  isVolatile :: a -> Bool
  isTransient :: a -> Bool
  isSynthetic :: a -> Bool
  isEnum :: a -> Bool
  default fieldAccess :: HasAccessFlag a =>
    a -> FieldAccess
  fieldAccess = fieldAccess . _getAccessFlag
  default isStatic :: HasAccessFlag a =>
    a -> Bool
  isStatic = isStatic . _getAccessFlag
  default isFinal :: HasAccessFlag a =>
    a -> Bool
  isFinal = isFinal . _getAccessFlag
  default isVolatile :: HasAccessFlag a =>
    a -> Bool
  isVolatile = isVolatile . _getAccessFlag
  default isTransient :: HasAccessFlag a =>
    a -> Bool
  isTransient = isTransient . _getAccessFlag
  default isSynthetic :: HasAccessFlag a =>
    a -> Bool
  isSynthetic = isSynthetic . _getAccessFlag
  default isEnum :: HasAccessFlag a =>
    a -> Bool
  isEnum = isEnum . _getAccessFlag

(.?.) :: Bits a => a -> a -> Bool
a .?. b = a .&. b == b

instance AccessInfo AccessFlag where
  fieldAccess (AccessFlag af) =
    if | af .?. 0x0001 -> FPublic
       | af .?. 0x0002 -> FPrivate
       | af .?. 0x0004 -> FProtected
       | otherwise -> FPackagePrivate
  isStatic (AccessFlag af) = af .?. 0x0008
  isFinal (AccessFlag af) = af .?. 0x0010
  isVolatile (AccessFlag af) = af .?. 0x0040
  isTransient (AccessFlag af) = af .?. 0x0080
  isSynthetic (AccessFlag af) = af .?. 0x1000
  isEnum (AccessFlag af) = af .?. 0x4000

data ClassFile = ClassFile
  { minorVersion :: Word16
  , majorVersion :: Word16
  , constantPool :: ConstantPool
  , accessFlags  :: AccessFlag
  , thisClass    :: ClassIndex
  , superClass   :: ClassIndex
  , interfaces   :: Interfaces
  , fields       :: Fields
  , methods      :: Methods
  , attrs        :: Attributes
  } deriving (Show, Eq)

type ConstantPool = ConstantPool' ConstantPoolInfo

newtype ConstantPool' a =
  ConstantPool [a]
  deriving (Eq, Foldable)

instance Show a => Show (ConstantPool' a) where
  show (ConstantPool l) = showList (Just 1) (Just "ConstantPool") l

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo
  -- class_index
  = CpClass ClassIndex
  -- class_index, name_and_type_index
  | CpFieldRef ClassIndex
               NameAndTypeIndex
  | CpMethodRef ClassIndex
                NameAndTypeIndex
  | CpInterfaceMethodRef ClassIndex
                         NameAndTypeIndex
  | CpString StringIndex
  -- bytes
  | CpInteger Int32
  | CpFloat Float
  -- high_bytes, low_bytes
  | CpLong Int64
  | CpDouble Double
  -- name_index, descriptor_index
  | CpNameAndType NameIndex
                  DescIndex
  | CpInfo ByteString
  | CpMethodHandle CpMethodHandle
                   RefIndex
  -- descriptor_index
  | CpMethodType DescIndex
  -- bootstrap_method_attr_index, name_and_type_index
  | CpInvokeDynamic Index
                    NameAndTypeIndex
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
  deriving (Eq)

instance Show AccessFlag where
  show (AccessFlag f) = "AccessFlag " ++ hexString f

type Interfaces = Interfaces' InterfaceInfo

newtype Interfaces' l =
  Interfaces [l]
  deriving (Eq, Foldable)

instance Show a => Show (Interfaces' a) where
  show (Interfaces l) = showList (Just 0) (Just "Interfaces") l

newtype InterfaceInfo =
  InterfaceInfo ClassIndex
  deriving (Show, Eq)

type Fields = Fields' FieldInfo

newtype Fields' l =
  Fields [l]
  deriving (Eq, Foldable)

instance Show a => Show (Fields' a) where
  show (Fields l) = showList (Just 0) (Just "Fields") l

data FieldInfo = FieldInfo
  { fAccessFlags :: AccessFlag
  , fNameIndex   :: NameIndex
  , fDescIndex   :: DescIndex
  , fAttrs       :: Attributes
  } deriving (Show, Eq)

instance HasAccessFlag FieldInfo where
  _getAccessFlag = fAccessFlags

instance AccessInfo FieldInfo

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
  deriving (Eq, Foldable)

instance Show a => Show (Methods' a) where
  show (Methods l) = showList (Just 0) (Just "Methods") l

data MethodInfo = MethodInfo
  { mAccessFlags :: AccessFlag
  , mNameIndex   :: NameIndex
  , mDescIndex   :: DescIndex
  , mAttrs       :: Attributes
  } deriving (Show, Eq)

instance HasAccessFlag MethodInfo where
  _getAccessFlag = mAccessFlags

instance AccessInfo MethodInfo

type Attributes = Attributes' AttributeInfo

newtype Attributes' l =
  Attributes [l]
  deriving (Eq, Foldable)

instance Show a => Show (Attributes' a) where
  show (Attributes l) = showList (Just 0) (Just "Attributes") l

data AttributeInfo =
  AttributeInfo Index
                ByteString
  deriving (Show, Eq)

type ExceptionTables = ExceptionTables' ExceptionTable

newtype ExceptionTables' l =
  ExceptionTables [l]
  deriving (Eq, Foldable)

instance Show a => Show (ExceptionTables' a) where
  show (ExceptionTables l) = showList (Just 0) (Just "ExceptionTables") l

data ExceptionTable = ExceptionTable
  { eStartPc   :: Word16
  , eEndPc     :: Word16
  , eHandlerPc :: Word16
  , eCatchType :: Word16
  } deriving (Show, Eq)
