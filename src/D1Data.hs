{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html

Note that this represents the first iteration, where we focus on parsing.
All instances of indices are left as is, and no verification is made
with regards to the indices.
-}
module D1Data where

import           Base
import           DData
import           IR1Data

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

-- | Points to valid index in bootstrap_methods attribute
newtype AttrIndex =
  AttrIndex Word16
  deriving (Show, Eq)

-- | Points to one of:
-- * CONSTANT_Long
-- * CONSTANT_Float
-- * CONSTANT_Double
-- * CONSTANT_Integer
-- * CONSTANT_Integer
-- * CONSTANT_String
newtype ConstIndex =
  ConstIndex Word16
  deriving (Show, Eq)

-- | Points to one of:
-- * CONSTANT_Fieldref_info
-- * CONSTANT_Methodref_info
-- * CONSTANT_InterfaceMethodref_info
newtype RefIndex =
  RefIndex Word16
  deriving (Show, Eq)

type ClassFile
   = ClassFile' ClassIndex NameIndex DescIndex NameAndTypeIndex StringIndex RefIndex AttrIndex AttributeInfo

type ConstantPool = ConstantPool' ConstantPoolInfo

type ConstantPoolInfo
   = ConstantPoolInfo' ClassIndex NameIndex DescIndex NameAndTypeIndex StringIndex RefIndex AttrIndex

type Interfaces = Interfaces' InterfaceInfo

type InterfaceInfo = InterfaceInfo' ClassIndex

type Fields = Fields' FieldInfo

type FieldInfo = FieldInfo' NameIndex DescIndex AttributeInfo

type Methods = Methods' MethodInfo

type MethodInfo = MethodInfo' NameIndex DescIndex AttributeInfo

type Attributes = Attributes' AttributeInfo

type AttributeInfo
   = AttributeInfo' ClassIndex NameIndex ConstIndex IRIndex IRIndexw IRLabel IRLabelw IntByte IntShort ArrayType Count

type GenericAttribute = GenericAttribute' NameIndex

type Exceptions = Exceptions' Exception

type Exception = Exception' ClassIndex
