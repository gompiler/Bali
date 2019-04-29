-- | Data mapping for Java IR
-- Sources:
-- https://docs.oracle.com/javase/specs/jvms/se8/html/
-- https://en.wikibooks.org/wiki/Java_Programming/Byte_Code
-- https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html
-- https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
-- https://www.guardsquare.com/en/blog/string-concatenation-java-9-untangling-invokedynamic
-- http://www.cs.sjsu.edu/~pearce/modules/lectures/co/jvm/jasmin/demos/demos.html
-- http://homepages.inf.ed.ac.uk/kwxm/JVM/fcmpg.html
-- https://stackoverflow.com/questions/43782187/why-does-go-have-a-bit-clear-and-not-operator
module Data where

import           Base
import           Data.Word (Word16, Word32, Word8)

type TODO = ()

type VarIndex = Int

type LabelName = String

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  | FPackagePrivate
  deriving (Eq)

instance Show FieldAccess where
  show FPublic         = "public"
  show FPrivate        = "private"
  show FProtected      = "protected"
  show FPackagePrivate = ""

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
  deriving (Show, Eq)

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo
  -- | Index + associated data
  = CpIndex Word16
            CpIndex
  -- | Constant data
  | CpConst CpConst
  deriving (Show, Eq)

-- | Values within the constant pool with an associated index
-- Unless specified, extra data relates to name_and_type index
data CpIndex
  = CpClass
  | CpField Word16
  | CpMethod Word16
  | CpInterfaceMethod Word16
  | CpString
  -- | Referenced by name & type index of other index types.
  -- Points to a descriptor index
  -- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.6
  | CpNameAndType Word16
  | CpMethodHandle CpMethodHandle
  -- | Index must point to info
  | CpMethodType
  -- | First index is bootstrap_method_attr_index
  -- Attached index below is name_and_type index
  | CpInvokeDynamic Word16
  deriving (Show, Eq)

-- | Data is stored inline
data CpConst
  = CpInteger Word32
  | CpFloat Word32
  | CpLong Word32
           Word32
  | CpDouble Word32
             Word32
  -- | Represents constant string
  -- First value represents length
  -- Second value is a byte array
  -- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.7
  | CpInfo Word16
           ByteString
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

newtype Interfaces =
  Interfaces [Word16]
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
data MethodDescriptor = MethodDescriptor [FieldDescriptor] (Maybe FieldDescriptor)

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

data AttributeInfo =
  AttributeInfo Word16
                [Word8]
  deriving (Show, Eq)
