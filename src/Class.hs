{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
-}
module Class where

import           Base
import           Instructions

type TODO = ()

type VarIndex = Int

type LabelName = String

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

newtype ConstantPool =
  ConstantPool [ConstantPoolInfo]
  deriving (Eq)

cpInfo :: ConstantPool -> Index -> ByteString
(ConstantPool pool) `cpInfo` i =
  case pool !! (fromIntegral i - 1) of
    CpInfo info -> info
    _ -> error $ "ConstantPool did not return info at index " ++ show i

instance Show ConstantPool where
  show (ConstantPool inf) =
    "ConstantPool\n" ++ concatMap showInfo (zip [1 ..] inf)
    where
      showInfo :: (Integer, ConstantPoolInfo) -> String
      showInfo (i, info) = "\t" ++ show i ++ ": " ++ show info ++ "\n"

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
  | CpInteger Word32
  | CpFloat Word32
  -- high_bytes, low_bytes
  | CpLong Word32
           Word32
  | CpDouble Word32
             Word32
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
data MethodDescriptor =
  MethodDescriptor [FieldDescriptor]
                   (Maybe FieldDescriptor)

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
  AttributeInfo Index
                ByteString
  deriving (Show, Eq)

data AttributeInfo' = ACode
  { stackLimit      :: Word16
  , localLimit      :: Word16
  , code            :: Instructions
  , exceptionTables :: ExceptionTables
  , cAttrs          :: Attributes
  } deriving (Show, Eq)

newtype ExceptionTables =
  ExceptionTables [ExceptionTable]
  deriving (Show, Eq)

data ExceptionTable = ExceptionTable
  { eStartPc   :: Word16
  , eEndPc     :: Word16
  , eHandlerPc :: Word16
  , eCatchType :: Word16
  } deriving (Show, Eq)
