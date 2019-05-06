{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html

Note that this represents the first iteration, where we focus on parsing.
All instances of indices are left as is, and no verification is made
with regards to the indices.
-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module DData where

import           Base
import           Data.Bits (Bits, (.&.))
import           IRData

type Index = Word16

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

data ClassFile' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attrIndex attr = ClassFile
  { minorVersion :: Word16
  , majorVersion :: Word16
  , constantPool :: ConstantPool' (ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attrIndex)
  , accessFlag :: AccessFlag
  , thisClass :: classIndex
  , superClass :: classIndex
  , interfaces :: Interfaces' (InterfaceInfo' classIndex)
  , fields :: Fields' (FieldInfo' nameIndex descIndex attr)
  , methods :: Methods' (MethodInfo' nameIndex descIndex attr)
  , attrs :: Attributes' attr
  } deriving (Show, Eq)

instance ( Convertible c e classIndex classIndex'
         , Convertible c e nameIndex nameIndex'
         , Convertible c e descIndex descIndex'
         , Convertible c e nameAndTypeIndex nameAndTypeIndex'
         , Convertible c e stringIndex stringIndex'
         , Convertible c e (CpMethodHandle, refIndex) refIndex'
         , Convertible c e attrIndex attrIndex'
         , Convertible c e attr attr'
         ) =>
         Convertible c e (ClassFile' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attrIndex attr) (ClassFile' classIndex' nameIndex' descIndex' nameAndTypeIndex' stringIndex' refIndex' attrIndex' attr') where
  convert c ClassFile {..} =
    ClassFile <$-> minorVersion <*-> majorVersion <*>
    convert c constantPool <*-> accessFlag <*>
    convert c thisClass <*>
    convert c superClass <*>
    convert c interfaces <*>
    convert c fields <*>
    convert c methods <*>
    convert c attrs

newtype ConstantPool' a =
  ConstantPool [a]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (ConstantPool' a) (ConstantPool' b) where
  convert = mapM . convert

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attrIndex
  -- class_index
  = CpClass nameIndex
  -- class_index, name_and_type_index
  | CpFieldRef classIndex
               nameAndTypeIndex
  | CpMethodRef classIndex
                nameAndTypeIndex
  | CpInterfaceMethodRef classIndex
                         nameAndTypeIndex
  | CpString stringIndex
  -- bytes
  | CpInteger Int32
  | CpFloat Float
  -- high_bytes, low_bytes
  | CpLong Int64
  | CpDouble Double
  -- name_index, descriptor_index
  | CpNameAndType nameIndex
                  descIndex
  | CpInfo ByteString
  | CpMethodHandle CpMethodHandle
                   refIndex
  -- descriptor_index
  | CpMethodType descIndex
  -- bootstrap_method_attr_index, name_and_type_index
  | CpInvokeDynamic attrIndex
                    nameAndTypeIndex
  deriving (Show, Eq)

instance ( Convertible c e classIndex classIndex'
         , Convertible c e nameIndex nameIndex'
         , Convertible c e descIndex descIndex'
         , Convertible c e nameAndTypeIndex nameAndTypeIndex'
         , Convertible c e stringIndex stringIndex'
         , Convertible c e (CpMethodHandle, refIndex) refIndex'
         , Convertible c e attrIndex attrIndex'
         ) =>
         Convertible c e (ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attrIndex) (ConstantPoolInfo' classIndex' nameIndex' descIndex' nameAndTypeIndex' stringIndex' refIndex' attrIndex') where
  convert c constantPoolInfo =
    case constantPoolInfo of
      CpClass ci -> CpClass <$> convert c ci
      CpFieldRef ci nti -> CpFieldRef <$> convert c ci <*> convert c nti
      CpMethodRef ci nti -> CpMethodRef <$> convert c ci <*> convert c nti
      CpInterfaceMethodRef ci nti ->
        CpInterfaceMethodRef <$> convert c ci <*> convert c nti
      CpString si -> CpString <$> convert c si
      CpInteger i -> pure $ CpInteger i
      CpFloat f -> pure $ CpFloat f
      CpLong l -> pure $ CpLong l
      CpDouble d -> pure $ CpDouble d
      CpNameAndType ni di -> CpNameAndType <$> convert c ni <*> convert c di
      CpInfo s -> pure $ CpInfo s
      CpMethodHandle mh ri -> CpMethodHandle <$-> mh <*> convert c (mh, ri)
      CpMethodType di -> CpMethodType <$> convert c di
      CpInvokeDynamic i nti -> CpInvokeDynamic <$> convert c i <*> convert c nti

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

newtype Interfaces' l =
  Interfaces [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (Interfaces' a) (Interfaces' b) where
  convert = mapM . convert

newtype InterfaceInfo' classIndex =
  InterfaceInfo classIndex
  deriving (Show, Eq)

instance Convertible c e classIndex classIndex' =>
         Convertible c e (InterfaceInfo' classIndex) (InterfaceInfo' classIndex') where
  convert c (InterfaceInfo i) = InterfaceInfo <$> convert c i

newtype Fields' l =
  Fields [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b => Convertible c e (Fields' a) (Fields' b) where
  convert = mapM . convert

data FieldInfo' nameIndex descIndex attr = FieldInfo
  { accessFlag :: AccessFlag
  , nameIndex  :: nameIndex
  , descIndex  :: descIndex
  , attrs      :: Attributes' attr
  } deriving (Show, Eq)

instance HasAccessFlag (FieldInfo' nameIndex descIndex attr) where
  _getAccessFlag = accessFlag

instance AccessInfo (FieldInfo' nameIndex descIndex attr)

instance ( Convertible c e nameIndex nameIndex'
         , Convertible c e descIndex descIndex'
         , Convertible c e attr attr'
         ) =>
         Convertible c e (FieldInfo' nameIndex descIndex attr) (FieldInfo' nameIndex' descIndex' attr') where
  convert c FieldInfo {..} =
    FieldInfo <$-> accessFlag <*> convert c nameIndex <*> convert c descIndex <*>
    convert c attrs

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

newtype Methods' l =
  Methods [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b => Convertible c e (Methods' a) (Methods' b) where
  convert = mapM . convert

data MethodInfo' nameIndex descIndex attr = MethodInfo
  { accessFlag :: AccessFlag
  , nameIndex  :: nameIndex
  , descIndex  :: descIndex
  , attrs      :: Attributes' attr
  } deriving (Show, Eq)

instance ( Convertible c e nameIndex nameIndex'
         , Convertible c e descIndex descIndex'
         , Convertible c e attr attr'
         ) =>
         Convertible c e (MethodInfo' nameIndex descIndex attr) (MethodInfo' nameIndex' descIndex' attr') where
  convert c MethodInfo {..} =
    MethodInfo <$-> accessFlag <*> convert c nameIndex <*> convert c descIndex <*>
    convert c attrs

instance HasAccessFlag (MethodInfo' nameIndex descIndex attr) where
  _getAccessFlag = accessFlag

instance AccessInfo (MethodInfo' nameIndex descIndex attr)

type ExceptionTables = ExceptionTables' ExceptionTable

newtype ExceptionTables' l =
  ExceptionTables [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (ExceptionTables' a) (ExceptionTables' b) where
  convert = mapM . convert

data ExceptionTable = ExceptionTable
  { startPc   :: Word16
  , endPc     :: Word16
  , handlerPc :: Word16
  , catchType :: Word16
  } deriving (Show, Eq)

newtype Attributes' l =
  Attributes [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (Attributes' a) (Attributes' b) where
  convert = mapM . convert

newtype StackLimit =
  StackLimit Word16
  deriving (Show, Eq)

newtype LocalLimit =
  LocalLimit Word16
  deriving (Show, Eq)

data AttributeInfo' classIndex nameIndex constIndex index indexw label labelw intByte intShort arrayType count
  = ACode { stackLimit :: StackLimit
          , localLimit :: LocalLimit
          , code :: Instructions' (Instruction' index indexw label labelw intByte intShort arrayType count)
          , exceptionTables :: ExceptionTables
          , attrs :: Attributes' (AttributeInfo' classIndex nameIndex constIndex index indexw label labelw intByte intShort arrayType count) }
  | AConst constIndex
  | ALineNumberTable LineNumberTable
  | AExceptions (Exceptions' (Exception' classIndex))
  | ASourceFile nameIndex
  | AInnerClasses (InnerClasses' (InnerClass' classIndex nameIndex))
  | ASynthetic
  | AGeneric (GenericAttribute' nameIndex)
  deriving (Show, Eq)

-- | Our definition here slightly differs in that we require generic conversions
-- that result in any possible attribute info
-- Given that the references are initially indices, attributes will all be forwarded to generics
-- With this pattern, we allow conversions to modify generic attributes into other types if they wish
-- If such transformations are not required, the definition may simply rewrap it into a generic attribute
instance ( Convertible c e classIndex classIndex'
         , Convertible c e nameIndex nameIndex'
         , Convertible c e constIndex constIndex'
         , Convertible c e index index'
         , Convertible c e indexw indexw'
         , Convertible c e label label'
         , Convertible c e labelw labelw'
         , Convertible c e intByte intByte'
         , Convertible c e intShort intShort'
         , Convertible c e arrayType arrayType'
         , Convertible c e count count'
         , Convertible c e (GenericAttribute' nameIndex) (AttributeInfo' classIndex' nameIndex' constIndex' index' indexw' label' labelw' intByte' intShort' arrayType' count')
         ) =>
         Convertible c e (AttributeInfo' classIndex nameIndex constIndex index indexw label labelw intByte intShort arrayType count) (AttributeInfo' classIndex' nameIndex' constIndex' index' indexw' label' labelw' intByte' intShort' arrayType' count') where
  convert c attributeInfo =
    case attributeInfo of
      ACode {..} ->
        ACode <$-> stackLimit <*-> localLimit <*>
        convert c code <*-> exceptionTables <*>
        convert c attrs
      AConst ci -> AConst <$> convert c ci
      ALineNumberTable l -> pure $ ALineNumberTable l
      AExceptions l -> AExceptions <$> convert c l
      ASourceFile l -> ASourceFile <$> convert c l
      AInnerClasses l -> AInnerClasses <$> convert c l
      ASynthetic -> pure ASynthetic
      AGeneric n -> convert c n

newtype LineNumberTable =
  LineNumberTable [LineNumberInfo]
  deriving (Show, Eq)

data LineNumberInfo = LineNumberInfo
  { startPc    :: Word16
  , lineNumber :: Word16
  } deriving (Show, Eq)

newtype Exceptions' l =
  Exceptions [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (Exceptions' a) (Exceptions' b) where
  convert = mapM . convert

-- | Derived from class info index
newtype Exception' classIndex =
  Exception classIndex
  deriving (Show, Eq)

instance Convertible c e classIndex classIndex' =>
         Convertible c e (Exception' classIndex) (Exception' classIndex') where
  convert c (Exception ci) = Exception <$> convert c ci

data GenericAttribute' nameIndex =
  GenericAttribute nameIndex
                   ByteString
  deriving (Show, Eq)

instance Convertible c e nameIndex nameIndex' =>
         Convertible c e (GenericAttribute' nameIndex) (GenericAttribute' nameIndex') where
  convert c (GenericAttribute ni s) = GenericAttribute <$> convert c ni <*-> s

newtype InnerClasses' l =
  InnerClasses [l]
  deriving (Show, Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (InnerClasses' a) (InnerClasses' b) where
  convert = mapM . convert

data InnerClass' classIndex nameIndex = InnerClass
  { innerClass :: classIndex
  , outerClass :: classIndex
  , innerName  :: nameIndex
  , accessFlag :: AccessFlag
  } deriving (Show, Eq)

instance ( Convertible c e classIndex classIndex'
         , Convertible c e nameIndex nameIndex'
         ) =>
         Convertible c e (InnerClass' classIndex nameIndex) (InnerClass' classIndex' nameIndex') where
  convert c InnerClass {..} =
    InnerClass <$> convert c innerClass <*> convert c outerClass <*>
    convert c innerName <*-> accessFlag
