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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts #-}

module DData where

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

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  | FPackagePrivate
  deriving (Show, Eq)

type Index = Word16

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

data ClassFile' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attr = ClassFile
  { minorVersion :: Word16
  , majorVersion :: Word16
  , constantPool :: ConstantPool' (ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex)
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
         , Convertible c e attr attr'
         ) =>
         Convertible c e (ClassFile' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex attr) (ClassFile' classIndex' nameIndex' descIndex' nameAndTypeIndex' stringIndex' refIndex' attr') where
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
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (ConstantPool' a) (ConstantPool' b) where
  convert = mapM . convert

instance Show a => Show (ConstantPool' a) where
  show (ConstantPool l) = showList (Just 1) (Just "ConstantPool") l

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex
  -- class_index
  = CpClass classIndex
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
  | CpInvokeDynamic Index
                    nameAndTypeIndex
  deriving (Show, Eq)

instance ( Convertible c e classIndex classIndex'
         , Convertible c e nameIndex nameIndex'
         , Convertible c e descIndex descIndex'
         , Convertible c e nameAndTypeIndex nameAndTypeIndex'
         , Convertible c e stringIndex stringIndex'
         , Convertible c e (CpMethodHandle, refIndex) refIndex'
         ) =>
         Convertible c e (ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex) (ConstantPoolInfo' classIndex' nameIndex' descIndex' nameAndTypeIndex' stringIndex' refIndex') where
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
      CpInvokeDynamic i nti -> CpInvokeDynamic <$-> i <*> convert c nti

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
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (Interfaces' a) (Interfaces' b) where
  convert = mapM . convert

instance Show a => Show (Interfaces' a) where
  show (Interfaces l) = showList (Just 0) (Just "Interfaces") l

newtype InterfaceInfo' classIndex =
  InterfaceInfo classIndex
  deriving (Show, Eq)

instance Convertible c e classIndex classIndex' =>
         Convertible c e (InterfaceInfo' classIndex) (InterfaceInfo' classIndex') where
  convert c (InterfaceInfo i) = InterfaceInfo <$> convert c i

newtype Fields' l =
  Fields [l]
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b => Convertible c e (Fields' a) (Fields' b) where
  convert = mapM . convert

instance Show a => Show (Fields' a) where
  show (Fields l) = showList (Just 0) (Just "Fields") l

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
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b => Convertible c e (Methods' a) (Methods' b) where
  convert = mapM . convert

instance Show a => Show (Methods' a) where
  show (Methods l) = showList (Just 0) (Just "Methods") l

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

newtype Attributes' l =
  Attributes [l]
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (Attributes' a) (Attributes' b) where
  convert = mapM . convert

instance Show a => Show (Attributes' a) where
  show (Attributes l) = showList (Just 0) (Just "Attributes") l

type ExceptionTables = ExceptionTables' ExceptionTable

newtype ExceptionTables' l =
  ExceptionTables [l]
  deriving (Eq, Foldable, Functor, Traversable)

instance Convertible c e a b =>
         Convertible c e (ExceptionTables' a) (ExceptionTables' b) where
  convert = mapM . convert

instance Show a => Show (ExceptionTables' a) where
  show (ExceptionTables l) = showList (Just 0) (Just "ExceptionTables") l

data ExceptionTable = ExceptionTable
  { eStartPc   :: Word16
  , eEndPc     :: Word16
  , eHandlerPc :: Word16
  , eCatchType :: Word16
  } deriving (Show, Eq)
