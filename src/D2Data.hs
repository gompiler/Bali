{-
Data mapping for Java class data
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module D2Data where

import           Base
import           D1Data  (AttrIndex (..))
import           DData
import           IR1Data

-- TODO replace AttrIndex usage
type ClassFile
   = ClassFile' ByteString ByteString ByteString NameAndTypeInfo ByteString RefInfo AttrIndex AttributeInfo

type ConstantPool = ConstantPool' ConstantPoolInfo

-- TODO replace AttrIndex usage
type ConstantPoolInfo
   = ConstantPoolInfo' ByteString ByteString ByteString NameAndTypeInfo ByteString RefInfo AttrIndex

type Interfaces = Interfaces' InterfaceInfo

type InterfaceInfo = InterfaceInfo' ByteString

type Fields = Fields' FieldInfo

type FieldInfo = FieldInfo' ByteString ByteString AttributeInfo

type Methods = Methods' MethodInfo

type MethodInfo = MethodInfo' ByteString ByteString AttributeInfo

type Attributes = Attributes' AttributeInfo

data NameAndTypeInfo = NameAndTypeInfo
  { nameInfo :: ByteString
  , typeInfo :: ByteString
  } deriving (Show, Eq)

data RefInfo = RefInfo
  { classInfo :: ByteString
  , nameInfo  :: ByteString
  , typeInfo  :: ByteString
  } deriving (Show, Eq)

refInfo :: ByteString -> NameAndTypeInfo -> RefInfo
refInfo classInfo NameAndTypeInfo {..} =
  RefInfo {classInfo = classInfo, nameInfo = nameInfo, typeInfo = typeInfo}

type AttributeInfo
   = AttributeInfo' ByteString ByteString ByteString IRIndex IRIndexw IRLabel IRLabelw IntByte IntShort ArrayType Count

type GenericAttribute = GenericAttribute' ByteString

type Exceptions = Exceptions' Exception

type Exception = Exception' ByteString