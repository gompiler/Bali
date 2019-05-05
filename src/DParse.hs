{-
Disassembler Parser, which converts from .class to .j

Note that values are stored using big-endian

References:
- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
-}
{-# LANGUAGE FlexibleInstances #-}

module DParse
  ( DParser
  , Parser
  , DParse(..)
  , DParseError
  ) where

import           Base
import           Control.Monad              (replicateM)
import qualified D2Data                     as D2
import           Data.Binary                (encode)
import qualified Data.Binary.Get            as G
import           Data.ByteString.Internal   (c2w, w2c)
import           Data.ByteString.Lazy       (pack)
import           D1Data
import           IRData
import           IR1Data
import           Text.Megaparsec
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser' e a = Parsec e ByteString a

type Parser a = Parser' DParseError a

type DParser = Parser ClassFile

data DParseError
  = InvalidConstantPoolTag Word8
  | InvalidRefKind Word8
  | InvalidFieldDescriptor Char
  | InvalidArrayType Word8
  | InvalidOpCode Word8
  | Generic String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent DParseError where
  showErrorComponent err =
    case err of
      InvalidConstantPoolTag i -> "Invalid constant pool tag " ++ show i
      InvalidRefKind i         -> "Invalid reference kind " ++ show i
      InvalidFieldDescriptor c -> "Invalid field descriptor " ++ show c
      InvalidArrayType i       -> "Invalid array type " ++ show i
      InvalidOpCode i          -> "Invalid op code " ++ hexString i
      Generic s                -> s

class DParse a where
  dparse :: Parser a
  dparse = dparse' <* eof
  dparse' :: Parser a

dparseM :: Integral n => Parser n -> Parser a -> Parser [a]
dparseM counter parser = do
  c <- counter
  replicateM (fromIntegral c) parser

dparse2M :: DParse a => String -> Parser [a]
dparse2M tag = dparseM (u2 $ tag ++ " count") dparse'

instance DParse ClassFile where
  dparse' =
    ClassFile <$ magic <*> u2 "minor version" <*> u2 "major version" <*>
    dparse' <*>
    dparse' <*>
    (dparse' <?> "this class") <*>
    (dparse' <?> "super class") <*>
    dparse' <*>
    dparse' <*>
    dparse' <*>
    dparse'
    where
      magic :: Parser ByteString
      magic = chunk (encode (0xcafebabe :: Word32)) <?> "magic"

-- | Parses the constant pool
-- The first entry is u2 denoting the pool count
-- Based on the count, we then parse the appropriate number of pool info
instance DParse ConstantPool where
  dparse' = do
    c <- num2 "constant pool count"
    -- Note that count is one greater than the number of pool info entries
    info <- replicateM (c - 1) dparse'
    return $ ConstantPool info

instance DParse ConstantPoolInfo where
  dparse' = info =<< u1 "constant pool info"
    where
      info :: Word8 -> Parser ConstantPoolInfo
      info 7 = CpClass <$> dparse'
      info 9 = CpFieldRef <$> dparse' <*> dparse'
      info 10 = CpMethodRef <$> dparse' <*> dparse'
      info 11 = CpInterfaceMethodRef <$> dparse' <*> dparse'
      info 8 = CpString <$> dparse'
      info 3 = CpInteger <$> num4 "integer"
      info 4 = CpFloat <$> num4 "float" -- TODO verify float conversion
      info 5 = CpLong <$> num8 "long"
      info 6 = CpDouble <$> num8 "double"
      info 12 = CpNameAndType <$> dparse' <*> dparse'
      info 1 = do
        l <- num2 "length"
        b <- takeP (Just "string info") l
        return $ CpInfo b
      info 15 =
        CpMethodHandle <$> (refKind =<< num1 "reference kind") <*> dparse'
          -- Note that the reference kind should actually be parsed first
        where
          refKind :: Word8 -> Parser CpMethodHandle
          refKind kind =
            case kind of
              1 -> pure CpmGetField
              2 -> pure CpmGetStatic
              3 -> pure CpmPutField
              4 -> pure CpmPutStatic
              5 -> pure CpmInvokeVirtual
              6 -> pure CpmInvokeStatic
              7 -> pure CpmInvokeSpecial
              8 -> pure CpmNewInvokeSpecial
              9 -> pure CpmInvokeInterface
              _ -> customFailure $ InvalidRefKind kind
      info 16 = CpMethodType <$> dparse'
      info 18 =
        CpInvokeDynamic <$> dparse' <*> dparse'
      info i = customFailure $ InvalidConstantPoolTag i

instance DParse NameIndex where
  dparse' = NameIndex <$> u2 "name index"

instance DParse ClassIndex where
  dparse' = ClassIndex <$> u2 "class index"

instance DParse NameAndTypeIndex where
  dparse' = NameAndTypeIndex <$> u2 "name and type index"

instance DParse DescIndex where
  dparse' = DescIndex <$> u2 "desc index"

instance DParse StringIndex where
  dparse' = StringIndex <$> u2 "string index"

instance DParse RefIndex where
  dparse' = RefIndex <$> u2 "reference index"

instance DParse AttrIndex where
  dparse' = AttrIndex <$> u2 "attr index"

instance DParse Interfaces where
  dparse' = Interfaces <$> dparse2M "interfaces"

instance DParse InterfaceInfo where
  dparse' = InterfaceInfo <$> dparse'

instance DParse Fields where
  dparse' = Fields <$> dparse2M "fields"

instance DParse FieldInfo where
  dparse' = FieldInfo <$> dparse' <*> dparse' <*> dparse' <*> dparse'

instance DParse Methods where
  dparse' = Methods <$> dparse2M "methods"

instance DParse MethodInfo where
  dparse' = MethodInfo <$> dparse' <*> dparse' <*> dparse' <*> dparse'

instance DParse AccessFlag where
  dparse' = AccessFlag <$> num2 "access flags"

instance DParse Attributes where
  dparse' = Attributes <$> dparse2M "attributes"

instance DParse AttributeInfo where
  dparse' = do
    name <- num2 "attribute name index"
    c <- num4 "attribute length"
    b <- takeP (Just "attribute info") c
    return $ AttributeInfo name b

instance DParse Int32 where
  dparse' = L.decimal

instance DParse Integer where
  dparse' = L.decimal

instance DParse Float where
  dparse' = L.float

instance DParse Double where
  dparse' = L.float

instance DParse FieldDescriptor where
  dparse' = do
    tag <- anySingle
    case w2c tag of
      'B' -> pure TByte
      'C' -> pure TChar
      'D' -> pure TDouble
      'F' -> pure TFloat
      'I' -> pure TInt
      'J' -> pure TLong
      'L' ->
        TRef <$> (pack <$> many (satisfy (semicolon /=))) <* single semicolon
      'S' -> pure TShort
      'Z' -> pure TBool
      '[' -> TArray <$> dparse'
      c -> customFailure $ InvalidFieldDescriptor c
    where
      semicolon :: Word8
      semicolon = c2w ';'

instance DParse ArrayType where
  dparse' = do
    tag <- num1 "array type"
    case tag of
      4  -> pure AtBool
      5  -> pure AtChar
      6  -> pure AtFloat
      7  -> pure AtDouble
      8  -> pure AtByte
      9  -> pure AtShort
      10 -> pure AtInt
      11 -> pure AtLong
      _  -> customFailure $ InvalidArrayType tag

instance DParse Instructions where
  dparse' = do
    c <- num4 "code count"
    content <- takeP (Just "code block") c
    let parser = many (dparse' :: Parser Instruction) <* eof
    -- TODO see if there is a better way of handling nested instructions
    instrs <-
      either (customFailure . Generic . errorBundlePretty) return $
      parse parser "" content
    return $ Instructions instrs

instance DParse IRIndex where
  dparse' = IRIndex <$> u1 "index"

instance DParse IRIndexw where
  dparse' = IRIndexw <$> u2 "index 2"

instance DParse IRLabel where
  dparse' = IRLabel <$> u2 "label"

instance DParse IRLabelw where
  dparse' = IRLabelw <$> u4 "labelw"

instance DParse IntByte where
  dparse' = IntByte <$> num1 "int byte"

instance DParse IntShort where
  dparse' = IntShort <$> num2 "int short"

instance DParse Count where
  dparse' = Count <$> num2 "count"

instance DParse Instruction where
  dparse' = instr =<< anySingle
    where
      instr :: Word8 -> Parser Instruction
      instr op =
        case op of
          0x32 -> pure Aaload
          0x53 -> pure Aastore
          0x01 -> pure AconstNull
          0x19 -> Aload <$> dparse'
          0x2a -> pure Aload0
          0x2b -> pure Aload1
          0x2c -> pure Aload2
          0x2d -> pure Aload3
          0xbd -> Anewarray <$> dparse'
          0xb0 -> pure Areturn
          0xbe -> pure Arraylength
          0x3a -> Astore <$> dparse'
          0x4b -> pure Astore0
          0x4c -> pure Astore1
          0x4d -> pure Astore2
          0x4e -> pure Astore3
          0xbf -> pure Athrow
          0x33 -> pure Baload
          0x54 -> pure Bastore
          0x10 -> Bipush <$> dparse'
          0xca -> pure Breakpoint
          0x34 -> pure Caload
          0x55 -> pure Castore
          0xc0 -> Checkcast <$> dparse'
          0x90 -> pure D2f
          0x8e -> pure D2i
          0x8f -> pure D2l
          0x63 -> pure Dadd
          0x31 -> pure Daload
          0x52 -> pure Dastore
          0x98 -> pure Dcmpg
          0x97 -> pure Dcmpl
          0x0e -> pure Dconst0
          0x0f -> pure Dconst1
          0x6f -> pure Ddiv
          0x18 -> Dload <$> dparse'
          0x26 -> pure Dload0
          0x27 -> pure Dload1
          0x28 -> pure Dload2
          0x29 -> pure Dload3
          0x6b -> pure Dmul
          0x77 -> pure Dneg
          0x73 -> pure Drem
          0xaf -> pure Dreturn
          0x39 -> Dstore <$> dparse'
          0x47 -> pure Dstore0
          0x48 -> pure Dstore1
          0x49 -> pure Dstore2
          0x4a -> pure Dstore3
          0x67 -> pure Dsub
          0x59 -> pure Dup
          0x5c -> pure Dup2
          0x5d -> pure Dup2X1
          0x5e -> pure Dup2X2
          0x5a -> pure DupX1
          0x5b -> pure DupX2
          0x8d -> pure F2d
          0x8b -> pure F2i
          0x8c -> pure F2l
          0x62 -> pure Fadd
          0x30 -> pure Faload
          0x51 -> pure Fastore
          0x96 -> pure Fcmpg
          0x95 -> pure Fcmpl
          0x0b -> pure Fconst0
          0x0c -> pure Fconst1
          0x0d -> pure Fconst2
          0x6e -> pure Fdiv
          0x17 -> Fload <$> dparse'
          0x22 -> pure Fload0
          0x23 -> pure Fload1
          0x24 -> pure Fload2
          0x25 -> pure Fload3
          0x6a -> pure Fmul
          0x76 -> pure Fneg
          0x72 -> pure Frem
          0xae -> pure Freturn
          0x38 -> Fstore <$> dparse'
          0x43 -> pure Fstore0
          0x44 -> pure Fstore1
          0x45 -> pure Fstore2
          0x46 -> pure Fstore3
          0x66 -> pure Fsub
          0xb4 -> Getfield <$> dparse'
          0xb2 -> Getstatic <$> dparse'
          0xa7 -> Goto <$> dparse'
          0xc8 -> GotoW <$> dparse'
          0x91 -> pure I2b
          0x92 -> pure I2c
          0x87 -> pure I2d
          0x86 -> pure I2f
          0x85 -> pure I2l
          0x93 -> pure I2s
          0x60 -> pure Iadd
          0x2e -> pure Iaload
          0x7e -> pure Iand
          0x4f -> pure Iastore
          0x03 -> pure Iconst0
          0x04 -> pure Iconst1
          0x05 -> pure Iconst2
          0x06 -> pure Iconst3
          0x07 -> pure Iconst4
          0x08 -> pure Iconst5
          0x02 -> pure IconstM1
          0x6c -> pure Idiv
          0xa5 -> IfAcmpeq <$> dparse'
          0xa6 -> IfAcmpne <$> dparse'
          0x9f -> IfIcmpeq <$> dparse'
          0xa2 -> IfIcmpge <$> dparse'
          0xa3 -> IfIcmpgt <$> dparse'
          0xa4 -> IfIcmple <$> dparse'
          0xa1 -> IfIcmplt <$> dparse'
          0xa0 -> IfIcmpne <$> dparse'
          0x99 -> Ifeq <$> dparse'
          0x9c -> Ifge <$> dparse'
          0x9d -> Ifgt <$> dparse'
          0x9e -> Ifle <$> dparse'
          0x9b -> Iflt <$> dparse'
          0x9a -> Ifne <$> dparse'
          0xc7 -> Ifnonnull <$> dparse'
          0xc6 -> Ifnull <$> dparse'
          0x84 -> Iinc <$> dparse' <*> dparse'
          0x15 -> Iload <$> dparse'
          0x1a -> pure Iload0
          0x1b -> pure Iload1
          0x1c -> pure Iload2
          0x1d -> pure Iload3
          0x68 -> pure Imul
          0x74 -> pure Ineg
          0xc1 -> Instanceof <$> dparse'
          0xba -> Invokedynamic <$> dparse'
          0xb9 -> Invokeinterface <$> dparse' <*> dparse'
          0xb7 -> Invokespecial <$> dparse'
          0xb8 -> Invokestatic <$> dparse'
          0xb6 -> Invokevirtual <$> dparse'
          0x80 -> pure Ior
          0x70 -> pure Irem
          0xac -> pure Ireturn
          0x78 -> pure Ishl
          0x7a -> pure Ishr
          0x36 -> Istore <$> dparse'
          0x3b -> pure Istore0
          0x3c -> pure Istore1
          0x3d -> pure Istore2
          0x3e -> pure Istore3
          0x64 -> pure Isub
          0x7c -> pure Iushr
          0x82 -> pure Ixor
          0xa8 -> Jsr <$> dparse'
          0xc9 -> JsrW <$> dparse'
          0x8a -> pure L2d
          0x89 -> pure L2f
          0x88 -> pure L2i
          0x61 -> pure Ladd
          0x2f -> pure Laload
          0x7f -> pure Land
          0x50 -> pure Lastore
          0x94 -> pure Lcmp
          0x09 -> pure Lconst0
          0x0a -> pure Lconst1
          0x12 -> Ldc <$> dparse'
          0x14 -> Ldc2W <$> dparse'
          0x13 -> LdcW <$> dparse'
          0x6d -> pure Ldiv
          0x16 -> Lload <$> dparse'
          0x1e -> pure Lload0
          0x1f -> pure Lload1
          0x20 -> pure Lload2
          0x21 -> pure Lload3
          0x69 -> pure Lmul
          0x75 -> pure Lneg
          0x81 -> pure Lor
          0x71 -> pure Lrem
          0xad -> pure Lreturn
          0x79 -> pure Lshl
          0x7b -> pure Lshr
          0x37 -> Lstore <$> dparse'
          0x3f -> pure Lstore0
          0x40 -> pure Lstore1
          0x41 -> pure Lstore2
          0x42 -> pure Lstore3
          0x65 -> pure Lsub
          0x7d -> pure Lushr
          0x83 -> pure Lxor
          0xc2 -> pure Monitorenter
          0xc3 -> pure Monitorexit
          0xc5 -> Multianewarray <$> dparse' <*> dparse'
          0xbb -> New <$> dparse'
          0xbc -> Newarray <$> dparse'
          0x00 -> pure Nop
          0x57 -> pure Pop
          0x58 -> pure Pop2
          0xb5 -> Putfield <$> dparse'
          0xb3 -> Putstatic <$> dparse'
          0xa9 -> Ret <$> dparse'
          0xb1 -> pure Return
          0x35 -> pure Saload
          0x56 -> pure Sastore
          0x11 -> Sipush <$> dparse'
          0x5f -> pure Swap
          _    -> customFailure $ InvalidOpCode op

instance DParse ExceptionTables where
  dparse' = ExceptionTables <$> dparse2M "exception table"

instance DParse ExceptionTable where
  dparse' =
    ExceptionTable <$> num2 "start pointer" <*> num2 "end pointer" <*>
    num2 "handler pointer" <*>
    num2 "catch pointer"

instance DParse D2.StackLimit where
  dparse' = D2.StackLimit <$> u2 "max stack"

instance DParse D2.LocalLimit where
  dparse' = D2.LocalLimit <$> u2 "max local"

u1 :: (Ord e) => String -> Parser' e Word8
u1 err = anySingle <?> err

u2 :: (Ord e) => String -> Parser' e Word16
u2 err = G.runGet G.getWord16be <$> takeP (Just err) 2

u4 :: (Ord e) => String -> Parser' e Word32
u4 err = G.runGet G.getWord32be <$> takeP (Just err) 4

u8 :: (Ord e) => String -> Parser' e Word64
u8 err = G.runGet G.getWord64be <$> takeP (Just err) 8

num1 :: (Ord e, Num a) => String -> Parser' e a
num1 err = fromIntegral <$> u1 err

num2 :: (Ord e, Num a) => String -> Parser' e a
num2 err = fromIntegral <$> u2 err

num4 :: (Ord e, Num a) => String -> Parser' e a
num4 err = fromIntegral <$> u4 err

num8 :: (Ord e, Num a) => String -> Parser' e a
num8 err = fromIntegral <$> u8 err
