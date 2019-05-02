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
  , u1
  , u2
  , u4
  , u8
  ) where

import           Base
import           Control.Monad              (replicateM, void)
import           Data.Binary                (encode)
import qualified Data.Binary.Get            as G
import           Data.ByteString.Internal   (c2w, w2c)
import           Data.ByteString.Lazy       (pack)
import           Data.Int                   (Int32)
import           DData
import           Instructions
import           Numeric                    (showHex)
import           Text.Megaparsec
import qualified Text.Megaparsec.Byte.Lexer as L

--import           Text.Megaparsec.Byte
type Parser' e a = Parsec e ByteString a

type Parser a = Parser' DParseError a

type DParser = Parser ClassFile

data DParseError
  = InvalidConstantPoolTag Integer
  | InvalidRefKind Integer
  | InvalidFieldDescriptor Char
  | InvalidOpCode Word8
  | Generic String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent DParseError where
  showErrorComponent err =
    case err of
      InvalidConstantPoolTag i -> "Invalid constant pool tag " ++ show i
      InvalidRefKind i         -> "Invalid reference kind " ++ show i
      InvalidFieldDescriptor c -> "Invalid field descriptor " ++ show c
      InvalidOpCode i          -> "Invalid op code " ++ showHex i "0x"
      Generic s                -> s

class DParse a where
  dparse :: Parser a
  dparse = dparse' <* eof
  dparse' :: Parser a

dparseM :: Parser Int -> Parser a -> Parser [a]
dparseM counter parser = do
  c <- counter
  replicateM c parser

dparse2M :: DParse a => String -> Parser [a]
dparse2M tag = dparseM (u2 $ tag ++ " count") dparse'

dparse4M :: DParse a => String -> Parser [a]
dparse4M tag = dparseM (u4 $ tag ++ " count") dparse'

instance DParse ClassFile where
  dparse' =
    ClassFile <$ magic <*> u2 "minor version" <*> u2 "major version" <*> dparse' <*>
    dparse' <*>
    u2 "this class" <*>
    u2 "super class" <*>
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
    c <- u2 "constant pool count"
    -- Note that count is one greater than the number of pool info entries
    info <- replicateM (c - 1) dparse'
    return $ ConstantPool info

instance DParse ConstantPoolInfo where
  dparse' = info =<< u1 "constant pool info"
    where
      info :: Integer -> Parser ConstantPoolInfo
      info 7 = CpClass <$> nameIndex
      info 9 = CpFieldRef <$> classIndex <*> ntIndex
      info 10 = CpMethodRef <$> classIndex <*> ntIndex
      info 11 = CpInterfaceMethodRef <$> classIndex <*> ntIndex
      info 8 = CpString <$> classIndex
      info 3 = CpInteger <$> u4 "integer"
      info 4 = CpFloat <$> u4 "float" -- TODO verify float conversion
      info 5 = CpLong <$> u8 "long"
      info 6 = CpDouble <$> u8 "double"
      info 12 = CpNameAndType <$> nameIndex <*> descIndex
      info 1 = do
        l <- u2 "length"
        b <- takeP (Just "string info") l
        return $ CpInfo b
      info 15 =
        CpMethodHandle <$> (refKind =<< u1 "reference kind") <*>
        u2 "reference index"
          -- Note that the reference kind should actually be parsed first
        where
          refKind :: Integer -> Parser CpMethodHandle
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
      info 16 = CpMethodType <$> descIndex
      info 18 = CpInvokeDynamic <$> u2 "bootstrap method attr index" <*> ntIndex
      info i = customFailure $ InvalidConstantPoolTag i
      classIndex = u2 "class index"
      ntIndex = u2 "name and type index"

instance DParse Interfaces where
  dparse' = Interfaces <$> dparseM (u2 "interfaces count") (u2 "interfaces")

instance DParse Fields where
  dparse' = Fields <$> dparse2M "fields"

instance DParse FieldInfo where
  dparse' = FieldInfo <$> dparse' <*> nameIndex <*> descIndex <*> dparse'

instance DParse Methods where
  dparse' = Methods <$> dparse2M "methods"

instance DParse MethodInfo where
  dparse' = MethodInfo <$> dparse' <*> nameIndex <*> descIndex <*> dparse'

instance DParse AccessFlag where
  dparse' = AccessFlag <$> u2 "access flags"

instance DParse Attributes where
  dparse' = Attributes <$> dparse2M "attributes"

instance DParse AttributeInfo where
  dparse' = do
    name <- u2 "attribute name index"
    c <- u4 "attribute length"
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

instance DParse Instructions where
  dparse' = do
    c <- u4 "code count"
    content <- takeP (Just "code block") c
    let parser = many (dparse' :: Parser Instruction) <* eof
    -- TODO see if there is a better way of handling nested instructions
    instrs <-
      either (customFailure . Generic . errorBundlePretty) return $
      parse parser "" content
    return $ Instructions instrs

instance DParse Instruction where
  dparse' = instr =<< anySingle
    where
      instr :: Word8 -> Parser Instruction
      instr op =
        case op of
          0x32 -> pure Aaload
          0x33 -> pure $ Paload TpByte
          0x34 -> pure $ Paload TpChar
          0x35 -> pure $ Paload TpShort
          0x2e -> pure $ Paload TpInt
          0x2f -> pure $ Paload TpLong
          0x30 -> pure $ Paload TpFloat
          0x31 -> pure $ Paload TpDouble
          -- _    -> customFailure $ InvalidOpCode op
          _ -> pure Aaload

instance DParse ExceptionTables where
  dparse' = ExceptionTables <$> dparse2M "exception table"

instance DParse ExceptionTable where
  dparse' =
    ExceptionTable <$> u2 "start pointer" <*> u2 "end pointer" <*>
    u2 "handler pointer" <*>
    u2 "catch pointer"

u1 :: (Ord e, Num a) => String -> Parser' e a
u1 err = fromIntegral <$> anySingle <?> err

u2 :: (Ord e, Num a) => String -> Parser' e a
u2 err = fromIntegral . G.runGet G.getWord16be <$> takeP (Just err) 2

u4 :: (Ord e, Num a) => String -> Parser' e a
u4 err = fromIntegral . G.runGet G.getWord32be <$> takeP (Just err) 4

u8 :: (Ord e, Num a) => String -> Parser' e a
u8 err = fromIntegral . G.runGet G.getWord64be <$> takeP (Just err) 8

nameIndex :: Parser Word16
nameIndex = u2 "name index"

descIndex :: Parser Word16
descIndex = u2 "descriptor index"
