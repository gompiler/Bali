{-
Disassembler Parser, which converts from .class to .j

Note that values are stored using big-endian

References:
- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
-}
{-# LANGUAGE FlexibleInstances #-}

module DParse
  ( dparse
  , DParser
  , Parser
  , module DData
  ) where

import           Base
import           Control.Monad            (replicateM)
import           Data.Binary              (encode)
import qualified Data.Binary.Get          as G
import           Data.ByteString.Internal (c2w, w2c)
import           Data.ByteString.Lazy     (pack)
import           DData
import           Instructions
import           Text.Megaparsec

--import           Text.Megaparsec.Byte
type Parser = Parsec DParseError ByteString

type DParser = Parser ClassFile

data DParseError
  = InvalidConstantPoolTag Integer
  | InvalidRefKind Integer
  | InvalidFieldDescriptor Char
  | General String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent DParseError where
  showErrorComponent err =
    case err of
      InvalidConstantPoolTag i -> "Invalid constant pool tag " ++ show i
      InvalidRefKind i         -> "Invalid reference kind " ++ show i
      InvalidFieldDescriptor c -> "Invalid field descriptor " ++ show c
      General s                -> s

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
      info 3 = CpInteger <$> bIndex
      info 4 = CpFloat <$> bIndex
      info 5 = CpLong <$> bIndex <*> bIndex
      info 6 = CpDouble <$> bIndex <*> bIndex
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
      bIndex = u2 "bytes"

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

--codeAttribute cp = do
--  stackLimit <- u2 "max stack"
--  localLimit <- u2 "max locals"
--  code <- dparse'
--  exceptionTables <- dparse'
--  attrs <- dparse'
--  return $
--    ACode
--      { stackLimit = stackLimit
--      , localLimit = localLimit
--      , code = code
--      , exceptionTables = exceptionTables
--      , cAttrs = attrs
--      }
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
  dparse' = Instructions <$> dparse4M "code"

instance DParse Instruction where
  dparse' = undefined

instance DParse ExceptionTables where
  dparse' = ExceptionTables <$> dparse2M "exception table"

instance DParse ExceptionTable where
  dparse' = undefined

u1 :: Num a => String -> Parser a
u1 err = fromIntegral <$> anySingle <?> err

u2 :: Num a => String -> Parser a
u2 err = fromIntegral . G.runGet G.getWord16be <$> takeP (Just err) 2

u4 :: Num a => String -> Parser a
u4 err = fromIntegral . G.runGet G.getWord32be <$> takeP (Just err) 4

nameIndex :: Parser Word16
nameIndex = u2 "name index"

descIndex :: Parser Word16
descIndex = u2 "descriptor index"
