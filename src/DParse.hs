{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Disassembler Parser, which converts from .class to .j

Note that values are stored using big-endian

References:
- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
-}
module DParse
  ( dparse
  , dparse'
  , Parser(..)
  , module Class
  ) where

import           Base
import           Class
import           Control.Monad              (replicateM)
import           Control.Monad.Except       (throwError)
import           Data.Binary                (encode)
import qualified Data.Binary.Get            as G
import           Data.ByteString.Internal   (c2w, w2c)
import           Data.ByteString.Lazy       (pack, unpack)
import           Data.Void
import           GHC.Base                   (unsafeChr)
import           Instructions
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

sc :: Parser ()
sc = L.space space1 empty empty

class DParse a where
  dparse :: Parser a
  dparse = dparse' <* eof
  dparse' :: Parser a

dparseM ::
     forall a. DParse a
  => Parser Int
  -> Parser [a]
dparseM counter = do
  count <- counter
  replicateM count (dparse' :: Parser a)

dparse2M ::
     forall a. DParse a
  => String
  -> Parser [a]
dparse2M tag = dparseM $ u2 $ tag ++ " count"

dparse4M ::
     forall a. DParse a
  => String
  -> Parser [a]
dparse4M tag = dparseM $ u4 $ tag ++ " count"

emptyConstantPool :: ConstantPool
emptyConstantPool = ConstantPool []

instance DParse ClassFile where
  dparse' =
    ClassFile <$ magic <*> u2 "minor version" <*> u2 "major version" <*> dparse' <*>
    dparse' <*>
    u2 "this class" <*>
    u2 "super class" <*>
    dparse' <*>
    dparse' <*>
    dparse' <*>
    dparse' <*>
    takeRest
    where
      magic :: Parser ByteString
      magic = chunk (encode (0xcafebabe :: Word32)) <?> "magic"

-- | Parses the constant pool
-- The first entry is u2 denoting the pool count
-- Based on the count, we then parse the appropriate number of pool info
instance DParse ConstantPool where
  dparse' = do
    count <- u2 "constant pool count"
    -- Note that count is one greater than the number of pool info entries
    info <- replicateM (count - 1) dparse'
    return $ ConstantPool info

instance DParse ConstantPoolInfo where
  dparse' = info =<< u1 "constant pool info"
    where
      info :: Int -> Parser ConstantPoolInfo
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
        CpMethodHandle <$> (refKind <$> u1 "reference kind") <*>
        u2 "reference index"
          -- Note that the reference kind should actually be parsed first
        where
          refKind kind =
            case kind of
              1 -> CpmGetField
              2 -> CpmGetStatic
              3 -> CpmPutField
              4 -> CpmPutStatic
              5 -> CpmInvokeVirtual
              6 -> CpmInvokeStatic
              7 -> CpmInvokeSpecial
              8 -> CpmNewInvokeSpecial
              9 -> CpmInvokeInterface
              _ -> error "Invalid reference kind" -- TODO throw parsec error
      info 16 = CpMethodType <$> descIndex
      info 18 = CpInvokeDynamic <$> u2 "bootstrap method attr index" <*> ntIndex
      info _ = error "Invalid tag" -- TODO throw parsec error
      classIndex = u2 "class index"
      ntIndex = u2 "name and type index"
      bIndex = u2 "bytes"

instance DParse Interfaces where
  dparse' = do
    count <- u2 "interfaces count"
    info <- replicateM count (u2 "interfaces")
    return $ Interfaces info

instance DParse Fields where
  dparse' = do
    count <- u2 "fields count"
    info <- replicateM count dparse'
    return $ Fields info

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
    count <- u4 "attribute length"
    b <- takeP (Just "attribute info") count
    return $ AttributeInfo name b

codeAttribute cp = do
  stackLimit <- u2 "max stack"
  localLimit <- u2 "max locals"
  code <- dparse'
  exceptionTables <- dparse'
  attrs <- dparse'
  return $
    ACode
      { stackLimit = stackLimit
      , localLimit = localLimit
      , code = code
      , exceptionTables = exceptionTables
      , cAttrs = attrs
      }

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
      _ -> error "Invalid field type"
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
