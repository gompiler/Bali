{-# LANGUAGE OverloadedStrings #-}

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
  , module Data
  ) where

import           Base
import           Control.Monad              (replicateM)
import           Control.Monad.Except       (throwError)
import           Data
import           Data.Binary                (encode)
import qualified Data.Binary.Get            as G
import           Data.Void
import           Data.Word                  (Word16, Word32, Word8)
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

sc :: Parser ()
sc = L.space space1 empty empty

class DParse a where
  dparse :: Parser a
  dparse = between sc eof dparse'
  dparse' :: Parser a

instance DParse ClassFile where
  dparse' =
    ClassFile <$ magic <*> u2 "minor version" <*> u2 "major version" <*> dparse' <*>
    dparse' <*>
    u2 "this class" <*>
    u2 "super class" <*>
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
    count <- u2 "constant pool count"
    -- Note that count is one greater than the number of pool info entries
    info <- replicateM (count - 1) dparse'
    return $ ConstantPool info

instance DParse ConstantPoolInfo where
  dparse' = info =<< u1 "constant pool info"
    where
      info :: Int -> Parser ConstantPoolInfo
      info 7 = CpIndex <$> nameIndex <*-> CpClass
      info 9 = CpIndex <$> classIndex <*> (CpField <$> ntIndex)
      info 10 = CpIndex <$> classIndex <*> (CpMethod <$> ntIndex)
      info 11 = CpIndex <$> classIndex <*> (CpInterfaceMethod <$> ntIndex)
      info 8 = CpIndex <$> classIndex <*-> CpString
      info 3 = CpConst <$> (CpInteger <$> bIndex)
      info 4 = CpConst <$> (CpFloat <$> bIndex)
      info 5 = CpConst <$> (CpLong <$> bIndex <*> bIndex)
      info 6 = CpConst <$> (CpDouble <$> bIndex <*> bIndex)
      info 12 = CpIndex <$> nameIndex <*> (CpNameAndType <$> descIndex)
      info 1 =
        CpConst <$> do
          l <- u2 "length"
          b <- takeP (Just "string info") l
          return $ CpInfo (fromIntegral l) b
      info 15 = methodHandle <$> u1 "reference kind" <*> u2 "reference index"
          -- Note that the reference kind should actually be parsed first
        where
          methodHandle kind index =
            CpIndex index $
            CpMethodHandle $
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
      info 16 = CpIndex <$> descIndex <*-> CpMethodType
      info 18 =
        CpIndex <$> u2 "bootstrap method attr index" <*>
        (CpInvokeDynamic <$> ntIndex)
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
  dparse' = do
    count <- u2 "methods count"
    info <- replicateM count dparse'
    return $ Methods info

instance DParse MethodInfo where
  dparse' = MethodInfo <$> dparse' <*> nameIndex <*> descIndex <*> dparse'

instance DParse AccessFlag where
  dparse' = AccessFlag <$> u2 "access flags"

instance DParse Attributes where
  dparse' = undefined

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

u1 :: Num a => String -> Parser a
u1 err = fromIntegral <$> anySingle <?> err

u2 :: Num a => String -> Parser a
u2 err = fromIntegral . G.runGet G.getWord16be <$> takeP (Just err) 2

nameIndex :: Parser Word16
nameIndex = u2 "name index"

descIndex :: Parser Word16
descIndex = u2 "descriptor index"

-- | 'semi' parses a semicolon.
semi :: Parser ByteString
semi = symbol ";"
