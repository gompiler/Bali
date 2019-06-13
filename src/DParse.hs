{-|
Module      : DParse
Description : Disassembler parser, which takes in .class files
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3

Note that values are stored using big-endian

References:
- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module DParse
  ( dparser
  , DParseError
  , dparseAttribute
  ) where

import           Base
import           BaseByteParse
import           Control.Monad            (replicateM)
import           D1Data
import           Data.Binary              (encode)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.ByteString.Lazy     (pack)
import           DData
import           IR1Data
import           IRData
import           IRParse                  (IRParseError)
import           Text.Megaparsec

type Parser a = Parser' DParseError a

dparser :: Parser ClassFile
dparser = p

data DParseError
  = InvalidConstantPoolTag Word8
  | InvalidRefKind Word8
  | InvalidFieldDescriptor Char
  | IRError IRParseError
  deriving (Show, Eq, Ord)

instance ShowErrorComponent DParseError where
  showErrorComponent err =
    case err of
      InvalidConstantPoolTag i -> "Invalid constant pool tag " ++ show i
      InvalidRefKind i         -> "Invalid reference kind " ++ show i
      InvalidFieldDescriptor c -> "Invalid field descriptor " ++ show c
      IRError e                -> showErrorComponent e

parseM :: Integral n => Parser n -> Parser a -> Parser [a]
parseM counter parser = do
  c <- counter
  replicateM (fromIntegral c) parser

parse2M :: Parse DParseError a => String -> Parser [a]
parse2M tag = parseM ((p' :: Parser Word16) <?> tag ++ " count") p'

instance Parse DParseError ClassFile where
  p' =
    ClassFile <$ magic <*> (p' <?> "minor version") <*> (p' <?> "major version") <*>
    p' <*>
    p' <*>
    (p' <?> "this class") <*>
    (p' <?> "super class") <*>
    p' <*>
    p' <*>
    p' <*>
    p'
    where
      magic :: Parser ByteString
      magic = chunk (encode (0xcafebabe :: Word32)) <?> "magic"

-- | Parses the constant pool
-- The first entry is u2 denoting the pool count
-- Based on the count, we then parse the appropriate number of pool info
instance Parse DParseError ConstantPool where
  p' = do
    c :: Word16 <- p' <?> "constant pool count"
    -- Note that count is one greater than the number of pool info entries
    info <- replicateM (fromIntegral c - 1) p'
    return $ ConstantPool info

instance Parse DParseError ConstantPoolInfo where
  p' = info =<< p' <?> "constant pool info"
    where
      info :: Word8 -> Parser ConstantPoolInfo
      info 7 = CpClass <$> p'
      info 9 = CpFieldRef <$> p' <*> p'
      info 10 = CpMethodRef <$> p' <*> p'
      info 11 = CpInterfaceMethodRef <$> p' <*> p'
      info 8 = CpString <$> p'
      info 3 = CpInteger <$> (p' <?> "integer")
      info 4 = CpFloat <$> (p' <?> "float") -- TODO verify float conversion
      info 5 = CpLong <$> (p' <?> "long")
      info 6 = CpDouble <$> (p' <?> "double")
      info 12 = CpNameAndType <$> p' <*> p'
      info 1 = do
        l :: Word16 <- p' <?> "length"
        b <- takeP (Just "string info") (fromIntegral l)
        return $ CpInfo b
      info 15 = CpMethodHandle <$> (refKind =<< p' <?> "reference kind") <*> p'
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
              _ -> parseFailure $ InvalidRefKind kind
      info 16 = CpMethodType <$> p'
      info 18 = CpInvokeDynamic <$> p' <*> p'
      info i = parseFailure $ InvalidConstantPoolTag i

instance Parse DParseError NameIndex where
  p' = NameIndex <$> (p' <?> "name index")

instance Parse DParseError ClassIndex where
  p' = ClassIndex <$> (p' <?> "class index")

instance Parse DParseError NameAndTypeIndex where
  p' = NameAndTypeIndex <$> (p' <?> "name and type index")

instance Parse DParseError DescIndex where
  p' = DescIndex <$> (p' <?> "desc index")

instance Parse DParseError StringIndex where
  p' = StringIndex <$> (p' <?> "string index")

instance Parse DParseError RefIndex where
  p' = RefIndex <$> (p' <?> "reference index")

instance Parse DParseError AttrIndex where
  p' = AttrIndex <$> (p' <?> "attr index")

instance Parse DParseError ConstIndex where
  p' = ConstIndex <$> (p' <?> "constant index")

instance Parse DParseError Interfaces where
  p' = Interfaces <$> parse2M "interfaces"

instance Parse DParseError InterfaceInfo where
  p' = InterfaceInfo <$> p'

instance Parse DParseError Fields where
  p' = Fields <$> parse2M "fields"

instance Parse DParseError FieldInfo where
  p' = FieldInfo <$> p' <*> p' <*> p' <*> p'

instance Parse DParseError Methods where
  p' = Methods <$> parse2M "methods"

instance Parse DParseError MethodInfo where
  p' = MethodInfo <$> p' <*> p' <*> p' <*> p'

instance Parse DParseError AccessFlag where
  p' = AccessFlag <$> (p' <?> "access flags")

instance Parse DParseError Attributes where
  p' = Attributes <$> parse2M "attributes"

instance Parse DParseError AttributeInfo where
  p' = AGeneric <$> p'

dparseAttribute :: ByteString -> Maybe (Parser AttributeInfo)
dparseAttribute name =
  case name of
    "Code"            -> Just $ ACode <$> p' <*> p' <*> p' <*> p' <*> p'
    "ConstantValue"   -> Just $ AConst <$> p'
    "LineNumberTable" -> Just $ ALineNumberTable <$> p'
    "SourceFile"      -> Just $ ASourceFile <$> p'
    "InnerClasses"    -> Just $ AInnerClasses <$> p'
    "Synthetic"       -> Just $ pure ASynthetic
    _                 -> Nothing

instance Parse DParseError GenericAttribute where
  p' = do
    name <- p' <?> "attribute name index"
    c :: Word32 <- p' <?> "attribute length"
    b <- takeP (Just "attribute info") (fromIntegral c)
    return $ GenericAttribute name b

instance Parse DParseError InnerClasses where
  p' = InnerClasses <$> parse2M "inner classes"

instance Parse DParseError InnerClass where
  p' =
    InnerClass <$> (p' <?> "inner class") <*> (p' <?> "outer class") <*>
    (p' <?> "inner name") <*>
    p'

instance Parse DParseError Exceptions where
  p' = Exceptions <$> parse2M "exceptions"

instance Parse DParseError Exception where
  p' = Exception <$> (p' <?> "exception")

instance Parse DParseError FieldDescriptor where
  p' = do
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
      '[' -> TArray <$> p'
      c -> parseFailure $ InvalidFieldDescriptor c
    where
      semicolon :: Word8
      semicolon = c2w ';'

instance Parse DParseError Instructions where
  p' = do
    c :: Word32 <- p' <?> "code count"
    content <- takeP (Just "code block") (fromIntegral c)
    let parser = many (p' :: Parser' IRParseError Instruction) <* eof
    instrs <- nestedParse parser content
    return $ Instructions instrs

instance Parse DParseError ExceptionTables where
  p' = ExceptionTables <$> parse2M "exception table"

instance Parse DParseError ExceptionTable where
  p' =
    ExceptionTable <$> (p' <?> "start pointer") <*> (p' <?> "end pointer") <*>
    (p' <?> "handler pointer") <*>
    (p' <?> "catch pointer")

instance Parse DParseError StackLimit where
  p' = StackLimit <$> (p' <?> "max stack")

instance Parse DParseError LocalLimit where
  p' = LocalLimit <$> (p' <?> "max local")

instance Parse DParseError LineNumberTable where
  p' = LineNumberTable <$> parse2M "line number table"

instance Parse DParseError LineNumberInfo where
  p' = LineNumberInfo <$> (p' <?> "start pc") <*> (p' <?> "line number")
--instance Parse DParseError Exceptions where
--  p' = Exceptions <$> parse2M "exceptions"
