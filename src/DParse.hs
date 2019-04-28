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
import           Data
import           Data.Binary                (encode)
import qualified Data.Binary.Get            as G
import           Data.ByteString.Lazy       (ByteString)
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
    ClassFile <$ magic <*> u2 "minor version" <*> u2 "major version" <*>
    u2 "constant pool count"
    where
      magic :: Parser ByteString
      magic = chunk (encode (0xcafebabe :: Word32)) <?> "magic"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

u2 :: Num a => String -> Parser a
u2 err = fromIntegral . G.runGet G.getWord16be <$> takeP (Just err) 2

-- | 'semi' parses a semicolon.
semi :: Parser ByteString
semi = symbol ";"
