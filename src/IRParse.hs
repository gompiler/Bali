{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module IRParse
  ( irparser
  , IRParseError
  ) where

import           Base
import           BaseByteParse
import           IR1Data
import           IRData
import           Text.Megaparsec

data IRParseError
  = InvalidOpCode Word8
  | InvalidArrayType Word8
  deriving (Show, Ord, Eq)

instance ShowErrorComponent IRParseError where
  showErrorComponent err =
    case err of
      InvalidArrayType i -> "Invalid array type " ++ show i
      InvalidOpCode i    -> "Invalid op code " ++ hexString i

type Parser a = Parser' IRParseError a

irparser :: Parser Instructions
irparser = p

instance Parse IRParseError IRIndex where
  p' = IRIndex <$> (p' <?> "index")

instance Parse IRParseError IRIndexw where
  p' = IRIndexw <$> (p' <?> "index 2")

instance Parse IRParseError IRLabel where
  p' = IRLabel <$> (p' <?> "label")

instance Parse IRParseError IRLabelw where
  p' = IRLabelw <$> (p' <?> "labelw")

instance Parse IRParseError IntByte where
  p' = IntByte <$> (p' <?> "int byte")

instance Parse IRParseError IntShort where
  p' = IntShort <$> (p' <?> "int short")

instance Parse IRParseError Count where
  p' = Count <$> (p' <?> "count")

instance Parse IRParseError ArrayType where
  p' = do
    tag <- p' <?> "array type"
    case tag of
      4  -> pure AtBool
      5  -> pure AtChar
      6  -> pure AtFloat
      7  -> pure AtDouble
      8  -> pure AtByte
      9  -> pure AtShort
      10 -> pure AtInt
      11 -> pure AtLong
      _  -> parseFailure $ InvalidArrayType tag

instance Parse IRParseError Instructions where
  p' = Instructions <$> many p'

instance Parse IRParseError Instruction where
  p' = instr =<< p'
    where
      instr :: Word8 -> Parser Instruction
      instr op =
        case op of
          0x32 -> pure Aaload
          0x53 -> pure Aastore
          0x01 -> pure AconstNull
          0x19 -> Aload <$> p'
          0x2a -> pure Aload0
          0x2b -> pure Aload1
          0x2c -> pure Aload2
          0x2d -> pure Aload3
          0xbd -> Anewarray <$> p'
          0xb0 -> pure Areturn
          0xbe -> pure Arraylength
          0x3a -> Astore <$> p'
          0x4b -> pure Astore0
          0x4c -> pure Astore1
          0x4d -> pure Astore2
          0x4e -> pure Astore3
          0xbf -> pure Athrow
          0x33 -> pure Baload
          0x54 -> pure Bastore
          0x10 -> Bipush <$> p'
          0xca -> pure Breakpoint
          0x34 -> pure Caload
          0x55 -> pure Castore
          0xc0 -> Checkcast <$> p'
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
          0x18 -> Dload <$> p'
          0x26 -> pure Dload0
          0x27 -> pure Dload1
          0x28 -> pure Dload2
          0x29 -> pure Dload3
          0x6b -> pure Dmul
          0x77 -> pure Dneg
          0x73 -> pure Drem
          0xaf -> pure Dreturn
          0x39 -> Dstore <$> p'
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
          0x17 -> Fload <$> p'
          0x22 -> pure Fload0
          0x23 -> pure Fload1
          0x24 -> pure Fload2
          0x25 -> pure Fload3
          0x6a -> pure Fmul
          0x76 -> pure Fneg
          0x72 -> pure Frem
          0xae -> pure Freturn
          0x38 -> Fstore <$> p'
          0x43 -> pure Fstore0
          0x44 -> pure Fstore1
          0x45 -> pure Fstore2
          0x46 -> pure Fstore3
          0x66 -> pure Fsub
          0xb4 -> Getfield <$> p'
          0xb2 -> Getstatic <$> p'
          0xa7 -> Goto <$> p'
          0xc8 -> GotoW <$> p'
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
          0xa5 -> IfAcmpeq <$> p'
          0xa6 -> IfAcmpne <$> p'
          0x9f -> IfIcmpeq <$> p'
          0xa2 -> IfIcmpge <$> p'
          0xa3 -> IfIcmpgt <$> p'
          0xa4 -> IfIcmple <$> p'
          0xa1 -> IfIcmplt <$> p'
          0xa0 -> IfIcmpne <$> p'
          0x99 -> Ifeq <$> p'
          0x9c -> Ifge <$> p'
          0x9d -> Ifgt <$> p'
          0x9e -> Ifle <$> p'
          0x9b -> Iflt <$> p'
          0x9a -> Ifne <$> p'
          0xc7 -> Ifnonnull <$> p'
          0xc6 -> Ifnull <$> p'
          0x84 -> Iinc <$> p' <*> p'
          0x15 -> Iload <$> p'
          0x1a -> pure Iload0
          0x1b -> pure Iload1
          0x1c -> pure Iload2
          0x1d -> pure Iload3
          0x68 -> pure Imul
          0x74 -> pure Ineg
          0xc1 -> Instanceof <$> p'
          0xba -> Invokedynamic <$> p'
          0xb9 -> Invokeinterface <$> p' <*> p'
          0xb7 -> Invokespecial <$> p'
          0xb8 -> Invokestatic <$> p'
          0xb6 -> Invokevirtual <$> p'
          0x80 -> pure Ior
          0x70 -> pure Irem
          0xac -> pure Ireturn
          0x78 -> pure Ishl
          0x7a -> pure Ishr
          0x36 -> Istore <$> p'
          0x3b -> pure Istore0
          0x3c -> pure Istore1
          0x3d -> pure Istore2
          0x3e -> pure Istore3
          0x64 -> pure Isub
          0x7c -> pure Iushr
          0x82 -> pure Ixor
          0xa8 -> Jsr <$> p'
          0xc9 -> JsrW <$> p'
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
          0x12 -> Ldc <$> p'
          0x14 -> Ldc2W <$> p'
          0x13 -> LdcW <$> p'
          0x6d -> pure Ldiv
          0x16 -> Lload <$> p'
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
          0x37 -> Lstore <$> p'
          0x3f -> pure Lstore0
          0x40 -> pure Lstore1
          0x41 -> pure Lstore2
          0x42 -> pure Lstore3
          0x65 -> pure Lsub
          0x7d -> pure Lushr
          0x83 -> pure Lxor
          0xc2 -> pure Monitorenter
          0xc3 -> pure Monitorexit
          0xc5 -> Multianewarray <$> p' <*> p'
          0xbb -> New <$> p'
          0xbc -> Newarray <$> p'
          0x00 -> pure Nop
          0x57 -> pure Pop
          0x58 -> pure Pop2
          0xb5 -> Putfield <$> p'
          0xb3 -> Putstatic <$> p'
          0xa9 -> Ret <$> p'
          0xb1 -> pure Return
          0x35 -> pure Saload
          0x56 -> pure Sastore
          0x11 -> Sipush <$> p'
          0x5f -> pure Swap
          _    -> parseFailure $ InvalidOpCode op
