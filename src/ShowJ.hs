{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ShowJ
  ( ShowJ(..)
  , printJ
  , printJ'
  , stringJ
  ) where

import           D2Data                     (AccessFlag (..), AccessInfo (..),
                                             ClassFile (..), FieldAccess (..),
                                             FieldDescriptor (..))
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intersperse)
import           Instructions
import           System.IO

tabSize :: Int
tabSize = 2

_nl :: Builder
_nl = charUtf8 '\n'

_sp :: Builder
_sp = charUtf8 ' '

_sc :: Builder
_sc = charUtf8 ';'

_tab :: Int -> Builder
_tab 0        = mempty
_tab tabCount = mconcat $ replicate (tabCount * tabSize) _sp

stringJ :: ShowJ a => a -> String
stringJ s = L.unpack $ toLazyByteString $ showJ s

printJ :: ShowJ a => a -> IO ()
printJ = printJ' 0

printJ' :: ShowJ a => Int -> a -> IO ()
printJ' tabCount s = do
  hFlush stdout
  L.putStrLn $ toLazyByteString $ showJ' tabCount s

class ShowJ a where
  {-# MINIMAL showJ | showJ' #-}
  showJ :: a -> Builder
  showJ = showJ' 0
  showJ' :: Int -> a -> Builder
  showJ' tabCount a = _tab tabCount <> showJ a

instance ShowJ ClassFile where
  showJ' tabCount ClassFile {..} =
    _tab tabCount <> byteString ".version " <> word16Dec majorVersion <> _sp <>
    word16Dec minorVersion

instance ShowJ Instructions where
  showJ' tabCount (Instructions l) =
    mconcat $ intersperse _nl $ map (showJ' tabCount) l

instance ShowJ IRIndex where
  showJ (IRIndex i) = word8Dec i

instance ShowJ IRIndexw where
  showJ (IRIndexw i) = word16Dec i

instance ShowJ IntByte where
  showJ (IntByte i) = int8Dec i

instance ShowJ IntShort where
  showJ (IntShort i) = int16Dec i

instance ShowJ IRLabel where
  showJ (IRLabel i) = word16Dec i

instance ShowJ IRLabelw where
  showJ (IRLabelw i) = word32Dec i

instance ShowJ ArrayType where
  showJ at =
    showJ $
    case at of
      AtBool   -> TBool
      AtChar   -> TChar
      AtFloat  -> TFloat
      AtDouble -> TDouble
      AtByte   -> TByte
      AtShort  -> TShort
      AtInt    -> TInt
      AtLong   -> TLong

instance ShowJ FieldDescriptor where
  showJ fd =
    case fd of
      TByte    -> charUtf8 'B'
      TChar    -> charUtf8 'C'
      TDouble  -> charUtf8 'D'
      TFloat   -> charUtf8 'F'
      TInt     -> charUtf8 'I'
      TLong    -> charUtf8 'J'
      TRef s   -> charUtf8 'L' <> lazyByteString s <> _sc
      TShort   -> charUtf8 'S'
      TBool    -> charUtf8 'Z'
      TArray d -> charUtf8 '[' <> showJ d

instance ShowJ Instruction where
  showJ inst =
    case inst of
      Aaload -> byteString "aaload"
      Aastore -> byteString "aastore"
      AconstNull -> byteString "aconst_null"
      Aload i -> byteString "aload " <> showJ i
      Aload0 -> byteString "aload_0"
      Aload1 -> byteString "aload_1"
      Aload2 -> byteString "aload_2"
      Aload3 -> byteString "aload_3"
      Anewarray i -> byteString "anewarray " <> showJ i
      Areturn -> byteString "areturn"
      Arraylength -> byteString "arraylength"
      Astore i -> byteString "astore " <> showJ i
      Astore0 -> byteString "astore_0"
      Astore1 -> byteString "astore_1"
      Astore2 -> byteString "astore_2"
      Astore3 -> byteString "astore_3"
      Athrow -> byteString "athrow"
      Baload -> byteString "baload"
      Bastore -> byteString "bastore"
      Bipush i -> byteString "bipush " <> showJ i
      Breakpoint -> byteString "breakpoint"
      Caload -> byteString "caload"
      Castore -> byteString "castore"
      Checkcast i -> byteString "checkcast" <> showJ i
      D2f -> byteString "d2f"
      D2i -> byteString "d2i"
      D2l -> byteString "d2l"
      Dadd -> byteString "dadd"
      Daload -> byteString "daload"
      Dastore -> byteString "dastore"
      Dcmpg -> byteString "dcmpg"
      Dcmpl -> byteString "dcmpl"
      Dconst0 -> byteString "dconst_0"
      Dconst1 -> byteString "dconst_1"
      Ddiv -> byteString "ddiv"
      Dload i -> byteString "dload " <> showJ i
      Dload0 -> byteString "dload_0"
      Dload1 -> byteString "dload_1"
      Dload2 -> byteString "dload_2"
      Dload3 -> byteString "dload_3"
      Dmul -> byteString "dmul"
      Dneg -> byteString "dneg"
      Drem -> byteString "drem"
      Dreturn -> byteString "dreturn"
      Dstore i -> byteString "dstore " <> showJ i
      Dstore0 -> byteString "dstore_0"
      Dstore1 -> byteString "dstore_1"
      Dstore2 -> byteString "dstore_2"
      Dstore3 -> byteString "dstore_3"
      Dsub -> byteString "dsub"
      Dup -> byteString "dup"
      Dup2 -> byteString "dup2"
      Dup2X1 -> byteString "dup2_x1"
      Dup2X2 -> byteString "dup2_x2"
      DupX1 -> byteString "dup_x1"
      DupX2 -> byteString "dup_x2"
      F2d -> byteString "f2d"
      F2i -> byteString "f2i"
      F2l -> byteString "f2l"
      Fadd -> byteString "fadd"
      Faload -> byteString "faload"
      Fastore -> byteString "fastore"
      Fcmpg -> byteString "fcmpg"
      Fcmpl -> byteString "fcmpl"
      Fconst0 -> byteString "fconst_0"
      Fconst1 -> byteString "fconst_1"
      Fconst2 -> byteString "fconst_2"
      Fdiv -> byteString "fdiv"
      Fload i -> byteString "fload " <> showJ i
      Fload0 -> byteString "fload_0"
      Fload1 -> byteString "fload_1"
      Fload2 -> byteString "fload_2"
      Fload3 -> byteString "fload_3"
      Fmul -> byteString "fmul"
      Fneg -> byteString "fneg"
      Frem -> byteString "frem"
      Freturn -> byteString "freturn"
      Fstore i -> byteString "fstore " <> showJ i
      Fstore0 -> byteString "fstore_0"
      Fstore1 -> byteString "fstore_1"
      Fstore2 -> byteString "fstore_2"
      Fstore3 -> byteString "fstore_3"
      Fsub -> byteString "fsub"
      Getfield i -> byteString "getfield " <> showJ i
      Getstatic i -> byteString "getstatic " <> showJ i
      Goto l -> byteString "goto " <> showJ l
      GotoW l -> byteString "goto_w " <> showJ l
      I2b -> byteString "i2b"
      I2c -> byteString "i2c"
      I2d -> byteString "i2d"
      I2f -> byteString "i2f"
      I2l -> byteString "i2l"
      I2s -> byteString "i2s"
      Iadd -> byteString "iadd"
      Iaload -> byteString "iaload"
      Iand -> byteString "iand"
      Iastore -> byteString "iastore"
      Iconst0 -> byteString "iconst_0"
      Iconst1 -> byteString "iconst_1"
      Iconst2 -> byteString "iconst_2"
      Iconst3 -> byteString "iconst_3"
      Iconst4 -> byteString "iconst_4"
      Iconst5 -> byteString "iconst_5"
      IconstM1 -> byteString "iconst_m1"
      Idiv -> byteString "idiv"
      IfAcmpeq l -> byteString "if_acmpeq " <> showJ l
      IfAcmpne l -> byteString "if_acmpne " <> showJ l
      IfIcmpeq l -> byteString "if_icmpeq " <> showJ l
      IfIcmpge l -> byteString "if_icmpge " <> showJ l
      IfIcmpgt l -> byteString "if_icmpgt " <> showJ l
      IfIcmple l -> byteString "if_icmple " <> showJ l
      IfIcmplt l -> byteString "if_icmplt " <> showJ l
      IfIcmpne l -> byteString "if_icmpne " <> showJ l
      Ifeq l -> byteString "ifeq " <> showJ l
      Ifge l -> byteString "ifge " <> showJ l
      Ifgt l -> byteString "ifgt " <> showJ l
      Ifle l -> byteString "ifle " <> showJ l
      Iflt l -> byteString "iflt " <> showJ l
      Ifne l -> byteString "ifne " <> showJ l
      Ifnonnull l -> byteString "ifnonnull " <> showJ l
      Ifnull l -> byteString "ifnull " <> showJ l
      Iinc i b -> byteString "iinc " <> showJ i <> _sp <> showJ b
      Iload i -> byteString "iload " <> showJ i
      Iload0 -> byteString "iload_0"
      Iload1 -> byteString "iload_1"
      Iload2 -> byteString "iload_2"
      Iload3 -> byteString "iload_3"
      Imul -> byteString "imul"
      Ineg -> byteString "ineg"
      Instanceof i -> byteString "instanceof " <> showJ i
      Invokedynamic i -> byteString "invokedynamic " <> showJ i
      Invokeinterface i b ->
        byteString "invokeinterface " <> showJ i <> _sp <> showJ b
      Invokespecial i -> byteString "invokespecial " <> showJ i
      Invokestatic i -> byteString "invokestatic " <> showJ i
      Invokevirtual i -> byteString "invokevirtual " <> showJ i
      Ior -> byteString "ior"
      Irem -> byteString "irem"
      Ireturn -> byteString "ireturn"
      Ishl -> byteString "ishl"
      Ishr -> byteString "ishr"
      Istore i -> byteString "istore " <> showJ i
      Istore0 -> byteString "istore_0"
      Istore1 -> byteString "istore_1"
      Istore2 -> byteString "istore_2"
      Istore3 -> byteString "istore_3"
      Isub -> byteString "isub"
      Iushr -> byteString "iushr"
      Ixor -> byteString "ixor"
      Jsr l -> byteString "jsr " <> showJ l
      JsrW l -> byteString "jsr_w " <> showJ l
      L2d -> byteString "l2d"
      L2f -> byteString "l2f"
      L2i -> byteString "l2i"
      Ladd -> byteString "ladd"
      Laload -> byteString "laload"
      Land -> byteString "land"
      Lastore -> byteString "lastore"
      Lcmp -> byteString "lcmp"
      Lconst0 -> byteString "lconst_0"
      Lconst1 -> byteString "lconst_1"
      Ldc i -> byteString "ldc " <> showJ i
      Ldc2W i -> byteString "ldc2_w " <> showJ i
      LdcW i -> byteString "ldc_w " <> showJ i
      Ldiv -> byteString "ldiv"
      Lload i -> byteString "lload " <> showJ i
      Lload0 -> byteString "lload_0"
      Lload1 -> byteString "lload_1"
      Lload2 -> byteString "lload_2"
      Lload3 -> byteString "lload_3"
      Lmul -> byteString "lmul"
      Lneg -> byteString "lneg"
      Lor -> byteString "lor"
      Lrem -> byteString "lrem"
      Lreturn -> byteString "lreturn"
      Lshl -> byteString "lshl"
      Lshr -> byteString "lshr"
      Lstore i -> byteString "lstore " <> showJ i
      Lstore0 -> byteString "lstore_0"
      Lstore1 -> byteString "lstore_1"
      Lstore2 -> byteString "lstore_2"
      Lstore3 -> byteString "lstore_3"
      Lsub -> byteString "lsub"
      Lushr -> byteString "lushr"
      Lxor -> byteString "lxor"
      Monitorenter -> byteString "monitorenter"
      Monitorexit -> byteString "monitorexit"
      Multianewarray i b ->
        byteString "multianewarray " <> showJ i <> _sp <> showJ b
      New i -> byteString "new " <> showJ i
      Newarray t -> byteString "newarray " <> showJ t
      Nop -> byteString "nop"
      Pop -> byteString "pop"
      Pop2 -> byteString "pop2"
      Putfield i -> byteString "putfield " <> showJ i
      Putstatic i -> byteString "putstatic " <> showJ i
      Ret i -> byteString "ret " <> showJ i
      Return -> byteString "return"
      Saload -> byteString "saload"
      Sastore -> byteString "sastore"
      Sipush i -> byteString "sipush " <> showJ i
      Swap -> byteString "swap"
