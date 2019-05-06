{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShowJ
  ( ShowJ(..)
  , byteStringJ
  , byteStringJ'
  , printJ
  , printJ'
  , stringJ
  , stringJ'
  , showJDefaultConfigs
  , ShowJConfig(..)
  ) where

import           Base
import           D2Data
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intersperse)
import           DData
import           IR1Data
import           IRData
import           System.IO

data ShowJConfig = ShowJConfig
  { tabSize       :: Int
  , showCodeIndex :: Bool
  }

showJDefaultConfigs :: ShowJConfig
showJDefaultConfigs = ShowJConfig {tabSize = 4, showCodeIndex = True}

_nl :: Builder
_nl = charUtf8 '\n'

_sp :: Builder
_sp = charUtf8 ' '

_sc :: Builder
_sc = charUtf8 ';'

_tab :: ShowJConfig -> Int -> Builder
_tab _ 0 = mempty
_tab ShowJConfig {tabSize} tabCount =
  mconcat $ replicate (tabCount * tabSize) _sp

byteStringJ :: ShowJ a => a -> ByteString
byteStringJ = byteStringJ' showJDefaultConfigs 0

byteStringJ' :: ShowJ a => ShowJConfig -> Int -> a -> ByteString
byteStringJ' config tabCount s = toLazyByteString $ showJ' config tabCount s

stringJ :: ShowJ a => a -> String
stringJ = stringJ' showJDefaultConfigs 0

stringJ' :: ShowJ a => ShowJConfig -> Int -> a -> String
stringJ' config tabCount s = L.unpack $ byteStringJ' config tabCount s

printJ :: ShowJ a => a -> IO ()
printJ = printJ' showJDefaultConfigs 0

printJ' :: ShowJ a => ShowJConfig -> Int -> a -> IO ()
printJ' config tabCount s = do
  hFlush stdout
  L.putStrLn $ byteStringJ' config tabCount s

-- | Given a list of items, show the prefix and the list if it is not empty
-- or nothing otherwise
showJlist' ::
     (ShowJ (f a), Foldable f)
  => ShowJConfig
  -> Builder
  -> Int
  -> f a
  -> Builder
showJlist' config prefix tabCount info =
  if null info
    then mempty
    else prefix <> showJ' config tabCount info

-- | Bytecode show implementation using efficient ByteString builders
-- showJ is the default builder without any tab prefixes
-- showJ' will take tab prefixes into account
class ShowJ a where
  {-# MINIMAL showJ | showJ' #-}
  showJ :: ShowJConfig -> a -> Builder
  showJ c = showJ' c 0
  showJ' :: ShowJConfig -> Int -> a -> Builder
  showJ' c tabCount a = _tab c tabCount <> showJ c a

instance ShowJ L.ByteString where
  showJ _ = lazyByteString

instance ShowJ ClassFile where
  showJ' c tabCount ClassFile {..} =
    tab <> byteString ".version " <> word16Dec majorVersion <> _sp <>
    word16Dec minorVersion <>
    _nl <>
    tab <>
    byteString ".class" <>
    showJ c accessFlag <>
    _sp <>
    showJ c thisClass <>
    _nl <>
    tab <>
    byteString ".super " <>
    showJ c superClass <>
    showJlist' c (_nl <> _nl) tabCount interfaces <>
    showJlist' c (_nl <> _nl) tabCount fields <>
    showJlist' c (_nl <> _nl) tabCount methods <>
    showJlist' c (_nl <> _nl) tabCount attrs <>
    _nl <>
    _nl <>
    byteString ".end class"
    where
      tab = _tab c tabCount

instance ShowJ AccessFlag
  -- Attaches a space beforehand
                                 where
  showJ c flag =
    showJ c (fieldAccess flag) <> optional "static" isStatic <>
    optional "final" isFinal <>
    optional "volatile" isVolatile <>
    optional "transient" isTransient <>
    optional "synthetic" isSynthetic <>
    optional "enum" isEnum
    where
      optional :: L.ByteString -> (AccessFlag -> Bool) -> Builder
      optional s f =
        if f flag
          then _sp <> showJ c s
          else mempty

instance ShowJ FieldAccess where
  showJ _ fa =
    case fa of
      FPublic         -> byteString " public"
      FPrivate        -> byteString " private"
      FProtected      -> byteString " protected"
      FPackagePrivate -> mempty

instance ShowJ Interfaces where
  showJ' c tabCount (Interfaces l) =
    mconcat $ intersperse _nl $ map (showJ' c tabCount) l

instance ShowJ InterfaceInfo where
  showJ c (InterfaceInfo s) = byteString ".implements " <> showJ c s

instance ShowJ Fields where
  showJ' c tabCount (Fields l) =
    mconcat $ intersperse _nl $ map (showJ' c tabCount) l

instance ShowJ FieldInfo where
  showJ' c tabCount FieldInfo {..} =
    _tab c tabCount <> byteString ".field" <> showJ c accessFlag <> _sp <>
    showJ c nameIndex <>
    _sp <>
    showJ c descIndex <>
    showJlist' c _nl (tabCount + 1) attrs

instance ShowJ Methods where
  showJ' c tabCount (Methods l) =
    mconcat $ intersperse _nl $ map (showJ' c tabCount) l

instance ShowJ MethodInfo where
  showJ' c tabCount MethodInfo {..} =
    _tab c tabCount <> byteString ".method" <> showJ c accessFlag <> _sp <>
    showJ c nameIndex <>
    byteString " : " <>
    showJ c descIndex <>
    showJlist' c _nl (tabCount + 1) attrs

instance ShowJ Attributes where
  showJ' c tabCount (Attributes l) =
    mconcat $ intersperse _nl $ map (showJ' c tabCount) l

instance ShowJ AttributeInfo where
  showJ' c tabCount attributeInfo =
    _tab c tabCount <>
    case attributeInfo of
      ACode {..} ->
        byteString ".code stack " <> showJ c stackLimit <> byteString " locals " <>
        showJ c localLimit <>
        showJlist' c _nl (tabCount + 1) code <> -- TODO add exception tables
        showJlist' c (_nl <> _nl) (tabCount + 1) attrs
      AConst s -> byteString ".const " <> lazyByteString s
      ALineNumberTable t -> showJ' c tabCount t
      AGeneric (GenericAttribute name _) ->
        byteString ".generic " <> lazyByteString name
      _ -> byteString "todo" -- TODO

instance ShowJ LineNumberTable where
  showJ' c tabCount (LineNumberTable l) =
    byteString ".linenumbertable" <> _nl <>
    mconcat (intersperse _nl $ map (showJ' c $ tabCount + 1) l)

instance ShowJ LineNumberInfo where
  showJ c LineNumberInfo {..} =
    charUtf8 'L' <> showJ c startPc <> _sp <> showJ c lineNumber

instance ShowJ StackLimit where
  showJ c (StackLimit i) = showJ c i

instance ShowJ LocalLimit where
  showJ c (LocalLimit i) = showJ c i

instance ShowJ IRIndex where
  showJ c (IRIndex i) = showJ c i

instance ShowJ IRIndexw where
  showJ c (IRIndexw i) = showJ c i

instance ShowJ IntByte where
  showJ c (IntByte i) = showJ c i

instance ShowJ IntShort where
  showJ c (IntShort i) = showJ c i

instance ShowJ Count where
  showJ c (Count i) = showJ c i

instance ShowJ IRLabel where
  showJ c (IRLabel i) = showJ c i

instance ShowJ IRLabelw where
  showJ c (IRLabelw i) = showJ c i

instance ShowJ Word8 where
  showJ _ = word8Dec

instance ShowJ Word16 where
  showJ _ = word16Dec

instance ShowJ Word32 where
  showJ _ = word32Dec

instance ShowJ Word64 where
  showJ _ = word64Dec

instance ShowJ Int8 where
  showJ _ = int8Dec

instance ShowJ Int16 where
  showJ _ = int16Dec

instance ShowJ Int32 where
  showJ _ = int32Dec

instance ShowJ Int64 where
  showJ _ = int64Dec

instance ShowJ ArrayType where
  showJ c at =
    showJ c $
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
  showJ c fd =
    case fd of
      TByte    -> charUtf8 'B'
      TChar    -> charUtf8 'C'
      TDouble  -> charUtf8 'D'
      TFloat   -> charUtf8 'F'
      TInt     -> charUtf8 'I'
      TLong    -> charUtf8 'J'
      TRef s   -> charUtf8 'L' <> showJ c s <> _sc
      TShort   -> charUtf8 'S'
      TBool    -> charUtf8 'Z'
      TArray d -> charUtf8 '[' <> showJ c d

instance ShowJ Instructions where
  showJ' c@ShowJConfig {tabSize, showCodeIndex} tabCount (Instructions l) =
    mconcat $
    intersperse _nl $
    if showCodeIndex
      then zipWith showInstr [0 ..] l
      else map (showJ' c tabCount) l
    where
      showInstr :: Int -> Instruction -> Builder
      showInstr i inst =
        charUtf8 'L' <> intDec i <> charUtf8 ':' <>
        mconcat (replicate (spaceCount i) _sp) <>
        showJ c inst
      maxSpaceCount :: Int
      maxSpaceCount = tabCount * tabSize
      spaceCount :: Int -> Int
      spaceCount i = maxSpaceCount - (i `div` 10) - 2

instance ShowJ Instruction where
  showJ c inst =
    case inst of
      Aaload -> byteString "aaload"
      Aastore -> byteString "aastore"
      AconstNull -> byteString "aconst_null"
      Aload i -> byteString "aload " <> showJ c i
      Aload0 -> byteString "aload_0"
      Aload1 -> byteString "aload_1"
      Aload2 -> byteString "aload_2"
      Aload3 -> byteString "aload_3"
      Anewarray i -> byteString "anewarray " <> showJ c i
      Areturn -> byteString "areturn"
      Arraylength -> byteString "arraylength"
      Astore i -> byteString "astore " <> showJ c i
      Astore0 -> byteString "astore_0"
      Astore1 -> byteString "astore_1"
      Astore2 -> byteString "astore_2"
      Astore3 -> byteString "astore_3"
      Athrow -> byteString "athrow"
      Baload -> byteString "baload"
      Bastore -> byteString "bastore"
      Bipush i -> byteString "bipush " <> showJ c i
      Breakpoint -> byteString "breakpoint"
      Caload -> byteString "caload"
      Castore -> byteString "castore"
      Checkcast i -> byteString "checkcast" <> showJ c i
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
      Dload i -> byteString "dload " <> showJ c i
      Dload0 -> byteString "dload_0"
      Dload1 -> byteString "dload_1"
      Dload2 -> byteString "dload_2"
      Dload3 -> byteString "dload_3"
      Dmul -> byteString "dmul"
      Dneg -> byteString "dneg"
      Drem -> byteString "drem"
      Dreturn -> byteString "dreturn"
      Dstore i -> byteString "dstore " <> showJ c i
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
      Fload i -> byteString "fload " <> showJ c i
      Fload0 -> byteString "fload_0"
      Fload1 -> byteString "fload_1"
      Fload2 -> byteString "fload_2"
      Fload3 -> byteString "fload_3"
      Fmul -> byteString "fmul"
      Fneg -> byteString "fneg"
      Frem -> byteString "frem"
      Freturn -> byteString "freturn"
      Fstore i -> byteString "fstore " <> showJ c i
      Fstore0 -> byteString "fstore_0"
      Fstore1 -> byteString "fstore_1"
      Fstore2 -> byteString "fstore_2"
      Fstore3 -> byteString "fstore_3"
      Fsub -> byteString "fsub"
      Getfield i -> byteString "getfield " <> showJ c i
      Getstatic i -> byteString "getstatic " <> showJ c i
      Goto l -> byteString "goto " <> showJ c l
      GotoW l -> byteString "goto_w " <> showJ c l
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
      IfAcmpeq l -> byteString "if_acmpeq " <> showJ c l
      IfAcmpne l -> byteString "if_acmpne " <> showJ c l
      IfIcmpeq l -> byteString "if_icmpeq " <> showJ c l
      IfIcmpge l -> byteString "if_icmpge " <> showJ c l
      IfIcmpgt l -> byteString "if_icmpgt " <> showJ c l
      IfIcmple l -> byteString "if_icmple " <> showJ c l
      IfIcmplt l -> byteString "if_icmplt " <> showJ c l
      IfIcmpne l -> byteString "if_icmpne " <> showJ c l
      Ifeq l -> byteString "ifeq " <> showJ c l
      Ifge l -> byteString "ifge " <> showJ c l
      Ifgt l -> byteString "ifgt " <> showJ c l
      Ifle l -> byteString "ifle " <> showJ c l
      Iflt l -> byteString "iflt " <> showJ c l
      Ifne l -> byteString "ifne " <> showJ c l
      Ifnonnull l -> byteString "ifnonnull " <> showJ c l
      Ifnull l -> byteString "ifnull " <> showJ c l
      Iinc i b -> byteString "iinc " <> showJ c i <> _sp <> showJ c b
      Iload i -> byteString "iload " <> showJ c i
      Iload0 -> byteString "iload_0"
      Iload1 -> byteString "iload_1"
      Iload2 -> byteString "iload_2"
      Iload3 -> byteString "iload_3"
      Imul -> byteString "imul"
      Ineg -> byteString "ineg"
      Instanceof i -> byteString "instanceof " <> showJ c i
      Invokedynamic i -> byteString "invokedynamic " <> showJ c i
      Invokeinterface i b ->
        byteString "invokeinterface " <> showJ c i <> _sp <> showJ c b
      Invokespecial i -> byteString "invokespecial " <> showJ c i
      Invokestatic i -> byteString "invokestatic " <> showJ c i
      Invokevirtual i -> byteString "invokevirtual " <> showJ c i
      Ior -> byteString "ior"
      Irem -> byteString "irem"
      Ireturn -> byteString "ireturn"
      Ishl -> byteString "ishl"
      Ishr -> byteString "ishr"
      Istore i -> byteString "istore " <> showJ c i
      Istore0 -> byteString "istore_0"
      Istore1 -> byteString "istore_1"
      Istore2 -> byteString "istore_2"
      Istore3 -> byteString "istore_3"
      Isub -> byteString "isub"
      Iushr -> byteString "iushr"
      Ixor -> byteString "ixor"
      Jsr l -> byteString "jsr " <> showJ c l
      JsrW l -> byteString "jsr_w " <> showJ c l
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
      Ldc i -> byteString "ldc " <> showJ c i
      Ldc2W i -> byteString "ldc2_w " <> showJ c i
      LdcW i -> byteString "ldc_w " <> showJ c i
      Ldiv -> byteString "ldiv"
      Lload i -> byteString "lload " <> showJ c i
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
      Lstore i -> byteString "lstore " <> showJ c i
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
        byteString "multianewarray " <> showJ c i <> _sp <> showJ c b
      New i -> byteString "new " <> showJ c i
      Newarray t -> byteString "newarray " <> showJ c t
      Nop -> byteString "nop"
      Pop -> byteString "pop"
      Pop2 -> byteString "pop2"
      Putfield i -> byteString "putfield " <> showJ c i
      Putstatic i -> byteString "putstatic " <> showJ c i
      Ret i -> byteString "ret " <> showJ c i
      Return -> byteString "return"
      Saload -> byteString "saload"
      Sastore -> byteString "sastore"
      Sipush i -> byteString "sipush " <> showJ c i
      Swap -> byteString "swap"
