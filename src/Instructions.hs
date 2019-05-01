{-
Instruction set for jvm operations
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-}
module Instructions where

import           Base

data NumericType
  = TnInt
  | TnLong
  | TnFloat
  | TnDouble
  deriving (Show, Eq)

data IntegralType
  = TiInt
  | TiLong
  deriving (Show, Eq)

data BitOp
  = Or
  | Shl
  | Shr
  | Ushr
  | Xor
  deriving (Show, Eq)

data NumOp
  = Add
  | Sub
  | Mul
  | Div
  | Neg
  | Rem
  deriving (Show, Eq)

data PrimitiveType
  = TpByte
  | TpChar
  | TpShort
  | TpInt
  | TpLong
  | TpFloat
  | TpDouble
  deriving (Show, Eq)

data IRCmp
  = G_
  | GT_
  | EQ_
  | NEQ_
  | L_
  | LT_
  deriving (Show, Eq)

data Ldc
  = Ldc Word8
  | LdcW Word16
  | Ldc2W Word16
  deriving (Show, Eq)

type IRIndex = Word8

type IRIndexw = Word16

type Label = Word16

newtype Instructions =
  Instructions [Instruction]
  deriving (Show, Eq)

data Instruction
  -- arrayref, index -> value
  = Aaload
  | Paload PrimitiveType
  -- arrayref, index, value ->
  | Aastore
  | Pastore PrimitiveType
  -- -> null
  | AconstNull
  -- -> ref
  -- Note that this encompasses xload_0 .. _3
  | Aload IRIndex
  | Pload NumericType
          IRIndex
  -- ref -> [empty]
  | Areturn
  | Numreturn NumericType
  | Return
  -- ref ->
  -- Note that this encompasses xstore_0 .. _3
  | Astore IRIndex
  | Pstore NumericType
           IRIndex
  -- ref -> [empty] ref
  | AThrow
  -- -> value
  -- Push byte as integer
  | Bipush Word8
  | Sipush Word16
  -- ref -> ref
  | Checkcast
  -- value -> result
  -- Encompasses ops such as d2f, d2i, etc
  -- Note that operations like d2d are not valid, and will be ignored
  | NumConv NumericType
            NumericType
  -- value1, value2 -> result
  | NumOp NumOp
          NumericType
  -- value1, value2 -> result
  | Dcmpg
  | Dcmpl
  | Fcmpg
  | Fcmpl
  | Lcmp
  -- value1, value2 ->
  | IfacmpEq Label
  | IfacmpNeq Label
  | Ificmp IRCmp
           Label
  -- value ->
  -- Comparisons against 0
  | If IRCmp
       Label
  | Ifnonnull Label
  | Ifnull Label
  -- [no change]
  | Iinc
  -- -> value
  -- Applies to all ldc variants, as well as xconst_y
  | Const Ldc
  -- value2, value1 -> value1, value2
  | Swap
  | Goto Label
  | Gotow Word32
  -- -> ref
  | New IRIndexw
  -- count -> ref
  | Anewarray IRIndexw
  | Newarray PrimitiveType
  -- count1, [count2, ...] -> arrayref
  | Multianewarray IRIndexw
                   Word8
  | Getfield IRIndex
  | Getstatic IRIndex
  | Putfield IRIndex
  | Putstatic IRIndex
  | Wide Instruction
  | BitOp BitOp
          IntegralType
  | Instanceof IRIndexw
  | Invokedynamic IRIndexw
  | Invokeinterface IRIndexw
  | Invokespecial IRIndexw
  | Invokestatic IRIndexw
  | Invokevirtual IRIndexw
  | Nop
  | Pop
  | Pop2
  deriving (Show, Eq)
