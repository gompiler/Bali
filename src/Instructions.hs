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

data BitOp
  = Or
  | Shl
  | Shr
  | Ushr
  | Xor

data NumOp
  = Add
  | Sub
  | Mul
  | Div
  | Neg
  | Rem

data PrimitiveType
  = TpByte
  | TpChar
  | TpShort
  | TpInt
  | TpLong
  | TpFloat
  | TpDouble

data IRCmp
  = G_
  | GT_
  | EQ_
  | NEQ_
  | L_
  | LT_

data Ldc
  = Ldc Word8
  | LdcW Word16
  | Ldc2W Word16

type Index = Word8

type Indexw = Word16

type Label = Word16

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
  | Aload Index
  | Pload NumericType Index
  -- ref -> [empty]
  | Areturn
  | Numreturn NumericType
  | Return
  -- ref ->
  -- Note that this encompasses xstore_0 .. _3
  | Astore Index
  | Pstore NumericType Index
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
  | New Indexw
  -- count -> ref
  | Anewarray Indexw
  | Newarray PrimitiveType
  -- count1, [count2, ...] -> arrayref
  | Multianewarray Indexw
                   Word8
  | Getfield Index
  | Getstatic Index
  | Putfield Index
  | Putstatic Index
  | Wide Instruction
  | BitOp BitOp
          IntegralType
  | Instanceof Indexw
  | Invokedynamic Indexw
  | Invokeinterface Indexw
  | Invokespecial Indexw
  | Invokestatic Indexw
  | Invokevirtual Indexw
  | Nop
  | Pop
  | Pop2
