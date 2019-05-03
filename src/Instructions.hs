{-
Instruction set for jvm operations
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-}
module Instructions where

import           Base
import           Data.List (intercalate)

newtype IRIndex =
  IRIndex Word8
  deriving (Show, Eq)

newtype IRIndexw =
  IRIndexw Word16
  deriving (Show, Eq)

newtype IRLabel =
  IRLabel Word16
  deriving (Show, Eq)

newtype IRLabelw =
  IRLabelw Word32
  deriving (Show, Eq)

newtype IntByte =
  IntByte Int8
  deriving (Show, Eq)

newtype IntShort =
  IntShort Int16
  deriving (Show, Eq)

data ArrayType
  = AtBool
  | AtChar
  | AtFloat
  | AtDouble
  | AtByte
  | AtShort
  | AtInt
  | AtLong
  deriving (Show, Eq)

newtype Instructions =
  Instructions [Instruction]
  deriving (Eq)

instance Show Instructions where
  show (Instructions l) = intercalate "\n" $ map show l

-- | Collection of all jvm bytecode instructions
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-- While the instructions are not one to one with the opcode list,
-- the data is meant to be equally expressive
-- Every op has exactly one instruction representation, and vice versa
data Instruction
  = Aaload -- aaload: arrayref, index -> value
  | Aastore -- aastore: arrayref, index, value ->
  | AconstNull -- aconst_null: -> null
  | Aload IRIndex -- aload: -> objectref
  | Aload0 -- aload_0: -> objectref
  | Aload1 -- aload_1: -> objectref
  | Aload2 -- aload_2: -> objectref
  | Aload3 -- aload_3: -> objectref
  | Anewarray IRIndexw -- anewarray: count -> arrayref
  | Areturn -- areturn: objectref -> [empty]
  | Arraylength -- arraylength: arrayref -> length
  | Astore IRIndex -- astore: objectref ->
  | Astore0 -- astore_0: objectref ->
  | Astore1 -- astore_1: objectref ->
  | Astore2 -- astore_2: objectref ->
  | Astore3 -- astore_3: objectref ->
  | Athrow -- athrow: objectref -> [empty], objectref
  | Baload -- baload: arrayref, index -> value
  | Bastore -- bastore: arrayref, index, value ->
  | Bipush IntByte -- bipush: -> value
  | Breakpoint -- breakpoint:
  | Caload -- caload: arrayref, index -> value
  | Castore -- castore: arrayref, index, value ->
  | Checkcast IRIndexw -- checkcast: objectref -> objectref
  | D2f -- d2f: value -> result
  | D2i -- d2i: value -> result
  | D2l -- d2l: value -> result
  | Dadd -- dadd: value1, value2 -> result
  | Daload -- daload: arrayref, index -> value
  | Dastore -- dastore: arrayref, index, value ->
  | Dcmpg -- dcmpg: value1, value2 -> result
  | Dcmpl -- dcmpl: value1, value2 -> result
  | Dconst0 -- dconst_0: -> 0.0
  | Dconst1 -- dconst_1: -> 1.0
  | Ddiv -- ddiv: value1, value2 -> result
  | Dload IRIndex -- dload: -> value
  | Dload0 -- dload_0: -> value
  | Dload1 -- dload_1: -> value
  | Dload2 -- dload_2: -> value
  | Dload3 -- dload_3: -> value
  | Dmul -- dmul: value1, value2 -> result
  | Dneg -- dneg: value -> result
  | Drem -- drem: value1, value2 -> result
  | Dreturn -- dreturn: value -> [empty]
  | Dstore IRIndex -- dstore: value ->
  | Dstore0 -- dstore_0: value ->
  | Dstore1 -- dstore_1: value ->
  | Dstore2 -- dstore_2: value ->
  | Dstore3 -- dstore_3: value ->
  | Dsub -- dsub: value1, value2 -> result
  | Dup -- dup: value -> value, value
  | Dup2 -- dup2: {value2, value1} -> {value2, value1}, {value2, value1}
  | Dup2X1 -- dup2_x1: value3, {value2, value1} -> {value2, value1}, value3, {value2, value1}
  | Dup2X2 -- dup2_x2: {value4, value3}, {value2, value1} -> {value2, value1}, {value4, value3}, {value2, value1}
  | DupX1 -- dup_x1: value2, value1 -> value1, value2, value1
  | DupX2 -- dup_x2: value3, value2, value1 -> value1, value3, value2, value1
  | F2d -- f2d: value -> result
  | F2i -- f2i: value -> result
  | F2l -- f2l: value -> result
  | Fadd -- fadd: value1, value2 -> result
  | Faload -- faload: arrayref, index -> value
  | Fastore -- fastore: arrayref, index, value ->
  | Fcmpg -- fcmpg: value1, value2 -> result
  | Fcmpl -- fcmpl: value1, value2 -> result
  | Fconst0 -- fconst_0: -> 0.0f
  | Fconst1 -- fconst_1: -> 1.0f
  | Fconst2 -- fconst_2: -> 2.0f
  | Fdiv -- fdiv: value1, value2 -> result
  | Fload IRIndex -- fload: -> value
  | Fload0 -- fload_0: -> value
  | Fload1 -- fload_1: -> value
  | Fload2 -- fload_2: -> value
  | Fload3 -- fload_3: -> value
  | Fmul -- fmul: value1, value2 -> result
  | Fneg -- fneg: value -> result
  | Frem -- frem: value1, value2 -> result
  | Freturn -- freturn: value -> [empty]
  | Fstore IRIndex -- fstore: value ->
  | Fstore0 -- fstore_0: value ->
  | Fstore1 -- fstore_1: value ->
  | Fstore2 -- fstore_2: value ->
  | Fstore3 -- fstore_3: value ->
  | Fsub -- fsub: value1, value2 -> result
  | Getfield IRIndexw -- getfield: objectref -> value
  | Getstatic IRIndexw -- getstatic: -> value
  | Goto IRLabel -- goto: [no change]
  | GotoW IRLabelw -- goto_w: [no change]
  | I2b -- i2b: value -> result
  | I2c -- i2c: value -> result
  | I2d -- i2d: value -> result
  | I2f -- i2f: value -> result
  | I2l -- i2l: value -> result
  | I2s -- i2s: value -> result
  | Iadd -- iadd: value1, value2 -> result
  | Iaload -- iaload: arrayref, index -> value
  | Iand -- iand: value1, value2 -> result
  | Iastore -- iastore: arrayref, index, value ->
  | Iconst0 -- iconst_0: -> 0
  | Iconst1 -- iconst_1: -> 1
  | Iconst2 -- iconst_2: -> 2
  | Iconst3 -- iconst_3: -> 3
  | Iconst4 -- iconst_4: -> 4
  | Iconst5 -- iconst_5: -> 5
  | IconstM1 -- iconst_m1: -> -1
  | Idiv -- idiv: value1, value2 -> result
  | IfAcmpeq IRLabel -- if_acmpeq: value1, value2 ->
  | IfAcmpne IRLabel -- if_acmpne: value1, value2 ->
  | IfIcmpeq IRLabel -- if_icmpeq: value1, value2 ->
  | IfIcmpge IRLabel -- if_icmpge: value1, value2 ->
  | IfIcmpgt IRLabel -- if_icmpgt: value1, value2 ->
  | IfIcmple IRLabel -- if_icmple: value1, value2 ->
  | IfIcmplt IRLabel -- if_icmplt: value1, value2 ->
  | IfIcmpne IRLabel -- if_icmpne: value1, value2 ->
  | Ifeq IRLabel -- ifeq: value ->
  | Ifge IRLabel -- ifge: value ->
  | Ifgt IRLabel -- ifgt: value ->
  | Ifle IRLabel -- ifle: value ->
  | Iflt IRLabel -- iflt: value ->
  | Ifne IRLabel -- ifne: value ->
  | Ifnonnull IRLabel -- ifnonnull: value ->
  | Ifnull IRLabel -- ifnull: value ->
  | Iinc IRIndex
         IntByte -- iinc: [No change]
  | Iload IRIndex -- iload: -> value
  | Iload0 -- iload_0: -> value
  | Iload1 -- iload_1: -> value
  | Iload2 -- iload_2: -> value
  | Iload3 -- iload_3: -> value
  | Imul -- imul: value1, value2 -> result
  | Ineg -- ineg: value -> result
  | Instanceof IRIndexw -- instanceof: objectref -> result
  | Invokedynamic IRIndexw -- invokedynamic: [arg1, [arg2 ...]] -> result
  | Invokeinterface IRIndexw
                    IntByte -- invokeinterface: objectref, [arg1, arg2, ...] -> result
  | Invokespecial IRIndexw -- invokespecial: objectref, [arg1, arg2, ...] -> result
  | Invokestatic IRIndexw -- invokestatic: [arg1, arg2, ...] -> result
  | Invokevirtual IRIndexw -- invokevirtual: objectref, [arg1, arg2, ...] -> result
  | Ior -- ior: value1, value2 -> result
  | Irem -- irem: value1, value2 -> result
  | Ireturn -- ireturn: value -> [empty]
  | Ishl -- ishl: value1, value2 -> result
  | Ishr -- ishr: value1, value2 -> result
  | Istore IRIndex -- istore: value ->
  | Istore0 -- istore_0: value ->
  | Istore1 -- istore_1: value ->
  | Istore2 -- istore_2: value ->
  | Istore3 -- istore_3: value ->
  | Isub -- isub: value1, value2 -> result
  | Iushr -- iushr: value1, value2 -> result
  | Ixor -- ixor: value1, value2 -> result
  | Jsr IRLabel -- jsr: -> address
  | JsrW IRLabelw -- jsr_w: -> address
  | L2d -- l2d: value -> result
  | L2f -- l2f: value -> result
  | L2i -- l2i: value -> result
  | Ladd -- ladd: value1, value2 -> result
  | Laload -- laload: arrayref, index -> value
  | Land -- land: value1, value2 -> result
  | Lastore -- lastore: arrayref, index, value ->
  | Lcmp -- lcmp: value1, value2 -> result
  | Lconst0 -- lconst_0: -> 0L
  | Lconst1 -- lconst_1: -> 1L
  | Ldc IRIndex -- ldc: -> value
  | Ldc2W IRIndexw -- ldc2_w: -> value
  | LdcW IRIndexw -- ldc_w: -> value
  | Ldiv -- ldiv: value1, value2 -> result
  | Lload IRIndex -- lload: -> value
  | Lload0 -- lload_0: -> value
  | Lload1 -- lload_1: -> value
  | Lload2 -- lload_2: -> value
  | Lload3 -- lload_3: -> value
  | Lmul -- lmul: value1, value2 -> result
  | Lneg -- lneg: value -> result
  | Lor -- lor: value1, value2 -> result
  | Lrem -- lrem: value1, value2 -> result
  | Lreturn -- lreturn: value -> [empty]
  | Lshl -- lshl: value1, value2 -> result
  | Lshr -- lshr: value1, value2 -> result
  | Lstore IRIndex -- lstore: value ->
  | Lstore0 -- lstore_0: value ->
  | Lstore1 -- lstore_1: value ->
  | Lstore2 -- lstore_2: value ->
  | Lstore3 -- lstore_3: value ->
  | Lsub -- lsub: value1, value2 -> result
  | Lushr -- lushr: value1, value2 -> result
  | Lxor -- lxor: value1, value2 -> result
  | Monitorenter -- monitorenter: objectref ->
  | Monitorexit -- monitorexit: objectref ->
  | Multianewarray IRIndexw
                   IntByte -- multianewarray: count1, [count2,...] -> arrayref
  | New IRIndexw -- new: -> objectref
  | Newarray ArrayType -- newarray: count -> arrayref
  | Nop -- nop: [No change]
  | Pop -- pop: value ->
  | Pop2 -- pop2: {value2, value1} ->
  | Putfield IRIndexw -- putfield: objectref, value ->
  | Putstatic IRIndexw -- putstatic: value ->
  | Ret IRIndex -- ret: [No change]
  | Return -- return: -> [empty]
  | Saload -- saload: arrayref, index -> value
  | Sastore -- sastore: arrayref, index, value ->
  | Sipush IntShort -- sipush: -> value
  | Swap -- swap: value2, value1 -> value1, value2
  deriving (Show, Eq)
