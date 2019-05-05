{-
Instruction set for jvm operations
See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-}
{-# LANGUAGE DeriveFoldable #-}

module IRData where

import           Base
import           Data.List (intercalate)

newtype Instructions' l =
  Instructions [l]
  deriving (Eq, Foldable)

instance Show l => Show (Instructions' l) where
  show (Instructions l) = intercalate "\n" $ map show l

-- | Collection of all jvm bytecode instructions
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5
-- While the instructions are not one to one with the opcode list,
-- the data is meant to be equally expressive
-- Every op has exactly one instruction representation, and vice versa
data Instruction' index indexw label labelw intByte intShort arrayType count
  = Aaload -- aaload: arrayref, index -> value
  | Aastore -- aastore: arrayref, index, value ->
  | AconstNull -- aconst_null: -> null
  | Aload index -- aload: -> objectref
  | Aload0 -- aload_0: -> objectref
  | Aload1 -- aload_1: -> objectref
  | Aload2 -- aload_2: -> objectref
  | Aload3 -- aload_3: -> objectref
  | Anewarray indexw -- anewarray: count -> arrayref
  | Areturn -- areturn: objectref -> [empty]
  | Arraylength -- arraylength: arrayref -> length
  | Astore index -- astore: objectref ->
  | Astore0 -- astore_0: objectref ->
  | Astore1 -- astore_1: objectref ->
  | Astore2 -- astore_2: objectref ->
  | Astore3 -- astore_3: objectref ->
  | Athrow -- athrow: objectref -> [empty], objectref
  | Baload -- baload: arrayref, index -> value
  | Bastore -- bastore: arrayref, index, value ->
  | Bipush intByte -- bipush: -> value
  | Breakpoint -- breakpoint:
  | Caload -- caload: arrayref, index -> value
  | Castore -- castore: arrayref, index, value ->
  | Checkcast indexw -- checkcast: objectref -> objectref
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
  | Dload index -- dload: -> value
  | Dload0 -- dload_0: -> value
  | Dload1 -- dload_1: -> value
  | Dload2 -- dload_2: -> value
  | Dload3 -- dload_3: -> value
  | Dmul -- dmul: value1, value2 -> result
  | Dneg -- dneg: value -> result
  | Drem -- drem: value1, value2 -> result
  | Dreturn -- dreturn: value -> [empty]
  | Dstore index -- dstore: value ->
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
  | Fload index -- fload: -> value
  | Fload0 -- fload_0: -> value
  | Fload1 -- fload_1: -> value
  | Fload2 -- fload_2: -> value
  | Fload3 -- fload_3: -> value
  | Fmul -- fmul: value1, value2 -> result
  | Fneg -- fneg: value -> result
  | Frem -- frem: value1, value2 -> result
  | Freturn -- freturn: value -> [empty]
  | Fstore index -- fstore: value ->
  | Fstore0 -- fstore_0: value ->
  | Fstore1 -- fstore_1: value ->
  | Fstore2 -- fstore_2: value ->
  | Fstore3 -- fstore_3: value ->
  | Fsub -- fsub: value1, value2 -> result
  | Getfield indexw -- getfield: objectref -> value
  | Getstatic indexw -- getstatic: -> value
  | Goto label -- goto: [no change]
  | GotoW labelw -- goto_w: [no change]
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
  | IfAcmpeq label -- if_acmpeq: value1, value2 ->
  | IfAcmpne label -- if_acmpne: value1, value2 ->
  | IfIcmpeq label -- if_icmpeq: value1, value2 ->
  | IfIcmpge label -- if_icmpge: value1, value2 ->
  | IfIcmpgt label -- if_icmpgt: value1, value2 ->
  | IfIcmple label -- if_icmple: value1, value2 ->
  | IfIcmplt label -- if_icmplt: value1, value2 ->
  | IfIcmpne label -- if_icmpne: value1, value2 ->
  | Ifeq label -- ifeq: value ->
  | Ifge label -- ifge: value ->
  | Ifgt label -- ifgt: value ->
  | Ifle label -- ifle: value ->
  | Iflt label -- iflt: value ->
  | Ifne label -- ifne: value ->
  | Ifnonnull label -- ifnonnull: value ->
  | Ifnull label -- ifnull: value ->
  | Iinc index
         intByte -- iinc: [No change]
  | Iload index -- iload: -> value
  | Iload0 -- iload_0: -> value
  | Iload1 -- iload_1: -> value
  | Iload2 -- iload_2: -> value
  | Iload3 -- iload_3: -> value
  | Imul -- imul: value1, value2 -> result
  | Ineg -- ineg: value -> result
  | Instanceof indexw -- instanceof: objectref -> result
  | Invokedynamic indexw -- invokedynamic: [arg1, [arg2 ...]] -> result
  | Invokeinterface indexw
                    count -- invokeinterface: objectref, [arg1, arg2, ...] -> result
  | Invokespecial indexw -- invokespecial: objectref, [arg1, arg2, ...] -> result
  | Invokestatic indexw -- invokestatic: [arg1, arg2, ...] -> result
  | Invokevirtual indexw -- invokevirtual: objectref, [arg1, arg2, ...] -> result
  | Ior -- ior: value1, value2 -> result
  | Irem -- irem: value1, value2 -> result
  | Ireturn -- ireturn: value -> [empty]
  | Ishl -- ishl: value1, value2 -> result
  | Ishr -- ishr: value1, value2 -> result
  | Istore index -- istore: value ->
  | Istore0 -- istore_0: value ->
  | Istore1 -- istore_1: value ->
  | Istore2 -- istore_2: value ->
  | Istore3 -- istore_3: value ->
  | Isub -- isub: value1, value2 -> result
  | Iushr -- iushr: value1, value2 -> result
  | Ixor -- ixor: value1, value2 -> result
  | Jsr label -- jsr: -> address
  | JsrW labelw -- jsr_w: -> address
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
  | Ldc index -- ldc: -> value
  | Ldc2W indexw -- ldc2_w: -> value
  | LdcW indexw -- ldc_w: -> value
  | Ldiv -- ldiv: value1, value2 -> result
  | Lload index -- lload: -> value
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
  | Lstore index -- lstore: value ->
  | Lstore0 -- lstore_0: value ->
  | Lstore1 -- lstore_1: value ->
  | Lstore2 -- lstore_2: value ->
  | Lstore3 -- lstore_3: value ->
  | Lsub -- lsub: value1, value2 -> result
  | Lushr -- lushr: value1, value2 -> result
  | Lxor -- lxor: value1, value2 -> result
  | Monitorenter -- monitorenter: objectref ->
  | Monitorexit -- monitorexit: objectref ->
  | Multianewarray indexw
                   count -- multianewarray: count1, [count2,...] -> arrayref
  | New indexw -- new: -> objectref
  | Newarray arrayType -- newarray: count -> arrayref
  | Nop -- nop: [No change]
  | Pop -- pop: value ->
  | Pop2 -- pop2: {value2, value1} ->
  | Putfield indexw -- putfield: objectref, value ->
  | Putstatic indexw -- putstatic: value ->
  | Ret index -- ret: [No change]
  | Return -- return: -> [empty]
  | Saload -- saload: arrayref, index -> value
  | Sastore -- sastore: arrayref, index, value ->
  | Sipush intShort -- sipush: -> value
  | Swap -- swap: value2, value1 -> value1, value2
  deriving (Show, Eq)

convInstr ::
     ( Convertible c e index index'
     , Convertible c e indexw indexw'
     , Convertible c e label label'
     , Convertible c e labelw labelw'
     , Convertible c e intByte intByte'
     , Convertible c e intShort intShort'
     , Convertible c e arrayType arrayType'
     , Convertible c e count count'
     )
  => c
  -> Instruction' index indexw label labelw intByte intShort arrayType count
  -> Either e (Instruction' index' indexw' label' labelw' intByte' intShort' arrayType' count')
convInstr c instruction =
  case instruction of
    Aaload              -> pure Aaload
    Aastore             -> pure Aastore
    AconstNull          -> pure AconstNull
    Aload i             -> Aload <$> convert c i
    Aload0              -> pure Aload0
    Aload1              -> pure Aload1
    Aload2              -> pure Aload2
    Aload3              -> pure Aload3
    Anewarray i         -> Anewarray <$> convert c i
    Areturn             -> pure Areturn
    Arraylength         -> pure Arraylength
    Astore i            -> Astore <$> convert c i
    Astore0             -> pure Astore0
    Astore1             -> pure Astore1
    Astore2             -> pure Astore2
    Astore3             -> pure Astore3
    Athrow              -> pure Athrow
    Baload              -> pure Baload
    Bastore             -> pure Bastore
    Bipush ib           -> Bipush <$> convert c ib
    Breakpoint          -> pure Breakpoint
    Caload              -> pure Caload
    Castore             -> pure Castore
    Checkcast i         -> Checkcast <$> convert c i
    D2f                 -> pure D2f
    D2i                 -> pure D2i
    D2l                 -> pure D2l
    Dadd                -> pure Dadd
    Daload              -> pure Daload
    Dastore             -> pure Dastore
    Dcmpg               -> pure Dcmpg
    Dcmpl               -> pure Dcmpl
    Dconst0             -> pure Dconst0
    Dconst1             -> pure Dconst1
    Ddiv                -> pure Ddiv
    Dload i             -> Dload <$> convert c i
    Dload0              -> pure Dload0
    Dload1              -> pure Dload1
    Dload2              -> pure Dload2
    Dload3              -> pure Dload3
    Dmul                -> pure Dmul
    Dneg                -> pure Dneg
    Drem                -> pure Drem
    Dreturn             -> pure Dreturn
    Dstore i            -> Dstore <$> convert c i
    Dstore0             -> pure Dstore0
    Dstore1             -> pure Dstore1
    Dstore2             -> pure Dstore2
    Dstore3             -> pure Dstore3
    Dsub                -> pure Dsub
    Dup                 -> pure Dup
    Dup2                -> pure Dup2
    Dup2X1              -> pure Dup2X1
    Dup2X2              -> pure Dup2X2
    DupX1               -> pure DupX1
    DupX2               -> pure DupX2
    F2d                 -> pure F2d
    F2i                 -> pure F2i
    F2l                 -> pure F2l
    Fadd                -> pure Fadd
    Faload              -> pure Faload
    Fastore             -> pure Fastore
    Fcmpg               -> pure Fcmpg
    Fcmpl               -> pure Fcmpl
    Fconst0             -> pure Fconst0
    Fconst1             -> pure Fconst1
    Fconst2             -> pure Fconst2
    Fdiv                -> pure Fdiv
    Fload i             -> Fload <$> convert c i
    Fload0              -> pure Fload0
    Fload1              -> pure Fload1
    Fload2              -> pure Fload2
    Fload3              -> pure Fload3
    Fmul                -> pure Fmul
    Fneg                -> pure Fneg
    Frem                -> pure Frem
    Freturn             -> pure Freturn
    Fstore i            -> Fstore <$> convert c i
    Fstore0             -> pure Fstore0
    Fstore1             -> pure Fstore1
    Fstore2             -> pure Fstore2
    Fstore3             -> pure Fstore3
    Fsub                -> pure Fsub
    Getfield i          -> Getfield <$> convert c i
    Getstatic i         -> Getstatic <$> convert c i
    Goto l              -> Goto <$> convert c l
    GotoW l             -> GotoW <$> convert c l
    I2b                 -> pure I2b
    I2c                 -> pure I2c
    I2d                 -> pure I2d
    I2f                 -> pure I2f
    I2l                 -> pure I2l
    I2s                 -> pure I2s
    Iadd                -> pure Iadd
    Iaload              -> pure Iaload
    Iand                -> pure Iand
    Iastore             -> pure Iastore
    Iconst0             -> pure Iconst0
    Iconst1             -> pure Iconst1
    Iconst2             -> pure Iconst2
    Iconst3             -> pure Iconst3
    Iconst4             -> pure Iconst4
    Iconst5             -> pure Iconst5
    IconstM1            -> pure IconstM1
    Idiv                -> pure Idiv
    IfAcmpeq l          -> IfAcmpeq <$> convert c l
    IfAcmpne l          -> IfAcmpne <$> convert c l
    IfIcmpeq l          -> IfIcmpeq <$> convert c l
    IfIcmpge l          -> IfIcmpge <$> convert c l
    IfIcmpgt l          -> IfIcmpgt <$> convert c l
    IfIcmple l          -> IfIcmple <$> convert c l
    IfIcmplt l          -> IfIcmplt <$> convert c l
    IfIcmpne l          -> IfIcmpne <$> convert c l
    Ifeq l              -> Ifeq <$> convert c l
    Ifge l              -> Ifge <$> convert c l
    Ifgt l              -> Ifgt <$> convert c l
    Ifle l              -> Ifle <$> convert c l
    Iflt l              -> Iflt <$> convert c l
    Ifne l              -> Ifne <$> convert c l
    Ifnonnull l         -> Ifnonnull <$> convert c l
    Ifnull l            -> Ifnull <$> convert c l
    Iinc i ib           -> Iinc <$> convert c i <*> convert c ib
    Iload i             -> Iload <$> convert c i
    Iload0              -> pure Iload0
    Iload1              -> pure Iload1
    Iload2              -> pure Iload2
    Iload3              -> pure Iload3
    Imul                -> pure Imul
    Ineg                -> pure Ineg
    Instanceof i        -> Instanceof <$> convert c i
    Invokedynamic i     -> Invokedynamic <$> convert c i
    Invokeinterface i n -> Invokeinterface <$> convert c i <*> convert c n
    Invokespecial i     -> Invokespecial <$> convert c i
    Invokestatic i      -> Invokestatic <$> convert c i
    Invokevirtual i     -> Invokevirtual <$> convert c i
    Ior                 -> pure Ior
    Irem                -> pure Irem
    Ireturn             -> pure Ireturn
    Ishl                -> pure Ishl
    Ishr                -> pure Ishr
    Istore i            -> Istore <$> convert c i
    Istore0             -> pure Istore0
    Istore1             -> pure Istore1
    Istore2             -> pure Istore2
    Istore3             -> pure Istore3
    Isub                -> pure Isub
    Iushr               -> pure Iushr
    Ixor                -> pure Ixor
    Jsr l               -> Jsr <$> convert c l
    JsrW l              -> JsrW <$> convert c l
    L2d                 -> pure L2d
    L2f                 -> pure L2f
    L2i                 -> pure L2i
    Ladd                -> pure Ladd
    Laload              -> pure Laload
    Land                -> pure Land
    Lastore             -> pure Lastore
    Lcmp                -> pure Lcmp
    Lconst0             -> pure Lconst0
    Lconst1             -> pure Lconst1
    Ldc i               -> Ldc <$> convert c i
    Ldc2W i             -> Ldc2W <$> convert c i
    LdcW i              -> LdcW <$> convert c i
    Ldiv                -> pure Ldiv
    Lload i             -> Lload <$> convert c i
    Lload0              -> pure Lload0
    Lload1              -> pure Lload1
    Lload2              -> pure Lload2
    Lload3              -> pure Lload3
    Lmul                -> pure Lmul
    Lneg                -> pure Lneg
    Lor                 -> pure Lor
    Lrem                -> pure Lrem
    Lreturn             -> pure Lreturn
    Lshl                -> pure Lshl
    Lshr                -> pure Lshr
    Lstore i            -> Lstore <$> convert c i
    Lstore0             -> pure Lstore0
    Lstore1             -> pure Lstore1
    Lstore2             -> pure Lstore2
    Lstore3             -> pure Lstore3
    Lsub                -> pure Lsub
    Lushr               -> pure Lushr
    Lxor                -> pure Lxor
    Monitorenter        -> pure Monitorenter
    Monitorexit         -> pure Monitorexit
    Multianewarray i n  -> Multianewarray <$> convert c i <*> convert c n
    New i               -> New <$> convert c i
    Newarray arrayType  -> Newarray <$> convert c arrayType
    Nop                 -> pure Nop
    Pop                 -> pure Pop
    Pop2                -> pure Pop2
    Putfield i          -> Putfield <$> convert c i
    Putstatic i         -> Putstatic <$> convert c i
    Ret i               -> Ret <$> convert c i
    Return              -> pure Return
    Saload              -> pure Saload
    Sastore             -> pure Sastore
    Sipush is           -> Sipush <$> convert c is
    Swap                -> pure Swap
