-- | Data mapping for Java IR
-- Sources:
-- https://docs.oracle.com/javase/specs/jvms/se8/html/
-- https://en.wikibooks.org/wiki/Java_Programming/Byte_Code
-- https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html
-- https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
-- https://www.guardsquare.com/en/blog/string-concatenation-java-9-untangling-invokedynamic
-- http://www.cs.sjsu.edu/~pearce/modules/lectures/co/jvm/jasmin/demos/demos.html
-- http://homepages.inf.ed.ac.uk/kwxm/JVM/fcmpg.html
-- https://stackoverflow.com/questions/43782187/why-does-go-have-a-bit-clear-and-not-operator
module Data where

import           Base
import           Data.Word (Word16, Word32)

type TODO = ()

type VarIndex = Int

type LabelName = String

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  | FPackagePrivate
  deriving (Eq)

instance Show FieldAccess where
  show FPublic         = "public"
  show FPrivate        = "private"
  show FProtected      = "protected"
  show FPackagePrivate = ""

newtype MethodSpec =
  MethodSpec ([JType], JType)
  deriving (Show, Eq)

data Field = Field
  { access     :: FieldAccess
  , static     :: Bool
  , fname      :: String
  , descriptor :: JType
  -- , value :: LDCType TODO?
  } deriving (Show, Eq)

data ClassFile = ClassFile
  { minorVersion :: Word16
  , majorVersion :: Word16
  , constantPool :: ConstantPool
  , accessFlags  :: AccessFlag
  , thisClass    :: Word16
  , superClass   :: Word16
  , interfaces   :: Interfaces
  , fields       :: Fields
  , methods      :: Methods
  } deriving (Show, Eq)

newtype ConstantPool =
  ConstantPool [ConstantPoolInfo]
  deriving (Show, Eq)

-- | Constant pool info
-- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data ConstantPoolInfo
  -- | Index + associated data
  = CpIndex Word16
            CpIndex
  -- | Constant data
  | CpConst CpConst
  deriving (Show, Eq)

-- | Values within the constant pool with an associated index
-- Unless specified, extra data relates to name_and_type index
data CpIndex
  = CpClass
  | CpField Word16
  | CpMethod Word16
  | CpInterfaceMethod Word16
  | CpString
  -- | Referenced by name & type index of other index types.
  -- Points to a descriptor index
  -- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.6
  | CpNameAndType Word16
  | CpMethodHandle CpMethodHandle
  -- | Index must point to info
  | CpMethodType
  -- | First index is bootstrap_method_attr_index
  -- Attached index below is name_and_type index
  | CpInvokeDynamic Word16
  deriving (Show, Eq)

-- | Data is stored inline
data CpConst
  = CpInteger Word32
  | CpFloat Word32
  | CpLong Word32
           Word32
  | CpDouble Word32
             Word32
  -- | Represents constant string
  -- First value represents length
  -- Second value is a byte array
  -- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.7
  | CpInfo Word16
           ByteString
  deriving (Show, Eq)

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-5.html#jvms-5.4.3.5
data CpMethodHandle
  -- getfield C.f:T
  = CpmGetField
  -- getstatic C.f:T
  | CpmGetStatic
  -- putfield C.f:T
  | CpmPutField
  -- putstatic C.f:T
  | CpmPutStatic
  -- invokevirtual C.m:(A*)T
  | CpmInvokeVirtual
  -- invokestatic C.m:(A*)T
  | CpmInvokeStatic
  -- invokespecial C.m:(A*)T
  | CpmInvokeSpecial
  -- new C; dup; invokespecial C.<init>:(A*)V
  | CpmNewInvokeSpecial
  -- invokeinterface C.m:(A*)T
  | CpmInvokeInterface
  deriving (Show, Eq)

-- | See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.2
newtype AccessFlag =
  AccessFlag Word16
  deriving (Show, Eq)

--data Class = Class
--  { cname   :: String
--  , bstruct :: Bool
--  , fields  :: [Field]
--  , methods :: [Method]
--  } deriving (Show, Eq)

newtype Interfaces =
  Interfaces [Word16]
  deriving (Show, Eq)

newtype Fields =
  Fields [FieldInfo]
  deriving (Show, Eq)

data FieldInfo = FieldInfo
  { fAccessFlags :: AccessFlag
  , fNameIndex   :: Word16
  , fDescIndex   :: Word16
  , fAttrs       :: Attributes
  } deriving (Show, Eq)

newtype Methods =
  Methods [MethodInfo]
  deriving (Show, Eq)

data MethodInfo = MethodInfo
  { mAccessFlags :: AccessFlag
  , mNameIndex   :: Word16
  , mDescIndex   :: Word16
  , mAttrs       :: Attributes
  } deriving (Show, Eq)

newtype Attributes =
  Attributes [AttributeInfo]
  deriving (Show, Eq)

data AttributeInfo =
  AttributeInfo Word16
                TODO -- todo figure out what info is
  deriving (Show, Eq)

data Method = Method
  { mname       :: String
  , mstatic     :: Bool
  , stackLimit  :: Int
  , localsLimit :: Int
  , spec        :: MethodSpec
  , body        :: [IRItem]
  } deriving (Show, Eq)

newtype ClassRef =
  ClassRef String
  deriving (Show, Eq)

data ClassOrArrayRef
  = CRef ClassRef
  | ARef JType
  deriving (Show, Eq)

data FieldRef =
  FieldRef ClassRef
           String
  deriving (Show, Eq)

data MethodRef =
  MethodRef ClassOrArrayRef
            String
            MethodSpec
  deriving (Eq)

instance Show MethodRef where
  show (MethodRef (CRef (ClassRef cn)) mn (MethodSpec (tl, t))) =
    "Method " ++ cn ++ " " ++ mn ++ " (" ++ concatMap show tl ++ ")" ++ show t
  show (MethodRef (ARef jt) mn (MethodSpec (tl, t))) =
    "Method [" ++
    show jt ++ " " ++ mn ++ " (" ++ concatMap show tl ++ ")" ++ show t

-- | See https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3
data JType
  = JClass ClassRef -- Lwhatever;
  | JArray JType -- [ as a prefix, ex. [I
  | JByte -- B
  | JChar -- C
  | JDouble -- D
  | JFloat -- F
  | JInt -- I
  | JLong -- J
  | JShort -- S
  | JBool -- Z
  deriving (Eq)

--  | JVoid -- V
instance Show JType where
  show (JClass (ClassRef cn)) = "L" ++ cn ++ ";"
  show (JArray jt)            = "[" ++ show jt
  show JChar                  = "C"
  show JInt                   = "I"
  show JDouble                = "D"
  show JBool                  = "Z"

--  show JVoid                  = "V"
data IRItem
  = IRInst Instruction
  | IRLabel LabelName
  deriving (Show, Eq)

data IRPrimitive
  = IRInt -- Integers, booleans, runes
  | IRDouble -- Float64s
  deriving (Show, Eq)

data IRType
  = Prim IRPrimitive -- Integer, boolean, rune, float64
  | Object -- String, array, struct, slice
  deriving (Show, Eq)

data IRCmp
  = LT
  | LE
  | GT
  | GE
  | EQ
  | NE
  deriving (Eq)

instance Show IRCmp where
  show Data.LT = "lt"
  show LE      = "le"
  show Data.GT = "gt"
  show GE      = "ge"
  show Data.EQ = "eq"
  show NE      = "ne"

data LDCType
  = LDCInt Int -- Integers, booleans, runes
  | LDCDouble Float -- Float64s
  | LDCString String -- Strings
  | LDCClass ClassRef -- Class constants
  deriving (Show, Eq)

data Instruction
  = Load IRType
         VarIndex
  | ArrayLoad IRType -- consumes an object reference and an index
  | Store IRType
          VarIndex
  | ArrayStore IRType -- consumes an object reference and an index
  | Return (Maybe IRType)
  | Dup
  | Dup2 -- ..., v, w -> ..., v, w, v, w
  | DupX1
  | Dup2X2
  | Goto LabelName
  | Add IRPrimitive
  | Div IRPrimitive
  | Mul IRPrimitive
  | Neg IRPrimitive
  | Sub IRPrimitive
  | IRem
  | IShL
  | IShR
  | IAnd
  | IOr
  | IXOr
  | IntToDouble
  | DoubleToInt
  | IfACmpNE LabelName
  | IfACmpEQ LabelName
  | IfNonNull LabelName
  | If IRCmp
       LabelName
  | IfICmp IRCmp
           LabelName
  | LDC LDCType -- pushes an int/double/string value onto the stack
  | IConstM1 -- -1
  | IConst0 -- 0
  | IConst1 -- 1
  | InstanceOf ClassOrArrayRef
  | AConstNull
  | DCmpG -- Same: 0, Second greater: 1, First greater: -1; 1 on NAN
  | New ClassRef -- class
  | CheckCast ClassOrArrayRef
  | ANewArray ClassRef
  | MultiANewArray JType
                   Int
  | NewArray IRPrimitive
  | NOp
  | Pop
  | Pop2
  | Swap
  | GetStatic FieldRef
              JType -- field spec, descriptor
  | GetField FieldRef
             JType
  | PutStatic FieldRef
              JType
  | PutField FieldRef
             JType
  | InvokeSpecial MethodRef -- method spec
  | InvokeVirtual MethodRef -- method spec
  | InvokeStatic MethodRef
  deriving (Show, Eq)
