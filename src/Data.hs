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

type VarIndex = Int

type LabelName = String

data FieldAccess
  = FPublic
  | FPrivate
  | FProtected
  deriving (Eq)

instance Show FieldAccess where
  show FPublic    = "public"
  show FPrivate   = "private"
  show FProtected = "protected"

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

data Class = Class
  { cname   :: String
  , bstruct :: Bool
  , fields  :: [Field]
  , methods :: [Method]
  } deriving (Show, Eq)

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
--  | JVoid -- V
  deriving (Eq)

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
