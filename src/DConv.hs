{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module DConv
  ( dconv
  ) where

import           Base
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (throwError)
import           D2Data
import qualified Data.Binary.Get            as G
import           Data.Function              ((&))
import           DData                      (Index)
import qualified DData                      as T
import           Instructions
import           Text.Megaparsec
import qualified Text.Megaparsec.Byte.Lexer as L

data ConvError
  = BadConstantPoolConversion Info
                              CpCategory
                              Index
                              ConvInfo
  | BadConstantPoolAccess ConstantPool
                          CpCategory
                          Index
                          ConstantPoolInfo
  | ParseError (ParseErrorBundle ByteString ConvError)
  | Generic String
  deriving (Eq)

instance Ord ConvError
  -- Just to satisfy ShowErrorComponent
  -- We don't care what order the errors are displayed
                                                       where
  compare _ _ = EQ

type Parser = Parsec ConvError ByteString

instance ShowErrorComponent ConvError where
  showErrorComponent = show

instance Show ConvError where
  show err =
    case err of
      BadConstantPoolConversion cp tag i info ->
        "Bad constant pool info at index " ++
        show i ++
        ": expected " ++ show tag ++ " but got " ++ show info ++ "\n" ++ show cp
      BadConstantPoolAccess cp tag i info ->
        "Bad constant pool info at index " ++
        show i ++
        ": expected " ++ show tag ++ " but got " ++ show info ++ "\n" ++ show cp
      ParseError s -> errorBundlePretty s
      Generic s -> "Generic error: " ++ s

type DConv = Either ConvError

dconv :: T.ClassFile -> DConv ClassFile
dconv T.ClassFile { T.minorVersion
                  , T.majorVersion
                  , T.constantPool
                  , T.accessFlags
                  , T.thisClass
                  , T.superClass
                  , T.interfaces
                  , T.fields
                  , T.methods
                  , T.attrs
                  } = do
  cp <- cpconv constantPool
  ClassFile <$-> minorVersion <*-> majorVersion <*-> cp <*-> accessFlags <*->
    thisClass <*->
    superClass <*>
    conv cp interfaces <*>
    conv cp fields <*>
    conv cp methods <*>
    conv cp attrs

class Parseable a where
  parser :: ConstantPool -> Parser a

class DConvertible a b where
  conv :: ConstantPool -> a -> DConv b

instance Parseable a => DConvertible ByteString a where
  conv cp = either (Left . ParseError) Right . parse (parser cp) ""

instance DConvertible T.Interfaces Interfaces where
  conv cp (T.Interfaces l) = Interfaces <$> mapM (conv cp) l

instance DConvertible Index ByteString where
  conv = getInfo

instance DConvertible T.Fields Fields where
  conv cp (T.Fields l) = Fields <$> mapM (conv cp) l

instance DConvertible T.FieldInfo FieldInfo where
  conv cp T.FieldInfo {T.fAccessFlags, T.fNameIndex, T.fDescIndex, T.fAttrs} = do
    fAttrs' <- conv cp fAttrs
    return
      FieldInfo
        { fAccessFlags = fAccessFlags
        , fNameIndex = fNameIndex
        , fDescIndex = fDescIndex
        , fAttrs = fAttrs'
        }

instance DConvertible T.Methods Methods where
  conv cp (T.Methods l) = Methods <$> mapM (conv cp) l

instance DConvertible T.MethodInfo MethodInfo where
  conv cp T.MethodInfo {T.mAccessFlags, T.mNameIndex, T.mDescIndex, T.mAttrs} = do
    mAttrs' <- conv cp mAttrs
    return
      MethodInfo
        { mAccessFlags = mAccessFlags
        , mNameIndex = mNameIndex
        , mDescIndex = mDescIndex
        , mAttrs = mAttrs'
        }

instance DConvertible T.Attributes Attributes where
  conv cp (T.Attributes l) = Attributes <$> mapM (conv cp) l

instance DConvertible T.AttributeInfo AttributeInfo where
  conv cp (T.AttributeInfo i s) =
    getInfo cp i >>= \case
      "Code" ->
        return
          ACode
            { stackLimit = 0
            , localLimit = 0
            , code = Instructions []
            , exceptionTables = ExceptionTables []
            , cAttrs = Attributes []
            }
      _ -> return $ AConst s
--      "ConstantValue" -> getInfo cp (G.runGet G.getWord16be s) >>= undefined

data CpCategory
  = CpcClass
  | CpcFieldRef
  | CpcMethodRef
  | CpcInterfaceMethodRef
  | CpcConst
  | CpcNameAndType
  | CpcInfo
  | CpcOther
  deriving (Show, Eq)

type ConvInfo = Either T.ConstantPoolInfo ConstantPoolInfo

type Info = ConstantPool' ConvInfo

type ConvStage = Info -> ConvInfo -> DConv ConvInfo

type GetInfo' c a = ConstantPool' c -> Index -> DConv a

type GetInfo a = GetInfo' ConstantPoolInfo a

type GetConvInfo a = GetInfo' ConvInfo a

-- | Helper functions to access constant pools
-- Note that regardless of the original pool, accessors are done with the non indexed info variants
-- We provide mappings to the variant, and fallback to an error if it does not exist
class ConstantPoolGet c where
  _cpThrow :: ConstantPool' c -> CpCategory -> Index -> c -> ConvError
  cpThrowError :: ConstantPool' c -> CpCategory -> Index -> c -> DConv a
  cpThrowError cp tag i cpi = throwError $ _cpThrow cp tag i cpi
  _cpMap :: c -> Maybe ConstantPoolInfo
  get :: CpCategory -> (ConstantPoolInfo -> Maybe a) -> GetInfo' c a
  get tag f cp'@(ConstantPool cp) index =
    let cpi = cp !! (fromIntegral index - 1)
        err = cpThrowError cp' tag index cpi
     in maybe err return $ f =<< _cpMap cpi
  getInfo :: GetInfo' c ByteString
  getInfo =
    get
      CpcInfo
      (\case
         CpInfo s -> Just s
         _ -> Nothing)
  getClass :: GetInfo' c ByteString
  getClass =
    get
      CpcClass
      (\case
         CpClass s -> Just s
         _ -> Nothing)
  getNameAndType :: GetInfo' c (ByteString, ByteString)
  getNameAndType =
    get
      CpcNameAndType
      (\case
         CpNameAndType n d -> Just (n, d)
         _ -> Nothing)
  getFieldRef :: GetInfo' c RefInfo
  getFieldRef =
    get
      CpcFieldRef
      (\case
         CpFieldRef ref -> Just ref
         _ -> Nothing)
  getMethodRef :: GetInfo' c RefInfo
  getMethodRef =
    get
      CpcMethodRef
      (\case
         CpMethodRef ref -> Just ref
         _ -> Nothing)
  getInterfaceMethodRef :: GetInfo' c RefInfo
  getInterfaceMethodRef =
    get
      CpcInterfaceMethodRef
      (\case
         CpInterfaceMethodRef ref -> Just ref
         _ -> Nothing)

instance ConstantPoolGet ConstantPoolInfo where
  _cpThrow = BadConstantPoolAccess
  _cpMap = pure

instance ConstantPoolGet ConvInfo where
  _cpThrow = BadConstantPoolConversion
  _cpMap = either (const Nothing) Just

-- | Convert constant pool indices to their actual data
cpconv :: T.ConstantPool -> DConv ConstantPool
cpconv cp =
  initInfo cp & stageM convInfo >>= stageM convNameAndType >>=
  stageM convMethodHandle >>=
  extractInfo
    -- | Apply a conversion stage on the info
    -- Essentially, we use the provided info as a snapshot,
    -- and iterate through the conversion for each info entry
  where
    stageM :: ConvStage -> Info -> DConv Info
    stageM f info@(ConstantPool info') = ConstantPool <$> mapM (f info) info'
    -- | Initializes info entry from constant pool info entry
    -- Data that is already complete is automatically moved to the right
    initInfo :: T.ConstantPool -> Info
    initInfo (ConstantPool info) = ConstantPool $ map initInfo' info
      where
        initInfo' :: T.ConstantPoolInfo -> ConvInfo
        initInfo' cpi =
          case cpi of
            T.CpInteger n -> Right $ CpInteger n
            T.CpFloat n   -> Right $ CpFloat n
            T.CpLong n    -> Right $ CpLong n
            T.CpDouble n  -> Right $ CpDouble n
            T.CpInfo s    -> Right $ CpInfo s
            _             -> Left cpi
    -- | Convert info back to constant pool
    -- It is expected that all entries are converted (on the right)
    extractInfo :: Info -> DConv ConstantPool
    extractInfo info'@(ConstantPool info) =
      ConstantPool <$> zipWithM extractInfo' [0 ..] info
      where
        extractInfo' :: Index -> ConvInfo -> DConv ConstantPoolInfo
        extractInfo' _ (Right cpi) = return cpi
        extractInfo' i cpi         = cpThrowError info' CpcOther i cpi
    -- | Stage 1: Extract cpinfo for appropriate indices
    convInfo :: ConvStage
    convInfo info (Left cpi) =
      case cpi of
        T.CpClass i -> Right . CpClass <$> getInfo info i
        T.CpString i -> Right . CpString <$> getInfo info i
        T.CpMethodType i -> Right . CpMethodType <$> getInfo info i
        T.CpNameAndType name desc ->
          Right <$> (CpNameAndType <$> getInfo info name <*> getInfo info desc)
        _ -> return $ Left cpi
    convInfo _ cpi = return cpi
    -- | Stage 2: Extract name and type data
    convNameAndType :: ConvStage
    convNameAndType info (Left cpi) =
      case cpi of
        T.CpFieldRef i1 i2 -> Right . CpFieldRef <$> convRef i1 i2
        T.CpMethodRef i1 i2 -> Right . CpMethodRef <$> convRef i1 i2
        T.CpInterfaceMethodRef i1 i2 ->
          Right . CpInterfaceMethodRef <$> convRef i1 i2
        _ -> return $ Left cpi
      where
        convRef :: Index -> Index -> DConv RefInfo
        convRef classIndex nameAndTypeIndex = do
          className <- getClass info classIndex
          (nameInfo, typeInfo) <- getNameAndType info nameAndTypeIndex
          return $
            RefInfo {rClass = className, rName = nameInfo, rInfo = typeInfo}
    convNameAndType _ cpi = return cpi
    -- | Stage 3: Extract method handle
    convMethodHandle :: ConvStage
    convMethodHandle info (Left (T.CpMethodHandle mh i)) =
      Right . CpMethodHandle mh <$> handleRef info i
      where
        handleRef :: GetConvInfo RefInfo
        handleRef =
          case mh of
            T.CpmGetField         -> getFieldRef
            T.CpmGetStatic        -> getFieldRef
            T.CpmPutField         -> getFieldRef
            T.CpmPutStatic        -> getFieldRef
            T.CpmInvokeVirtual    -> getMethodRef
            T.CpmInvokeStatic     -> getMethodRef
            T.CpmInvokeSpecial    -> getMethodRef
            T.CpmInvokeInterface  -> getInterfaceMethodRef
            T.CpmNewInvokeSpecial -> getMethodRef
    convMethodHandle _ cpi = return cpi