{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module DConv
  ( dconv
  ) where

import           Base
import           Control.Monad        (zipWithM)
import           Control.Monad.Except (throwError)
import qualified D1Data               as T
import           D2Data
import qualified Data.Binary.Get      as G
import           Data.Function        ((&))
import           DParse
import           IR1Data
import           IRData
import           Text.Megaparsec

data ConvError
  = BadConstantPoolConversion Info
                              CpCategory
                              Index
                              ConvInfo
  | BadConstantPoolAccess ConstantPool
                          CpCategory
                          Index
                          ConstantPoolInfo
  | ParseError (ParseErrorBundle ByteString DParseError)
  | Generic String
  deriving (Eq)

dconv :: T.ClassFile -> DConv ClassFile
dconv classFile@T.ClassFile {constantPool} = do
  cp <- cpconv constantPool
  convert cp classFile

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.ClassIndex ByteString where
  convert cp (T.ClassIndex i) = getClass cp i

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.NameIndex ByteString where
  convert cp (T.NameIndex i) = getInfo cp i

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.DescIndex ByteString where
  convert cp (T.DescIndex i) = getInfo cp i

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.StringIndex ByteString where
  convert cp (T.StringIndex i) = getInfo cp i

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError (CpMethodHandle, T.RefIndex) RefInfo where
  convert cp (mh, T.RefIndex i) = handleRef cp i
    where
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

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.NameAndTypeIndex NameAndTypeInfo where
  convert cp (T.NameAndTypeIndex i) = getNameAndType cp i

instance Ord ConvError
  -- Just to satisfy ShowErrorComponent
  -- We don't care what order the errors are displayed
                                                       where
  compare _ _ = EQ

--type Parser = Parsec ConvError ByteString
instance ShowErrorComponent ConvError where
  showErrorComponent = show

instance Show ConvError where
  show err =
    case err of
      BadConstantPoolConversion cp tag i info ->
        "Bad constant pool conversion at index " ++
        show i ++
        ": expected " ++ show tag ++ " but got " ++ show info ++ "\n" ++ show cp
      BadConstantPoolAccess cp tag i info ->
        "Bad constant pool access at index " ++
        show i ++
        ": expected " ++ show tag ++ " but got " ++ show info ++ "\n" ++ show cp
      ParseError s -> errorBundlePretty s
      Generic s -> "Generic error: " ++ s

type DConv = Either ConvError

instance Convertible ConstantPool ConvError T.AttributeInfo AttributeInfo where
  convert cp (T.AttributeInfo i s) =
    getInfo cp i >>= \case
      "Code" -> do
        ACodePart {..} <- convParse attrCodeParser
        cAttrs <- convert cp pAttributes
        return
          ACode
            { stackLimit = pStackLimit
            , localLimit = pLocalLimit
            , code = pCode
            , exceptionTables = pExceptionTables
            , cAttrs = cAttrs
            }
        where attrCodeParser :: Parser ACodePart
              attrCodeParser = do
                stackLimit <- dparse'
                localLimit <- dparse'
                code <- dparse'
                exceptionTables <- dparse'
                attrs <- dparse'
                return
                  ACodePart
                    { pStackLimit = stackLimit
                    , pLocalLimit = localLimit
                    , pCode = code
                    , pExceptionTables = exceptionTables
                    , pAttributes = attrs
                    }
      "ConstantValue" -> AConst <$> getInfo cp (G.runGet G.getWord16be s)
      tag -> return $ AConst tag
    where
      convParse :: Parser a -> DConv a
      convParse parser = either (Left . ParseError) Right $ parse parser "" s

-- | Given that the code attribute contains nested attributes,
-- We must convert the first layer using the constant pool,
-- then parse the following layer before we can convert again.
-- This data type serves as an intermediate state
data ACodePart = ACodePart
  { pStackLimit      :: StackLimit
  , pLocalLimit      :: LocalLimit
  , pCode            :: Instructions
  , pExceptionTables :: ExceptionTables
  , pAttributes      :: T.Attributes
  }

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

type GetInfo c a = ConstantPool' c -> Index -> DConv a

type GetConvInfo a = GetInfo ConvInfo a

-- | Helper functions to access constant pools
-- Note that regardless of the original pool, accessors are done with the non indexed info variants
-- We provide mappings to the variant, and fallback to an error if it does not exist
class ConstantPoolGet c where
  _cpThrow :: ConstantPool' c -> CpCategory -> Index -> c -> ConvError
  cpThrowError :: ConstantPool' c -> CpCategory -> Index -> c -> DConv a
  cpThrowError cp tag i cpi = throwError $ _cpThrow cp tag i cpi
  _cpMap :: c -> Maybe ConstantPoolInfo
  get :: CpCategory -> (ConstantPoolInfo -> Maybe a) -> GetInfo c a
  get tag f cp'@(ConstantPool cp) index =
    let cpi = cp !! (fromIntegral index - 1)
        err = cpThrowError cp' tag index cpi
     in maybe err return $ f =<< _cpMap cpi
  getInfo :: GetInfo c ByteString
  getInfo =
    get
      CpcInfo
      (\case
         CpInfo s -> Just s
         _ -> Nothing)
  getClass :: GetInfo c ByteString
  getClass =
    get
      CpcClass
      (\case
         CpClass s -> Just s
         _ -> Nothing)
  getNameAndType :: GetInfo c NameAndTypeInfo
  getNameAndType =
    get
      CpcNameAndType
      (\case
         CpNameAndType n t ->
           Just $ NameAndTypeInfo {nameInfo = n, typeInfo = t}
         _ -> Nothing)
  getFieldRef :: GetInfo c RefInfo
  getFieldRef =
    get
      CpcFieldRef
      (\case
         CpFieldRef c nt -> Just $ refInfo c nt
         _ -> Nothing)
  getMethodRef :: GetInfo c RefInfo
  getMethodRef =
    get
      CpcMethodRef
      (\case
         CpMethodRef c nt -> Just $ refInfo c nt
         _ -> Nothing)
  getInterfaceMethodRef :: GetInfo c RefInfo
  getInterfaceMethodRef =
    get
      CpcInterfaceMethodRef
      (\case
         CpInterfaceMethodRef c nt -> Just $ refInfo c nt
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
        T.CpClass _         -> Right <$> convert info cpi
        T.CpString _        -> Right <$> convert info cpi
        T.CpNameAndType _ _ -> Right <$> convert info cpi
        T.CpMethodType _    -> Right <$> convert info cpi
        _                   -> return $ Left cpi
    convInfo _ cpi = return cpi
    -- | Stage 2: Extract name and type data
    convNameAndType :: ConvStage
    convNameAndType info (Left cpi) =
      case cpi of
        T.CpFieldRef _ _           -> Right <$> convert info cpi
        T.CpMethodRef _ _          -> Right <$> convert info cpi
        T.CpInterfaceMethodRef _ _ -> Right <$> convert info cpi
        T.CpInvokeDynamic _ _      -> Right <$> convert info cpi
        _                          -> return $ Left cpi
    convNameAndType _ cpi = return cpi
    -- | Stage 3: Extract method handle
    convMethodHandle :: ConvStage
    convMethodHandle info (Left cpi@(T.CpMethodHandle _ _)) =
      Right <$> convert info cpi
    convMethodHandle _ cpi = return cpi
