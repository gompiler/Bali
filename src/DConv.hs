{-|
Module      : DConv
Description : Conversion from D1Data to DConv
Copyright   : (c) Gompiler Team, 2019
License     : GPL-3
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module DConv
  ( dconv
  ) where

import           Base
import           Control.Monad        (zipWithM)
import           Control.Monad.Except (throwError)
import qualified D1Data               as T
import           D2Data
import           Data.Function        ((&))
import           DData
import           DParse
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
  | Generic String
  deriving (Eq)

dconv :: T.ClassFile -> DConv ClassFile
dconv classFile@ClassFile {constantPool} = do
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
         Convertible (ConstantPool' c) ConvError T.ConstIndex ByteString where
  convert cp (T.ConstIndex i) = getInfo cp i -- todo update?

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError (CpMethodHandle, T.RefIndex) RefInfo where
  convert cp (mh, T.RefIndex i) = handleRef cp i
    where
      handleRef =
        case mh of
          CpmGetField         -> getFieldRef
          CpmGetStatic        -> getFieldRef
          CpmPutField         -> getFieldRef
          CpmPutStatic        -> getFieldRef
          CpmInvokeVirtual    -> getMethodRef
          CpmInvokeStatic     -> getMethodRef
          CpmInvokeSpecial    -> getMethodRef
          CpmInvokeInterface  -> getInterfaceMethodRef
          CpmNewInvokeSpecial -> getMethodRef

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
      Generic s -> "Generic error: " ++ s

type DConv = Either ConvError

-- | With just parsing + convertible, only generic attributes will be generated.
-- To collect all variants, we must first generate the name bytestring, then run it against
-- an attribute specific parser.
-- After doing this recursively, we are left with a fully converted attribute info
instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError GenericAttribute AttributeInfo where
  convert cp attr@(GenericAttribute name s) =
    case dparseAttribute name of
      Just parser ->
        convert cp =<<
        either (Left . Generic . errorBundlePretty) Right (parse parser "" s)
      Nothing -> return $ AGeneric attr

instance ConstantPoolGet c =>
         Convertible (ConstantPool' c) ConvError T.GenericAttribute AttributeInfo where
  convert cp a = convert cp =<< (convert cp a :: DConv GenericAttribute)

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
    -- Apply a conversion stage on the info
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
            CpInteger n -> Right $ CpInteger n
            CpFloat n   -> Right $ CpFloat n
            CpLong n    -> Right $ CpLong n
            CpDouble n  -> Right $ CpDouble n
            CpInfo s    -> Right $ CpInfo s
            _           -> Left cpi
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
        CpClass _         -> Right <$> convert info cpi
        CpString _        -> Right <$> convert info cpi
        CpNameAndType _ _ -> Right <$> convert info cpi
        CpMethodType _    -> Right <$> convert info cpi
        _                 -> return $ Left cpi
    convInfo _ cpi = return cpi
    -- | Stage 2: Extract name and type data
    convNameAndType :: ConvStage
    convNameAndType info (Left cpi) =
      case cpi of
        CpFieldRef _ _           -> Right <$> convert info cpi
        CpMethodRef _ _          -> Right <$> convert info cpi
        CpInterfaceMethodRef _ _ -> Right <$> convert info cpi
        CpInvokeDynamic _ _      -> Right <$> convert info cpi
        _                        -> return $ Left cpi
    convNameAndType _ cpi = return cpi
    -- | Stage 3: Extract method handle
    convMethodHandle :: ConvStage
    convMethodHandle info (Left cpi@(CpMethodHandle _ _)) =
      Right <$> convert info cpi
    convMethodHandle _ cpi = return cpi
