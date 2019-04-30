{-# LANGUAGE LambdaCase #-}

module ConstantPoolConv
  ( cpconv
  , CpConvError(..)
  ) where

import           Base
import           Control.Monad        (zipWithM)
import           Control.Monad.Except (throwError)
import           D2Data
import           Data.Function        ((&))
import           DData                (Index)
import qualified DData                as T

data CpConvError =
  BadInfo T.ConstantPool
          CpCategory
          Int
          (Maybe ConvInfo)
  deriving (Eq)

instance Show CpConvError where
  show (BadInfo cp tag i info) =
    "Bad constant pool info at index " ++
    show i ++
    ": expected " ++
    show tag ++ maybe [] (\ci -> " but got " ++ show ci) info ++ "\n" ++ show cp

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

type Info = [ConvInfo]

type ConvStage = Info -> ConvInfo -> CpConv' ConvInfo

type GetInfo a = Info -> Index -> CpConv' a

type CpConv a = Either CpConvError a

type CpConv' a = Either (T.ConstantPool -> CpConvError) a

-- | Convert constant pool indices to their actual data
cpconv :: T.ConstantPool -> CpConv ConstantPool
cpconv cp =
  let result =
        initInfo cp & stageM convInfo >>= stageM convNameAndType >>=
        stageM convMethodHandle >>=
        extractInfo
   in either (\err -> Left $ err cp) Right result
    -- | Apply a conversion stage on the info
    -- Essentially, we use the provided info as a snapshot,
    -- and iterate through the conversion for each info entry
  where
    stageM :: ConvStage -> Info -> CpConv' Info
    stageM f info = mapM (f info) info
    -- | Initializes info entry from constant pool info entry
    -- Data that is already complete is automatically moved to the right
    initInfo :: T.ConstantPool -> Info
    initInfo (T.ConstantPool info) = map initInfo' info
      where
        initInfo' :: T.ConstantPoolInfo -> ConvInfo
        initInfo' cpi =
          case cpi of
            T.CpInteger b    -> Right $ CpInteger b
            T.CpFloat b      -> Right $ CpFloat b
            T.CpLong b1 b2   -> Right $ CpLong b1 b2
            T.CpDouble b1 b2 -> Right $ CpDouble b1 b2
            T.CpInfo s       -> Right $ CpInfo s
            _                -> Left cpi
    -- | Convert info back to constant pool
    -- It is expected that all entries are converted (on the right)
    extractInfo :: Info -> CpConv' ConstantPool
    extractInfo info = ConstantPool <$> zipWithM extractInfo' [0 ..] info
      where
        extractInfo' :: Int -> ConvInfo -> CpConv' ConstantPoolInfo
        extractInfo' _ (Right cpi) = return cpi
        extractInfo' i cpi         = throwInfo CpcOther i cpi
    throwInfo :: CpCategory -> Int -> ConvInfo -> CpConv' a
    throwInfo tag i cpi = throwError (\cp' -> BadInfo cp' tag i $ Just cpi)
    get :: CpCategory -> (ConstantPoolInfo -> Maybe a) -> GetInfo a
    get tag f info index =
      let i = fromIntegral index
          cpi = info !! (i - 1)
          throw = throwInfo tag i cpi
       -- If value is right, check that f returns some value
       -- otherwise, throw cpinfo
       in either (const throw) (maybe throw return . f) cpi
    getInfo :: GetInfo ByteString
    getInfo =
      get
        CpcInfo
        (\case
           CpInfo s -> Just s
           _ -> Nothing)
    getClass :: GetInfo ByteString
    getClass =
      get
        CpcClass
        (\case
           CpClass s -> Just s
           _ -> Nothing)
    getNameAndType :: GetInfo (ByteString, ByteString)
    getNameAndType =
      get
        CpcNameAndType
        (\case
           CpNameAndType n d -> Just (n, d)
           _ -> Nothing)
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
        convRef :: Index -> Index -> CpConv' RefInfo
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
        handleRef :: GetInfo RefInfo
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
        getFieldRef :: GetInfo RefInfo
        getFieldRef =
          get
            CpcFieldRef
            (\case
               CpFieldRef ref -> Just ref
               _ -> Nothing)
        getMethodRef :: GetInfo RefInfo
        getMethodRef =
          get
            CpcMethodRef
            (\case
               CpMethodRef ref -> Just ref
               _ -> Nothing)
        getInterfaceMethodRef :: GetInfo RefInfo
        getInterfaceMethodRef =
          get
            CpcInterfaceMethodRef
            (\case
               CpInterfaceMethodRef ref -> Just ref
               _ -> Nothing)
    convMethodHandle _ cpi = return cpi
