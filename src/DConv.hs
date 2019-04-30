{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module DConv
  ( dconv
  ) where

import           Base
import           ConstantPoolConv
import           D2Data
import qualified DData            as T

data ConvError
  = CpConvError CpConvError
  | Generic String
  deriving (Show, Eq)

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
  cp <- either (Left . CpConvError) Right $ cpconv constantPool
  ClassFile <$-> minorVersion <*-> majorVersion <*-> cp <*-> accessFlags <*->
    thisClass <*->
    superClass <*>
    conv cp interfaces <*>
    conv cp fields <*>
    conv cp methods <*>
    conv cp attrs

class DConvertible a b where
  conv :: ConstantPool -> a -> DConv b

instance DConvertible T.Interfaces Interfaces where
  conv _ _ = return $ Interfaces []

instance DConvertible T.Fields Fields where
  conv _ _ = return $ Fields []

instance DConvertible T.Methods Methods where
  conv _ _ = return $ Methods []

instance DConvertible T.Attributes Attributes where
  conv _ _ = return $ Attributes []
