{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Sample where

import           Control.Monad.Except (MonadError, throwError)

class Convertible c e a b where
  convert :: MonadError e m => c -> a -> m b

instance Convertible Int String Float Char where
  convert 0 f = pure 'a'
  convert i f = throwError "bad"

data A a b c =
  A a
    b
    c

convertA ::
     ( Convertible context e a a'
     , Convertible context e b b'
     , Convertible context e c c'
     , MonadError e m
     )
  => context
  -> A a b c
  -> m (A a' b' c')
convertA context (A a b c) =
  A <$> convert context a <*> convert context b <*> convert context c

convInstr ::
     ( MonadError e m
     , Convertible c e index index'
     , Convertible c e indexw indexw'
     , Convertible c e label label'
     , Convertible c e labelw labelw'
     , Convertible c e intByte intByte'
     , Convertible c e intShort intShort'
     , Convertible c e arrayType arrayType'
     )
  => c
  -> Instruction' index indexw label labelw intByte intShort arrayType
  -> m (Instruction' index' indexw' label' labelw' intByte' intShort' arrayType')
convConstantPool ::
     ( MonadError e m
     , Convertible c e classIndex classIndex'
     , Convertible c e nameIndex nameIndex'
     , Convertible c e descIndex descIndex'
     , Convertible c e nameAndTypeIndex nameAndTypeIndex'
     , Convertible c e stringIndex stringIndex'
     , Convertible c e refIndex refIndex'
     )
  => c
  -> ConstantPool' (ConstantPoolInfo' classIndex nameIndex descIndex nameAndTypeIndex stringIndex refIndex)
  -> m (ConstantPool' (ConstantPoolInfo' classIndex' nameIndex' descIndex' nameAndTypeIndex' stringIndex' refIndex'))
convConstantPool c (ConstantPool l) =
  ConstantPool <$> mapM (convConstantPoolInfo c) l
