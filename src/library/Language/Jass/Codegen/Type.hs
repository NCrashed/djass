{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Jass.Codegen.Type(
    toLLVMType
  , toLLVMType'
  , defaultValue
  , jassArray
  , arraySize
  , sizeOfType
  , getFunctionType
  , getFunctionArgumentsTypes
  , getFunctionReturnType
  , isIntegralType
  , isStringType
  , getReference
  , getTypeId
  , getTypeFromId
  , module SemType
  ) where

import Language.Jass.JassType
import Language.Jass.Parser.AST.Parameter as AST
import Language.Jass.Codegen.Context
import Language.Jass.Semantic.Type as SemType
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type
import LLVM.General.AST.Constant as LLVM
import LLVM.General.AST.Float
import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace
import Control.Applicative
import Control.Monad.Error
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import qualified Data.Map.Lazy as ML
 
-- | Default array size
arraySize :: Num a => a
arraySize = 65536

  
-- | Returns reference to local or global variable
getReference :: String -> Codegen (LLVM.Type, LLVM.Operand)
getReference name = do
  mvar <- getVariable name
  case mvar of
    Just var -> do
      varType <- ptr <$> if isVarArray var then toLLVMType (JArray $ getVarType var) else toLLVMType (getVarType var)
      return (varType, if isGlobalVariable var then LLVM.ConstantOperand $ LLVM.GlobalReference varType (LLVM.Name name) else LLVM.LocalReference varType (LLVM.Name name))
    Nothing -> throwError $ strMsg $ "ICE: cannot find variable " ++ name
    
-- | Converts jass type to LLVM type    
toLLVMType :: JassType -> Codegen Type
toLLVMType JInteger = return i32
toLLVMType JReal = return float
toLLVMType JBoolean = return i1
toLLVMType JString = return $ ptr i8
toLLVMType JHandle = return i64
toLLVMType JCode = return i64 -- TODO: stub here
toLLVMType (JArray et) = ArrayType arraySize <$> toLLVMType et
toLLVMType t@(JUserDefined _) = toLLVMType =<< getRootType t 
toLLVMType JNull = throwError $ strMsg "ICE: cannot generate code for special type JNull"

sizeOfType :: JassType -> Codegen Int
sizeOfType JInteger = return 4
sizeOfType JReal = return 4
sizeOfType JBoolean = return 1
sizeOfType JString = return pointerSize
sizeOfType JHandle = return 8
sizeOfType JCode = return 8 -- TODO: stub here
sizeOfType (JArray et) = (arraySize *) <$> sizeOfType et
sizeOfType t@(JUserDefined _) = sizeOfType =<< getRootType t 
sizeOfType JNull = throwError $ strMsg "ICE: cannot generate code for special type JNull"

pointerSize :: Int
pointerSize = fromIntegral $ fst (pointerLayouts defaultDataLayout ML.! AddrSpace 0)

-- | Ditto, including void type
toLLVMType' :: Maybe JassType -> Codegen Type
toLLVMType' = maybe (return VoidType) toLLVMType

-- | Returns default value for a type
defaultValue :: JassType -> Codegen Constant
defaultValue JInteger = return $ Int 32 0
defaultValue JReal = return $ Float (Single 0.0)
defaultValue JBoolean = return $ Int 1 0
defaultValue JString = return $ Null (ptr i8)
defaultValue JHandle = return $ Int 64 0
defaultValue JCode = return $ Int 64 0 -- TODO: stub here
defaultValue t@(JArray _) = Null <$> toLLVMType t
defaultValue t@(JUserDefined _) = defaultValue =<< getRootType t
defaultValue JNull = throwError $ strMsg "ICE: cannot generate code for special type JNull"

-- | Generates array type from element LLVM type
jassArray :: Type -> Type
jassArray = ArrayType arraySize

-- | Returns LLVM type of a function
getFunctionType :: String -> Codegen LLVM.Type
getFunctionType name = do
  callable <- getCallable name
  case callable of
    Nothing -> throwError $ strMsg $ "ICE: cannot find function " ++ name
    Just fn -> do
      retType <- toLLVMType' $ getCallableReturnType fn
      pars <- mapM convertPars $ getCallableParameters fn
      return $ FunctionType retType pars False
  where
  convertPars (AST.Parameter _ t _) = toLLVMType t

-- | Converts callable arguments types to LLVM types
getFunctionArgumentsTypes :: String -> Codegen [LLVM.Type]
getFunctionArgumentsTypes name = do
  callable <- getCallable name
  case callable of
    Nothing -> throwError $ strMsg $ "ICE: cannot find function " ++ name
    Just fn -> mapM convertPars $ getCallableParameters fn
  where
  convertPars (AST.Parameter _ t _) = toLLVMType t
  
-- | Returns function return type in LLVM typesystem
getFunctionReturnType :: String -> Codegen LLVM.Type
getFunctionReturnType name = do
  callable <- getCallable name
  case callable of
    Nothing -> throwError $ strMsg $ "ICE: cannot find function " ++ name
    Just fn -> maybe (return VoidType) toLLVMType $ getCallableReturnType fn

-- | Returns true if type is some sort of integer
isIntegralType :: LLVM.Type -> Bool
isIntegralType (IntegerType _) = True
isIntegralType _ = False

-- | Returns true if type is represents string type
isStringType :: LLVM.Type -> Bool
isStringType (PointerType (IntegerType 8) _) = True
isStringType _ = False

-- | Returns jass type id, custom types should be registered before the function is called
getTypeId :: JassType -> Codegen Int
getTypeId JInteger = return 1
getTypeId JReal = return 2
getTypeId JBoolean = return 3
getTypeId JString = return 4
getTypeId JHandle = return 5
getTypeId JCode = return 6
getTypeId (JArray et) = (256 +) <$> getTypeId et 
getTypeId (JUserDefined n) = (512 +) <$> getCustomTypeId n
getTypeId JNull = throwError $ strMsg "ICE: cannot generate code for special type JNull"

-- | Returns jass type by runtime id, custom types should be registered before the function is called
getTypeFromId :: Int -> Codegen JassType
getTypeFromId 1 = return JInteger
getTypeFromId 2 = return JReal
getTypeFromId 3 = return JBoolean
getTypeFromId 4 = return JString
getTypeFromId 5 = return JHandle
getTypeFromId 6 = return JCode
getTypeFromId n 
  | n > 512 = JUserDefined <$> getCustomTypeFromId (n - 512)
  | n > 256 = JArray <$> getTypeFromId (n - 256)
  | otherwise = throwError $ strMsg $ "ICE: unknown id of type '" ++ show n ++ "'" 