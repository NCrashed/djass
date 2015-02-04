{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Jass.Codegen.Type(
    toLLVMType
  , defaultValue
  , getRootType
  , jassArray
  , getFunctionType
  , getFunctionArgumentsTypes
  , getFunctionReturnType
  , isIntegralType
  , getReference
  , module SemType
  , mallocType
  ) where

import Language.Jass.JassType
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Parser.AST.Parameter as AST
import Language.Jass.Codegen.Context
import Language.Jass.Semantic.Type as SemType
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type
import LLVM.General.AST.Constant as LLVM
import LLVM.General.AST.Float
import Control.Applicative
import Control.Monad.Error
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
  
-- | Default array size
arraySize :: Num a => a
arraySize = 65536

  
-- | Returns reference to local or global variable
getReference :: String -> Codegen (LLVM.Type, LLVM.Operand)
getReference name = do
  mvar <- getVariable name
  case mvar of
    Just var -> do
      varType <- toLLVMType $ getVarType var
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
defaultValue (JArray et) = do
  et' <- toLLVMType et
  val <- defaultValue et
  return $ Array et' $ replicate arraySize val
defaultValue t@(JUserDefined _) = defaultValue =<< getRootType t
defaultValue JNull = throwError $ strMsg "ICE: cannot generate code for special type JNull"

-- | Returns most parent type of specified type
getRootType :: JassType -> Codegen JassType
getRootType (JUserDefined tname) = do
  mtp <- getType tname
  case mtp of
    Nothing -> throwError $ strMsg $ "ICE: cannot find type " ++ tname
    Just tp -> return $ getTypeBase tp
getRootType t = return t

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

-- | Type of C malloc function
mallocType :: LLVM.Type
mallocType = LLVM.FunctionType (ptr i8) [i32] False