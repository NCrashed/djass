{-# LANGUAGE MultiWayIf #-}
module Language.Jass.Semantic.Type(
  inferType,
  typeSubsetOf,
  isNumericType,
  getGeneralType
  ) where
  
import Language.Jass.Parser.AST
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import Language.Jass.Semantic.Context
import Control.Monad.Error

type Name = String

-- | Infers expression type
inferType :: Expression -> JassSem s JassType
inferType (BinaryExpression src op left right) = if
  | op == And || op == Or -> return JBoolean
  | op == Reminder -> return JInteger
  | isRelationalOperator op -> return JBoolean
  | isArithmeticOperator op -> do
    t1 <- inferType left
    t2 <- inferType right
    mgt <- getGeneralType t1 t2
    case mgt of
      Nothing -> throwError $ SemanticError src $ "Type mismatch, cannot use types " ++ show t1 
        ++ " and " ++ show t2 ++ " in operator " ++ show op
      Just gt -> return gt  
inferType (UnaryExpression _ _ expr) = inferType expr 
inferType (ArrayReference src name _) = inferVariableType src name
inferType (FunctionCall src name _) = do
  mfunc <- getCallable name
  case mfunc of
    Nothing -> throwError $ SemanticError src $ "Unknown function/native " ++ name
    Just callable -> case getCallableReturnType callable of
      Nothing -> throwError $ SemanticError src $ "Cannot use function " ++ name ++ " with return type nothing"
      Just retType -> return retType
inferType (FunctionReference _ _) = return JCode
inferType (VariableReference src name) = inferVariableType src name
inferType (IntegerLiteral _ _) = return JInteger
inferType (StringLiteral _ _) = return JString
inferType (RealLiteral _ _) = return JReal
inferType (BoolLiteral _ _) = return JBoolean
inferType (NullLiteral _) = return JNull

-- | Returns variable type
inferVariableType :: SrcPos -> Name -> JassSem s JassType
inferVariableType src name = do
  mvar <- getVariable name
  case mvar of
    Nothing -> throwError $ SemanticError src $ "Unknown variable " ++ name
    Just var -> return $ getVarType var
    
-- | Is b type is more general than a type
typeSubsetOf :: JassType -> JassType -> JassSem s Bool
a `typeSubsetOf` b = do
  mgt <- getGeneralType a b
  case mgt of
    Nothing -> return False
    Just gt -> return $ gt == b
    
-- | Is a numeric jass type
isNumericType :: JassType -> Bool
isNumericType JInteger = True
isNumericType JReal = True
isNumericType _ = False

-- | Returns true if type is a reference
isHandleSuccessor :: JassType -> JassSem s Bool
isHandleSuccessor JHandle = return True
isHandleSuccessor JString = return True
isHandleSuccessor JCode = return True
isHandleSuccessor JNull = return True
isHandleSuccessor (JUserDefined name) = do
  mtype <- getType name
  case mtype of
    Nothing -> return False
    Just t -> isHandleSuccessor $ getTypeBase t
isHandleSuccessor _ = return False

-- | Returns first type that is ancestor of two types
getGeneralType :: JassType -> JassType -> JassSem s (Maybe JassType)
getGeneralType t1 t2
  | t1 == t2 = return $ Just t1
  | t1 == JReal && t2 == JInteger = return $ Just JReal
  | t1 == JInteger && t2 == JReal = return $ Just JReal
  | t1 == JNull = do
    cond <- isHandleSuccessor t2 
    return $ if cond then Just t2 else Nothing
  | t2 == JNull = do
    cond <- isHandleSuccessor t1 
    return $ if cond then Just t1 else Nothing
  | JUserDefined name1 <- t1, 
    JUserDefined name2 <- t2 = do 
      mtype1 <- getType name1
      mtype2 <- getType name2
      case mtype1 of
        Nothing -> throwError $ strMsg $ "Unknown type " ++ name1
        Just type1 -> case mtype2 of
          Nothing -> throwError $ strMsg $ "Unknown type " ++ name2
          Just type2 -> let
            base1 = getTypeBase type1
            base2 = getTypeBase type2
            in if 
              | base1 == t2 -> return $ Just base1
              | t1 == base2 -> return $ Just base2
              | otherwise   -> getGeneralType base1 base2
  | JUserDefined name1 <- t1 = do
      mtype1 <- getType name1
      case mtype1 of
        Nothing -> throwError $ strMsg $ "Unknown type " ++ name1
        Just type1 -> getGeneralType (getTypeBase type1) t2
  | JUserDefined name2 <- t2 = do
      mtype2 <- getType name2
      case mtype2 of
        Nothing -> throwError $ strMsg $ "Unknown type " ++ name2
        Just type2 -> getGeneralType t1 (getTypeBase type2) 
getGeneralType _ _ = return Nothing