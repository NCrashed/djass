{-# LANGUAGE MultiWayIf #-}
module Language.Jass.Semantic.Check(
  JassModule,
  checkModuleSemantic,
  checkModulesSemantic
  ) where

import Language.Jass.Parser.AST
import Language.Jass.Semantic.Context
import Language.Jass.Semantic.Type
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Error
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.Foldable as F(forM_)

class SemanticCheck a where
  checkSemantic :: a -> SemanticError -> JassSem s ()
 
-- | Checks one module for semantic errors
checkModuleSemantic :: JassModule -> Either SemanticError ()
checkModuleSemantic m = runST $ evalStateT (runErrorT $ checkSemantic m noMsg) =<< newContext

-- | Checks several modules for semantic errors
checkModulesSemantic :: [JassModule] -> Either SemanticError ()
checkModulesSemantic ms = runST $ evalStateT (runErrorT $
  mapM_ (`checkSemantic` noMsg) ms) =<< newContext
  
instance SemanticCheck a => SemanticCheck [a] where
  checkSemantic list err = mapM_ (`checkSemantic` err) list
  
instance SemanticCheck JassModule where
  checkSemantic (JassModule _ types globals natives funcs) _ = do
    checkSemantic types noMsg
    registerPrototypes $ fmap CallableNative natives
    registerPrototypes $ fmap CallableFunc funcs
    checkSemantic globals noMsg
    checkSemantic natives noMsg
    checkSemantic funcs noMsg
    where
      registerPrototypes = mapM_ registerCallable
      
instance SemanticCheck JassType where
  checkSemantic (JUserDefined name) err = do
    t <- getType name
    case t of
      Nothing -> throwError err
      _ -> return ()
  checkSemantic _ _ = return ()

instance SemanticCheck TypeDef where
  checkSemantic td@(TypeDef src name jtype) _ = do
    lookres <- getType name
    case lookres of
      Nothing -> checkSemantic jtype $ SemanticError src $ "Extend type " ++ show jtype ++ " is unknown"
      Just td2@(TypeDef src2 _ _) -> unless (td == td2) $ throwError $ SemanticError src $ 
        "Type " ++ name ++ " is already defined at " ++ showPos src2      
    registerType td
    
instance SemanticCheck Variable where
  checkSemantic var _ = do
    checkRedefinition
    checkInitializer
    where
      -- | Fails if variable already exists
      checkRedefinition = do
        let name = getVarName var
        let src = getVarPos var
        mvar <- getVariable name 
        case mvar of
          Nothing -> return ()
          Just var2 -> throwError $ SemanticError src $ "Variable " ++ name ++ " is already defined at " 
            ++ showPos (getVarPos var2)
      -- | Fails if initial value type isn't equal declared one
      checkInitializer =  do
        let jtype = getVarType var
        case getVarInitializator var of
          Nothing -> return ()
          Just initExpr -> do
            checkSemantic initExpr noMsg
            exprType <- inferType initExpr
            cond <- exprType `typeSubsetOf` jtype
            unless cond $ throwError $ SemanticError (getExpressionPos initExpr) $ 
              "Type mismatch in variable initial value, expected " ++
                show jtype ++ " but got " ++ show exprType
        
instance SemanticCheck Parameter where
  checkSemantic p@(Parameter src jtype name) _ = do
    checkSemantic jtype $ SemanticError src $ "Type of parameter " ++ name ++ " is unknown!"
    checkSemantic (VarParam p) noMsg

instance SemanticCheck GlobalVar where
  checkSemantic gvar err = 
    let var = VarGlobal gvar
    in checkSemantic var err >> registerVariable var
      
instance SemanticCheck LocalVar where
  checkSemantic lvar = checkSemantic $ VarLocal lvar
  
instance SemanticCheck FunctionDecl where
  checkSemantic (FunctionDecl _ _ params Nothing) err = checkSemantic params err
  checkSemantic (FunctionDecl _ _ params (Just retType)) err = do
    checkSemantic params err 
    checkSemantic retType $ updateErrorMsg err $ const $ "Return type " ++ show retType ++ " is unknown"

instance SemanticCheck Callable where
  checkSemantic callable _ = do
    let name = getCallableName callable
    let src = getCallablePos callable
    lookres <- getCallable name
    case lookres of
      Nothing -> return ()
      Just callable2 -> let src2 = getCallablePos callable2 in unless (src2 == src) $ throwError $ SemanticError src $ 
        "Function/native " ++ name ++ " is already defined at " ++ showPos src2
        
instance SemanticCheck NativeDecl where
  checkSemantic native@(NativeDecl src _ decl) _= do
    checkSemantic (CallableNative native) noMsg
    checkSemantic decl $ SemanticError src ""
    registerCallable (CallableNative native)
    
instance SemanticCheck Function where
  checkSemantic func@(Function src isConst decl@(FunctionDecl _ funcName params retType) locals stmts) _ = do
    checkSemantic (CallableFunc func) noMsg
    checkSemantic decl $ SemanticError src ""    
    
    mapM_ registerVariable $ fmap VarParam params
    forM_ locals $ \local -> do
      checkSemantic local noMsg
      registerVariable $ VarLocal local
    checkSemantic stmts noMsg
    
    when isConst $ mapM_ checkConstantness stmts
    mapM_ checkSetConstantness stmts
    mapM_ checkNakedExitWhen stmts
    mapM_ checkReturnType stmts
    when (isJust retType && not (checkTerminationFlow stmts)) $ throwError $ 
      SemanticError src $ "Function " ++ funcName ++ " must be ended with return statement"
    mapM_ removeVariable $ fmap getParamName params
    mapM_ removeVariable $ fmap getLocalVarName locals
    
    registerCallable (CallableFunc func)
    where
      -- | Checking that constant function uses only constant natives and functions
      checkConstantness (CallStatement stmtSrc _ name _) = do
        mcallable <- getCallable name
        case mcallable of
          Nothing -> return ()
          Just callable -> unless (getCallableConstness callable) $
            throwError $ SemanticError stmtSrc $ "Cannot use non constant function/native in constant function " 
              ++ getCallableName callable ++ " at " ++ showPos src
      checkConstantness _ = return ()
      
      -- | Checking that set is not setting immutable variables
      checkSetConstantness (SetStatement stmtSrc _ name _) = checkVarConstantness name stmtSrc "variable"
      checkSetConstantness (SetArrayStatement stmtSrc _ name _ _) = checkVarConstantness name stmtSrc "array"
      checkSetConstantness (LoopStatement _ _ loopStmts) = mapM_ checkSetConstantness loopStmts
      checkSetConstantness (IfThenElseStatement _ _ _ thenStmts elseClauses) = do
        mapM_ checkSetConstantness thenStmts
        mapM_ (mapM_ checkSetConstantness . snd) elseClauses
      checkSetConstantness _ = return ()
      
      checkVarConstantness name stmtSrc msgPart = do
        mvar <- getVariable name
        case mvar of
          Nothing -> return ()
          Just var -> when (getVarConstness var) $
            throwError $ SemanticError stmtSrc $ "Cannot set constant " ++ msgPart ++ " " ++ getVarName var 
              ++ " at " ++ showPos (getVarPos var)
      
      -- | Fails if there is exitwhen without loop
      checkNakedExitWhen (ExitWhenStatement stmtSrc _) = throwError $ SemanticError stmtSrc "exitwhen without corresponding loop"
      checkNakedExitWhen (IfThenElseStatement _ _ _ thenStmts elseClauses) = do
        mapM_ checkNakedExitWhen thenStmts
        mapM_ (mapM_ checkNakedExitWhen . snd) elseClauses
      checkNakedExitWhen _ = return ()
      
      -- | Checking that return types matches, no return bug for you ;)
      checkReturnType (ReturnStatement stmtSrc Nothing) = when (isJust retType) $ throwError $ SemanticError stmtSrc $
        "Type mismatch in return, expected " ++ show (fromJust retType)++ ", but got nothing" 
      checkReturnType (ReturnStatement _ (Just expr)) = do
        checkSemantic expr noMsg
        exprType <- inferType expr
        unless (isJust retType) $ throwError $ SemanticError (getExpressionPos expr) $
          "Type mismatch in return, expected nothing, but got " ++ show exprType
        let retType' = fromJust retType
        cond <- exprType `typeSubsetOf` retType'
        unless cond $ throwError $ SemanticError (getExpressionPos expr) $ 
          "Type mismatch in return, expected "++ show retType' ++", but got " ++ show exprType
      checkReturnType (LoopStatement _ _ loopStmts) = mapM_ checkReturnType loopStmts
      checkReturnType (IfThenElseStatement _ _ _ thenStmts elseClauses) = do
        mapM_ checkReturnType thenStmts
        mapM_ (mapM_ checkReturnType . snd) elseClauses
      checkReturnType _ = return ()

      -- | Checking that all statement tree has returns at leaves
      checkTerminationFlow :: [Statement] -> Bool
      checkTerminationFlow [] = False
      checkTerminationFlow (ReturnStatement _ _ : _) = True
      checkTerminationFlow (LoopStatement _ _ loopStmts : xs) = checkTerminationFlow loopStmts || checkTerminationFlow xs
      checkTerminationFlow (IfThenElseStatement _ _ _ thenStmts elseClauses : xs) = 
        if not $ hasLastElse elseClauses then checkTerminationFlow xs
        else all checkTerminationFlow (thenStmts : fmap snd elseClauses) || checkTerminationFlow xs
        where hasLastElse [] = False
              hasLastElse ys = isNothing $ fst $ last ys 
      checkTerminationFlow (_ : xs) = checkTerminationFlow xs  
      
instance SemanticCheck Statement where
  checkSemantic (SetStatement src _ name expr) _ = do
    checkVariableName src name
    checkVariableExpr name expr
                
  checkSemantic (SetArrayStatement src _ name exprIndex expr) _ = do
      checkArrayName src name
      checkIndexExpr exprIndex
      checkVariableExpr name expr      
                
  checkSemantic (IfThenElseStatement src _ cond thenStmts elseClauses) _ = do
    checkCondition cond "if condition"
    mapM_ (`checkSemantic` noMsg) thenStmts
    checkMiddleElse elseClauses
    forM_ elseClauses $ \(mcond, elseStmts) -> do
      F.forM_ mcond (`checkCondition` "condition of if else clause")
      mapM_ (`checkSemantic` noMsg) elseStmts
    where
      -- | Checks that else without condition is last and only last
      checkMiddleElse [] = return ()
      checkMiddleElse [(Nothing, _)] = return ()
      checkMiddleElse ((Nothing, _):_) = throwError $ SemanticError src "Else clause followed by another elseif or else isn't legal"
      checkMiddleElse _ = return ()
      
  checkSemantic (CallStatement src _ name args) _ = do
    checkFunctionName src name
    checkFunctionArgsCount src name $ length args
    checkFunctionArgs name args
  checkSemantic (LoopStatement _ _ stmts) _ = mapM_ (`checkSemantic` noMsg) stmts
  checkSemantic (ExitWhenStatement _ cond) _ = checkCondition cond "exitwhen"
  checkSemantic (ReturnStatement _ _) _ = return ()
  
-- | Checks that condition type is boolean
checkCondition :: Expression -> String -> JassSem s ()
checkCondition condExpr msgPart = do
  checkSemantic condExpr noMsg
  condType <- inferType condExpr
  when (condType /= JBoolean) $ throwError $ SemanticError (getExpressionPos condExpr) $ 
    "Type mismatch for " ++ msgPart ++ ", expected " ++ show JBoolean ++ " but got " ++ show condType
    
-- | Checks that index expression has type of integer
checkIndexExpr :: Expression -> JassSem s ()
checkIndexExpr exprIndex = do
  checkSemantic exprIndex noMsg
  exprType <- inferType exprIndex
  when (exprType /= JInteger) $ throwError $ SemanticError (getExpressionPos exprIndex) $ 
    "Type mismatch for array index, expected " ++ show JInteger ++ " but got " ++ show exprType    

-- | Checks that expression has valid type for variable
checkVariableExpr :: String -> Expression -> JassSem s ()
checkVariableExpr name expr = do
  mvar <- getVariable name
  case mvar of
    Nothing -> return ()
    Just var -> do
      checkSemantic expr noMsg
      exprType <- inferType expr 
      let varType = getVarType var
      cond <- exprType `typeSubsetOf` varType
      unless cond $
        throwError $ SemanticError (getExpressionPos expr) $ "Type mismatch, expected " 
          ++ show varType ++ " but got " ++ show exprType

-- | Checks that name of plain (not array) variable exists
checkVariableName :: SrcPos -> String -> JassSem s ()
checkVariableName src name = do 
  mvar <- getVariable name
  case mvar of
    Nothing -> throwError $ SemanticError src $ "Unknown variable " ++ name
    Just var -> when (isVarArray var) $ throwError $ SemanticError src
      "Expected plain variable, but got array"
            
-- | Checks that name of variable exists
checkArrayName :: SrcPos -> String -> JassSem s ()
checkArrayName src name = do 
  mvar <- getVariable name
  case mvar of
    Nothing -> throwError $ SemanticError src $ "Unknown variable " ++ name
    Just var -> unless (isVarArray var) $ throwError $ SemanticError src
      "Expected array variable, but got plain type "

-- | Checks if there is actual function name exists
checkFunctionName :: SrcPos -> String -> JassSem s ()
checkFunctionName src name = do
  mfunc <- getCallable name
  case mfunc of
    Just _ -> return ()
    Nothing -> throwError $ SemanticError src $ "Unknown function/native " ++ name

-- | Checks that return type of function is not nothing
checkFunctionReturnType :: SrcPos -> String -> JassSem s ()
checkFunctionReturnType src name = do
  mfunc <- getCallable name
  case mfunc of
    Nothing -> return ()
    Just callable -> unless (isJust $ getCallableReturnType callable) $ throwError $
      SemanticError src $ "Cannot use function " ++ name ++ " in expression, it has nothing return type"
          
-- | Checks if argument count matches
checkFunctionArgsCount :: SrcPos -> String -> Int -> JassSem s ()
checkFunctionArgsCount src name argsCount = do
  mfunc <- getCallable name
  case mfunc of
    Nothing -> return ()
    Just callable -> let parsCount = length $ getCallableParameters callable
                     in when (argsCount /= parsCount) $ throwError $ SemanticError src $ "Arguments count mismatch, expected "
                      ++ show parsCount ++ ", but got " ++ show argsCount
                      
-- | Checks arguments types
checkFunctionArgs :: String -> [Expression] -> JassSem s ()
checkFunctionArgs name args = do 
  mfunc <- getCallable name
  case mfunc of
    Nothing -> return ()
    Just callable -> mapM_ checkArg $ zip3 [1..] args (getCallableParameters callable)
  where
    checkArg :: (Int, Expression, Parameter) -> JassSem s ()
    checkArg (i, argExpr, parameter) = do
      checkSemantic argExpr noMsg
      argType <- inferType argExpr
      let parType = getParamType parameter
      cond <- argType `typeSubsetOf` parType
      unless cond $ throwError $ SemanticError (getExpressionPos argExpr) $ 
        "Type mismatch at function " ++ name ++ " argument number " ++ show i ++ ", expected " ++ show parType 
          ++ " but got " ++ show argType
                                                    
instance SemanticCheck Expression where
  checkSemantic (BinaryExpression _ op left right) _ = do
    checkSemantic left noMsg
    checkSemantic right noMsg
    if | op == And || op == Or -> left `typeEqual` JBoolean >> right `typeEqual` JBoolean
       | op == Reminder -> left `typeEqual` JInteger >> right `typeEqual` JInteger
       | isArithmeticOperator op -> typeCheckArithmetic op left right
       | isRelationalOperator op -> typeCheckRelational op left right
  checkSemantic (UnaryExpression _ op expr) _ = do
    checkSemantic expr noMsg
    case op of
      Plus -> checkNumeric expr
      Negation -> checkNumeric expr
      Not -> expr `typeEqual` JBoolean
  checkSemantic (ArrayReference src name indexExpr) _ = do
    checkArrayName src name
    checkSemantic indexExpr noMsg
    indexExpr `typeEqual` JInteger
  checkSemantic (FunctionCall src name args) _ = do
    checkFunctionName src name
    checkFunctionReturnType src name
    checkFunctionArgsCount src name $ length args
    checkFunctionArgs name args
  checkSemantic (FunctionReference src name) _ = checkFunctionName src name
  checkSemantic (VariableReference src name) _ = checkVariableName src name
  checkSemantic _ _ = return ()
  
-- | Checks that expression has specific type
typeEqual :: Expression -> JassType -> JassSem s ()
expr `typeEqual` t2 = do
  t1 <- inferType expr
  unless (t1 == t2) $ throwError $
    SemanticError (getExpressionPos expr) $
      "Type mismatch in expression, expected " ++
        show t1 ++ ", but got " ++ show t2    

-- | Checks if expression is numeric type        
checkNumeric :: Expression -> JassSem s ()
checkNumeric expr = do
  t1 <- inferType expr
  unless (isNumericType t1) $ throwError $
    SemanticError (getExpressionPos expr) $
      "Type mismatch in expression, expected numeric type, but got " ++ show t1

-- | Performs type checking for expression arithmetic operators    
typeCheckArithmetic :: BinaryOperator -> Expression -> Expression -> JassSem s ()
typeCheckArithmetic Summ expr1 = 
  typeCheckBinaryExpression (arithmeticAssertion Summ (getExpressionPos expr1) [JInteger, JReal, JString]) Summ expr1
typeCheckArithmetic op expr1 = 
  typeCheckBinaryExpression (arithmeticAssertion op (getExpressionPos expr1) [JInteger, JReal]) op expr1

arithmeticAssertion :: BinaryOperator -> SrcPos -> [JassType] -> JassType -> JassType -> JassType -> JassSem s ()
arithmeticAssertion op src types gt t1 t2 = unless (gt `elem` types) $ throwError $ 
  SemanticError src $ "Type mismatch, cannot use " ++ show t1 ++ " and " ++ show t2 ++ " in " ++ show op

-- | Performs type checking for relational operators (==, !=, >, <, >=, <=)      
typeCheckRelational :: BinaryOperator -> Expression -> Expression -> JassSem s ()
typeCheckRelational = typeCheckBinaryExpression (const.const.const $ return ())

-- | Type checking binary operator, first assertion takes general type of left and right expressions
typeCheckBinaryExpression :: (JassType -> JassType -> JassType -> JassSem s ()) 
  -> BinaryOperator -> Expression -> Expression -> JassSem s ()
typeCheckBinaryExpression assertion op expr1 expr2 = do
  t1 <- inferType expr1
  t2 <- inferType expr2
  mgt <- getGeneralType t1 t2
  case mgt of
    Nothing -> throwError $ SemanticError (getExpressionPos expr1) $ 
      "Type mismatch, cannot use " ++ show t1 ++ " and " ++ show t2 ++ " in " ++ show op
    Just gt -> assertion gt t1 t2