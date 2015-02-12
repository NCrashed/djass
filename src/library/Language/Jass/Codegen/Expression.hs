{-# LANGUAGE MultiWayIf #-}
module Language.Jass.Codegen.Expression(
    genLLVMExpression
  ) where

import Language.Jass.Parser.AST as AST
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Type
import Language.Jass.Runtime.String
import Language.Jass.Runtime.Code
import LLVM.General.AST as LLVM
import qualified LLVM.General.AST.IntegerPredicate as LLVMI
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import LLVM.General.AST.Global as Glob
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant as Const
import LLVM.General.AST.Float as Const
import LLVM.General.AST.Type
import Control.Applicative
import Control.Monad.Error
import Data.Char

-- | Generates instruction to calculate 
genLLVMExpression :: Expression -> Codegen (Name, [Named Instruction])
genLLVMExpression (BinaryExpression _ op left right) = do
  (leftNameRaw, leftInstr) <- genLLVMExpression left
  (rightNameRaw, rightInstr) <- genLLVMExpression right
  opName <- generateName $ showBinaryOpAsWord op
  leftJassType <- inferType left
  rightJassType <- inferType right
  mgeneralType <- getGeneralType leftJassType rightJassType
  case mgeneralType of
    Nothing -> throwError $ strMsg $ "ICE: cannot use types " ++ show leftJassType ++ " and " ++ show rightJassType ++ " at " ++ show op
    Just JString -> if
      | op == Summ -> do
        let strAddCall = [opName := Call False C [] 
                         (Right $ ConstantOperand $ GlobalReference (ptr i8) (Name strAddFuncName)) 
                         [(LocalReference (ptr i8) leftNameRaw, []), (LocalReference (ptr i8) rightNameRaw, [])] [] []]
        return (opName, leftInstr ++ rightInstr ++ strAddCall)
      | isRelationalOperator op -> do
        cmpName <- generateName "strcmp"
        let strCmpCall = [cmpName := Call False C [] 
                         (Right $ ConstantOperand $ GlobalReference i32 (Name strCmpFunctionName)) 
                         [(LocalReference (ptr i8) leftNameRaw, []), (LocalReference (ptr i8) rightNameRaw, [])] [] []]
        let stringRelationInstrs llvmOp constVal = [opName := LLVM.ICmp llvmOp (LocalReference i32 cmpName) (ConstantOperand $ Const.Int 32 constVal) []]
        let strRelInstrs = case op of
                            Equal -> stringRelationInstrs LLVMI.EQ 0
                            NotEqual -> stringRelationInstrs LLVMI.NE 0
                            Greater -> stringRelationInstrs LLVMI.EQ 1
                            Less -> stringRelationInstrs LLVMI.EQ (-1)
                            GreaterEqual -> stringRelationInstrs LLVMI.NE (-1)
                            LessEqual -> stringRelationInstrs LLVMI.NE 1
                            _ -> error $ "ICE: unknown relational operator " ++ show op
        return (opName, leftInstr ++ rightInstr ++ strCmpCall ++ strRelInstrs)
      | otherwise -> throwError $ strMsg $ "ICE: unsupported operator " ++ show op ++ " for strings"
    Just generalType -> do 
      (leftName, leftConvInstr) <- genConvertion leftJassType generalType leftNameRaw
      (rightName, rightConvInstr) <- genConvertion rightJassType generalType rightNameRaw
      genType <- toLLVMType generalType
      let instr = opName := genBinaryOp op genType leftName rightName
      return (opName, leftInstr ++ leftConvInstr ++ rightInstr ++ rightConvInstr ++ [instr])
genLLVMExpression expr@(UnaryExpression _ op val) = do
  (valNameRaw, valInstr) <- genLLVMExpression val
  resJassType <- inferType expr
  valJassType <- inferType val
  (valName, valConvInstr) <- genConvertion valJassType resJassType valNameRaw
  opName <- generateName $ showUnaryOpAsWord op
  resType <- toLLVMType resJassType
  let instr = opName := genUnaryOp op resType valName
  return (opName, valInstr ++ valConvInstr ++ [instr])
genLLVMExpression expr@(ArrayReference _ arrName indExpr) = do
  (indName, indInstr) <- genLLVMExpression indExpr
  ptrName <- generateName "arrptr"
  opName <- generateName "arrelem"
  indType <- toLLVMType =<< inferType indExpr
  (_, ref) <- getReference arrName
  elemType <- ptr <$> (toLLVMType =<< inferType expr) 
  return (opName, indInstr ++ [
      ptrName := LLVM.GetElementPtr True ref [ConstantOperand $ Const.Int 32 0, LocalReference indType indName] [],
      opName := LLVM.Load False (LocalReference elemType ptrName) Nothing 0 []
    ])
genLLVMExpression (FunctionCall _ funcName args) = do
  args' <- mapM genLLVMExpression args
  let (argNames, argInstr) = unzip args' 
  funcType <- ptr <$> getFunctionType funcName
  argTypes <- getFunctionArgumentsTypes funcName
  opName <- generateName "callres"
  let argOperands = uncurry LocalReference <$> zip argTypes argNames
  let callInstr = [opName := Call False C [] 
                  (Right $ ConstantOperand $ GlobalReference funcType (Name funcName)) 
                  (zip argOperands (repeat [])) [] []]
  return (opName, concat argInstr ++ callInstr)
genLLVMExpression (FunctionReference _ nm) = do
  mc <- getCallable nm
  case mc of
    Nothing -> throwError $ strMsg $ "ICE: cannot find function " ++ nm
    Just callable -> generateCodeValue callable
genLLVMExpression (VariableReference _ varName) = do
  opName <- generateName "varptr"
  (_, ref) <- getReference varName
  return (opName, [opName := Load False ref Nothing 0 []])
    
genLLVMExpression (IntegerLiteral _ val) = allocLiteral JInteger $ Const.Int 32 $ toInteger val
genLLVMExpression (RealLiteral _ val) = allocLiteral JReal $ Const.Float $ Single val
genLLVMExpression (BoolLiteral _ val) = allocLiteral JBoolean $ Const.Int 1 $ if val then 1 else 0
genLLVMExpression (NullLiteral _) = do
  tp <- toLLVMType JHandle
  allocLiteral JHandle $ Const.Null tp
genLLVMExpression (StringLiteral _ val) = do
  opName <- generateName "strptr"
  let size = fromIntegral(length val + 1)
  let chars = Const.Int 8 . toInteger . ord <$> val ++ "\0"
  literalName <- generateGlobalName "str"
  addDefinition $ GlobalDefinition $ globalVariableDefaults {
        name = literalName
      , Glob.type' = ArrayType size i8
      , isConstant = True
      , initializer = Just $ Const.Array i8 chars
    }
  let zero = ConstantOperand $ Const.Int 32 0
  return (opName, [
    opName := LLVM.GetElementPtr True (ConstantOperand $ GlobalReference (ptr $ ArrayType size i8) literalName) 
              [zero, zero] []
    ])

genBinaryOp :: BinaryOperator -> Type -> Name -> Name -> Instruction
genBinaryOp AST.And res left right = LLVM.And (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.Or res left right = LLVM.Or (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.Equal res left right
  | isIntegralType res = LLVM.ICmp LLVMI.EQ (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: equality op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.OEQ (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.NotEqual res left right 
  | isIntegralType res = LLVM.ICmp LLVMI.NE (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: not-equality op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.ONE (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.GreaterEqual res left right 
  | isIntegralType res = LLVM.ICmp LLVMI.UGE (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: greater equal op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.OGE (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.LessEqual res left right 
  | isIntegralType res = LLVM.ICmp LLVMI.ULE (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: less equal op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.OLE (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.Greater res left right 
  | isIntegralType res = LLVM.ICmp LLVMI.UGT (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: greater op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.OGT (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.Less res left right 
  | isIntegralType res = LLVM.ICmp LLVMI.ULT (LocalReference res left) (LocalReference res right) []
  | isStringType res = error "ICE: less op generation for string malformed call"
  | otherwise = LLVM.FCmp LLVMF.OLT (LocalReference res left) (LocalReference res right) [] 
genBinaryOp AST.Summ rest left right 
  | isIntegralType rest = LLVM.Add False False (LocalReference rest left) (LocalReference rest right) []
  | isStringType rest = error "ICE: summ op generation for string malformed call"
  | otherwise = LLVM.FAdd fastMathflags (LocalReference rest left) (LocalReference rest right) []
genBinaryOp AST.Substract rest left right 
  | isIntegralType rest = LLVM.Sub False False (LocalReference rest left) (LocalReference rest right) []
  | otherwise = LLVM.FSub fastMathflags (LocalReference rest left) (LocalReference rest right) []
genBinaryOp AST.Multiply rest left right 
  | isIntegralType rest = LLVM.Mul False False (LocalReference rest left) (LocalReference rest right) []
  | otherwise = LLVM.FMul fastMathflags (LocalReference rest left) (LocalReference rest right) []
genBinaryOp AST.Divide rest left right
  | isIntegralType rest = LLVM.SDiv False (LocalReference rest left) (LocalReference rest right) []
  | otherwise = LLVM.FDiv fastMathflags (LocalReference rest left) (LocalReference rest right) []
genBinaryOp AST.Reminder rest left right
  | isIntegralType rest = LLVM.SRem (LocalReference rest left) (LocalReference rest right) []
  | otherwise = LLVM.FRem fastMathflags (LocalReference rest left) (LocalReference rest right) []

genUnaryOp :: UnaryOperator -> Type -> Name -> Instruction
genUnaryOp AST.Plus rest val
  | isIntegralType rest = LLVM.Mul False False (ConstantOperand $ Const.Int 32 1) (LocalReference rest val) []
  | otherwise = LLVM.FMul fastMathflags (ConstantOperand $ Const.Float $ Single 1.0) (LocalReference rest val) []
genUnaryOp AST.Negation rest val
  | isIntegralType rest = LLVM.Mul False False (ConstantOperand $ Const.Int 32 (-1)) (LocalReference rest val) []
  | otherwise = LLVM.FMul fastMathflags (ConstantOperand $ Const.Float $ Single (-1.0)) (LocalReference rest val) []
genUnaryOp AST.Not rest val = LLVM.Xor (ConstantOperand $ Const.Int 1 1) (LocalReference rest val) []

fastMathflags :: FastMathFlags
fastMathflags = FastMathFlags {
    noNaNs = True,
    noInfs = True,
    noSignedZeros = True,
    allowReciprocal = True
  }

allocLiteral :: JassType -> Constant -> Codegen (Name, [Named Instruction])
allocLiteral litType val = do
  allocName <- generateName "litptr"
  opName <- generateName "litval"
  tp <- toLLVMType litType
  let ptrRef = LocalReference (ptr tp) allocName
  return (opName, [
    allocName := LLVM.Alloca tp Nothing 0 [],
    Do $ LLVM.Store False ptrRef (ConstantOperand val) Nothing 0 [],
    opName := LLVM.Load False ptrRef Nothing 0 [] 
    ])
    
-- | Generates type casting code (int -> float and etc)
genConvertion :: JassType -> JassType -> Name -> Codegen (Name, [Named Instruction])
genConvertion JReal JInteger valName = convHelper JReal JInteger $ \tpSource tpDist -> LLVM.FPToSI (LocalReference tpSource valName) tpDist []
genConvertion JInteger JReal valName = convHelper JInteger JReal $ \tpSource tpDist -> LLVM.SIToFP (LocalReference tpSource valName) tpDist []
genConvertion JInteger JBoolean valName = convHelper JInteger JBoolean $ \tpSource tpDist -> LLVM.Trunc (LocalReference tpSource valName) tpDist []
genConvertion JBoolean JInteger valName = convHelper JBoolean JInteger $ \tpSource tpDist -> LLVM.ZExt (LocalReference tpSource valName) tpDist []
genConvertion from to valName 
  | from == to = return (valName, [])
  | otherwise = throwError $ strMsg $ "ICE: cannot convert from " ++ show from ++ " to " ++ show to 

convHelper :: JassType -> JassType -> (Type -> Type -> Instruction) -> Codegen (Name, [Named Instruction]) 
convHelper tSource tDist instr= do
  opName <- generateName $ "to" ++ show tDist
  tpSource <- toLLVMType tSource
  tpDist <- toLLVMType tDist
  return (opName, [opName := instr tpSource tpDist])