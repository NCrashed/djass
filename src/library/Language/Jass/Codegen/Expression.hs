module Language.Jass.Codegen.Expression(
    genLLVMExpression
  ) where

import Language.Jass.Parser.AST as AST
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Type
import LLVM.General.AST as LLVM
import LLVM.General.AST.IntegerPredicate as LLVM
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
genLLVMExpression expr@(BinaryExpression _ op left right) = do
  (leftNameRaw, leftInstr) <- genLLVMExpression left
  (rightNameRaw, rightInstr) <- genLLVMExpression right
  opName <- generateName
  leftJassType <- inferType left
  rightJassType <- inferType right
  resJassType <- inferType expr
  (leftName, leftConvInstr) <- genConvertion leftJassType resJassType leftNameRaw
  (rightName, rightConvInstr) <- genConvertion rightJassType resJassType rightNameRaw
  resType <- toLLVMType resJassType
  let instr = opName := genBinaryOp op resType leftName rightName
  return (opName, leftInstr ++ leftConvInstr ++ rightInstr ++ rightConvInstr ++ [instr])
genLLVMExpression expr@(UnaryExpression _ op val) = do
  (valNameRaw, valInstr) <- genLLVMExpression val
  resJassType <- inferType expr
  valJassType <- inferType val
  (valName, valConvInstr) <- genConvertion valJassType resJassType valNameRaw
  opName <- generateName
  resType <- toLLVMType resJassType
  let instr = opName := genUnaryOp op resType valName
  return (opName, valInstr ++ valConvInstr ++ [instr])
genLLVMExpression (ArrayReference _ arrName indExpr) = do
  (indName, indInstr) <- genLLVMExpression indExpr
  ptrName <- generateName
  opName <- generateName
  indType <- toLLVMType =<< inferType indExpr
  (refType, ref) <- getReference arrName
  return (opName, indInstr ++ [
      ptrName := LLVM.GetElementPtr True ref [LocalReference indType indName] [],
      opName := LLVM.Load False (LocalReference refType opName) Nothing 0 []
    ])
genLLVMExpression (FunctionCall _ funcName args) = do
  args' <- mapM genLLVMExpression args
  let (argNames, argInstr) = unzip args' 
  funcType <- ptr <$> getFunctionType funcName
  argTypes <- getFunctionArgumentsTypes funcName
  opName <- generateName
  let argOperands = uncurry LocalReference <$> zip argTypes argNames
  isNative <- isDefinedNative funcName
  callInstr <- if not isNative then 
                return [Do $ Call False C [] 
                       (Right $ ConstantOperand $ GlobalReference funcType (Name funcName)) 
                       (zip argOperands (repeat [])) [] []]
               else do
                tempName <- generateName
                return [
                  tempName := Load False (ConstantOperand $ GlobalReference (ptr funcType) (Name funcName)) Nothing 0 [],
                  Do $ Call False C [] (Right $ LocalReference funcType tempName) (zip argOperands (repeat [])) [] []]
  return (opName, concat argInstr ++ callInstr)
genLLVMExpression (FunctionReference _ _) = throwError $ strMsg "ICE: function references aren't implemented!" --TODO: here
genLLVMExpression (VariableReference _ varName) = do
  isPar <- isParameter varName
  if isPar then return (Name varName, []) else do
    opName <- generateName
    (_, ref) <- getReference varName
    return (opName, [opName := Load False ref Nothing 0 []])
    
genLLVMExpression (IntegerLiteral _ val) = allocLiteral JInteger $ Const.Int 32 $ toInteger val
genLLVMExpression (RealLiteral _ val) = allocLiteral JReal $ Const.Float $ Single val
genLLVMExpression (BoolLiteral _ val) = allocLiteral JReal $ Const.Int 1 $ if val then 1 else 0
genLLVMExpression (NullLiteral _) = do
  tp <- toLLVMType JHandle
  allocLiteral JHandle $ Const.Null tp
genLLVMExpression (StringLiteral _ val) = do
  opName <- generateName
  let size = fromIntegral(length val + 1)
  let chars = Const.Int 8 . toInteger . ord <$> val ++ "\0"
  literalName <- generateGlobalName
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
genBinaryOp AST.Equal res left right = LLVM.ICmp LLVM.EQ (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.NotEqual res left right = LLVM.ICmp LLVM.NE (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.GreaterEqual res left right = LLVM.ICmp LLVM.UGE (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.LessEqual res left right = LLVM.ICmp LLVM.ULE (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.Greater res left right = LLVM.ICmp LLVM.UGT (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.Less res left right = LLVM.ICmp LLVM.ULT (LocalReference res left) (LocalReference res right) []
genBinaryOp AST.Summ rest left right 
  | isIntegralType rest = LLVM.Add False False (LocalReference rest left) (LocalReference rest right) []
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
  allocName <- generateName
  opName <- generateName
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
  opName <- generateName
  tpSource <- toLLVMType tSource
  tpDist <- toLLVMType tDist
  return (opName, [opName := instr tpSource tpDist])