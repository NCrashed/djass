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
  (leftName, leftInstr) <- genLLVMExpression left
  (rightName, rightInstr) <- genLLVMExpression right
  opName <- generateName
  resType <- toLLVMType =<< inferType expr
  leftType <- toLLVMType =<< inferType left
  rightType <- toLLVMType =<< inferType right
  let instr = opName := genBinaryOp op resType leftType leftName rightType rightName
  return (opName, leftInstr ++ rightInstr ++ [instr])
genLLVMExpression expr@(UnaryExpression _ op val) = do
  (valName, valInstr) <- genLLVMExpression val
  opName <- generateName
  resType <- toLLVMType =<< inferType expr
  valType <- toLLVMType =<< inferType val
  let instr = opName := genUnaryOp op resType valType valName
  return (opName, valInstr ++ [instr])
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

genBinaryOp :: BinaryOperator -> Type -> Type -> Name -> Type -> Name -> Instruction
genBinaryOp AST.And _ tl left tr right = LLVM.And (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Or _ tl left tr right = LLVM.Or (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Equal _ tl left tr right = LLVM.ICmp LLVM.EQ (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.NotEqual _ tl left tr right = LLVM.ICmp LLVM.NE (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.GreaterEqual _ tl left tr right = LLVM.ICmp LLVM.UGE (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.LessEqual _ tl left tr right = LLVM.ICmp LLVM.ULE (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Greater _ tl left tr right = LLVM.ICmp LLVM.UGT (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Less _ tl left tr right = LLVM.ICmp LLVM.ULT (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Summ rest tl left tr right 
  | isIntegralType rest = LLVM.Add False False (LocalReference tl left) (LocalReference tr right) []
  | otherwise = LLVM.FAdd fastMathflags (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Substract rest tl left tr right 
  | isIntegralType rest = LLVM.Sub False False (LocalReference tl left) (LocalReference tr right) []
  | otherwise = LLVM.FSub fastMathflags (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Multiply rest tl left tr right 
  | isIntegralType rest = LLVM.Mul False False (LocalReference tl left) (LocalReference tr right) []
  | otherwise = LLVM.FMul fastMathflags (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Divide rest tl left tr right 
  | isIntegralType rest = LLVM.SDiv False (LocalReference tl left) (LocalReference tr right) []
  | otherwise = LLVM.FDiv fastMathflags (LocalReference tl left) (LocalReference tr right) []
genBinaryOp AST.Reminder rest tl left tr right 
  | isIntegralType rest = LLVM.SRem (LocalReference tl left) (LocalReference tr right) []
  | otherwise = LLVM.FRem fastMathflags (LocalReference tl left) (LocalReference tr right) []

genUnaryOp :: UnaryOperator -> Type -> Type -> Name -> Instruction
genUnaryOp AST.Plus rest t val
  | isIntegralType rest = LLVM.Mul False False (ConstantOperand $ Const.Int 32 1) (LocalReference t val) []
  | otherwise = LLVM.FMul fastMathflags (ConstantOperand $ Const.Float $ Single 1.0) (LocalReference t val) []
genUnaryOp AST.Negation rest t val
  | isIntegralType rest = LLVM.Mul False False (ConstantOperand $ Const.Int 32 (-1)) (LocalReference t val) []
  | otherwise = LLVM.FMul fastMathflags (ConstantOperand $ Const.Float $ Single (-1.0)) (LocalReference t val) []
genUnaryOp AST.Not _ t val = LLVM.Xor (ConstantOperand $ Const.Int 1 1) (LocalReference t val) []

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