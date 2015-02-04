module Language.Jass.Codegen.Statement(
    genBodyBlocks
  ) where

import Language.Jass.Parser.AST
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Expression
import Language.Jass.Codegen.Type
import Control.Monad.Error
import Control.Applicative
import LLVM.General.AST as LLVM
import LLVM.General.AST.CallingConvention as LLVM
import LLVM.General.AST.Type as LLVM
import LLVM.General.AST.Constant as Const
import LLVM.General.AST.Instruction as Instr

genBodyBlocks :: [Statement] -> Codegen (Name, [BasicBlock])
genBodyBlocks stmts = do
  epilogueName <- generateName
  -- finishCurrentBlock $ Br epilogueName []
  (start, blocks) <- genBlocks stmts epilogueName
  pushNewBlock epilogueName
  retType <- getFunctionReturnType =<< getCurrentFunction
  let endTerm = case retType of
                  VoidType -> Ret Nothing []
                  _ -> Unreachable []
  finishCurrentBlock endTerm
  endBlock <- purgeBlocks
  return (start, init blocks ++ endBlock)
    
genBlocks :: [Statement] -> Name -> Codegen (Name, [BasicBlock])
genBlocks stmts nextBlock = do
  startName <- generateName
  pushNewBlock startName
  mapM_ genLLVMStatement stmts
  finishCurrentBlock $ Br nextBlock []
  blocks <- purgeBlocks
  return (startName, blocks)
          
genLLVMStatement :: Statement -> Codegen ()
genLLVMStatement (SetStatement _ _ name expr) = do
  (exprName, exprInstr) <- genLLVMExpression expr
  (varType, varRef) <- getReference name
  let saveInstr = Do $ Store False 
                  varRef 
                  (LocalReference varType exprName)
                  Nothing 0 []
  appendCurrentBlock $ exprInstr ++ [saveInstr]
genLLVMStatement (SetArrayStatement _ _ name indExpr valExpr) = do 
  (indExprName, indExprInstr) <- genLLVMExpression indExpr
  (valExprName, valExprInstr) <- genLLVMExpression valExpr
  (elemType, varRef) <- getReference name
  indType <- toLLVMType JInteger
  indexPtrName <- generateName
  let indexInstr = indexPtrName := Instr.GetElementPtr True 
                   varRef
                   [LocalReference indType indExprName] []
      storeInstr = Do $ Store False
                   (LocalReference elemType indexPtrName)
                   (LocalReference elemType valExprName) Nothing 0 []
  appendCurrentBlock $ indExprInstr ++ valExprInstr ++ [indexInstr, storeInstr]
genLLVMStatement (IfThenElseStatement _ _ condExpr thenStmts elseifs) = do  
  (condExprName, condExprInstr) <- genLLVMExpression condExpr
  afterBlockName <- generateName
  (thenStart, thenBlocks) <- genBlocks thenStmts afterBlockName
  elseifTriples <- mapM (genElseIf afterBlockName) elseifs
  let firstBranchName = if null elseifTriples then afterBlockName
      else let (_, nm, _) = head elseifTriples in nm
  let condJump = CondBr 
                 (LocalReference i1 condExprName)
                 thenStart -- true
                 firstBranchName -- false
                 []
  appendCurrentBlock condExprInstr
  finishCurrentBlock condJump
  pushBlocks thenBlocks
  __ <- foldM linkElses afterBlockName $ reverse elseifTriples
  pushNewBlock afterBlockName
  where
    genElseIf :: 
      Name -- ^ Next block after if name
      -> (Maybe Expression, [Statement]) -- ^ Elseif clause
      -> Codegen (Maybe (Name, [Named Instruction]), Name, [BasicBlock])  -- ^ Maybe expression for condition, name of start block and blocks
    genElseIf afterName (Nothing, stmts) = do
      (elseStart, elseBlocks) <- genBlocks stmts afterName
      return (Nothing, elseStart, elseBlocks)
    genElseIf afterName (Just cond, stmts) = do
      condPair <- genLLVMExpression cond
      (elseStart, elseBlocks) <- genBlocks stmts afterName
      return (Just condPair, elseStart, elseBlocks)
      

    linkElses :: Name -> (Maybe (Name, [Named Instruction]), Name, [BasicBlock]) -> Codegen Name
    linkElses _ (Nothing, elseName, elseBlocks) = do
      pushBlocks elseBlocks
      return elseName
    linkElses nextBlock (Just (condName, condInstr), branchName, branchBlocks) = do
      thisName <- generateName
      pushNewBlock thisName
      appendCurrentBlock condInstr
      finishCurrentBlock condJump
      pushBlocks branchBlocks
      return thisName
      where condJump = CondBr (LocalReference i1 condName) branchName nextBlock []
genLLVMStatement (CallStatement _ _ name args) = do
  funcType <- ptr <$> getFunctionType name
  argsTypes <- getFunctionArgumentsTypes name
  (argsNames, argsInstrs) <- unzip <$> mapM genLLVMExpression args
  isNative <- isDefinedNative name
  let argsRefs = uncurry LocalReference <$> zip argsTypes argsNames
  
  callInstr <- if not isNative then 
                return [Do $ Call False C [] 
                       (Right $ ConstantOperand $ GlobalReference funcType (Name name)) 
                       (zip argsRefs (repeat [])) [] []]
               else do
                tempName <- generateName
                return [
                  tempName := Load False (ConstantOperand $ GlobalReference (ptr funcType) (Name name)) Nothing 0 [],
                  Do $ Call False C [] (Right $ LocalReference funcType tempName) (zip argsRefs (repeat [])) [] []]
  appendCurrentBlock $ concat argsInstrs ++ callInstr
genLLVMStatement (LoopStatement _ _ stmts) = do
  preBlock <- generateName
  finishCurrentBlock (Br preBlock [])
  pushNewBlock preBlock
  
  afterBlock <- generateName
  saveLoopReturn afterBlock
  (loopStart, loopBlocks) <- genBlocks stmts preBlock
  finishCurrentBlock (Br loopStart [])
  pushBlocks loopBlocks
  pushNewBlock afterBlock
genLLVMStatement (ExitWhenStatement _ cond) = do
  (condName, condInstr) <- genLLVMExpression cond
  retName <- getLoopReturn
  afterBlock <- generateName
  appendCurrentBlock condInstr
  finishCurrentBlock $ CondBr (LocalReference i1 condName) retName afterBlock []
  pushNewBlock afterBlock
genLLVMStatement (ReturnStatement _ Nothing) = finishCurrentBlock $ Ret Nothing []
genLLVMStatement (ReturnStatement _ (Just expr)) = do
  (exprName, epxrInstr) <- genLLVMExpression expr
  retType <- getFunctionReturnType =<< getCurrentFunction 
  appendCurrentBlock epxrInstr
  finishCurrentBlock $ Ret (Just $ LocalReference retType exprName) []