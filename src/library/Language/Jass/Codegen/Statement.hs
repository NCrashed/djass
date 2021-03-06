module Language.Jass.Codegen.Statement(
    genBodyBlocks
  ) where

import Language.Jass.Parser.AST
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Expression
import Language.Jass.Codegen.Type
import LLVM.General.AST as LLVM
import LLVM.General.AST.CallingConvention as LLVM
import LLVM.General.AST.Type as LLVM
import LLVM.General.AST.Constant as Const
import LLVM.General.AST.Instruction as Instr
import Control.Monad.Except
import Control.Arrow
import Data.Maybe

genBodyBlocks :: Name -> [Statement] -> Codegen [BasicBlock]
genBodyBlocks entryName stmts = do
  epilogueName <- generateName "epilogue"
  (start, blocks) <- genBlocks stmts epilogueName
  let entryBlock = BasicBlock entryName [] (Do $ Br start [])
  pushNewBlock epilogueName
  retType <- getFunctionReturnType =<< getCurrentFunction
  let endTerm = case retType of
                  VoidType -> Ret Nothing []
                  _ -> Unreachable []
  appendCurrentBlock =<< getEpilogueInstructions
  finishCurrentBlock endTerm
  [endBlock] <- purgeBlocks
  finalBlocks <- redirectToEpilogue retType epilogueName endBlock blocks
  return $ entryBlock:finalBlocks

-- | Replaces all returns in blocks to jumps into epilogue
redirectToEpilogue :: LLVM.Type -> Name -> BasicBlock -> [BasicBlock] -> Codegen [BasicBlock]
redirectToEpilogue VoidType epilogueName epilogueBlock blocks =
  return $ (replaceRet <$> blocks) ++ [epilogueBlock]
  where
    replaceRet (BasicBlock n is (rn := Ret _ _)) = BasicBlock n is $ rn := Br epilogueName []
    replaceRet (BasicBlock n is (Do (Ret _ _))) = BasicBlock n is $ Do $ Br epilogueName []
    replaceRet b = b
    
redirectToEpilogue retType epilogueName epilogueBlock blocks = do
  let (blocks', phiVals) = second catMaybes $ unzip (modifyBlocks <$> blocks)
  epilogue' <- modifyEpilogue epilogueBlock phiVals
  return $ blocks' ++ [epilogue']
  where
    modifyBlocks (BasicBlock n is (rn := Ret mop _)) = (BasicBlock n is (rn := Br epilogueName []), Just (fromJust mop, n))
    modifyBlocks (BasicBlock n is (Do (Ret mop _))) = (BasicBlock n is (Do $ Br epilogueName []), Just (fromJust mop, n))
    modifyBlocks b
      | isJumpToEpilogue (getTerm b) = (b, Just (ConstantOperand $ Const.Undef retType, getName b))
      | otherwise = (b, Nothing)
    
    getTerm (BasicBlock _ _ (_ := term)) = term
    getTerm (BasicBlock _ _ (Do term)) = term
    
    getName (BasicBlock n _ _) = n
    
    isJumpToEpilogue (Br e _) = e == epilogueName
    isJumpToEpilogue (CondBr _ e1 e2 _) = e1 == epilogueName || e2 == epilogueName
    isJumpToEpilogue _ = False
     
    modifyEpilogue (BasicBlock n is _) phiVals = do
      tempName <- generateName "return"
      return $ BasicBlock n 
        ((tempName := Phi retType phiVals []) : is)
        (Do $ Ret (Just $ LocalReference retType tempName) [])
        
genBlocks :: [Statement] -> Name -> Codegen (Name, [BasicBlock])
genBlocks stmts nextBlock = catchBlocks $ do
  startName <- generateName "block"
  pushNewBlock startName
  mapM_ genLLVMStatement stmts
  finishCurrentBlock $ Br nextBlock []
  blocks <- purgeBlocks
  return (startName, reverse blocks)

catchBlocks :: Codegen a -> Codegen a
catchBlocks action = do
  savedBlocks <- purgeBlocks
  res <- action
  purgeBlocks >> pushBlocks savedBlocks
  return res
  
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
  valType <- toLLVMType =<< inferType valExpr
  indType <- toLLVMType JInteger
  indexPtrName <- generateName "index"
  let indexInstr = indexPtrName := Instr.GetElementPtr True 
                   varRef
                   [ConstantOperand $ Const.Int 32 0, LocalReference indType indExprName] []
      storeInstr = Do $ Store False
                   (LocalReference elemType indexPtrName)
                   (LocalReference valType valExprName) Nothing 0 []
  appendCurrentBlock $ indExprInstr ++ valExprInstr ++ [indexInstr, storeInstr]
  
genLLVMStatement (IfThenElseStatement _ _ condExpr thenStmts elseifs) = do  
  (condExprName, condExprInstr) <- genLLVMExpression condExpr
  afterBlockName <- generateName "block_afterif"
  (thenStart, thenBlocks) <- genBlocks thenStmts afterBlockName
  elseifTriples <- mapM (genElseIf afterBlockName) elseifs
  let firstBranchName = getFirstElseBlock afterBlockName elseifTriples
  let condJump = CondBr 
                 (LocalReference i1 condExprName)
                 thenStart -- true
                 firstBranchName -- false
                 []
  appendCurrentBlock condExprInstr
  finishCurrentBlock condJump
  pushBlocks thenBlocks
  (_, blocks) <- catchBlocks $ foldM linkElses (afterBlockName, []) $ reverse elseifTriples
  pushBlocks $ reverse blocks
  pushNewBlock afterBlockName
  where
    getFirstElseBlock :: Name -> [(Maybe (Name, Name, [Named Instruction]), Name, [BasicBlock])] -> Name
    getFirstElseBlock defName [] = defName
    getFirstElseBlock _ ((Nothing, elseBlock, _):_) = elseBlock
    getFirstElseBlock _ ((Just (_, condBlock, _), _, _):_) = condBlock
    
    genElseIf :: 
      Name -- ^ Next block after if name
      -> (Maybe Expression, [Statement]) -- ^ Elseif clause
      -> Codegen (Maybe (Name, Name, [Named Instruction]), Name, [BasicBlock])  -- ^ Maybe expression for condition, name of start block and blocks
    genElseIf afterName (Nothing, stmts) = do
      (elseStart, elseBlocks) <- genBlocks stmts afterName
      return (Nothing, elseStart, elseBlocks)
    genElseIf afterName (Just cond, stmts) = do
      (condName, condInstr) <- genLLVMExpression cond
      (elseStart, elseBlocks) <- genBlocks stmts afterName
      futureCondBlock <- generateName "block_elseifcond"
      return (Just (condName, futureCondBlock, condInstr), elseStart, elseBlocks)
      

    linkElses :: (Name, [BasicBlock]) -> (Maybe (Name, Name, [Named Instruction]), Name, [BasicBlock]) -> Codegen (Name, [BasicBlock])
    linkElses _ (Nothing, elseName, elseBlocks) = return (elseName, elseBlocks)
    linkElses (nextBlock, accBlocks) (Just (condName, condBlockName, condInstr), branchName, branchBlocks) = do
      pushNewBlock condBlockName
      appendCurrentBlock condInstr
      finishCurrentBlock condJump
      condBlock <- purgeBlocks
      return (condBlockName, condBlock ++ branchBlocks ++ accBlocks)
      where condJump = CondBr (LocalReference i1 condName) branchName nextBlock []
      
genLLVMStatement (CallStatement _ _ name args) = do
  funcType <- ptr <$> getFunctionType name
  argsTypes <- getFunctionArgumentsTypes name
  (argsNames, argsInstrs) <- unzip <$> mapM genLLVMExpression args
  let argsRefs = uncurry LocalReference <$> zip argsTypes argsNames
  let callInstr = [Do $ Call False C [] 
                  (Right $ ConstantOperand $ GlobalReference funcType (Name name)) 
                  (zip argsRefs (repeat [])) [] []]
  appendCurrentBlock $ concat argsInstrs ++ callInstr
genLLVMStatement (LoopStatement _ _ stmts) = do
  preBlock <- generateName "block_loop_ptr"
  finishCurrentBlock (Br preBlock [])
  pushNewBlock preBlock
  
  afterBlock <- generateName "block_loop_after"
  savedRet <- getLoopReturnMaybe
  saveLoopReturn afterBlock
  (loopStart, loopBlocks) <- genBlocks stmts preBlock
  finishCurrentBlock (Br loopStart [])
  pushBlocks loopBlocks
  pushNewBlock afterBlock
  case savedRet of
    Nothing -> return ()
    Just ret -> saveLoopReturn ret
genLLVMStatement (ExitWhenStatement _ cond) = do
  (condName, condInstr) <- genLLVMExpression cond
  retName <- getLoopReturn
  afterBlock <- generateName "block_exitwhen_after"
  appendCurrentBlock condInstr
  finishCurrentBlock $ CondBr (LocalReference i1 condName) retName afterBlock []
  pushNewBlock afterBlock
genLLVMStatement (ReturnStatement _ Nothing) = finishCurrentBlock $ Ret Nothing []
genLLVMStatement (ReturnStatement _ (Just expr)) = do
  (exprName, epxrInstr) <- genLLVMExpression expr
  retType <- getFunctionReturnType =<< getCurrentFunction 
  appendCurrentBlock epxrInstr
  finishCurrentBlock $ Ret (Just $ LocalReference retType exprName) []