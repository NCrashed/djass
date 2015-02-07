module Language.Jass.Runtime.String(
    strCmpFunctionName
  , strLenFuncName
  , strAddFuncName
  , getStringUtilityDefs
  ) where

import Language.Jass.Runtime.Memory
import LLVM.General.AST as LLVM
import LLVM.General.AST.Global as LLVM
import LLVM.General.AST.Visibility
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Linkage
import LLVM.General.AST.Type
import LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Constant as Const

getStringUtilityDefs :: [LLVM.Definition]
getStringUtilityDefs = [getStrCmpFunction, getStrLenFunction, getStrAddFunction]

strCmpFunctionName :: String
strCmpFunctionName = "$__jass__strcmp"

-- | Compiled from:
-- int strcmp(char* s1, char* s2)
-- {
--     for ( ; *s1 == *s2; s1++, s2++)
--       if (*s1 == '\0')
--         return 0;
--     return ((*(unsigned char *)s1 == *(unsigned char *)s2) ? -1 : +1);
-- }
getStrCmpFunction :: LLVM.Definition
getStrCmpFunction = GlobalDefinition $ functionDefaults {
  visibility = Hidden,
  linkage = Private,
  returnType = i32,
  name = Name strCmpFunctionName,
  parameters = ([Parameter (ptr i8) (Name "s1") [], Parameter (ptr i8) (Name "s2") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      UnName 0 := Load False (LocalReference (ptr i8) $ Name "s1") Nothing 0 [],
      UnName 1 := Load False (LocalReference (ptr i8) $ Name "s2") Nothing 0 [],
      Name "cmp12" := ICmp IP.EQ (LocalReference i8 $ UnName 0) (LocalReference i8 $ UnName 1) []
      ] (Do $  CondBr (LocalReference i1 $ Name "cmp12") (Name "for.body.preheader") (Name "return") []),
    BasicBlock (Name "for.body.preheader") [] (Do $ Br (Name "for.body") []),
    BasicBlock (Name "for.body") [
      UnName 2 := Phi i8 [(LocalReference i8 $ UnName 3, Name "for.inc"), (LocalReference i8 $ UnName 0, Name "for.body.preheader")] [],
      Name "s2.addr.014" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr4", Name "for.inc"), (LocalReference (ptr i8) $ Name "s2", Name "for.body.preheader")] [],
      Name "s1.addr.013" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr", Name "for.inc"), (LocalReference (ptr i8) $ Name "s1", Name "for.body.preheader")] [],
      Name "cmp3" := ICmp IP.EQ (LocalReference i8 $ UnName 2) (ConstantOperand $ Const.Int 8 0) []
      ] (Do $ CondBr (LocalReference i1 $ Name "cmp3") (Name "return.loopexit") (Name "for.inc") []),
    BasicBlock (Name "for.inc") [
      Name "incdec.ptr" := GetElementPtr True (LocalReference (ptr i8) $ Name "s1.addr.013") [ConstantOperand $ Const.Int 64 1] [],
      Name "incdec.ptr4" := GetElementPtr True (LocalReference (ptr i8) $ Name "s2.addr.014") [ConstantOperand $ Const.Int 64 1] [],
      UnName 3 := Load False (LocalReference (ptr i8) $ Name "incdec.ptr") Nothing 0 [],
      UnName 4 := Load False (LocalReference (ptr i8) $ Name "incdec.ptr4") Nothing 0 [],
      Name "cmp" := ICmp IP.EQ (LocalReference i8 $ UnName 3) (LocalReference i8 $ UnName 4) []
      ] (Do $  CondBr (LocalReference i1 $ Name "cmp") (Name "for.body") (Name "return.loopexit") []),
    BasicBlock (Name "return.loopexit") [
      Name "retval.0.ph" := Phi i32 [(ConstantOperand $ Const.Int 32 1, Name "for.inc"), (ConstantOperand $ Const.Int 32 0, Name "for.body")] []
      ] (Do $  Br (Name "return") []),
    BasicBlock (Name "return") [
      Name "retval.0" := Phi i32 [(ConstantOperand $ Const.Int 32 1, Name "entry"), (LocalReference i32 $ Name "retval.0.ph", Name "return.loopexit")] []
      ] (Do $ Ret (Just $ LocalReference i32 $ Name "retval.0") [])
    ]
}

strLenFuncName :: String
strLenFuncName = "$__jass__strlen"

-- | Compiled from
-- int strlen(char* s) {
--   int i = 0;
--   for(; *s != 0; s++, i++) {}
--   return i;
-- }
getStrLenFunction :: Definition
getStrLenFunction = GlobalDefinition $ functionDefaults {
  visibility = Hidden,
  linkage = Private,
  returnType = i32,
  name = Name strLenFuncName,
  parameters = ([Parameter (ptr i8) (Name "s") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      UnName 0 := Load False (LocalReference (ptr i8) $ Name "s") Nothing 0 [],
      Name "cmp4" := ICmp IP.EQ (LocalReference i8 $ UnName 0) (ConstantOperand $ Const.Int 8 0) []
    ] (Do $ CondBr (LocalReference i1 $ Name "cmp4") (Name "for.end") (Name "for.inc.preheader") []),
    BasicBlock (Name "for.inc.preheader") [] (Do $ Br (Name "for.inc") []),
    BasicBlock (Name "for.inc") [
      Name "i.06" := Phi i32 [(LocalReference i32 $ Name "inc", Name "for.inc"), (ConstantOperand $ Const.Int 32 0, Name "for.inc.preheader")] [],
      Name "s.addr.05" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr", Name "for.inc"), (LocalReference (ptr i8) $ Name "s", Name "for.inc.preheader")] [],
      Name "incdec.ptr" := GetElementPtr True (LocalReference (ptr i8) $ Name "s.addr.05") [ConstantOperand $ Const.Int 64 1] [],
      Name "inc" := Add True False (LocalReference i32 $ Name "i.06") (ConstantOperand $ Const.Int 32 1) [],
      UnName 1 := Load False (LocalReference (ptr i8) $ Name "incdec.ptr") Nothing 0 [],
      Name "cmp" := ICmp IP.EQ (LocalReference i8 $ UnName 1) (ConstantOperand $ Const.Int 8 0) []
    ] (Do $ CondBr (LocalReference i1 $ Name "cmp") (Name "for.end.loopexit") (Name "for.inc") []),
    BasicBlock (Name "for.end.loopexit") [
      Name "inc.lcssa" := Phi i32 [(LocalReference i32 $ Name "inc", Name "for.inc")] []
    ] (Do $ Br (Name "for.end") []),
    BasicBlock (Name "for.end") [
      Name "i.0.lcssa" := Phi i32 [(ConstantOperand $ Const.Int 32 0, Name "entry"), (LocalReference i32 $ Name "inc.lcssa", Name "for.end.loopexit")] []
    ] (Do $ Ret (Just $ LocalReference i32 $ Name "i.0.lcssa") [])
  ]
}

strAddFuncName :: String
strAddFuncName = "$__jass__stradd"

-- | Compiled from:
--char* stradd(char* s1, char* s2)
--{
--  int size = strlen(s1) + strlen(s2) + 1;
--  char* s = allocMemory(size);
--  char* si = s;
--  for(;*s1 != 0; s1++, si++) *si = *s1;
--  for(;*s2 != 0; s2++, si++) *si = *s2;
--  *si = 0;
--  return s;
--}
getStrAddFunction :: LLVM.Definition
getStrAddFunction = GlobalDefinition $ functionDefaults {
  visibility = Hidden,
  linkage = Private,
  returnType = ptr i8,
  name = Name strAddFuncName,
  parameters = ([Parameter (ptr i8) (Name "s1") [], Parameter (ptr i8) (Name "s2") []], False),
  basicBlocks = [
      BasicBlock (Name "entry") [
        Name "call" := Call True C [] (Right $ ConstantOperand $ Const.GlobalReference i32 $ Name strLenFuncName) [(LocalReference (ptr i8) $ Name "s1", [])] [] [],
        Name "call1" := Call True C [] (Right $ ConstantOperand $ Const.GlobalReference i32 $ Name strLenFuncName) [(LocalReference (ptr i8) $ Name "s2", [])] [] [],
        Name "add" := Add False False (LocalReference i32 $ Name "call") (ConstantOperand $ Const.Int 32 1) [],
        Name "add2" := Add False False (LocalReference i32 $ Name "add") (LocalReference i32 $ Name "call1") [],
        Name "call3" := Call True C [] (Right $ ConstantOperand $ Const.GlobalReference (ptr i8) $ Name allocMemoryFuncName) [(LocalReference i32 $ Name "add2", [])] [] [],
        UnName 0 := Load False (LocalReference (ptr i8) $ Name "s1") Nothing 0 [],
        Name "cmp29" := ICmp IP.EQ (LocalReference i8 $ UnName 0) (ConstantOperand $ Const.Int 8 0) []
      ] (Do $ CondBr (LocalReference i1 $ Name "cmp29") (Name "for.cond6.preheader") (Name "for.body.preheader") []),
      BasicBlock (Name "for.body.preheader") [] (Do $ Br (Name "for.body") []),
      BasicBlock (Name "for.cond6.preheader.loopexit") [
        Name "incdec.ptr5.lcssa" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr5", Name "for.body")] []
      ] (Do $ Br (Name "for.cond6.preheader") []),
      BasicBlock (Name "for.cond6.preheader") [
        Name "si.0.lcssa" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "call3", Name "entry"), (LocalReference (ptr i8) $ Name "incdec.ptr5.lcssa", Name "for.cond6.preheader.loopexit")] [],
        UnName 1 := Load False (LocalReference (ptr i8) $ Name "s2") Nothing 0 [],
        Name "cmp826" := ICmp IP.EQ (LocalReference i8 $ UnName 1) (ConstantOperand $ Const.Int 8 0) []
      ] (Do $ CondBr (LocalReference i1 $ Name "cmp826") (Name "for.end14") (Name "for.body10.preheader") []),
      BasicBlock (Name "for.body10.preheader") [] (Do $ Br (Name "for.body10") []),
      BasicBlock (Name "for.body") [
        UnName 2 := Phi i8 [(LocalReference i8 $ UnName 3, Name "for.body"), (LocalReference i8 $ UnName 0, Name "for.body.preheader")] [],
        Name "si.031" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr5", Name "for.body"), (LocalReference (ptr i8) $ Name "call3", Name "for.body.preheader")] [],
        Name "s1.addr.030" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr", Name "for.body"), (LocalReference (ptr i8) $ Name "s1", Name "for.body.preheader")] [],
        Do $ Store False (LocalReference (ptr i8) $ Name "si.031") (LocalReference i8 $ UnName 2) Nothing 0 [],
        Name "incdec.ptr" := GetElementPtr True (LocalReference (ptr i8) $ Name "s1.addr.030") [ConstantOperand $ Const.Int 64 1] [],
        Name "incdec.ptr5" := GetElementPtr True (LocalReference (ptr i8) $ Name "si.031") [ConstantOperand $ Const.Int 64 1] [],
        UnName 3 := Load False (LocalReference (ptr i8) $ Name "incdec.ptr") Nothing 0 [],
        Name "cmp" := ICmp IP.EQ (LocalReference i8 $ UnName 3) (ConstantOperand $ Const.Int 8 0) []
      ] (Do $ CondBr (LocalReference i1 $ Name "cmp") (Name "for.cond6.preheader.loopexit") (Name "for.body") []),
      BasicBlock (Name "for.body10") [
        UnName 4 := Phi i8 [(LocalReference i8 $ UnName 5, Name "for.body10"), (LocalReference i8 $ UnName 1, Name "for.body10.preheader")] [],
        Name "si.128" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr13", Name "for.body10"), (LocalReference (ptr i8) $ Name "si.0.lcssa", Name "for.body10.preheader")] [],
        Name "s2.addr.027" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr12", Name "for.body10"), (LocalReference (ptr i8) $ Name "s2", Name "for.body10.preheader")] [],
        Do $ Store False (LocalReference (ptr i8) $ Name "si.128") (LocalReference i8 $ UnName 4) Nothing 0 [],
        Name "incdec.ptr12" := GetElementPtr True (LocalReference (ptr i8) $ Name "s2.addr.027") [ConstantOperand $ Const.Int 64 1] [],
        Name "incdec.ptr13" := GetElementPtr True (LocalReference (ptr i8) $ Name "si.128") [ConstantOperand $ Const.Int 64 1] [],
        UnName 5 := Load False (LocalReference (ptr i8) $ Name "incdec.ptr12") Nothing 0 [],
        Name "cmp8" := ICmp IP.EQ (LocalReference i8 $ UnName 5) (ConstantOperand $ Const.Int 8 0) []
      ] (Do $ CondBr (LocalReference i1 $ Name "cmp8") (Name "for.end14.loopexit") (Name "for.body10") []),
      BasicBlock (Name "for.end14.loopexit") [
        Name "incdec.ptr13.lcssa" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "incdec.ptr13", Name "for.body10")] []
      ] (Do $ Br (Name "for.end14") []),
      BasicBlock (Name "for.end14") [
        Name "si.1.lcssa" := Phi (ptr i8) [(LocalReference (ptr i8) $ Name "si.0.lcssa", Name "for.cond6.preheader"), (LocalReference (ptr i8) $ Name "incdec.ptr13.lcssa", Name "for.end14.loopexit")] [],
        Do $ Store False (LocalReference (ptr i8) $ Name "si.1.lcssa") (ConstantOperand $ Const.Int 8 0) Nothing 0 []
      ] (Do $ Ret (Just $ LocalReference (ptr i8) $ Name "call3") [])
      ]
  }