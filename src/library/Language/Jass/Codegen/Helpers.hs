module Language.Jass.Codegen.Helpers(
    globalCall
  , globalFunc
  , constInt
  , jump
  , bitcast
  , getElementPtr
  , ret
  , retVoid
  , load
  , store
  ) where
  
import LLVM.General.AST
import LLVM.General.AST.CallingConvention
import qualified LLVM.General.AST.Constant as Const
import Data.Word

-- | Helps to generate calls to global functions
globalCall :: Type -> String -> [Operand] -> Instruction
globalCall retType funcName args = Call True C [] (Right $ globalFunc retType funcName) (args `zip` repeat []) [] []

-- | Helps to generate reference to global function
globalFunc :: Type -> String -> Operand
globalFunc tp nm = ConstantOperand $ Const.GlobalReference tp $ Name nm

-- | Generates constant integer operand for specified bits count and value 
constInt :: Word32 -> Int -> Operand
constInt i val = ConstantOperand $ Const.Int i $ toInteger val

-- | Alias for Br 
jump :: Name -> Terminator
jump n = Br n []

-- | Alias for BitCast
bitcast :: Operand -> Type -> Instruction
bitcast op t = BitCast op t []

-- | Alias for GetElementPtr
getElementPtr :: Operand -> [Operand] -> Instruction
getElementPtr op is = GetElementPtr True op is []

-- | Alias for Ret
ret :: Operand -> Terminator
ret op = Ret (Just op) []

-- | Alias for Ret Nothing
retVoid :: Terminator
retVoid = Ret Nothing []

-- | Alias for Load
load :: Operand -> Instruction
load op = Load False op Nothing 0 []

-- | Alias for Store
store :: Operand -> Operand -> Instruction
store adr val = Store False adr val Nothing 0 []