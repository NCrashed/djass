module Language.Jass.Codegen.Helpers(
    globalCall
  , constInt
  , jump
  , bitcast
  ) where
  
import LLVM.General.AST
import LLVM.General.AST.CallingConvention
import qualified LLVM.General.AST.Constant as Const
import Data.Word

-- | Helps to generate calls to global functions
globalCall :: Type -> String -> [Operand] -> Instruction
globalCall retType funcName args = Call True C [] (Right $ ConstantOperand $ Const.GlobalReference retType $ Name funcName) (args `zip` repeat []) [] []

-- | Generates constant integer operand for specified bits count and value 
constInt :: Word32 -> Int -> Operand
constInt i val = ConstantOperand $ Const.Int i $ toInteger val

-- | Alias for Br 
jump :: Name -> Terminator
jump n = Br n []

-- | Alias for BitCast
bitcast :: Operand -> Type -> Instruction
bitcast op t = BitCast op t []