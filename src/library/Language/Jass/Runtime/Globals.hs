module Language.Jass.Runtime.Globals(
    executeGlobalInitializers
  , genGlobalInitializersFunc
  ) where

import LLVM.General.AST as LLVM
import LLVM.General.AST.Global as LLVM
import Language.Jass.JIT.Module
import Language.Jass.JIT.Calling
import Control.Monad.Trans.Except
import Foreign.Ptr

-- | Returns name of function that sets initial values to all globals
globalsInitializerFuncName :: String
globalsInitializerFuncName = "$__jass__initGlobals"

-- | Generates function that sets initial values of global variables
genGlobalInitializersFunc :: [Named Instruction] -> Definition
genGlobalInitializersFunc initInstrs = 
  GlobalDefinition $ functionDefaults {
      name = Name globalsInitializerFuncName
    , parameters = ([], False)
    , returnType = VoidType
    , basicBlocks = [BasicBlock (Name "entry_block") 
        initInstrs
        (Do $ Ret Nothing [])
      ]
  }
    
type GlobalInitializersFunc = IO ()
foreign import ccall "dynamic"
  mkGlobalInitializersFunc :: FunPtr GlobalInitializersFunc -> GlobalInitializersFunc  
  
executeGlobalInitializers :: JITModule -> ExceptT String IO ()
executeGlobalInitializers module' = callFunc0 module' globalsInitializerFuncName mkGlobalInitializersFunc