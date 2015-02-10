module Language.Jass.TestUtils(
    assertExcept
  , module Utils
  ) where
  
import Control.Monad.Trans.Except
import Language.Jass.Utils as Utils
import Test.Tasty.HUnit

-- | Helps testing ExceptT 
assertExcept :: Show e => ExceptT e IO a -> Assertion
assertExcept action = do
  res <- runExceptT action
  case res of
    Left err -> assertFailure $ show err
    Right _ -> return ()