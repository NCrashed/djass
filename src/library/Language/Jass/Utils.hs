module Language.Jass.Utils(
    mapLeft
  , uncurry3
  , liftExcept
  , liftExceptPure
  ) where

import Control.Monad.Trans.Except

-- | Converts left constructor of either with function
mapLeft ::  (e -> e') -> Either e a -> Either e' a
mapLeft f me = case me of
    Left err -> Left $ f err
    Right r -> Right r

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c

-- | Converts implicit Except monad to explicit
liftExcept :: (Functor m, Monad m, Show e) => m (Either e a) -> ExceptT String m a
liftExcept action = ExceptT $ mapLeft show <$> action

-- | Converts implicit Except monad to explicit, pure version
liftExceptPure :: (Functor m, Monad m, Show e) => Either e a -> ExceptT String m a
liftExceptPure action = ExceptT $ return $ mapLeft show action