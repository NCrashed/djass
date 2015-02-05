-- | Unfinished module that should simplify interfacing with jass code
{-# LANGUAGE TypeOperators, TypeFamilies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Language.Jass.JIT.Calling(
  (:->)(..),
  ConvertToJass,
  ToJassConv(..),
  FromJassConv(..),
  ConvertToJassArguments,
  ReturnType,
  ApplyJassArgs(..)
  ) where

import Foreign.C.Types
import Foreign.C.String
import GHC.Float

-- | Jass function, same as (->)
data a :-> b = a :-> b

type family ConvertToJass a where
  ConvertToJass (a :-> b) = JassSide a -> ConvertToJass b
  ConvertToJass a = IO (JassSide a)

type family ConvertToJassArguments a where
  ConvertToJassArguments (a :-> b) = a :-> ConvertToJassArguments b
  ConvertToJassArguments a = ()

type family ReturnType a where
  ReturnType (a -> b) = ReturnType b
  ReturnType a = a

class ToJassConv a => ApplyJassArgs a b c | b -> c where
  applyJassArgs :: (JassSide a -> b) -> (a :-> c) -> ReturnType b
    
-- | Describes convertion to Jass runtime represenation of types
class ToJassConv a where
  type JassSide a :: *
  toJass          :: a -> IO (JassSide a) 

instance ToJassConv () where
  type JassSide () = ()
  toJass _ = return ()
  
instance ToJassConv Int where
  type JassSide Int = CInt
  toJass = return . CInt . fromIntegral

instance ToJassConv Integer where
  type JassSide Integer = CInt
  toJass = return . CInt . fromIntegral
    
instance ToJassConv Float where
  type JassSide Float = CFloat
  toJass = return . CFloat 
  
instance ToJassConv Double where
  type JassSide Double = CFloat
  toJass = return . CFloat . double2Float
  
instance ToJassConv String where
  type JassSide String = CString
  toJass = newCString -- | TODO: need to free

instance ToJassConv Bool where
  type JassSide Bool = CChar
  toJass = return . CChar . toInt
    where toInt b = if b then 1 else 0
    
-- | TODO: handle and code types

-- | Describes convertion from Jass runtime representation of types
class FromJassConv a where
  type HaskellSide a :: *
  fromJass           :: a -> IO (HaskellSide a)

instance FromJassConv () where
  type HaskellSide () = ()
  fromJass _ = return ()
  
instance FromJassConv CInt where
  type HaskellSide CInt = Int
  fromJass = return . fromIntegral
  
instance FromJassConv CFloat where
  type HaskellSide CFloat = Float
  fromJass (CFloat f) = return f
  
instance FromJassConv CString where
  type HaskellSide CString = String
  fromJass = peekCString
  
instance FromJassConv CChar where
  type HaskellSide CChar = Bool
  fromJass (CChar i) = return $ i /= 0
  
-- | TODO: handle and code types