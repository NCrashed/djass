{-# LANGUAGE 
 DeriveDataTypeable,
 DeriveGeneric #-}
module Language.Jass.JassType(
  JassType(..),
  isBasicType
  ) where

import Language.Jass.ShowIndent
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

-- | Supported types
data JassType = JInteger | JReal | JBoolean | JString 
    | JNull -- ^ Special type for null value, allow to use null as any handle type
    | JHandle 
    | JCode 
    | JArray JassType 
    | JUserDefined String
    deriving (Eq, Ord, Typeable, Generic)

instance Hashable JassType

instance Show JassType where
  show = showIndent 0
  
instance ShowIndent JassType where
  showIndent i JInteger = makeIndent i ++ "integer"
  showIndent i JReal = makeIndent i ++ "real"
  showIndent i JBoolean = makeIndent i ++ "boolean"
  showIndent i JString = makeIndent i ++ "string"
  showIndent i JHandle = makeIndent i ++ "handle"
  showIndent i JCode = makeIndent i ++ "code"
  showIndent i (JArray jtype) = makeIndent i ++ "array " ++ show jtype
  showIndent i (JUserDefined name) = makeIndent i ++ name
  showIndent i JNull = makeIndent i ++ "null"
  
-- | Checks if jass type is basic (not array and custom)
isBasicType :: JassType -> Bool
isBasicType (JArray _) = False
isBasicType (JUserDefined _) = False
isBasicType _ = True