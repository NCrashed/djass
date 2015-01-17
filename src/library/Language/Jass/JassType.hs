{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.JassType(
  JassType(..)
  ) where

import Language.Jass.ShowIndent
import Data.Typeable

-- | Supported types
data JassType = JInteger | JReal | JBoolean | JString | JHandle | JCode | JArray JassType | JUserDefined String
    deriving (Eq, Ord, Typeable)

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