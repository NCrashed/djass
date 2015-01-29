module Language.Jass.Parser.SourcePos(
    SrcPos(..),
    noPos
    ) where
    
data SrcPos = SrcPos {
        locFile :: FilePath,
        locLine :: Int,
        locCol :: Int
    } 
    deriving Eq
    
instance Show SrcPos where
  show pos = locFile pos ++ " (line" ++ show (locLine pos)++ ", column " ++ show (locCol pos) ++ ")"
  
noPos :: SrcPos
noPos = SrcPos "unknown" 0 0