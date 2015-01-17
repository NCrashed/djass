module Language.Jass.ShowIndent(
  ShowIndent(..),
  makeIndent,
  commaSep,
  newlineSep,
  sepWith
  ) where
  
-- | Printing with indentation
class ShowIndent a where
  -- | As show but makes indentation with specified level
  showIndent :: Int -> a -> String
  
-- | Default implementation of indentation
makeIndent :: Int -> String
makeIndent i = replicate i '\t'

-- | Print list separated by comma
commaSep :: [String] -> String
commaSep = sepWith ", "

-- | Print list separated by newline
newlineSep :: [String] -> String
newlineSep = sepWith "\n"

-- | Print list with specified separator
sepWith :: String -> [String] -> String
sepWith sep = go ""
    where go acc [] = acc
          go acc [x] = acc ++ x
          go acc (x:xs) = go (acc ++ x ++ sep) xs