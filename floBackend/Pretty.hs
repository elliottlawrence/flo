module Pretty where

import Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

-- Convert a document to a string
showP :: Pretty a => a -> String
showP = render . pp

-- Separate a list of documents with commas
commas :: [Doc] -> Doc
commas = hsep . punctuate comma
