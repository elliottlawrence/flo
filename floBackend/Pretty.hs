module Pretty where

import Text.PrettyPrint.Leijen as L hiding (Pretty)

class Pretty a where
  pp :: a -> Doc

  -- Pretty prints a list of a's. Default behavior is vertical concatenation.
  ppList :: [a] -> Doc
  ppList = vcat . map pp

instance Pretty a => Pretty [a] where
  pp = ppList

instance Pretty a => Pretty (Maybe a) where
  pp (Just x) = pp x
  pp Nothing = empty

instance Pretty Char where
  pp = char
  ppList = text

instance Pretty Int where
  pp = int

instance Pretty Double where
  pp = double

-- Convert a document to a string
showP :: Pretty a => a -> String
showP doc = displayS (renderPretty 0.8 80 (pp doc)) ""

-- Separate a list of documents with commas
commas :: [Doc] -> Doc
commas = hsep . punctuate comma

-- Same as commas but with no spaces
commas' :: [Doc] -> Doc
commas' = hcat . punctuate comma

-- Surround the doc with parentheses if the condition is true
maybeParens :: Bool -> Doc -> Doc
maybeParens False = id
maybeParens True = parens
