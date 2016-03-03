module Main where
import qualified Prelude as Hask
data Bool = False | True
data List a = Nil | Cons a (List a)
data Maybe a = Just a | Nothing
main = (Hask.>>) (Hask.print (iff (isNil (Cons 1 Nil)) "true" "false")) Hask.getLine
caseList list f g = case list of Nil -> f
                                 Cons head tail -> (Hask.$) g head tail
isNil list = let {constFalse input1 input2 = False} in
             caseList list True constFalse
iff cond thenn elsee = case cond of True -> thenn
                                    False -> elsee