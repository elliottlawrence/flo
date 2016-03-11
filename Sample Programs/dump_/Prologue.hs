module Prologue where
import qualified Prelude as Hask
data Bool = False | True
data List a = Nil | Cons a (List a)
data Maybe a = Just a | Nothing
caseList list f g = case list of Nil -> f
                                 Cons head tail -> g head tail
isNil list = let {constFalse input1 input2 = False} in
             caseList list True constFalse
iff cond thenn elsee = case cond of True -> thenn
                                    False -> elsee
id x = x
map f xs = case xs of Nil -> Nil
                      Cons y ys -> Cons (f y) (map f ys)
map1 f = let {mf xs = case xs of Nil -> Nil
                                 Cons y ys -> Cons (f y) (mf ys)} in
         mf