module Main where
import Prologue
data Bool = False | True
data List a = Nil | Cons a (List a)
data Maybe a = Just a | Nothing
main = (>>) (print "hi") getLine