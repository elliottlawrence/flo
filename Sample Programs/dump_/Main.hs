module Main where
import qualified Prelude as Hask
import Prologue
main = let {fac n = iff ((==) n 0) 1 ((Hask.*) n (fac ((Hask.-) n 1)))} in
       (Hask.>>) (Hask.print (fac 15)) Hask.getLine