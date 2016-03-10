module Main where
import qualified Prelude as Hask
import Prologue
main = (Hask.>>) (Hask.print (iff (isNil (Cons 1 Nil)) "true" "false")) Hask.getLine