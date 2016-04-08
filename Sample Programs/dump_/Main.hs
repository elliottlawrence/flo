module Main where
import qualified Prelude as Hask

main  = let {greet name = Hask.putStrLn ((Hask.++) "Hello " name)}
        in (Hask.>>) ((Hask.>>) (Hask.putStrLn "What's your name? ") ((Hask.>>=) Hask.getLine (\name -> greet name))) Hask.getLine
