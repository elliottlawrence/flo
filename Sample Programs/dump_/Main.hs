module Main where
import qualified Prelude as Hask

data Bool  = False | True
data Char  = MkChar Int$
data Empty  = Empty
data Float  = MkFloat Float$
data IORes a = MkIORes a World
data IORes$  = MkIORes$ Int$ World
data Int  = MkInt Int$
data List a = Nil | Cons a (List a)
data Maybe a = Just a | Nothing

main  = primes

echo  = let {echo1 a = iff (eqChar a (MkChar -1)) done (seq (putChar a) echo)}
        in bind getChar echo1

primes  = let {from n = Cons n (from (plus n (MkInt 1)));
          sieve list = let {sieve1 p ps = let {nonMultiple p n = neqInt (mod n p) (MkInt 0)}
                                          in Cons p (sieve (filter (nonMultiple p) ps))}
                       in caseList list Nil sieve1;
          printPrimes list = let {printPrime p = seq (putInt p) getLine}
                             in mapM printPrime list}
          in printPrimes (sieve (from (MkInt 2)))

printFactorial  = let {factorial n = iff (eqInt n (MkInt 0)) (MkInt 1) (multiply n (factorial (minus n (MkInt 1))))}
                  in seq (putInt (factorial (MkInt 10))) getLine

id x = x

map f xs = case xs of Nil -> Nil
                      Cons y ys -> Cons (f y) (map f ys)

compose f g x = f (g x)

const a b = a

foldr f b as = let {foldrHelp x xs = f x (foldr f b xs)}
               in caseList as b foldrHelp

filter pred xs = case xs of Nil -> Nil
                            Cons p ps -> iff (pred p) (Cons p (filter pred ps)) (filter pred ps)

iff cond thenn elsee = case cond of True -> thenn
                                    False -> elsee

primBool op a b = case a of MkInt a$ -> case b of
                                        MkInt b$ -> case op a$ b$ of 0 -> False
                                                                     1 -> True

eqInt  = primBool (\a$
         b$ -> ==$ a$ b$)

lessThan  = primBool (\a$
            b$ -> <$ a$ b$)

not b = iff b False True

and a b = iff a b False

or a b = iff a True b

lessThanEqualTo a b = or (lessThan a b) (eqInt a b)

greaterThan a b = not (lessThanEqualTo a b)

greaterThanEqualTo a b = not (lessThan a b)

neqInt a b = not (eqInt a b)

caseList list f g = case list of Nil -> f
                                 Cons head tail -> g head tail

isNil list = let {constFalse input1 input2 = False}
             in caseList list True constFalse

eqChar a b = case a of MkChar a$ -> case b of
                                    MkChar b$ -> case ==$ a$ b$ of 0 -> False
                                                                   1 -> True

newLine  = MkChar 10

primArith op e1 e2 = case e1 of MkInt x$ -> case e2 of
                                            MkInt y$ -> case op x$ y$ of
                                                        t$ -> MkInt t$

plus  = primArith (\x$
        y$ -> +$ x$ y$)

minus  = primArith (\x$
         y$ -> -$ x$ y$)

multiply  = primArith (\x$
            y$ -> *$ x$ y$)

divide  = primArith (\x$
          y$ -> /$ x$ y$)

mod  = primArith (\x$
       y$ -> %$ x$ y$)

done  = return Empty

seq m n = bind m (const n)

return a w = MkIORes a w

bind m k w = case m w of MkIORes a ww -> k a ww

sequence  = foldr seq done

getChar w = case ccall$ getchar w of
            MkIORes$ n$ ww -> MkIORes (MkChar n$) ww

putChar c w = case c of MkChar c$ -> case ccall$ putchar c$ w of
                                     MkIORes$ n$ ww -> MkIORes Empty ww

print string = mapM putChar string

printLn string = seq (print string) (putChar newLine)

getLine  = let {getLine1 c = let {getLine2 line = return (Cons c line)}
                             in iff (eqChar c newLine) (return Nil) (bind getLine getLine2)}
           in bind getChar getLine1

forever a = let {a1  = seq a a1} in a1

mapM m list = sequence (map m list)

putInt i w = case i of
             MkInt i$ -> case ccall$ printf "%d" i$ w of
                         MkIORes$ n$ ww -> MkIORes Empty ww
