module Main where
import Prologue
printPrime list = (>>) ((>>) (print (head list)) getLine) (printPrime (tail list))
from n = (:) n (from ((+) n 1))
sieve xs = let {p = head xs;
                ps = tail xs} in
           iff (null xs) [] ((:) p (sieve (Main.filter (nonMultiple p) ps)))
filter predicate xs = let {p = head xs;
                           ps = tail xs;
                           rest = Main.filter predicate ps} in
                      iff (null xs) [] (iff (($) predicate p) ((:) p rest) rest)
nonMultiple p n = (/=) (mod n p) 0
main = printPrime (sieve (from 2))