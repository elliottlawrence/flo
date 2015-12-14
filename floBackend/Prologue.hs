module Prologue where

iff cond thenn elsee = if cond then thenn else elsee

pair x y z = z x y

first pair = let {f y x = x} in pair f

second pair = let {f y x = y} in pair f

cons = pair

head = first

tail = second

nil = False

isNil list = let {f d t h = False} in list f True
