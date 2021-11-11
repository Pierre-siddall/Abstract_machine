module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x)=[Machine.LOADI x]
acomp (V v)=[Machine.LOAD v]
acomp (Plus a1 a2)=[head(acomp a1),head(acomp a2),Machine.ADD]
acomp Aundefined = undefined

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp = undefined

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp = undefined