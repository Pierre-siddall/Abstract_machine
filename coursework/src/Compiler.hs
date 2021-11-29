{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
bcomp (Bc x) b n | x==b =[JMP n]
                 | otherwise =[]

bcomp (Not (Bc x)) b n| not x==b =[JMP n]
                      | otherwise =[]

bcomp (Less (V v) (N x)) b n | b =[Machine.LOAD v,Machine.LOADI x,Machine.JMPLESS n]
                             | not b=[Machine.LOAD v,Machine.LOADI x,Machine.JMPGE n]

bcomp (And (Bc x) (Bc y)) b n| not y && b || x && y && not b =[]
                             | not x && y && b =[Machine.JMP 1,Machine.JMP n]
                             | x && y && b||not x && not y && not b =[Machine.JMP (n*2)]
                             | (not x || not y) && not b =[Machine.JMP n]

bcomp(And (Less (V v) (N x))(Bc y)) b n| y&&b =[Machine.LOAD v,Machine.LOADI x,Machine.JMPLESS n]
                                       | not y && b =[Machine.JMP n,Machine.LOAD v,Machine.LOADI x,Machine.JMPLESS n]
                                       | y&& not b =[Machine.LOAD v,Machine.LOADI x,Machine.JMPGE n]
                                       | not y && not b =[Machine.JMP (2*n),Machine.LOAD v,Machine.LOADI x,Machine.JMPGE n]

bcomp(And (Bc y) (Less (V v)(N x))) b n| y&&b =[Machine.LOAD v,Machine.LOADI x,Machine.JMPLESS n]
                                       | y&& not b =[Machine.LOAD v,Machine.LOADI x,Machine.JMPGE n]
                                       | not y && b =[Machine.JMP n,Machine.LOAD v,Machine.LOADI x,Machine.JMPLESS n]
                                       | not y && not b =[Machine.JMP (2*n),Machine.LOAD v,Machine.LOADI x,Machine.JMPGE n]

bcomp Bundefined b n=undefined
--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign v x)= acomp x++[STORE v]
ccomp (Seq c1 c2) = ccomp c1++[JMP 2]++ccomp c2
ccomp (If b c1 c2)= bcomp b False 5++ccomp (Seq c1 c2)
ccomp (While b c)= bcomp b False 5++ccomp c++[JMP (-(length(bcomp b False 5++ccomp c)+1))]
ccomp SKIP = [JMP 1] 
ccomp Cundefined=undefined 