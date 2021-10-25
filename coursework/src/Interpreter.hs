{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp =N Int|V Vname|Plus Int Vname|Aundefined
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val

aval (N x) s=x
aval (V v) s=V
aval (Plus a1 a2) s=head([x+aval a1|x<-(aval a2,x)])
aval Aundefined s = undefined

--TODO Task 2.1
data BExp =Bc Bool|Not Bool|And Bool Bool|Less AExp AExp|Bundefined
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool

bval(Bc x) s=x

bval(Not x) s | isNothing x =Nothing
               | x =False
               | not x =True

bval(And x y) s | x,y  =True
                 | Otherwise  =False

bval(Less a1 a2) s | aval a1<head([x | x<-(aval a2,x)]) =True
                 | Otherwise =False
bval Bundefined s=undefined

--TODO Task 2.1
data Com =Assign Vname AExp| Seq Instr Instr| If BExp Instr Instr|While BExp Instr|SKIP|Cundefined
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State

eval (Assign v x) s| isNothing x =s
                   | isNothing v =s
                   | Otherwise =[(v,x)]

eval (Seq c1 c2) s | isNothing c1 =s
                   | isNothing c2 =s
                   | otherwise   = iexec c1 iexec c2

eval (If b c1 c2) s | b = iexec c1 iexec c2
                    |otherwise =Nothing

eval (While b c) s | b =iexec c
                   | otherwise =Nothing

eval SKIP s= Nothing

eval Cundefined s= undefined