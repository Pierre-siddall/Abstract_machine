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
data AExp =N Val|V Vname|Plus AExp AExp|Aundefined
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N x) s=x
aval (V v) s=head[ head x | (_,_,x) <- [Machine.iexec (LOAD v) (0,s,[])]]
aval (Plus a1 a2) s= aval a1 s+aval a2 s
aval Aundefined s = undefined

--TODO Task 2.1
data BExp =Bc Bool|Not Bool|And Bool Bool|Less AExp AExp|Bundefined
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc b) s =b
bval (Not b) s | b =False
               | not b =True
bval (And b1 b2) s | b1 && b2==True =True
                   | otherwise =False
bval (Less a1 a2) s | aval a1 s<aval a2 s =True
                   | otherwise =False
bval Bundefined s =undefined
--TODO Task 2.1
data Com =Assign Vname AExp| Seq Com Com| If BExp Com Com|While BExp Com|SKIP|Cundefined
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign v x) s =head([x | (_,x,_)<- [Machine.exec [Machine.LOADI (aval x s) ,Machine.STORE v] (0,s,[])]])
eval (Seq c1 c2) s = eval c2 (eval c1 s)
eval (If b c1 c2) s= if bval b s then eval c1 s else eval c2 s
eval (While b c) s= if bval b s then eval c s else eval SKIP s 
eval SKIP s= s
eval Cundefined s= undefined