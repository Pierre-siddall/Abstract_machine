{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr = LOADI Int | LOAD Vname | ADD |STORE Vname | JMP Int | JMPLESS Int | JMPGE Int | IUndefined
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack =[Int]

--TODO Task 1.6
type Config = (Int,State,Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config

iexec (LOADI x) (a,b,c) =(a+1,b,x:c) --Done

iexec (LOAD v) (a,b,c) =(a+1,b,[y | (x,y)<-head b])

iexec ADD (a,b,c)=(a+1,b,[Prelude.sum (Prelude.take 2 c)]) --Done

--iexec (STORE v) (a,b,c) =(a+1,Data.Map v head c,Prelude.drop 1 c)

iexec (JMP i) (a,b,c) =(a+i,b,c) --Done

iexec (JMPLESS i) (a,b,c) = if head(tail c)<head c then (a+1+i,b,Prelude.drop 2 c) else (a+1,b,Prelude.drop 2 c)--Done 


iexec (JMPGE i) (a,b,c) =if head(tail c)>=head c then (a+1+i,b,Prelude.drop 2 c) else (a+1,b,Prelude.drop 2 c) --Done 

iexec (IUndefined) (a,b,c) = undefined --Done 

exec :: [Instr] -> Config -> Config
exec xs (a,b,c)= (Prelude.length xs,b,c)  
