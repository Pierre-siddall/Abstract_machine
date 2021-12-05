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
type Stack =[Val]

--TODO Task 1.6
type Config = (Int,State,Stack)

 --TODO Task 1.7
iexec :: Instr -> Config -> Config

iexec (LOADI x) (a,b,c) =(a+1,b,x:c) 

iexec (LOAD v) (a,b,c) =(a+1,b,[ x | Just x <- [Data.Map.lookup v b]]) 

iexec ADD (a,b,c)=(a+1,b,[Prelude.sum (Prelude.take 2 c)]) 

iexec (STORE v) (a,b,c) =(a+1,Data.Map.insert v (head c) b,Prelude.drop 1 c) 

iexec (JMP i) (a,b,c) =(a+i+1,b,c) 

iexec (JMPLESS i) (a,b,c) = if head(tail c)<head c then (a+1+i,b,Prelude.drop 2 c) else (a+1,b,Prelude.drop 2 c) 


iexec (JMPGE i) (a,b,c) =if head(tail c)>=head c then (a+1+i,b,Prelude.drop 2 c) else (a+1,b,Prelude.drop 2 c)  

iexec IUndefined (a,b,c) = undefined  

exec :: [Instr] -> Config -> Config
exec xs (a,b,c) | length xs==1 = iexec (head xs) (a,b,c)
                | otherwise = exec (tail xs) (iexec (head xs) (a,b,c))
                