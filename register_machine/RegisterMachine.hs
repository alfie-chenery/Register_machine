module RegisterMachine where

import Encodings

type Register = Integer
type PC = Integer
type State = (PC, [Register])



-- for input getItem i [] xs
-- returns ([elements before i], element at index i, [elements after i])
-- assumes length xs >= i
getItem :: Integer -> [a] -> [a] -> ([a],a,[a])
getItem 0 as (b:bs) = (as,b,bs)
getItem i as (b:bs) = getItem (i-1) (as++[b]) bs

-- i must be >= 0
replicate' :: Integer -> a -> [a]
replicate' 0 _ = []
replicate' i x = x : replicate' (i-1) x

getReg :: Integer -> [Register] -> ([Register],Register,[Register])
getReg i rs
  | i < n     = getItem i [] rs 
  | otherwise = getItem i [] (rs ++ replicate' (i-n + 1) 0) --add more registers, infinite number of registers
  where
    n = fromIntegral $ length rs

getInstr :: Integer -> [Instruction] -> Instruction
getInstr pc is
  | pc < n    = i
  | otherwise = Halt --eroneous halt
  where
    (_,i,_) = getItem pc [] is
    n = fromIntegral $ length is


exec :: Instruction -> State -> State
exec Halt s
  = s
exec (Inc r l) (pc,rs)
  = (l ,xs ++ [reg + 1] ++ ys)
  where
    (xs,reg,ys) = getReg r rs
exec (Dec r lt lf) (pc,rs)
  = if reg == 0
      then (lf, rs)
      else (lt ,xs ++ [reg - 1] ++ ys)
  where
    (xs,reg,ys) = getReg r rs

execAll :: [Instruction] -> State -> Integer
execAll is s@(pc, rs) = case i of
  Halt -> head $ snd s'
  _    -> execAll is s'
  where
    i  = getInstr pc is
    s' = exec i s

run :: [Instruction] -> [Register] -> Integer
run is params = execAll is (0,0:params) --pc starts 0, R0 starts 0
  --more zeroed scratch registers are added if required

trace :: [Instruction] -> [Register] -> [State]
trace is params = trace' is (0,0:params) --pc starts 0, R0 starts 0
  where
    trace' :: [Instruction] -> State -> [State]
    trace' is s@(pc,rs) = case i of
      Halt -> [s]
      _    -> s : trace' is s'
      where
        i  = getInstr pc is
        s' = exec i s