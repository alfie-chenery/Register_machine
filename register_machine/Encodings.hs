module Encodings where

data DPair = DP Integer Integer
           deriving (Eq)
data SPair = SP Integer Integer
           deriving (Eq)
data Instruction = Halt | Inc Integer Integer | Dec Integer Integer Integer
                 deriving (Eq)


class Encoding a where
  toNum         :: a -> Integer
  toDPair       :: a -> DPair
  toSPair       :: a -> SPair
  toInstruction :: a -> Instruction
  toList        :: a -> [Integer]


instance Encoding Integer where
  toNum         = id
  toDPair       = n2dp
  toSPair       = n2sp
  toInstruction = n2i
  toList        = n2l

instance Encoding DPair where
  toNum         = dp2n
  toDPair       = id
  toSPair       = toSPair . toNum
  toInstruction = dp2i
  toList        = dp2l

instance Encoding SPair where
  toNum         = sp2n
  toDPair       = toDPair . toNum
  toSPair       = id
  toInstruction = toInstruction . toNum
  toList        = toList . toNum

instance Encoding Instruction where
  toNum         = i2n
  toDPair       = toDPair . toNum
  toSPair       = toSPair . toNum
  toInstruction = id
  toList        = toList . toNum

instance (Encoding a) => Encoding [a] where
  toNum []      = 0
  toNum (x:xs)  = toNum (DP (toNum x) (toNum xs)) 
  toDPair       = toDPair . toNum
  toSPair       = toSPair . toNum
  toInstruction = toInstruction . toNum
  toList        = map toNum

nest :: (Encoding a) => [a] -> Integer
nest []     = 0
nest (x:xs) = toNum (DP (toNum x) (nest xs))


-- conversion functions for class instances

n2dp :: Integer -> DPair
n2dp n
  | n == 0    = error "0 cant be represented as DPair"
  | otherwise = DP x y 
  where
    SP x y = toSPair (n-1)

n2sp :: Integer -> SPair
n2sp n
  | n == 0    = SP 0 0
  | otherwise = SP x y
  where
    (x, j) = helper (n+1)
    y = (j-1) `div` 2

    helper :: Integer -> (Integer, Integer)
    helper n = helper' n 0
      where
        helper' :: Integer -> Integer -> (Integer, Integer)
        helper' n i
          | even n    = helper' (n `div` 2) (i+1)
          | otherwise = (i,n)

n2i :: Integer -> Instruction
n2i n
  | n == 0    = Halt
  | otherwise = toInstruction $ toDPair n

n2l :: Integer -> [Integer]
n2l 0 = []
n2l n = dp2l $ n2dp n 

dp2n :: DPair -> Integer
dp2n (DP x y)
  = 2^x * (2*y + 1)

dp2i :: DPair -> Instruction
dp2i (DP x y)
  | even x           = Inc (x `div` 2) y
  | otherwise        = Dec ((x-1) `div` 2) j k
  where
    SP j k = toSPair y

dp2l :: DPair -> [Integer]
dp2l (DP x 0) = [toNum x]
dp2l (DP x y) = x : dp2l (toDPair y)


sp2n :: SPair -> Integer
sp2n (SP x y)
  = 2^x * (2*y + 1) - 1

i2n :: Instruction -> Integer
i2n Halt
  = 0
i2n (Inc r l)
  = dp2n (DP (2*r) l)
i2n (Dec r lt lf)
  = dp2n (DP (2*r + 1) (sp2n (SP lt lf)))

--Pretty print functions
instance Show DPair where
  show (DP x y) = "<<" ++ show x ++ "," ++ show y ++ ">>"
instance Show SPair where
  show (SP x y) = "<" ++ show x ++ "," ++ show y ++ ">"
instance Show Instruction where
  show Halt          = "Halt"
  show (Inc r l)     = "R" ++ show r ++ "+ -> L" ++ show l
  show (Dec r lt lf) = "R" ++ show r ++ "- -> L" ++ show lt ++ ",L" ++ show lf