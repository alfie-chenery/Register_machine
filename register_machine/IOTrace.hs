module IOTrace where

import Encodings
import RegisterMachine
import Data.List ( transpose, intercalate )


printTrace :: [Instruction] -> [Register] -> IO ()
printTrace is params =
    putStrLn $ unlines $ text ++ footer ++ output
  where
    states = trace is params
    result = head $ snd $ last states
    steps = length states
    pc = fst $ last states
    eroneous = pc >= fromIntegral (length is)
    text = drawTable (tabulate states)
    footer = if eroneous
             then ["> Eroneously halted after executing" ++ show (steps-1) ++ " instructions"] 
                    --dont count the halt that was added to simulate eroneous halt, so steps-1
             else ["> Halted after executing" ++ show steps ++ " instructions"]
                    --includes executing the Halt
    output = ["> Final output R0 = " ++ show result]

tabulate :: [State] -> [[String]]
tabulate s = header : map (map show . combine) lines
  -- map combine over the list of states, to get a list of lists
  -- then map (map show) over the list of lists
  -- essentially map show over the internal items of the sub lists
  where
    lines = pad s
    n = length $ snd $ head lines --assumes states have already been padded to the same length
    header = "PC" : (['R' : show i | i <- [0..n-1]])

    combine :: State -> [Integer]
    combine (pc, rs) = pc : rs

    pad :: [State] -> [State]
    pad [] = []
    pad s@((pc,rs):s') = (pc,rs ++ replicate (n-l) 0) : pad s'
      where
        l = length rs
        n = maximum $ map (length . snd) s

drawTable :: [[String]] -> [String]
drawTable t = separator : header : separator : lines ++ [separator]
  where
    widths = [maximum $ map length col | col <- transpose t]
    separator = "+-" ++ (intercalate "-+-" [replicate width '-' | width <- widths] ++ "-+")
    rows = [ [buffer s n | (s,n) <- zip row widths] | row <- t]
    (header:lines) = map (surround . intercalate " | ") rows
    --lines = map concat rows

    buffer :: String -> Int -> String
    buffer s n = replicate l ' ' ++ s ++ replicate r ' '
      where
        x = n - length s
        l = x `div` 2
        r = x - l

    surround :: String -> String 
    surround s = "| " ++ s ++ " |"