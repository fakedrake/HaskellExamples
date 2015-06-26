{-# LANGUAGE BangPatterns #-}

import Text.Printf
import Data.String
import System.IO

fibonacci :: Int -> Int
fibonacci x = fst (fibm x)
fibm :: Int -> (Int,Int)
fibm 0 = (0,0)
fibm 1 = (1,0)
fibm x = ((fst lastCache + snd lastCache), fst lastCache) where
  lastCache = fibm (x - 1)

main :: IO ()
main = do
  putStr "Insert number for fibonacci "
  hFlush stdout
  -- Could be x <- getLine; .. (use x) .. but just to demo binding
  getLine >>= putStrLn . (printf "Fibonacci %d\n") . fibonacci . read

