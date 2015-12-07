import Graphics.Gnuplot.Simple
import System.Environment
import Data.List
import Text.Printf

type Op = Int -> Int -> Int
type Pair = (Int, Int)

operators :: [Op]
operators = reverse [(\x y -> abs $ x - y), max, min, (+)]
students  =      reverse ["Daphne", "Max", "Mindy", "Sam"]

-- |From an operator to which we could know the answer to, and the
-- maximum random number selected, find which pairs yield a unique
-- answer to the operation.
--
possiblePairs' :: (Op) -> Int -> [Pair]
possiblePairs' op n =
  map (fst . head) $
  filter ((== 1) . length) $    -- Get the unique answers
  groupBy (\x y -> (snd x == snd y)) $ -- Group common answers together
  sortOn snd $                         -- Sort by the answer
  [((x,y), op x y) | x <- [1..n], y<-[x..n]]

-- |For a student to win we assume tha the other students were unable
-- to do so. So each kid whose turn comes will disregard all the
-- number pairs that would lead a previous kid to win.
--
-- This function operates much like possiblePairs' using the head of
-- the first argument, only it first disregards all pairs that are
-- possible for any operators of the tail of the first arguments.
possiblePairs :: [Op] -> Int -> [Pair]
possiblePairs [] _ = []
possiblePairs ops n = (possiblePairs' (head ops) n) \\ previousStudents
  where
    previousStudents = concat $ map (\op -> possiblePairs' op n) $ tail ops

answer :: Int -> [(String, Float)]
answer maxNum = reverse $ zipWith anskv students (tails operators)
  where
    anskv :: String -> [Op] -> (String, Float)
    anskv n ops = (n, (fromIntegral $ length $ pairs ops)
                      / (fromIntegral (length pairList)))
    pairs ops = possiblePairs ops maxNum
    pairList = [(x,y) | x <- [1..maxNum], y<-[x..maxNum]]

-- From repl: plotData "/tmp/jane.png" [5..50]
plotData :: FilePath -> [Int] -> IO ()
plotData fname nums = plotPathsStyle plotArgs $
                      (zipWith lineTitle
                       (map fst $ answer 0) results) ++ [totals]
  where
    names2Index i = map (\(n, p) -> (fromIntegral i :: Float, p))
    lineSpec title = PlotStyle Lines (CustomStyle [LineTitle title])
    sumResults = [names2Index n (answer n) | n <- nums]
    results = transpose sumResults
    totals = lineTitle "Total" $ map
             (foldl (\(_,p1) (n, p2) -> (n, p1+p2)) (undefined, 0)) sumResults
    plotArgs = [PNG fname,
                YLabel "Probablilty of winning",
                XLabel "Maximum random number"]
    lineTitle title arr = (lineSpec title, arr)

main = do
  args <- getArgs
  let n = read $ args !! 0 :: Int
  putStrLn "Students' probability of winning:"
  go $ answer n
  putStrLn $ "Total possible pairs: " ++ show ((n + 1) * n `div` 2)
    where
      go :: [(String, Float)] -> IO ()
      go [] = return ()
      go ((n,p):xs) = do
        putStrLn $ "\t" ++ n ++ ":\t" ++ (show p)
        go xs
