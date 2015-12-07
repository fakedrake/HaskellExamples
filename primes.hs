fix f = f $ fix f

minus ls [] = ls
minus [] _ = []
minus (x:xs) (y:ys) | x == y = minus xs ys
                    | x > y = minus (x:xs) ys
                    | otherwise = x:minus xs (y:ys)

primes = fix (\f (x:xs) -> x:f (xs `minus` map (*x) [2..])) [2..]
