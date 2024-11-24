-- Infinite Sequence Generators

-- Fibonacci sequence using lazy evaluation
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Prime numbers using the sieve of Eratosthenes
primes :: [Integer]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Collatz sequence for a given starting number
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (next n)
    where
        next x | even x    = x `div` 2
              | otherwise  = 3 * x + 1

-- Perfect squares with their square roots
squares :: [(Integer, Integer)]
squares = [(n^2, n) | n <- [1..]]

-- Pascal's Triangle as infinite list of rows
pascal :: [[Integer]]
pascal = [1] : map nextRow pascal
    where
        nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- Utility functions to work with infinite sequences
take' :: Int -> [a] -> [a]
take' n xs = take n xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = takeWhile

-- Find first occurrence satisfying a predicate
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p xs = case dropWhile (not . p) xs of
    []    -> Nothing
    (x:_) -> Just x

-- Main function demonstrating usage
main :: IO ()
main = do
    putStrLn "First 10 Fibonacci numbers:"
    print $ take 10 fibs
    
    putStrLn "\nFirst 10 prime numbers:"
    print $ take 10 primes
    
    putStrLn "\nCollatz sequence starting from 13:"
    print $ collatz 13
    
    putStrLn "\nFirst 5 perfect squares with their roots:"
    print $ take 5 squares
    
    putStrLn "\nFirst 5 rows of Pascal's Triangle:"
    mapM_ print $ take 5 pascal
    
    putStrLn "\nFirst prime number above 100:"
    print $ findFirst (>100) primes
    
    putStrLn "\nFibonacci numbers less than 100:"
    print $ takeWhile (<100) fibs
    
    putStrLn "\nFirst Fibonacci number above 1000:"
    print $ findFirst (>1000) fibs