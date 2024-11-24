-- Advanced List Operations in Haskell

-- Custom implementation of map using recursion
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Custom implementation of filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

-- Custom implementation of foldl
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Custom implementation of foldr
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Advanced list operations

-- Zip with function (zipWith)
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- Group consecutive elements
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = let (same, different) = span (== x) xs
               in (x:same) : group different

-- Custom take while with predicate
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x       = x : myTakeWhile p xs
    | otherwise = []

-- Generate all subsequences of a list
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = let sub = subsequences xs
                     in sub ++ map (x:) sub

-- Example usage and testing
main :: IO ()
main = do
    -- Test myMap
    putStrLn "Testing myMap:"
    print $ myMap (*2) [1..5]
    
    -- Test myFilter
    putStrLn "\nTesting myFilter:"
    print $ myFilter even [1..10]
    
    -- Test folds
    putStrLn "\nTesting folds:"
    print $ myFoldl (+) 0 [1..5]
    print $ myFoldr (:) [] [1..5]
    
    -- Test zipWith
    putStrLn "\nTesting myZipWith:"
    print $ myZipWith (+) [1,2,3] [4,5,6]
    
    -- Test group
    putStrLn "\nTesting group:"
    print $ group [1,1,1,2,2,3,3,3,3]
    
    -- Test takeWhile
    putStrLn "\nTesting myTakeWhile:"
    print $ myTakeWhile (<5) [1..10]
    
    -- Test subsequences
    putStrLn "\nTesting subsequences:"
    print $ subsequences [1,2,3]