-- N-Queens Solver in Haskell
-- A queen's position is represented by (row, column)
type Position = (Int, Int)

-- Check if a queen can attack another queen
threatens :: Position -> Position -> Bool
threatens (r1, c1) (r2, c2) =
    r1 == r2 ||                -- Same row
    c1 == c2 ||                -- Same column
    abs (r1 - r2) == abs (c1 - c2)  -- Same diagonal

-- Check if a queen position is safe with respect to existing queens
isSafe :: Position -> [Position] -> Bool
isSafe pos = not . any (threatens pos)

-- Generate all solutions for n queens
queens :: Int -> [[Position]]
queens n = queensHelper n n
    where
        queensHelper :: Int -> Int -> [[Position]]
        queensHelper n row
            | row == 0 = [[]]  -- Base case: empty solution
            | otherwise = [   -- For each row
                (row, col) : others |
                others <- queensHelper n (row-1),  -- Recursive solutions
                col <- [1..n],                     -- Try each column
                isSafe (row, col) others           -- Check if safe
            ]

-- Pretty print a solution
printBoard :: [Position] -> IO ()
printBoard qs = mapM_ printRow [1..n]
    where
        n = length qs
        printRow row = do
            putStrLn [if (row, col) `elem` qs then 'Q' else '.' | col <- [1..n]]

-- Main function to print first solution
main :: IO ()
main = do
    let n = 8  -- Try 8-queens puzzle
    case queens n of
        (solution:_) -> do
            putStrLn $ "A solution for " ++ show n ++ "-queens:"
            printBoard solution
        [] -> putStrLn "No solution exists"