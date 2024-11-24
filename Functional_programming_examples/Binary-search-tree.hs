-- Binary Search Tree implementation in Haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Insert a value into the BST
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node value left right)
    | x < value  = Node value (insert x left) right
    | x > value  = Node value left (insert x right)
    | otherwise  = Node value left right  -- No duplicates

-- Search for a value in the BST
search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search x (Node value left right)
    | x == value = True
    | x < value  = search x left
    | otherwise  = search x right

-- Tree traversals
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node value left right) = [value] ++ preorder left ++ preorder right

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node value left right) = postorder left ++ postorder right ++ [value]

-- Find minimum value in a non-empty tree
findMin :: Tree a -> a
findMin (Node value Empty _) = value
findMin (Node _ left _) = findMin left

-- Delete a value from the BST
delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete x (Node value left right)
    | x < value  = Node value (delete x left) right
    | x > value  = Node value left (delete x right)
    | otherwise  = case (left, right) of
        (Empty, Empty) -> Empty
        (Empty, _)     -> right
        (_, Empty)     -> left
        _             -> Node minRight left (delete minRight right)
                        where minRight = findMin right

-- Example usage
main :: IO ()
main = do
    let tree = foldr insert Empty [5,3,7,1,9,2,8]
    putStrLn $ "Inorder traversal: " ++ show (inorder tree)
    putStrLn $ "Search for 3: " ++ show (search 3 tree)
    putStrLn $ "Search for 4: " ++ show (search 4 tree)
    let newTree = delete 3 tree
    putStrLn $ "After deleting 3: " ++ show (inorder newTree)