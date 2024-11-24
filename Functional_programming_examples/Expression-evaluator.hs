-- Mathematical Expression Evaluator

-- Define the expression data type
data Expr = Num Double
         | Add Expr Expr
         | Sub Expr Expr
         | Mul Expr Expr
         | Div Expr Expr
         | Pow Expr Expr
         | Var String
         | Fun String Expr  -- Function application (e.g., sin, cos)
         deriving Show

-- Environment to store variable values
type Env = [(String, Double)]

-- Built-in functions
type FunDef = Double -> Double

-- Map of available functions
functions :: [(String, FunDef)]
functions = [
    ("sin", sin),
    ("cos", cos),
    ("exp", exp),
    ("log", log),
    ("sqrt", sqrt)
    ]

-- Evaluate an expression in given environment
eval :: Env -> Expr -> Either String Double
eval _ (Num n) = Right n

eval env (Add e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    Right (v1 + v2)

eval env (Sub e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    Right (v1 - v2)

eval env (Mul e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    Right (v1 * v2)

eval env (Div e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    if v2 == 0
        then Left "Division by zero"
        else Right (v1 / v2)

eval env (Pow e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    Right (v1 ** v2)

eval env (Var name) = 
    case lookup name env of
        Just v  -> Right v
        Nothing -> Left $ "Undefined variable: " ++ name

eval env (Fun name e) = do
    v <- eval env e
    case lookup name functions of
        Just f  -> Right (f v)
        Nothing -> Left $ "Undefined function: " ++ name

-- Parse a simple expression (for demonstration)
parse :: String -> Either String Expr
parse "x" = Right (Var "x")
parse "y" = Right (Var "y")
parse s = case reads s of
    [(n, "")] -> Right (Num n)
    _         -> Left $ "Cannot parse: " ++ s

-- Example usage
main :: IO ()
main = do
    let env = [("x", 2), ("y", 3)]
    let expr1 = Add (Mul (Var "x") (Var "y")) (Num 1)
    let expr2 = Fun "sin" (Div (Var "x") (Num 2))
    
    putStrLn "Expression 1: x * y + 1"
    print $ eval env expr1
    
    putStrLn "\nExpression 2: sin(x/2)"
    print $ eval env expr2
    
    -- Test error handling
    let expr3 = Div (Num 1) (Num 0)
    putStrLn "\nExpression 3: 1/0"
    print $ eval env expr3