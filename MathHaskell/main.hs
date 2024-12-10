import Data.Char (isAlpha, isDigit, digitToInt, intToDigit, toUpper)
import Data.List (isPrefixOf)
import Numeric (readHex, showHex)
import Prelude hiding (log)
import qualified Prelude (log)

-- Data Types
data Token
    = TNum Double
    | TVar String
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TPow
    | TEqual
    | TLParen
    | TRParen
    deriving (Show)

data Expr
    = Num Double
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Log Expr
    | Sin Expr 
    | Cos Expr 
    deriving (Show)

data Equation = Equation Expr Expr deriving (Show)

-- Tokenizer
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c:cs)
    | "log" `isPrefixOf` str = TVar "log" : tokenize (drop 3 str)
    | "sin" `isPrefixOf` str = TVar "sin" : tokenize (drop 3 str)
    | "cos" `isPrefixOf` str = TVar "cos" : tokenize (drop 3 str)
    | c == '+'  = TPlus : tokenize cs
    | c == '-'  = TMinus : tokenize cs
    | c == '*'  = TMul : tokenize cs
    | c == '/'  = TDiv : tokenize cs
    | c == '^'  = TPow : tokenize cs
    | c == '='  = TEqual : tokenize cs
    | c == '('  = TLParen : tokenize cs
    | c == ')'  = TRParen : tokenize cs
    | isAlpha c = let (var, rest) = span isAlpha (c:cs) in TVar var : tokenize rest
    | isDigit c || c == '.' = tokenizeNumber (c:cs)
    | c == ' '  = tokenize cs
    | otherwise = error ("Unexpected character: " ++ [c])

tokenizeNumber :: String -> [Token]
tokenizeNumber cs =
    let (numStr, rest) = span (\x -> isDigit x || x == '.') cs
    in TNum (read numStr) : tokenize rest

-- Parser for Expressions
parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens = parseAddSub tokens

parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens =
    let (term, rest) = parseMulDiv tokens
    in case rest of
        (TPlus:ts) -> let (expr, rest') = parseAddSub ts in (Add term expr, rest')
        (TMinus:ts) -> let (expr, rest') = parseAddSub ts in (Sub term expr, rest')
        _ -> (term, rest)

parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens =
    let (factor, rest) = parseFactor tokens
    in case rest of
        (TMul:ts) -> let (expr, rest') = parseMulDiv ts in (Mul factor expr, rest')
        (TDiv:ts) -> let (expr, rest') = parseMulDiv ts in (Div factor expr, rest')
        _ -> (factor, rest)

parseFactor :: [Token] -> (Expr, [Token])
parseFactor (TNum n:TVar v:ts) = (Mul (Num n) (Var v), ts) -- Coefficient * Variable
parseFactor (TVar "log":TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (Log expr, ts') -- Logarithmic function
        _ -> error "Expected closing parenthesis after log"
parseFactor (TVar "sin":TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (Sin expr, ts') 
        _ -> error "Expected closing parenthesis after sin"
parseFactor (TVar "cos":TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (Cos expr, ts') 
        _ -> error "Expected closing parenthesis after cos"
parseFactor (TNum n:ts) = (Num n, ts)
parseFactor (TVar v:ts) = (Var v, ts)
parseFactor (TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (expr, ts')
        _ -> error "Expected closing parenthesis"
parseFactor tokens = parseFactorBase tokens
    
parseFactorBase :: [Token] -> (Expr, [Token])
parseFactorBase (TNum n:ts) = (Num n, ts)
parseFactorBase (TVar v:ts) = (Var v, ts)
parseFactorBase (TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (expr, ts')
        _ -> error "Expected closing parenthesis"
parseFactorBase _ = error "Unexpected token"

-- Parser for Equations
parseEquation :: [Token] -> (Equation, [Token])
parseEquation tokens =
    let (lhs, rest) = parseExpr tokens
    in case rest of
        (TEqual:ts) -> let (rhs, rest') = parseExpr ts in (Equation lhs rhs, rest')
        _ -> error $ "Expected '=' in equation, but got: " ++ show rest

-- Evaluator
evaluate :: Expr -> Double
evaluate (Num n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
evaluate (Div e1 e2) =
    let divisor = evaluate e2
    in if divisor == 0 then error "Division by zero" else evaluate e1 / divisor
evaluate (Pow e1 e2) = evaluate e1 ** evaluate e2
evaluate (Log e) = Prelude.log (evaluate e)
evaluate (Sin e) = Prelude.sin (evaluate e * pi / 180)
evaluate (Cos e) = Prelude.cos (evaluate e * pi / 180)

-- Solver for Equations
solveEquation :: Equation -> String
-- Case: 2x + 3 = log(3)
solveEquation (Equation (Add (Mul (Num a) (Var "x")) (Num b)) (Log (Num c))) =
    let x = (Prelude.log c - b) / a in "x = " ++ show x

-- Case: 2x + log(3) = 1
solveEquation (Equation (Add (Mul (Num a) (Var "x")) (Log (Num b))) (Num c)) =
    let x = (c - Prelude.log b) / a in "x = " ++ show x

-- Case: 2x * log(3) = 0
solveEquation (Equation (Mul (Mul (Num a) (Var "x")) (Log (Num b))) (Num c)) =
    if c == 0 then "x = 0 (Product is zero)" else "No solution for this form."

-- Case: 2x + 3 = 7
solveEquation (Equation (Add (Mul (Num a) (Var "x")) (Num b)) (Num c)) =
    let x = (c - b) / a in "x = " ++ show x

-- Case: log(a) + x = c
solveEquation (Equation (Add (Log (Num a)) (Var "x")) (Num c)) =
    let x = c - Prelude.log a in "x = " ++ show x


-- Case: log(a * x) = c
solveEquation (Equation (Log (Mul (Num a) (Var "x"))) (Num c)) =
    let x = (exp c) / a in "x = " ++ show x

-- Case: log(x) = c
solveEquation (Equation (Log (Var "x")) (Num c)) =
    let x = exp c in "x = " ++ show x

-- Case: log(a * x) + b = c
solveEquation (Equation (Add (Log (Mul (Num a) (Var "x"))) (Num b)) (Num c)) =
    let x = exp (c - b) / a in "x = " ++ show x

-- Default case
solveEquation _ = "Equation solver is not yet implemented for this form."


-- Conversion Functions
binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0

decimalToBinary :: Int -> String
decimalToBinary 0 = "0"
decimalToBinary n = reverse (helper n)
  where
    helper 0 = []
    helper x = let (q, r) = x `divMod` 2 in (intToDigit r) : helper q

hexToDecimal :: String -> Int
hexToDecimal hex = fst . head $ readHex hex

decimalToHex :: Int -> String
decimalToHex n = map toUpper (showHex n "")

binaryToHex :: String -> String
binaryToHex bin = decimalToHex (binaryToDecimal bin)

hexToBinary :: String -> String
hexToBinary hex = decimalToBinary (hexToDecimal hex)

-- Main Program
main :: IO ()
main = do
    putStrLn "Select an option:"
    putStrLn "1. Perform mathematical calculations"
    putStrLn "2. Convert between binary, hex, and decimal"
    putStrLn "3. Solve equations"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Enter a mathematical expression:"
            input <- getLine
            let tokens = tokenize input
            let (ast, _) = parseExpr tokens
            print $ evaluate ast
        "2" -> do
            putStrLn "Enter conversion type (binaryToHex, hexToBinary, decimalToBinary, binaryToDecimal, hexToDecimal, decimalToHex):"
            conversion <- getLine
            case conversion of
                "binaryToHex" -> do
                    putStrLn "Enter binary number:"
                    bin <- getLine
                    putStrLn $ "Hex: " ++ binaryToHex bin
                "hexToBinary" -> do
                    putStrLn "Enter hex number:"
                    hex <- getLine
                    putStrLn $ "Binary: " ++ hexToBinary hex
                "decimalToBinary" -> do
                    putStrLn "Enter decimal number:"
                    dec <- getLine
                    putStrLn $ "Binary: " ++ decimalToBinary (read dec)
                "binaryToDecimal" -> do
                    putStrLn "Enter binary number:"
                    bin <- getLine
                    putStrLn $ "Decimal: " ++ show (binaryToDecimal bin)
                "hexToDecimal" -> do
                    putStrLn "Enter hex number:"
                    hex <- getLine
                    putStrLn $ "Decimal: " ++ show (hexToDecimal hex)
                "decimalToHex" -> do
                    putStrLn "Enter decimal number:"
                    dec <- getLine
                    putStrLn $ "Hex: " ++ decimalToHex (read dec)
                _ -> putStrLn "Invalid conversion type!"
        "3" -> do
            putStrLn "Enter an equation (e.g., 2x + 3 = 7):"
            input <- getLine
            let tokens = tokenize input
            let (eq, _) = parseEquation tokens
            putStrLn $ solveEquation eq
        _ -> putStrLn "Invalid option!"
