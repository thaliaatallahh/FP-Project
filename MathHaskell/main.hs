import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Prelude hiding (log)
import qualified Prelude (log) -- Explicitly qualify the log function

-- Data type for tokens
data Token
    = TNum Double    -- A number token
    | TPlus          -- '+' token
    | TMinus         -- '-' token
    | TMul           -- '*' token
    | TDiv           -- '/' token
    | TLog           -- 'log' token
    | TExp           -- 'exp' token
    | TSin           -- 'sin' token
    | TCos           -- 'cos' token
    | TTan           -- 'tan' token
    | TCot           -- 'cot' token
    | TLParen        -- '(' token
    | TRParen        -- ')' token
    deriving (Show)

-- Data type for expressions
data Expr
    = Num Double               -- A number
    | Add Expr Expr            -- Addition
    | Sub Expr Expr            -- Subtraction
    | Mul Expr Expr            -- Multiplication
    | Div Expr Expr            -- Division
    | Log Expr                 -- Natural logarithm
    | Exp Expr                 -- Exponential function
    | Sin Expr                 -- Sine
    | Cos Expr                 -- Cosine
    | Tan Expr                 -- Tangent
    | Cot Expr                 -- Cotangent
    deriving (Show)

-- Function to tokenize input
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c:cs)
    | "log" `isPrefixOf` str = TLog : tokenize (drop 3 str)
    | "exp" `isPrefixOf` str = TExp : tokenize (drop 3 str)
    | "sin" `isPrefixOf` str = TSin : tokenize (drop 3 str)
    | "cos" `isPrefixOf` str = TCos : tokenize (drop 3 str)
    | "tan" `isPrefixOf` str = TTan : tokenize (drop 3 str)
    | "cot" `isPrefixOf` str = TCot : tokenize (drop 3 str)
    | c == '+'  = TPlus : tokenize cs
    | c == '-'  = TMinus : tokenize cs
    | c == '*'  = TMul : tokenize cs
    | c == '/'  = TDiv : tokenize cs
    | c == '('  = TLParen : tokenize cs
    | c == ')'  = TRParen : tokenize cs
    | isDigit c || c == '.' = tokenizeNumber (c:cs)
    | c == ' '  = tokenize cs
    | otherwise = error ("Unexpected character: " ++ [c])

-- Helper function to tokenize numbers
tokenizeNumber :: String -> [Token]
tokenizeNumber cs =
    let (numStr, rest) = span (\x -> isDigit x || x == '.') cs
    in TNum (read numStr) : tokenize rest

-- Parse an expression
parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens = parseAddSub tokens

-- Parse addition and subtraction
parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens =
    let (term, rest) = parseMulDiv tokens
    in case rest of
        (TPlus:ts) -> let (expr, rest') = parseAddSub ts in (Add term expr, rest')
        (TMinus:ts) -> let (expr, rest') = parseAddSub ts in (Sub term expr, rest')
        _ -> (term, rest)

-- Parse multiplication and division
parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens =
    let (factor, rest) = parseFactor tokens
    in case rest of
        (TMul:ts) -> let (expr, rest') = parseMulDiv ts in (Mul factor expr, rest')
        (TDiv:ts) -> let (expr, rest') = parseMulDiv ts in (Div factor expr, rest')
        _ -> (factor, rest)

-- Parse factors (numbers, parentheses, or functions)
parseFactor :: [Token] -> (Expr, [Token])
parseFactor (TNum n:ts) = (Num n, ts)
parseFactor (TLParen:ts) =
    let (expr, rest) = parseExpr ts
    in case rest of
        (TRParen:ts') -> (expr, ts')
        _ -> error "Expected closing parenthesis"
parseFactor (TLog:ts) = let (expr, rest) = parseFactor ts in (Log expr, rest)
parseFactor (TExp:ts) = let (expr, rest) = parseFactor ts in (Exp expr, rest)
parseFactor (TSin:ts) = let (expr, rest) = parseFactor ts in (Sin expr, rest)
parseFactor (TCos:ts) = let (expr, rest) = parseFactor ts in (Cos expr, rest)
parseFactor (TTan:ts) = let (expr, rest) = parseFactor ts in (Tan expr, rest)
parseFactor (TCot:ts) = let (expr, rest) = parseFactor ts in (Cot expr, rest)
parseFactor _ = error "Unexpected token"

-- Evaluate the expression
evaluate :: Expr -> Double
evaluate (Num n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
evaluate (Div e1 e2) =
    let divisor = evaluate e2
    in if divisor == 0 then error "Division by zero" else evaluate e1 / divisor
evaluate (Log e) =
    let x = evaluate e
    in if x <= 0 then error "Logarithm undefined for non-positive values" else Prelude.log x
evaluate (Exp e) = exp (evaluate e)
evaluate (Sin e) = sin (evaluate e)
evaluate (Cos e) = cos (evaluate e)
evaluate (Tan e) =
    let x = evaluate e
    in if cos x == 0 then error "Tangent undefined" else tan x
evaluate (Cot e) =
    let x = evaluate e
    in if sin x == 0 then error "Cotangent undefined" else 1 / tan x

-- Main function to run the calculator
main :: IO ()
main = do
    putStrLn "Enter a mathematical expression:"
    input <- getLine
    let tokens = tokenize input
    let (ast, _) = parseExpr tokens
    print $ evaluate ast
