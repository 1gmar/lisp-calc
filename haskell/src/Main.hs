module Main where

import Data.Char (isSpace, isLetter, isDigit)
import Control.Monad (when)

data BOperator = BPlus
               | BMinus
               | Times
               | Div
               | Pow deriving Show

data UOperator = UPlus
               | UMinus
               | Sqrt deriving Show

data LispTree = Numeral Double
              | UExpression UOperator LispTree
              | BExpression BOperator LispTree LispTree deriving Show

type TryParseResult = Either String (LispTree, String)

number :: String -> (LispTree, String)
number input = let (digits, rest) = span (\c -> isDigit c || c == '.') input
               in (Numeral $ read digits, rest)

keyword :: String -> TryParseResult
keyword input = let (kWord, rest) = span isLetter input
                in case kWord of
                    "sqrt" -> unaryExpression Sqrt rest
                    _      -> Left $ "Unrecognized keyword token: " ++ kWord

unaryExpression :: UOperator -> String -> TryParseResult
unaryExpression op input = do
    (tree, rest) <- expression input
    return (UExpression op tree, rest)

binaryExpression :: String -> TryParseResult
binaryExpression (c:rest) = case c of
    '+' -> buildExpression BPlus rest
    '-' -> buildExpression BMinus rest
    '*' -> buildExpression Times rest
    '/' -> buildExpression Div rest
    '^' -> buildExpression Pow rest
    _   -> Left $ "Unsupported operator token: " ++ show c
    where
        buildExpression op input = do
            (left, lTail) <- expression input
            (right, rTail) <- expression lTail
            return (BExpression op left right, rTail)

validate :: (LispTree, String) -> TryParseResult
validate (tree, input) = if not $ null input && head input == ')'
    then Right (tree, tail input)
    else Left "Missing enclosing ')' token"

expression :: String -> TryParseResult
expression input@(token:rest)
    | token == '('   = binaryExpression rest >>= validate
    | token == '+'   = unaryExpression UPlus rest
    | token == '-'   = unaryExpression UMinus rest
    | isDigit token  = Right $ number input
    | isLetter token = keyword input
    | isSpace token  = expression rest
    | otherwise      = Left $ "Unable to parse token: " ++ show token

parse :: String -> Either String LispTree
parse input = do
    (tree, rest) <- expression input
    if null rest then Right tree else Left $ "Leftover tokens: " ++ rest

binaryOperator :: BOperator -> Double -> Double -> Double
binaryOperator op = case op of
    BPlus  -> (+)
    BMinus -> (-)
    Times  -> (*)
    Div    -> (/)
    Pow    -> (**)

unaryOperator :: UOperator -> Double -> Double
unaryOperator op = case op of
    UPlus  -> id
    UMinus -> \x -> -x
    Sqrt   -> sqrt

evaluate :: LispTree -> Double
evaluate tree = case tree of
    Numeral val               -> val
    UExpression op expr       -> unaryOperator op $ evaluate expr
    BExpression op left right -> binaryOperator op (evaluate left) (evaluate right)

compute :: String -> Either String Double
compute input = evaluate <$> parse input

main :: IO ()
main = evaluateLoop

showResult :: Either String Double -> String
showResult (Left err) = "Error: " ++ err
showResult (Right res) = show res

evaluateLoop :: IO ()
evaluateLoop = do
    putStr "> "
    input <- getLine
    when (":q" /= input) $ do
        let result = compute input
        putStrLn $ showResult result
        evaluateLoop