module Lib where

import Data.List

writeOutSolutionFile :: Problem -> IO ()
writeOutSolutionFile problem = writeFile (outputPath problem) (unlines $ programme problem)

data Range = Naturals | Fibonacci
data Restriction = LTR Range | LTER Range
data Expr = Sum Expr | Set Restriction [Expr] | Filter String
data Problem = Problem {
    solution :: Expr,
    inputValue :: Int,
    outputPath :: String
}

codeExpression :: Expr -> String
codeExpression (Sum expr) = "sum $" ++ codeExpression expr
codeExpression (Set range expr) = "[n | n <- " ++ doSomethingToLimitRange range ++ ", " ++ (concat $ intersperse ", " $ map codeExpression expr) ++ "]"
codeExpression (Filter string) = string -- leave this as a code block for now but should be construction of boolean expression
                                            -- also needs tracking of available variables

doSomethingToLimitRange (LTR range) = "(takeWhile (<k) " ++ setRange range ++ ")" --Need a way to track variables in the expression and declare them at top level
doSomethingToLimitRange (LTER range) = "(takeWhile (<=k) " ++ setRange range ++ ")" --Need a way to track variables in the expression and declare them at top level

setRange Naturals = "naturals"
setRange Fibonacci = "fibonacci"

mopUp (Set (LTR Naturals) _)   = define_naturals
mopUp (Set (LTER Naturals) _)  = define_naturals
mopUp (Set (LTR Fibonacci) _)  = define_fibs
mopUp (Set (LTER Fibonacci) _) = define_fibs      -- Needs a lens?
mopUp (Sum expr)               = mopUp expr

define_naturals = "naturals = [1..]"
define_fibs = unlines ["fibonacci :: [Integer]","fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)"] --Future goal is to be able to generate this from definition

assignResult :: Expr -> String
assignResult expr = "result k = " ++ codeExpression expr

writeOutResultForInputValue :: Int -> String
writeOutResultForInputValue n = "main = print $ result " ++ show n

programme :: Problem -> [String]
programme problem = [assignResult (solution problem), mopUp (solution problem), writeOutResultForInputValue (inputValue problem)]
