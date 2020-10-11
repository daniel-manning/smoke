module Lib where

import Data.List

writeOutSolutionFile :: Problem -> IO ()
writeOutSolutionFile problem = writeFile (outputPath problem) (unlines $ programme problem)

data Range = Naturals
data Expr = Sum Expr | Set Range [Expr] | Filter String
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

doSomethingToLimitRange Naturals = "(takeWhile (<k) [1..])" --Need a way to track variables in the expression and declare them at top level

assignResult :: Expr -> String
assignResult expr = "result k = " ++ codeExpression expr

writeOutResultForInputValue :: Int -> String
writeOutResultForInputValue n = "main = print $ result " ++ show n

programme :: Problem -> [String]
programme problem = [assignResult (solution problem), writeOutResultForInputValue (inputValue problem)]
