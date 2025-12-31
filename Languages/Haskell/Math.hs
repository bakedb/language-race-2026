module Main where

import System.IO
import Data.List
import Data.Char
import Text.Read

-- Hello, World!
hello :: IO ()
hello = putStrLn "Hello, World!"

-- Simple expression evaluator
evaluateExpression :: String -> Double
evaluateExpression expr = 
    let parts = words expr
    in if length parts < 3 
       then 0.0
       else case (readMaybe (parts !! 0) :: Maybe Double,
                  readMaybe (parts !! 2) :: Maybe Double) of
                (Just a, Just b) -> 
                    case parts !! 1 of
                        "+" -> a + b
                        "-" -> a - b
                        "*" -> a * b
                        "/" -> if b /= 0.0 then a / b else 0.0
                        _ -> 0.0
                _ -> 0.0

solveEquation :: String -> IO ()
solveEquation line = do
    let equation = takeWhile (/= '=') line
    let trimmedEquation = dropWhile isSpace equation
    let result = evaluateExpression trimmedEquation
    putStrLn $ trimmedEquation ++ " = " ++ show (result :: Double)

processFile :: FilePath -> IO ()
processFile filename = do
    exists <- doesFileExist filename
    if not exists
        then putStrLn $ "Could not open file: " ++ filename
        else do
            contents <- readFile filename
            let lines' = lines contents
            mapM_ processLine lines'
  where
    processLine line
        | null (dropWhile isSpace line) = return ()
        | head (dropWhile isSpace line) == '#' = return ()
        | otherwise = do
            let start = if take 2 line == "- " then drop 2 line else line
            if '=' `elem` start
                then solveEquation start
                else return ()

main :: IO ()
main = do
    hello
    putStrLn "\nProcessing math equations..."
    
    processFile "../test_data/math_equations.txt"
    processFile "../test_data/math_equations.md"
    processFile "../test_data/math_equations.json"
    processFile "../test_data/math_equations.yaml"
    processFile "../test_data/math_equations"
