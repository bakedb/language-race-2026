module Main where

import System.IO
import System.Directory
import System.Random
import Control.Monad

-- Hello, World!
hello :: IO ()
hello = putStrLn "Hello, World!"

generateRandomNumbers :: IO ()
generateRandomNumbers = do
    let count = 1000
    let outputDir = "../rand_avg output"
    let outputFile = outputDir ++ "/random_numbers.txt"
    
    -- Create output directory
    createDirectoryIfMissing True outputDir
    
    -- Generate 1000 random numbers
    gen <- newStdGen
    let randomNumbers = take count $ randomRs (0, 999) gen
    let sum = sum randomNumbers
    let mean = fromIntegral sum / fromIntegral count :: Double
    
    -- Save to file
    writeFile outputFile $ unlines $ map show randomNumbers
    
    putStrLn "Generated 1000 random numbers"
    putStrLn $ "Mean: " ++ show (mean :: Double)
    putStrLn $ "Saved to: " ++ outputFile

main :: IO ()
main = do
    hello
    generateRandomNumbers
