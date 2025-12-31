module Main where

import System.IO
import Data.List
import Data.Char
import Data.Ord

-- Hello, World!
hello :: IO ()
hello = putStrLn "Hello, World!"

data LetterCount = LetterCount Char Int
    deriving (Show, Eq)

analyzeBeeMovie :: IO ()
analyzeBeeMovie = do
    let scriptPath = "../beemoviescript.txt"
    
    fileExists <- doesFileExist scriptPath
    if not fileExists
        then putStrLn $ "File not found: " ++ scriptPath
        else do
            putStrLn "Bee Movie Script:"
            putStrLn "--------------------------------------------------"
            
            content <- readFile scriptPath
            let lines' = lines content
            
            -- Print each line
            mapM_ putStrLn lines'
            
            -- Count letters
            let allChars = concat lines'
            let letters = filter isLetter allChars
            let lowerLetters = map toLower letters
            let letterCounts = countOccurrences lowerLetters
            
            putStrLn "--------------------------------------------------"
            putStrLn "Analysis complete."
            
            if not (null letters)
                then do
                    let sortedCounts = sortBy (comparing (\(LetterCount _ c) -> Down c)) letterCounts
                    putStrLn "\nTop 3 most commonly used letters:"
                    mapM_ printTop3 (take 3 sortedCounts)
                else putStrLn "No letters found in the script."

countOccurrences :: String -> [LetterCount]
countOccurrences str = 
    let uniqueLetters = nub $ sort str
    in map (\letter -> LetterCount letter (length $ filter (== letter) str)) uniqueLetters

printTop3 :: LetterCount -> IO ()
printTop3 (LetterCount letter count) = 
    putStrLn $ show letter ++ ": " ++ show count ++ " times"

main :: IO ()
main = do
    hello
    analyzeBeeMovie
