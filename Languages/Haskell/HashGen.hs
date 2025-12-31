module Main where

import System.IO
import System.Directory
import System.FilePath
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Time
import Control.Exception
import Control.Monad
import Data.List (sort)
import Data.Aeson
import Data.Aeson.Types

-- Hello, World!
hello :: IO ()
hello = putStrLn "Hello, World!"

data FileHashResult = FileHashResult
    { filename :: String
    , filepath :: String
    , size :: Integer
    , hash :: String
    , status :: String
    , error :: Maybe String
    } deriving (Show, Eq)

instance ToJSON FileHashResult where
    toJSON (FileHashResult filename' filepath' size' hash' status' error') =
        object [ "filename" .= filename'
               , "filepath" .= filepath'
               , "size" .= size'
               , "algorithm" .= ("sha256" :: String)
               , "hash" .= hash'
               , "status" .= status'
               , "error" .= error'
               ]

data HashResults = HashResults
    { totalFiles :: Int
    , successfulHashes :: Int
    , failedHashes :: Int
    , algorithm :: String
    , processingTimeSeconds :: Double
    , averageTimePerFileMs :: Double
    , timestamp :: String
    , directory :: String
    , results :: [FileHashResult]
    } deriving (Show, Eq)

instance ToJSON HashResults where
    toJSON (HashResults totalFiles' successfulHashes' failedHashes' algorithm' 
                processingTimeSeconds' averageTimePerFileMs' timestamp' directory' results') =
        object [ "total_files" .= totalFiles'
               , "successful_hashes" .= successfulHashes'
               , "failed_hashes" .= failedHashes'
               , "algorithm" .= algorithm'
               , "processing_time_seconds" .= processingTimeSeconds'
               , "average_time_per_file_ms" .= averageTimePerFileMs'
               , "timestamp" .= timestamp'
               , "directory" .= directory'
               , "results" .= results'
               ]

calculateFileHash :: FilePath -> IO String
calculateFileHash filepath = do
    content <- BL.readFile filepath
    return $ show $ sha256 content

getFileSize :: FilePath -> IO Integer
getFileSize filepath = do
    exists <- doesFileExist filepath
    if exists
        then getFileSize filepath
        else return 0

getTxtFiles :: FilePath -> IO [FilePath]
getTxtFiles directory = do
    exists <- doesDirectoryExist directory
    if not exists
        then do
            putStrLn $ "Directory not found: " ++ directory
            return []
        else do
            allFiles <- listDirectory directory
            let txtFiles = map (directory </>) $ filter (".txt" `isSuffixOf`) allFiles
            return $ sort txtFiles

getTimestamp :: IO String
getTimestamp = do
    currentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime

hashFilesInDirectory :: FilePath -> IO ()
hashFilesInDirectory directory = do
    files <- getTxtFiles directory
    
    if null files
        then putStrLn $ "No .txt files found in directory: " ++ directory
        else do
            putStrLn $ "Found " ++ show (length files) ++ " files to hash"
            
            startTime <- getCurrentTime
            
            results <- mapM processFile files
            
            endTime <- getCurrentTime
            let processingTime = realToFrac $ diffUTCTime endTime startTime :: Double
            let averageTimePerFile = (processingTime / fromIntegral (length files)) * 1000
            
            timestamp <- getTimestamp
            
            let successful = length $ filter (\r -> status r == "SUCCESS") results
            let failed = length - successful
            
            let finalResults = HashResults
                    { totalFiles = length files
                    , successfulHashes = successful
                    , failedHashes = failed
                    , algorithm = "sha256"
                    , processingTimeSeconds = processingTime
                    , averageTimePerFileMs = averageTimePerFile
                    , timestamp = timestamp
                    , directory = directory
                    , results = results
                    }
            
            -- Save to file
            let outputFile = "hash_results.json"
            BL.writeFile outputFile (encode finalResults)
            putStrLn $ "Results saved to: " ++ outputFile
            
            putStrLn "\nHashing completed!"
            putStrLn $ "Total files: " ++ show (length files)
            putStrLn $ "Successful: " ++ show successful
            putStrLn $ "Failed: " ++ show failed
            putStrLn $ "Processing time: " ++ show (round (processingTime * 1000) / 1000) ++ " seconds"
            putStrLn $ "Average time per file: " ++ show (round (averageTimePerFile * 100) / 100) ++ " ms"
            
            -- Progress indicator
            mapM_ (\i -> when (i `mod` 100 == 0) $ 
                     putStrLn $ "Processed " ++ show i ++ "/" ++ show (length files) ++ " files...") [1..length files]

processFile :: FilePath -> IO FileHashResult
processFile filepath = do
    let filename = takeFileName filepath
    size <- getFileSize filepath
    
    result <- catch (calculateFileHash filepath) (\(_ :: SomeException) -> return "")
    
    if not (null result)
        then return $ FileHashResult filename filepath size result "SUCCESS" Nothing
        else return $ FileHashResult filename filepath size "" "FAILED" (Just "Hash calculation failed")

main :: IO ()
main = do
    hello
    hashFilesInDirectory "../hashfiles"
