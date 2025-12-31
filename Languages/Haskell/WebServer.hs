module Main where

import System.IO
import Network.HTTP.Client
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Time
import Control.Exception
import Control.Monad

-- Hello, World!
hello :: IO ()
hello = putStrLn "Hello, World!"

data TestResult = TestResult
    { endpoint :: String
    , url :: String
    , serverHash :: Maybe String
    , expectedHash :: String
    , status :: String
    , error :: Maybe String
    } deriving (Show, Eq)

instance ToJSON TestResult where
    toJSON (TestResult endpoint' url' serverHash' expectedHash' status' error') =
        object [ "endpoint" .= endpoint'
               , "url" .= url'
               , "server_hash" .= serverHash'
               , "expected_hash" .= expectedHash'
               , "status" .= status'
               , "error" .= error'
               ]

data FinalResult = FinalResult
    { totalTests :: Int
    , passed :: Int
    , failed :: Int
    , successRate :: String
    , timestamp :: String
    , results :: [TestResult]
    } deriving (Show, Eq)

instance ToJSON FinalResult where
    toJSON (FinalResult totalTests' passed' failed' successRate' timestamp' results') =
        object [ "total_tests" .= totalTests'
               , "passed" .= passed'
               , "failed" .= failed'
               , "success_rate" .= successRate'
               , "timestamp" .= timestamp'
               , "results" .= results'
               ]

loadCompareJSON :: FilePath -> IO (Maybe Object)
loadCompareJSON filename = do
    exists <- doesFileExist filename
    if not exists
        then do
            putStrLn $ "Error: Could not find " ++ filename
            return Nothing
        else do
            content <- BL.readFile filename
            case decode content of
                Nothing -> do
                    putStrLn "Error: Could not parse JSON"
                    return Nothing
                Just obj -> return $ Just obj

makeHTTPRequest :: Manager -> String -> IO (Maybe ByteString)
makeHTTPRequest manager url = do
    request <- parseRequest url
    catch (do
        response <- httpLbs request manager
        if statusCode (responseStatus response) == 200
            then return $ Just (responseBody response)
            else return Nothing
    ) (\(_ :: SomeException) -> return Nothing)

testWebServer :: IO ()
testWebServer = do
    let baseUrl = "http://localhost:3000"
    let compareFile = "../webserver/compare.json"
    
    -- Load expected hashes
    maybeExpected <- loadCompareJSON compareFile
    case maybeExpected of
        Nothing -> return ()
        Just expected -> do
            putStrLn "Testing 100 endpoints..."
            
            manager <- newManager defaultManagerSettings
            
            let testEndpoint i = do
                    let endpoint = "test-" ++ show i
                    let url = baseUrl ++ "/" ++ endpoint
                    
                    response <- makeHTTPRequest manager url
                    
                    let expectedHash = case lookup endpoint (Object expected) of
                                        Just (String h) -> h
                                        _ -> ""
                    
                    case response of
                        Just resp -> do
                            case decode resp of
                                Just (Object obj) -> do
                                    let serverHash = case lookup "hash" obj of
                                                        Just (String h) -> h
                                                        _ -> ""
                                    
                                    let status = if serverHash == expectedHash then "PASSED" else "FAILED"
                                    let passed = if status == "PASSED" then 1 else 0
                                    let failed = if status == "FAILED" then 1 else 0
                                    
                                    return (TestResult endpoint url (Just serverHash) expectedHash status Nothing, passed, failed)
                                _ -> do
                                    return (TestResult endpoint url Nothing expectedHash "FAILED" (Just "JSON parse error"), 0, 1)
                        Nothing -> do
                            return (TestResult endpoint url Nothing expectedHash "FAILED" (Just "HTTP request failed"), 0, 1)
            
            -- Test all endpoints
            results <- mapM testEndpoint [0..99]
            let testResults = map (\(r, _, _) -> r) results
            let totalPassed = sum $ map (\(_, p, _) -> p) results
            let totalFailed = sum $ map (\(_, _, f) -> f) results
            
            -- Progress indicator
            mapM_ (\i -> when ((i + 1) `mod` 10 == 0) $ 
                     putStrLn $ "Tested " ++ show (i + 1) ++ "/100 endpoints...") [0..99]
            
            -- Create final result
            let totalTests = length testResults
            let successRate = show (fromIntegral totalPassed / fromIntegral totalTests * 100 :: Double) ++ "%"
            
            currentTime <- getCurrentTime
            let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
            
            let finalResult = FinalResult totalTests totalPassed totalFailed successRate timestamp testResults
            
            -- Save results to file
            let outputFile = "test-result.json"
            BL.writeFile outputFile (encode finalResult)
            putStrLn $ "Results saved to: " ++ outputFile
            
            putStrLn "\nTest completed!"
            putStrLn $ "Passed: " ++ show totalPassed ++ "/" ++ show totalTests ++ " (" ++ successRate ++ ")"
            putStrLn $ "Failed: " ++ show totalFailed ++ "/" ++ show totalTests ++ " (" ++ show (100 - read (takeWhile (/= '%') successRate) :: Double) ++ "%)"

main :: IO ()
main = do
    hello
    testWebServer
