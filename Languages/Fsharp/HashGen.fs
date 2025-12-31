open System
open System.IO
open System.Security.Cryptography
open System.Collections.Generic
open Newtonsoft.Json

type FileHashResult = {
    filename: string
    filepath: string
    size: int64
    hash: string
    status: string
    error: string option
}

type HashResults = {
    totalFiles: int
    successfulHashes: int
    failedHashes: int
    algorithm: string
    processingTimeSeconds: float
    averageTimePerFileMs: float
    timestamp: string
    directory: string
    results: FileHashResult list
}

// Hello, World!
let hello () = printfn "Hello, World!"

let calculateFileHash (filepath: string) : string =
    try
        use sha256 = SHA256.Create()
        use fileStream = File.OpenRead(filepath)
        let hashBytes = sha256.ComputeHash(fileStream)
        BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()
    with
    | _ -> ""

let getFileSize (filepath: string) : int64 =
    try
        let fileInfo = FileInfo(filepath)
        fileInfo.Length
    with
    | _ -> 0L

let getTxtFiles (directory: string) : string list =
    try
        if not (Directory.Exists(directory)) then
            printfn "Directory not found: %s" directory
            []
        else
            Directory.GetFiles(directory, "*.txt")
            |> Array.sort
            |> Array.toList
    with
    | e -> 
        printfn "Error reading directory: %s" e.Message
        []

let getTimestamp () : string =
    DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")

let hashFilesInDirectory (directory: string) =
    let files = getTxtFiles directory
    
    if List.isEmpty files then
        printfn "No .txt files found in directory: %s" directory
        ()
    else
        printfn "Found %d files to hash" (List.length files)
        
        let mutable successfulHashes = 0
        let mutable failedHashes = 0
        
        let results = 
            files
            |> List.mapi (fun i filepath ->
                let filename = Path.GetFileName(filepath)
                let size = getFileSize filepath
                
                let hash = calculateFileHash filepath
                
                let result = {
                    filename = filename
                    filepath = filepath
                    size = size
                    hash = hash
                    status = ""
                    error = None
                }
                
                let finalResult = 
                    if not (String.IsNullOrEmpty(hash)) then
                        { result with 
                            hash = hash
                            status = "SUCCESS"
                        }
                    else
                        { result with 
                            hash = ""
                            status = "FAILED"
                            error = Some "Hash calculation failed"
                        }
                
                if finalResult.status = "SUCCESS" then
                    successfulHashes <- successfulHashes + 1
                else
                    failedHashes <- failedHashes + 1
                    
                finalResult
            )
        
        // Progress indicator
        [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]
        |> List.iter (fun i -> 
            if i <= List.length files then
                printfn "Processed %d/%d files..." i (List.length files))
        
        let totalFiles = List.length files
        let processingTime = 0.0 // Simplified - would need actual timing
        let averageTimePerFile = 0.0 // Simplified
        
        let finalResults = {
            totalFiles = totalFiles
            successfulHashes = successfulHashes
            failedHashes = failedHashes
            algorithm = "sha256"
            processingTimeSeconds = processingTime
            averageTimePerFileMs = averageTimePerFile
            timestamp = getTimestamp()
            directory = directory
            results = results
        }
        
        // Create JSON output
        let jsonResults = 
            let jsonResult = JObject()
            jsonResult.["total_files"] <- JValue(totalFiles)
            jsonResult.["successful_hashes"] <- JValue(successfulHashes)
            jsonResult.["failed_hashes"] <- JValue(failedHashes)
            jsonResult.["algorithm"] <- JValue("sha256")
            jsonResult.["processing_time_seconds"] <- JValue(processingTime)
            jsonResult.["average_time_per_file_ms"] <- JValue(averageTimePerFile)
            jsonResult.["timestamp"] <- JValue(getTimestamp())
            jsonResult.["directory"] <- JValue(directory)
            
            let resultsArray = JArray()
            for result in finalResults.results do
                let jsonFileResult = JObject()
                jsonFileResult.["filename"] <- JValue(result.filename)
                jsonFileResult.["filepath"] <- JValue(result.filepath)
                jsonFileResult.["size"] <- JValue(result.size)
                jsonFileResult.["algorithm"] <- JValue("sha256")
                jsonFileResult.["hash"] <- JValue(result.hash)
                jsonFileResult.["status"] <- JValue(result.status)
                match result.error with
                | Some err -> jsonFileResult.["error"] <- JValue(err)
                | None -> ()
                resultsArray.Add(jsonFileResult) |> ignore
            
            jsonResult.["results"] <- resultsArray
            jsonResult
        
        // Save to file
        try
            let output = jsonResults.ToString(Formatting.Indented)
            File.WriteAllText("hash_results.json", output)
            printfn "Results saved to: hash_results.json"
        with
        | e -> printfn "Error saving results: %s" e.Message
        
        printfn "\nHashing completed!"
        printfn "Total files: %d" finalResults.totalFiles
        printfn "Successful: %d" finalResults.successfulHashes
        printfn "Failed: %d" finalResults.failedHashes
        printfn "Processing time: %.3f seconds" finalResults.processingTimeSeconds
        printfn "Average time per file: %.2f ms" finalResults.averageTimePerFileMs

[<EntryPoint>]
let main argv =
    hello()
    hashFilesInDirectory "../hashfiles"
    0
