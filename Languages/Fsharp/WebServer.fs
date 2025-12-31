open System
open System.IO
open System.Net
open System.Collections.Generic
open Newtonsoft.Json

type TestResult = {
    endpoint: string
    url: string
    serverHash: string option
    expectedHash: string
    status: string
    error: string option
}

type FinalResult = {
    totalTests: int
    passed: int
    failed: int
    successRate: string
    timestamp: string
    results: TestResult list
}

// Hello, World!
let hello () = printfn "Hello, World!"

let loadCompareJson (filename: string) : Map<string, string> option =
    try
        if not (File.Exists(filename)) then
            printfn "Error: Could not find %s" filename
            None
        else
            let content = File.ReadAllText(filename)
            let json = JsonConvert.DeserializeObject<Dictionary<string, string>>(content)
            Some (Map.ofSeq json)
    with
    | e ->
        printfn "Error reading compare.json: %s" e.Message
        None

let makeHttpRequest (url: string) : string option =
    try
        let request = WebRequest.Create(url) :?> HttpWebRequest
        request.Method <- "GET"
        request.Timeout <- 5000
        
        use response = request.GetResponse() :?> HttpWebResponse
        if response.StatusCode = HttpStatusCode.OK then
            use reader = new StreamReader(response.GetResponseStream())
            Some (reader.ReadToEnd())
        else
            None
    with
    | _ -> None

let testWebServer () =
    let baseUrl = "http://localhost:3000"
    let expectedHashes = 
        match loadCompareJson "../webserver/compare.json" with
        | Some hashes -> hashes
        | None -> Map.empty
    
    if expectedHashes.IsEmpty then
        printfn "Could not load expected hashes"
        ()
    else
        printfn "Testing 100 endpoints..."
        
        let mutable passed = 0
        let mutable failed = 0
        
        let results = 
            [0..99]
            |> List.map (fun i ->
                let endpoint = sprintf "test-%d" i
                let url = sprintf "%s/%s" baseUrl endpoint
                
                let response = makeHttpRequest url
                
                let expectedHash = Map.tryFind endpoint expectedHash |> Option.defaultValue ""
                
                let result = {
                    endpoint = endpoint
                    url = url
                    serverHash = None
                    expectedHash = expectedHash
                    status = ""
                    error = None
                }
                
                match response with
                | Some resp ->
                    try
                        let data = JsonConvert.DeserializeObject<Dictionary<string, string>>(resp)
                        let serverHash = 
                            data.TryGetValue("hash")
                            |> function
                            | true, value -> value
                            | false, _ -> ""
                        
                        let finalResult = { result with 
                            serverHash = Some serverHash
                            status = if serverHash = expectedHash then "PASSED" else "FAILED"
                        }
                        
                        if finalResult.status = "PASSED" then
                            passed <- passed + 1
                        else
                            failed <- failed + 1
                            
                        finalResult
                    with
                    | _ ->
                        failed <- failed + 1
                        { result with status = "FAILED"; error = Some "JSON parse error" }
                | None ->
                    failed <- failed + 1
                    { result with status = "FAILED"; error = Some "HTTP request failed" }
            )
        
        // Progress indicator
        [10; 20; 30; 40; 50; 60; 70; 80; 90; 100]
        |> List.iter (fun i -> printfn "Tested %d/100 endpoints..." i)
        
        // Create final result
        let totalTests = List.length results
        let successRate = (float passed / float totalTests * 100.0)
        
        let finalResult = {
            totalTests = totalTests
            passed = passed
            failed = failed
            successRate = sprintf "%.1f%%" successRate
            timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
            results = results
        }
        
        // Save results to file
        try
            let output = JsonConvert.SerializeObject(finalResult, Formatting.Indented)
            File.WriteAllText("test-result.json", output)
            printfn "Results saved to: test-result.json"
        with
        | e -> printfn "Error saving results: %s" e.Message
        
        printfn "\nTest completed!"
        printfn "Passed: %d/%d (%.1f%%)" passed totalTests successRate
        printfn "Failed: %d/%d (%.1f%%)" failed totalTests (100.0 - successRate)

[<EntryPoint>]
let main argv =
    hello()
    testWebServer()
    0
