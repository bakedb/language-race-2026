open System
open System.IO
open System.Collections.Generic

// Hello, World!
let hello () = printfn "Hello, World!"

let generateRandomNumbers () =
    let count = 1000
    let outputDir = "../rand_avg output"
    let outputFile = outputDir + "/random_numbers.txt"
    
    // Create output directory
    if not (Directory.Exists(outputDir)) then
        Directory.CreateDirectory(outputDir) |> ignore
    
    // Random number generator
    let random = Random()
    
    // Generate 1000 random numbers
    let randomNumbers = List<int>()
    let mutable sum = 0L
    
    for i in 1..count do
        let num = random.Next(0, 1000)
        randomNumbers.Add(num)
        sum <- sum + int64 num
    
    // Calculate mean
    let mean = float sum / float count
    
    // Save to file
    try
        use writer = new StreamWriter(outputFile)
        for num in randomNumbers do
            writer.WriteLine(num)
        
        printfn "Generated 1000 random numbers"
        printfn "Mean: %.2f" mean
        printfn "Saved to: %s" outputFile
    with
    | ex -> printfn "Could not create output file"

[<EntryPoint>]
let main argv =
    hello()
    generateRandomNumbers()
    0
