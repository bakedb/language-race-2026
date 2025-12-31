open System
open System.IO
open System.Collections.Generic

type LetterCount = {
    Letter: char
    Count: int
}

// Hello, World!
let hello () = printfn "Hello, World!"

let analyzeBeeMovie () =
    let scriptPath = "../beemoviescript.txt"
    
    if not (File.Exists(scriptPath)) then
        printfn "File not found: %s" scriptPath
    else
        printfn "Bee Movie Script:"
        printfn "--------------------------------------------------"
        
        let letterCounts = Dictionary<char, int>()
        let mutable totalLetters = 0
        
        // Read file line by line and print each line
        let lines = File.ReadAllLines(scriptPath)
        
        // Print each line and count letters
        for line in lines do
            printfn "%s" line
            
            // Count letters
            for char in line do
                if Char.IsLetter(char) then
                    let lower = Char.ToLower(char)
                    match letterCounts.TryGetValue(lower) with
                    | true, count -> letterCounts.[lower] <- count + 1
                    | false, _ -> letterCounts.[lower] <- 1
                    totalLetters <- totalLetters + 1
        
        printfn "--------------------------------------------------"
        printfn "Analysis complete."
        
        if totalLetters > 0 then
            // Convert to list and sort by count (descending)
            let sorted = 
                letterCounts
                |> Seq.map (|KeyValue|)
                |> Seq.map (fun (letter, count) -> { Letter = letter; Count = count })
                |> Seq.sortByDescending (fun lc -> lc.Count)
                |> Seq.take 3
                |> List.ofSeq
            
            printfn "\nTop 3 most commonly used letters:"
            sorted
            |> List.iteri (fun i lc -> 
                printfn "%d. '%c': %d times" (i + 1) lc.Letter lc.Count)
        else
            printfn "No letters found in the script."

[<EntryPoint>]
let main argv =
    hello()
    analyzeBeeMovie()
    0
