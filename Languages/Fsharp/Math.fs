open System
open System.IO
open System.Text.RegularExpressions

// Hello, World!
let hello () = printfn "Hello, World!"

let evaluateExpression (expr: string) =
    let trimmedExpr = expr.Trim()
    
    // Use regex to parse simple expressions
    let pattern = @"([\d.]+)\s*([+\-*/])\s*([\d.]+)"
    let match = Regex.Match(trimmedExpr, pattern)
    
    if match.Success then
        let a = float match.Groups.[1].Value
        let op = match.Groups.[2].Value.[0]
        let b = float match.Groups.[3].Value
        
        match op with
        | '+' -> a + b
        | '-' -> a - b
        | '*' -> a * b
        | '/' -> if b <> 0.0 then a / b else 0.0
        | _ -> 0.0
    else
        0.0

let solveEquation (line: string) =
    let mutable equation = line
    
    // Remove "= ?" part
    let equalsPos = equation.IndexOf('=')
    if equalsPos <> -1 then
        equation <- equation.Substring(0, equalsPos)
    
    // Trim whitespace
    equation <- equation.Trim()
    
    let result = evaluateExpression equation
    printfn "%s = %.2f" equation result

let processFile (filename: string) =
    try
        if File.Exists(filename) then
            let lines = File.ReadAllLines(filename)
            
            for line in lines do
                // Skip empty lines and markdown headers
                if not (String.IsNullOrWhiteSpace(line) || line.StartsWith("#")) then
                    // Handle markdown list items
                    let start = 
                        if line.StartsWith("- ") then
                            line.Substring(2)
                        else
                            line
                    
                    if start.Contains("=") then
                        solveEquation start
        else
            printfn "Could not open file: %s" filename
    with
    | ex -> printfn "Error processing file: %s" filename

[<EntryPoint>]
let main argv =
    hello()
    
    printfn "\nProcessing math equations..."
    
    processFile "../test_data/math_equations.txt"
    processFile "../test_data/math_equations.md"
    processFile "../test_data/math_equations.json"
    processFile "../test_data/math_equations.yaml"
    processFile "../test_data/math_equations"
    
    0
