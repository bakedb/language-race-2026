import Foundation

// Hello, World!
print("Hello, World!")

print("\nProcessing math equations...")

func evaluateExpression(_ expr: String) -> Double {
    let trimmedExpr = expr.trimmingCharacters(in: .whitespaces)
    
    // Use NSExpression to evaluate mathematical expressions
    let nsExpression = NSExpression(format: trimmedExpr)
    return nsExpression.expressionValue(with: nil, context: nil) as? Double ?? 0.0
}

func solveEquation(_ line: String) {
    var equation = line
    
    // Remove "= ?" part
    if let equalsRange = equation.range(of: "=") {
        equation = String(equation[..<equalsRange.lowerBound])
    }
    
    // Trim whitespace
    equation = equation.trimmingCharacters(in: .whitespaces)
    
    let result = evaluateExpression(equation)
    print("\(equation) = \(String(format: "%.2f", result))")
}

func processFile(_ filename: String) {
    guard let content = try? String(contentsOfFile: filename) else {
        print("Could not open file: \(filename)")
        return
    }
    
    let lines = content.components(separatedBy: .newlines)
    
    for line in lines {
        // Skip empty lines and markdown headers
        if line.isEmpty || line.hasPrefix("#") {
            continue
        }
        
        // Handle markdown list items
        let start: String
        if line.hasPrefix("- ") {
            start = String(line.dropFirst(2))
        } else {
            start = line
        }
        
        if start.contains("=") {
            solveEquation(start)
        }
    }
}

// Process all files
processFile("../test_data/math_equations.txt")
processFile("../test_data/math_equations.md")
processFile("../test_data/math_equations.json")
processFile("../test_data/math_equations.yaml")
processFile("../test_data/math_equations")
