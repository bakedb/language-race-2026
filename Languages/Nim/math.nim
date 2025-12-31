# Hello, World!
echo "Hello, World!"

echo ""
echo "Processing math equations..."

import os, strutils

proc evaluateExpression(expr: string): float =
  var expr = expr.strip()
  
  # Simple expression evaluator for basic operations
  var parts = expr.split()
  if parts.len < 3:
    return 0.0
  
  let a = parseFloat(parts[0])
  let op = parts[1]
  let b = parseFloat(parts[2])
  
  case op:
  of "+": return a + b
  of "-": return a - b
  of "*": return a * b
  of "/": 
    if b != 0.0: return a / b
    else: return 0.0
  else: return 0.0

proc solveEquation(line: string) =
  var equation = line
  
  # Remove "= ?" part
  let equalsPos = equation.find('=')
  if equalsPos != -1:
    equation = equation[0..<equalsPos]
  
  # Trim whitespace
  equation = equation.strip()
  
  let result = evaluateExpression(equation)
  echo equation & " = " & result.formatFloat(ffDecimal, 2)

proc processFile(filename: string) =
  if not fileExists(filename):
    echo "Could not open file: " & filename
    return
  
  for line in filename.lines():
    # Skip empty lines and markdown headers
    if line.strip().len == 0 or line.startsWith("#"):
      continue
    
    # Handle markdown list items
    var start = line
    if line.startsWith("- "):
      start = line[2..^1]
    
    if start.contains('='):
      solveEquation(start)

# Process all files
processFile("../test_data/math_equations.txt")
processFile("../test_data/math_equations.md")
processFile("../test_data/math_equations.json")
processFile("../test_data/math_equations.yaml")
processFile("../test_data/math_equations")
