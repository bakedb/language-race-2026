import java.io.File

// Hello, World!
fun hello() {
    println("Hello, World!")
}

fun evaluateExpression(expr: String): Double {
    val trimmedExpr = expr.trim()
    
    // Use regex to parse simple expressions
    val pattern = """([\\d.]+)\\s*([+\\-*/])\\s*([\\d.]+)""".toRegex()
    val match = pattern.find(trimmedExpr)
    
    if (match != null) {
        val a = match.groupValues[1].toDouble()
        val op = match.groupValues[2][0]
        val b = match.groupValues[3].toDouble()
        
        return when (op) {
            '+' -> a + b
            '-' -> a - b
            '*' -> a * b
            '/' -> if (b != 0.0) a / b else 0.0
            else -> 0.0
        }
    }
    
    return 0.0
}

fun solveEquation(line: String) {
    var equation = line
    
    // Remove "= ?" part
    val equalsPos = equation.indexOf('=')
    if (equalsPos != -1) {
        equation = equation.substring(0, equalsPos)
    }
    
    // Trim whitespace
    equation = equation.trim()
    
    val result = evaluateExpression(equation)
    println("$equation = ${String.format("%.2f", result)}")
}

fun processFile(filename: String) {
    try {
        val file = File(filename)
        if (!file.exists()) {
            println("Could not open file: $filename")
            return
        }
        
        file.forEachLine { line ->
            // Skip empty lines and markdown headers
            if (line.isBlank() || line.startsWith("#")) return@forEachLine
            
            // Handle markdown list items
            val start = if (line.startsWith("- ")) {
                line.substring(2)
            } else {
                line
            }
            
            if (start.contains("=")) {
                solveEquation(start)
            }
        }
    } catch (e: Exception) {
        println("Error processing file: $filename")
    }
}

fun main() {
    hello()
    
    println("\nProcessing math equations...")
    
    processFile("../test_data/math_equations.txt")
    processFile("../test_data/math_equations.md")
    processFile("../test_data/math_equations.json")
    processFile("../test_data/math_equations.yaml")
    processFile("../test_data/math_equations")
}
