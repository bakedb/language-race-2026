import std.stdio;
import std.string;
import std.algorithm;
import std.conv;
import std.file;
import std.path;

// Hello, World!
void hello() {
    writeln("Hello, World!");
}

double evaluateExpression(string expr) {
    expr = expr.strip();
    
    // Split by spaces
    auto parts = expr.split();
    if (parts.length < 3) return 0.0;
    
    double a = to!double(parts[0]);
    string op = parts[1];
    double b = to!double(parts[2]);
    
    switch (op) {
        case "+": return a + b;
        case "-": return a - b;
        case "*": return a * b;
        case "/": return b != 0 ? a / b : 0.0;
        default: return 0.0;
    }
}

void solveEquation(string line) {
    string equation = line;
    
    // Remove "= ?" part
    auto equalsPos = equation.indexOf('=');
    if (equalsPos != -1) {
        equation = equation[0..equalsPos];
    }
    
    // Trim whitespace
    equation = equation.strip();
    
    double result = evaluateExpression(equation);
    writefln("%s = %.2f", equation, result);
}

void processFile(string filename) {
    if (!exists(filename)) {
        writeln("Could not open file: ", filename);
        return;
    }
    
    auto content = read(filename);
    auto lines = content.splitLines();
    
    foreach (line; lines) {
        // Skip empty lines and markdown headers
        if (line.strip().length == 0 || line.startsWith("#")) continue;
        
        // Handle markdown list items
        string start = line;
        if (line.startsWith("- ")) {
            start = line[2..$];
        }
        
        if (start.canFind("=")) {
            solveEquation(start);
        }
    }
}

void main() {
    hello();
    
    writeln("\nProcessing math equations...");
    
    processFile("../test_data/math_equations.txt");
    processFile("../test_data/math_equations.md");
    processFile("../test_data/math_equations.json");
    processFile("../test_data/math_equations.yaml");
    processFile("../test_data/math_equations");
}
