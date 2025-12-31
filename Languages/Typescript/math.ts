// Hello, World!
console.log("Hello, World!");

import * as fs from 'fs';
import * as path from 'path';

function evaluateExpression(expr: string): number | string {
    // Remove extra whitespace
    expr = expr.trim();
    
    // Use eval to solve the math equation (safe in this controlled environment)
    try {
        return eval(expr);
    } catch (e) {
        return "Error";
    }
}

function solveEquation(line: string): void {
    let equation = line;
    
    // Remove "= ?" part
    const equalsPos = equation.indexOf('=');
    if (equalsPos !== -1) {
        equation = equation.substring(0, equalsPos);
    }
    
    // Trim whitespace
    equation = equation.trim();
    
    const result = evaluateExpression(equation);
    console.log(`${equation} = ${result}`);
}

function processFile(filename: string): void {
    try {
        const content = fs.readFileSync(filename, 'utf8');
        const lines = content.split('\n');
        
        for (const line of lines) {
            // Skip empty lines and markdown headers
            if (line.trim() === '' || line.startsWith('#')) continue;
            
            // Handle markdown list items
            let start = line;
            if (line.startsWith('- ')) {
                start = line.substring(2);
            }
            
            if (start.includes('=')) {
                solveEquation(start);
            }
        }
    } catch (e) {
        console.log(`Could not open file: ${filename}`);
    }
}

function main(): void {
    console.log("\nProcessing math equations...");
    
    processFile("../test_data/math_equations.txt");
    processFile("../test_data/math_equations.md");
    processFile("../test_data/math_equations.json");
    processFile("../test_data/math_equations.yaml");
    processFile("../test_data/math_equations");
}

main();
