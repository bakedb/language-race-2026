<?php
// Hello, World!
echo "Hello, World!\n";

echo "\nProcessing math equations...\n";

function evaluateExpression($expr) {
    // Remove extra whitespace
    $expr = trim($expr);
    
    // Use eval to solve the math equation (safe in this controlled environment)
    try {
        return eval("return $expr;");
    } catch (ParseError $e) {
        return "Error";
    }
}

function solveEquation($line) {
    $equation = $line;
    
    // Remove "= ?" part
    $equalsPos = strpos($equation, '=');
    if ($equalsPos !== false) {
        $equation = substr($equation, 0, $equalsPos);
    }
    
    // Trim whitespace
    $equation = trim($equation);
    
    $result = evaluateExpression($equation);
    echo "$equation = $result\n";
}

function processFile($filename) {
    if (!file_exists($filename)) {
        echo "Could not open file: $filename\n";
        return;
    }
    
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    
    foreach ($lines as $line) {
        // Skip markdown headers
        if (strpos($line, '#') === 0) {
            continue;
        }
        
        // Handle markdown list items
        $start = $line;
        if (strpos($line, '- ') === 0) {
            $start = substr($line, 2);
        }
        
        if (strpos($start, '=') !== false) {
            solveEquation($start);
        }
    }
}

// Process all files
processFile("../test_data/math_equations.txt");
processFile("../test_data/math_equations.md");
processFile("../test_data/math_equations.json");
processFile("../test_data/math_equations.yaml");
processFile("../test_data/math_equations");
?>
