package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Hello, World!
func hello() {
	fmt.Println("Hello, World!")
}

func evaluateExpression(expr string) (float64, error) {
	// Remove extra whitespace
	expr = strings.TrimSpace(expr)
	
	// Split by spaces
	parts := strings.Fields(expr)
	if len(parts) < 3 {
		return 0, fmt.Errorf("invalid expression")
	}
	
	a, err := strconv.ParseFloat(parts[0], 64)
	if err != nil {
		return 0, err
	}
	
	op := parts[1]
	b, err := strconv.ParseFloat(parts[2], 64)
	if err != nil {
		return 0, err
	}
	
	switch op {
	case "+":
		return a + b, nil
	case "-":
		return a - b, nil
	case "*":
		return a * b, nil
	case "/":
		if b == 0 {
			return 0, fmt.Errorf("division by zero")
		}
		return a / b, nil
	default:
		return 0, fmt.Errorf("unknown operator")
	}
}

func solveEquation(line string) {
	equation := line
	
	// Remove "= ?" part
	if equalsPos := strings.Index(equation, "="); equalsPos != -1 {
		equation = equation[:equalsPos]
	}
	
	// Trim whitespace
	equation = strings.TrimSpace(equation)
	
	result, err := evaluateExpression(equation)
	if err != nil {
		fmt.Printf("%s = Error\n", equation)
		return
	}
	
	fmt.Printf("%s = %.2f\n", equation, result)
}

func processFile(filename string) {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Printf("Could not open file: %s\n", filename)
		return
	}
	defer file.Close()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		
		// Skip empty lines and markdown headers
		if len(line) == 0 || strings.HasPrefix(line, "#") {
			continue
		}
		
		// Handle markdown list items
		start := line
		if strings.HasPrefix(line, "- ") {
			start = line[2:]
		}
		
		if strings.Contains(start, "=") {
			solveEquation(start)
		}
	}
	
	if err := scanner.Err(); err != nil {
		fmt.Printf("Error reading file: %s\n", filename)
	}
}

func main() {
	hello()
	
	fmt.Println("\nProcessing math equations...")
	
	processFile("../test_data/math_equations.txt")
	processFile("../test_data/math_equations.md")
	processFile("../test_data/math_equations.json")
	processFile("../test_data/math_equations.yaml")
	processFile("../test_data/math_equations")
}
