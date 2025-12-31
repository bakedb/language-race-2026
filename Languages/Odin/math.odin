package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

// Hello, World!
hello :: proc() {
	fmt.println("Hello, World!")
}

evaluate_expression :: proc(expr: string) -> f64 {
	parts := strings.split(expr, " ")
	if len(parts) < 3 {
		return 0.0
	}
	
	a, _ := strconv.parse_f64(parts[0])
	b, _ := strconv.parse_f64(parts[2])
	op := parts[1]
	
	switch op {
	case "+":
		return a + b
	case "-":
		return a - b
	case "*":
		return a * b
	case "/":
		if b != 0.0 {
			return a / b
		}
		return 0.0
	case:
		return 0.0
	}
}

solve_equation :: proc(line: string) {
	equation := line
	
	// Remove "= ?" part
	if equals_pos := strings.index(equation, "="); equals_pos != -1 {
		equation = equation[:equals_pos]
	}
	
	// Trim whitespace
	equation = strings.trim_space(equation)
	
	result := evaluate_expression(equation)
	fmt.printf("%s = %.2f\n", equation, result)
}

process_file :: proc(filename: string) {
	data, ok := os.read_file(filename)
	if !ok {
		fmt.printf("Could not open file: %s\n", filename)
		return
	}
	
	lines := strings.split(data, "\n")
	for line in lines {
		// Skip empty lines and markdown headers
		if len(strings.trim_space(line)) == 0 || strings.has_prefix(line, "#") {
			continue
		}
		
		// Handle markdown list items
		start := line
		if strings.has_prefix(line, "- ") {
			start = line[2:]
		}
		
		if strings.index(start, "=") != -1 {
			solve_equation(start)
		}
	}
}

main :: proc() {
	hello()
	
	fmt.println("\nProcessing math equations...")
	
	process_file("../test_data/math_equations.txt")
	process_file("../test_data/math_equations.md")
	process_file("../test_data/math_equations.json")
	process_file("../test_data/math_equations.yaml")
	process_file("../test_data/math_equations")
}
