#!/bin/bash

# Hello, World!
echo "Hello, World!"

echo ""
echo "Processing math equations..."

# Function to evaluate simple math expressions
evaluate_expression() {
    local expr="$1"
    # Remove "= ?" part
    expr="${expr%%=*}"
    expr="${expr// /}"
    
    # Use bc for floating point arithmetic
    local result
    result=$(echo "$expr" | bc -l 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo "$expr = $result"
    else
        echo "$expr = Error"
    fi
}

# Function to process a file
process_file() {
    local file="$1"
    if [ ! -f "$file" ]; then
        echo "Could not open file: $file"
        return
    fi
    
    while IFS= read -r line; do
        # Skip empty lines and markdown headers
        [[ -z "$line" || "$line" == \#* ]] && continue
        
        # Handle markdown list items
        if [[ "$line" == "- "* ]]; then
            line="${line#- }"
        fi
        
        # Process lines that contain equations
        if [[ "$line" == *"="* ]]; then
            evaluate_expression "$line"
        fi
    done < "$file"
}

# Process all files
process_file "../test_data/math_equations.txt"
process_file "../test_data/math_equations.md"
process_file "../test_data/math_equations.json"
process_file "../test_data/math_equations.yaml"
process_file "../test_data/math_equations"
