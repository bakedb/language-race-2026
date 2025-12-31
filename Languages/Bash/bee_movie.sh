#!/bin/bash

# Hello, World!
echo "Hello, World!"

script_path="../beemoviescript.txt"

if [ ! -f "$script_path" ]; then
    echo "File not found: $script_path"
    exit 1
fi

echo "Bee Movie Script:"
echo "--------------------------------------------------"

# Initialize letter counts
declare -A letter_counts
total_letters=0

# Read file line by line, print each line, and count letters
while IFS= read -r line; do
    echo "$line"
    
    # Count letters in the line
    for (( i=0; i<${#line}; i++ )); do
        char="${line:$i:1}"
        if [[ "$char" =~ [a-zA-Z] ]]; then
            lower="${char,,}"
            ((letter_counts[$lower]++))
            ((total_letters++))
        fi
    done
done < "$script_path"

echo "--------------------------------------------------"
echo "Analysis complete."

if [ $total_letters -gt 0 ]; then
    echo ""
    echo "Top 3 most commonly used letters:"
    
    # Create temporary file for sorting
    temp_file=$(mktemp)
    for letter in "${!letter_counts[@]}"; do
        echo "${letter_counts[$letter]} $letter" >> "$temp_file"
    done
    
    # Sort by count (descending) and take top 3
    sort -nr "$temp_file" | head -3 | while read count letter; do
        echo "'$letter': $count times"
    done
    
    rm "$temp_file"
else
    echo "No letters found in the script."
fi
