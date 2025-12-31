#!/bin/bash

# Hello, World!
echo "Hello, World!"

# Create output directory
output_dir="../rand_avg output"
mkdir -p "$output_dir"
output_file="$output_dir/random_numbers.txt"

# Generate 1000 random numbers
sum=0
count=1000

for ((i=1; i<=count; i++)); do
    num=$((RANDOM % 1000))
    echo "$num" >> "$output_file"
    sum=$((sum + num))
done

# Calculate mean
mean=$(echo "scale=2; $sum / $count" | bc)

echo "Generated 1000 random numbers"
echo "Mean: $mean"
echo "Saved to: $output_file"
