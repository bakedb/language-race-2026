#!/bin/bash

# Hello, World!
echo "Hello, World!"

HASHFILES_DIR="../hashfiles"
OUTPUT_FILE="hash_results.json"

# Check if directory exists
if [ ! -d "$HASHFILES_DIR" ]; then
    echo "Directory not found: $HASHFILES_DIR"
    exit 1
fi

# Get all .txt files and sort them
files=($(find "$HASHFILES_DIR" -name "*.txt" -type f | sort))

if [ ${#files[@]} -eq 0 ]; then
    echo "No .txt files found in directory: $HASHFILES_DIR"
    exit 1
fi

echo "Found ${#files[@]} files to hash"

# Initialize counters
total_files=${#files[@]}
successful_hashes=0
failed_hashes=0

# Start timing
start_time=$(date +%s.%N)

# Create temporary file for results
temp_results=$(mktemp)

# Process each file
for i in "${!files[@]}"; do
    filepath="${files[$i]}"
    filename=$(basename "$filepath")
    size=$(stat -c%s "$filepath" 2>/dev/null || echo "0")
    
    # Calculate SHA256 hash
    hash=$(sha256sum "$filepath" 2>/dev/null | cut -d' ' -f1)
    
    # Create result entry
    if [ -n "$hash" ]; then
        status="SUCCESS"
        successful_hashes=$((successful_hashes + 1))
    else
        hash=""
        status="FAILED"
        error="Hash calculation failed"
        failed_hashes=$((failed_hashes + 1))
    fi
    
    # Create JSON entry for this file
    entry="{\"filename\":\"$filename\",\"filepath\":\"$filepath\",\"size\":$size,\"algorithm\":\"sha256\",\"hash\":\"$hash\",\"status\":\"$status\""
    
    if [ "$status" = "FAILED" ]; then
        entry="$entry,\"error\":\"$error\""
    fi
    
    entry="$entry}"
    
    echo "$entry" >> "$temp_results"
    
    # Progress indicator
    if [ $((i + 1)) -eq 100 ] || [ $((i + 1)) -eq 200 ] || [ $((i + 1)) -eq 300 ] || [ $((i + 1)) -eq 400 ] || [ $((i + 1)) -eq 500 ] || [ $((i + 1)) -eq 600 ] || [ $((i + 1)) -eq 700 ] || [ $((i + 1)) -eq 800 ] || [ $((i + 1)) -eq 900 ] || [ $((i + 1)) -eq 1000 ]; then
        echo "Processed $((i + 1))/$total_files files..."
    fi
done

# End timing
end_time=$(date +%s.%N)
processing_time=$(echo "$end_time - $start_time" | bc -l)
average_time_per_file=$(echo "scale=2; $processing_time * 1000 / $total_files" | bc -l)

# Get timestamp
timestamp=$(date '+%Y-%m-%d %H:%M:%S')

# Read all results into array
readarray -t results_array < "$temp_results"

# Create JSON array
results_json=""
for result in "${results_array[@]}"; do
    if [ -z "$results_json" ]; then
        results_json="[$result"
    else
        results_json="$results_json,$result"
    fi
done
results_json="$results_json]"

# Create final JSON
success_rate=$(echo "scale=1; $successful_hashes * 100 / $total_files" | bc -l)
final_json="{\"total_files\":$total_files,\"successful_hashes\":$successful_hashes,\"failed_hashes\":$failed_hashes,\"algorithm\":\"sha256\",\"processing_time_seconds\":$processing_time,\"average_time_per_file_ms\":$average_time_per_file,\"timestamp\":\"$timestamp\",\"directory\":\"$HASHFILES_DIR\",\"results\":$results_json}"

# Save results to file
echo "$final_json" > "$OUTPUT_FILE"

# Cleanup
rm "$temp_results"

echo "Results saved to: $OUTPUT_FILE"
echo ""
echo "Hashing completed!"
echo "Total files: $total_files"
echo "Successful: $successful_hashes"
echo "Failed: $failed_hashes"
echo "Processing time: $(echo "scale=3; $processing_time" | bc -l) seconds"
echo "Average time per file: $average_time_per_file ms"
