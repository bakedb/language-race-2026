#!/bin/bash

# Hello, World!
echo "Hello, World!"

base_url="http://localhost:3000"
compare_file="../webserver/compare.json"
output_file="test-result.json"

# Load expected hashes
if [ ! -f "$compare_file" ]; then
    echo "Error: Could not find $compare_file"
    exit 1
fi

echo "Testing 100 endpoints..."

# Create temporary files for results
temp_results=$(mktemp)
passed=0
failed=0

# Test each endpoint
for i in {0..99}; do
    endpoint="test-$i"
    url="$base_url/$endpoint"
    
    # Make HTTP request using curl
    response=$(curl -s -m 5 "$url" 2>/dev/null)
    
    # Extract hash from response
    server_hash=$(echo "$response" | grep -o '"hash":"[^"]*"' | cut -d'"' -f4)
    
    # Get expected hash from compare.json
    expected_hash=$(grep "\"$endpoint\":" "$compare_file" | cut -d'"' -f4)
    
    # Create result entry
    result="{\"endpoint\":\"$endpoint\",\"url\":\"$url\""
    
    if [ -z "$server_hash" ]; then
        result="$result,\"status\":\"FAILED\",\"error\":\"HTTP request failed\""
        ((failed++))
    else
        result="$result,\"server_hash\":\"$server_hash\",\"expected_hash\":\"$expected_hash\""
        if [ "$server_hash" = "$expected_hash" ]; then
            result="$result,\"status\":\"PASSED\""
            ((passed++))
        else
            result="$result,\"status\":\"FAILED\""
            ((failed++))
        fi
    fi
    
    result="$result}"
    echo "$result" >> "$temp_results"
    
    # Progress indicator
    if [ $((i + 1)) -eq 10 ] || [ $((i + 1)) -eq 20 ] || [ $((i + 1)) -eq 30 ] || [ $((i + 1)) -eq 40 ] || [ $((i + 1)) -eq 50 ] || [ $((i + 1)) -eq 60 ] || [ $((i + 1)) -eq 70 ] || [ $((i + 1)) -eq 80 ] || [ $((i + 1)) -eq 90 ] || [ $((i + 1)) -eq 100 ]; then
        echo "Tested $((i + 1))/100 endpoints..."
    fi
done

# Create final result JSON
total_tests=$((passed + failed))
success_rate=$(echo "scale=1; $passed * 100 / $total_tests" | bc -l)
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
final_json="{\"total_tests\":$total_tests,\"passed\":$passed,\"failed\":$failed,\"success_rate\":\"$success_rate%\",\"timestamp\":\"$timestamp\",\"results\":$results_json}"

# Save results to file
echo "$final_json" > "$output_file"

# Cleanup
rm "$temp_results"

echo "Results saved to: $output_file"
echo ""
echo "Test completed!"
echo "Passed: $passed/$total_tests ($success_rate%)"
echo "Failed: $failed/$total_tests ($(echo "scale=1; 100 - $success_rate" | bc -l)%)"
