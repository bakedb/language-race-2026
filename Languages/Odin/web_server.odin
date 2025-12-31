package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:json"
import "core:net/http"
import "core:time"

// Hello, World!
hello :: proc() {
    fmt.println("Hello, World!")
}

Test_Result :: struct {
    endpoint:    string,
    url:         string,
    server_hash: string,
    expected_hash: string,
    status:      string,
    error:       string,
}

Final_Result :: struct {
    total_tests: int,
    passed:      int,
    failed:      int,
    success_rate: string,
    timestamp:   string,
    results:     [dynamic]Test_Result,
}

load_compare_json :: proc(filename: string) -> (map[string]string, bool) {
    data, ok := os.read_file(filename)
    if !ok {
        fmt.printf("Error: Could not read %s\n", filename)
        return {}, false
    }
    
    value, err := json.parse(data)
    if err != nil {
        fmt.printf("Error parsing JSON: %s\n", err)
        return {}, false
    }
    
    if value.kind != .Object {
        fmt.printf("Invalid JSON format\n")
        return {}, false
    }
    
    result := make(map[string]string)
    for key, val in value.object {
        if val.kind == .String {
            result[key] = val.string
        }
    }
    
    return result, true
}

make_http_request :: proc(url: string) -> string {
    client: http.Client
    client.init()
    defer client.destroy()
    
    headers: [dynamic]http.Header
    defer delete(headers)
    
    response, status_code, err := client.get(url, headers[:])
    defer delete(response)
    
    if err != nil || status_code != 200 {
        return ""
    }
    
    return string(response)
}

test_web_server :: proc() {
    base_url := "http://localhost:3000"
    compare_file := "../webserver/compare.json"
    output_file := "test-result.json"
    
    // Load expected hashes
    expected_hashes, ok := load_compare_json(compare_file)
    if !ok {
        return
    }
    
    fmt.println("Testing 100 endpoints...")
    
    results: [dynamic]Test_Result
    defer delete(results)
    
    passed := 0
    failed := 0
    
    // Test each endpoint
    for i in 0..<100 {
        endpoint := strings.concatenate("test-", itoa(i))
        url := strings.concatenate(base_url, "/", endpoint)
        
        response := make_http_request(url)
        
        result: Test_Result
        result.endpoint = endpoint
        result.url = url
        result.expected_hash = expected_hashes[endpoint] if expected_hashes[endpoint] != nil else ""
        
        if len(response) == 0 {
            result.status = "FAILED"
            result.error = "HTTP request failed"
            failed += 1
        } else {
            value, err := json.parse(response)
            if err != nil || value.kind != .Object {
                result.status = "FAILED"
                result.error = "JSON parse error"
                failed += 1
            } else {
                hash_val := value.object["hash"]
                if hash_val != nil && hash_val.kind == .String {
                    result.server_hash = hash_val.string
                    
                    if result.server_hash == result.expected_hash {
                        result.status = "PASSED"
                        passed += 1
                    } else {
                        result.status = "FAILED"
                        failed += 1
                    }
                } else {
                    result.status = "FAILED"
                    result.error = "Invalid hash format"
                    failed += 1
                }
            }
        }
        
        append(&results, result)
        
        // Progress indicator
        if (i + 1) % 10 == 0 {
            fmt.printf("Tested %d/100 endpoints...\n", i + 1)
        }
    }
    
    // Create final result
    total_tests := len(results)
    success_rate := f64(passed) / f64(total_tests) * 100.0
    
    final_result: Final_Result
    final_result.total_tests = total_tests
    final_result.passed = passed
    final_result.failed = failed
    final_result.success_rate = fmt.tprintf("%.1f%%", success_rate)
    final_result.timestamp = fmt.tprintf("%s", time.now())
    final_result.results = results
    
    // Save results to file (simplified - would need proper JSON serialization)
    fmt.printf("Results would be saved to: %s\n", output_file)
    fmt.printf("\nTest completed!\n")
    fmt.printf("Passed: %d/%d (%.1f%%)\n", passed, total_tests, success_rate)
    fmt.printf("Failed: %d/%d (%.1f%%)\n", failed, total_tests, 100.0 - success_rate)
}

main :: proc() {
    hello()
    test_web_server()
}
