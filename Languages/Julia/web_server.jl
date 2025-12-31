# Hello, World!
println("Hello, World!")

using HTTP
using JSON

function make_http_request(url)
    try
        response = HTTP.get(url; timeout=5, readtimeout=5)
        if response.status == 200
            return String(response.body)
        else
            return ""
        end
    catch e
        return ""
    end
end

function test_web_server()
    base_url = "http://localhost:3000"
    compare_file = "../webserver/compare.json"
    output_file = "test-result.json"
    
    # Load expected hashes
    if !isfile(compare_file)
        println("Error: Could not find $compare_file")
        return
    end
    
    expected_hashes = JSON.parsefile(compare_file)
    
    println("Testing 100 endpoints...")
    
    results = []
    passed = 0
    failed = 0
    
    # Test each endpoint
    for i in 0:99
        endpoint = "test-$i"
        url = "$base_url/$endpoint"
        
        response = make_http_request(url)
        
        result = Dict(
            "endpoint" => endpoint,
            "url" => url,
            "expected_hash" => get(expected_hashes, endpoint, "")
        )
        
        if response == ""
            result["status"] = "FAILED"
            result["error"] = "HTTP request failed"
            failed += 1
        else
            try
                data = JSON.parse(response)
                server_hash = get(data, "hash", "")
                result["server_hash"] = server_hash
                
                if server_hash == result["expected_hash"]
                    result["status"] = "PASSED"
                    passed += 1
                else
                    result["status"] = "FAILED"
                    failed += 1
                end
            catch e
                result["status"] = "FAILED"
                result["error"] = "JSON parse error"
                failed += 1
            end
        end
        
        push!(results, result)
        
        # Progress indicator
        if (i + 1) % 10 == 0
            println("Tested $(i + 1)/100 endpoints...")
        end
    end
    
    # Create final result
    total_tests = length(results)
    success_rate = round(passed / total_tests * 100, digits=1)
    
    final_result = Dict(
        "total_tests" => total_tests,
        "passed" => passed,
        "failed" => failed,
        "success_rate" => "$(success_rate)%",
        "timestamp" => string(now()),
        "results" => results
    )
    
    # Save results to file
    try
        open(output_file, "w") do f
            JSON.print(f, final_result, 2)
        end
        println("Results saved to: $output_file")
    catch e
        println("Error saving results: $e")
    end
    
    println("\nTest completed!")
    println("Passed: $passed/$total_tests ($(success_rate)%)")
    println("Failed: $failed/$total_tests ($(100 - success_rate)%)")
end

test_web_server()
