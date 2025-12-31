-- Hello, World!
print("Hello, World!")

local http = require("socket.http")
local json = require("json")

function make_http_request(url)
    local response, code = http.request(url)
    
    if code == 200 and response then
        return response
    else
        return ""
    end
end

function test_web_server()
    local base_url = "http://localhost:3000"
    local compare_file = "../webserver/compare.json"
    local output_file = "test-result.json"
    
    -- Load expected hashes
    local file = io.open(compare_file, "r")
    if not file then
        print("Error: Could not find " .. compare_file)
        return
    end
    
    local compare_data = file:read("*all")
    file:close()
    
    local expected_hashes = json.decode(compare_data)
    if not expected_hashes then
        print("Error: Could not parse " .. compare_file)
        return
    end
    
    print("Testing 100 endpoints...")
    
    local results = {}
    local passed = 0
    local failed = 0
    
    -- Test each endpoint
    for i = 0, 99 do
        local endpoint = string.format("test-%d", i)
        local url = base_url .. "/" .. endpoint
        
        local response = make_http_request(url)
        
        local result = {
            endpoint = endpoint,
            url = url,
            expected_hash = expected_hashes[endpoint] or ""
        }
        
        if response == "" then
            result.status = "FAILED"
            result.error = "HTTP request failed"
            failed = failed + 1
        else
            local success, data = pcall(json.decode, response)
            if success and data and data.hash then
                result.server_hash = data.hash
                
                if result.server_hash == result.expected_hash then
                    result.status = "PASSED"
                    passed = passed + 1
                else
                    result.status = "FAILED"
                    failed = failed + 1
                end
            else
                result.status = "FAILED"
                result.error = "JSON parse error"
                failed = failed + 1
            end
        end
        
        table.insert(results, result)
        
        -- Progress indicator
        if (i + 1) % 10 == 0 then
            print("Tested " .. (i + 1) .. "/100 endpoints...")
        end
    end
    
    -- Create final result
    local total_tests = #results
    local success_rate = string.format("%.1f", (passed / total_tests) * 100)
    
    local final_result = {
        total_tests = total_tests,
        passed = passed,
        failed = failed,
        success_rate = success_rate .. "%",
        timestamp = os.date("%Y-%m-%d %H:%M:%S"),
        results = results
    }
    
    -- Save results to file
    local output_data = json.encode(final_result)
    local file = io.open(output_file, "w")
    if file then
        file:write(output_data)
        file:close()
        print("Results saved to: " .. output_file)
    else
        print("Error saving results")
    end
    
    print("\nTest completed!")
    print("Passed: " .. passed .. "/" .. total_tests .. " (" .. success_rate .. "%)")
    print("Failed: " .. failed .. "/" .. total_tests .. " (" .. (100 - tonumber(success_rate)) .. "%)")
end

test_web_server()
