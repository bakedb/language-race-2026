import std.stdio;
import std.json;
import std.net.curl;
import std.conv;
import std.format;
import std.datetime;

// Hello, World!
void hello() {
    writeln("Hello, World!");
}

struct TestResult {
    string endpoint;
    string url;
    string server_hash;
    string expected_hash;
    string status;
    string error;
}

struct FinalResult {
    int total_tests;
    int passed;
    int failed;
    string success_rate;
    string timestamp;
    TestResult[] results;
}

JSONValue loadCompareJSON(string filename) {
    try {
        string content = cast(string)std.file.read(filename);
        return parseJSON(content);
    } catch (Exception e) {
        writeln("Error: Could not read ", filename);
        return JSONValue();
    }
}

string makeHTTPRequest(string url) {
    try {
        auto client = HTTP();
        client.timeout = dur!("seconds")(5);
        auto response = client.get(url);
        if (response.status == 200) {
            return response.responseBody;
        }
    } catch (Exception e) {
        // Handle exception
    }
    return "";
}

void testWebServer() {
    string baseUrl = "http://localhost:3000";
    string compareFile = "../webserver/compare.json";
    string outputFile = "test-result.json";
    
    // Load expected hashes
    auto expectedHashes = loadCompareJSON(compareFile);
    if (expectedHashes.type == JSONType.null) {
        return;
    }
    
    writeln("Testing 100 endpoints...");
    
    TestResult[] results;
    int passed = 0;
    int failed = 0;
    
    // Test each endpoint
    for (int i = 0; i < 100; i++) {
        string endpoint = format("test-%d", i);
        string url = baseUrl ~ "/" ~ endpoint;
        
        string response = makeHTTPRequest(url);
        
        TestResult result;
        result.endpoint = endpoint;
        result.url = url;
        result.expected_hash = expectedHashes[endpoint].str;
        
        if (response.length == 0) {
            result.status = "FAILED";
            result.error = "HTTP request failed";
            failed++;
        } else {
            try {
                auto data = parseJSON(response);
                string serverHash = data["hash"].str;
                result.server_hash = serverHash;
                
                if (serverHash == result.expected_hash) {
                    result.status = "PASSED";
                    passed++;
                } else {
                    result.status = "FAILED";
                    failed++;
                }
            } catch (Exception e) {
                result.status = "FAILED";
                result.error = "JSON parse error";
                failed++;
            }
        }
        
        results ~= result;
        
        // Progress indicator
        if ((i + 1) % 10 == 0) {
            writeln("Tested ", i + 1, "/100 endpoints...");
        }
    }
    
    // Create final result
    int totalTests = cast(int)results.length;
    double successRate = (cast(double)passed / totalTests) * 100.0;
    
    FinalResult finalResult;
    finalResult.total_tests = totalTests;
    finalResult.passed = passed;
    finalResult.failed = failed;
    finalResult.success_rate = format("%.1f%%", successRate);
    finalResult.timestamp = Clock.currTime().toISOExtString();
    finalResult.results = results;
    
    // Save results to file
    try {
        auto file = File(outputFile, "w");
        file.writeln(toJSON(&finalResult).toPrettyString());
        file.close();
        writeln("Results saved to: ", outputFile);
    } catch (Exception e) {
        writeln("Error saving results: ", e.msg);
    }
    
    writeln("\nTest completed!");
    writeln("Passed: ", passed, "/", totalTests, " (", format("%.1f%%", successRate), ")");
    writeln("Failed: ", failed, "/", totalTests, " (", format("%.1f%%", 100.0 - successRate), ")");
}

void main() {
    hello();
    testWebServer();
}
