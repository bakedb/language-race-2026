import Foundation

// Hello, World!
print("Hello, World!")

struct TestResult: Codable {
    let endpoint: String
    let url: String
    let serverHash: String?
    let expectedHash: String
    let status: String
    let error: String?
    
    enum CodingKeys: String, CodingKey {
        case endpoint, url, status, error
        case serverHash = "server_hash"
        case expectedHash = "expected_hash"
    }
}

struct FinalResult: Codable {
    let totalTests: Int
    let passed: Int
    let failed: Int
    let successRate: String
    let timestamp: String
    let results: [TestResult]
    
    enum CodingKeys: String, CodingKey {
        case totalTests, passed, failed, results
        case successRate = "success_rate"
        case timestamp
    }
}

func makeHttpRequest(url: String) -> String? {
    guard let url = URL(string: url) else { return nil }
    
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    request.timeoutInterval = 5.0
    
    let semaphore = DispatchSemaphore(value: 0)
    var result: String?
    
    URLSession.shared.dataTask(with: request) { data, response, error in
        defer { semaphore.signal() }
        
        if let error = error {
            return
        }
        
        guard let httpResponse = response as? HTTPURLResponse,
              httpResponse.statusCode == 200,
              let data = data else {
            return
        }
        
        result = String(data: data, encoding: .utf8)
    }.resume()
    
    semaphore.wait()
    return result
}

func loadCompareJSON(filename: String) -> [String: String]? {
    guard let data = try? Data(contentsOf: URL(fileURLWithPath: filename)),
          let dict = try? JSONSerialization.jsonObject(with: data) as? [String: String] else {
        return nil
    }
    return dict
}

func testWebServer() {
    let baseUrl = "http://localhost:3000"
    let compareFile = "../webserver/compare.json"
    let outputFile = "test-result.json"
    
    // Load expected hashes
    guard let expectedHashes = loadCompareJSON(filename: compareFile) else {
        print("Error: Could not load \(compareFile)")
        return
    }
    
    print("Testing 100 endpoints...")
    
    var results: [TestResult] = []
    var passed = 0
    var failed = 0
    
    // Test each endpoint
    for i in 0..<100 {
        let endpoint = "test-\(i)"
        let url = "\(baseUrl)/\(endpoint)"
        
        let response = makeHttpRequest(url: url)
        
        var result = TestResult(
            endpoint: endpoint,
            url: url,
            serverHash: nil,
            expectedHash: expectedHashes[endpoint] ?? "",
            status: "",
            error: nil
        )
        
        if let response = response {
            if let responseData = response.data(using: .utf8),
               let json = try? JSONSerialization.jsonObject(with: responseData) as? [String: Any],
               let serverHash = json["hash"] as? String {
                
                result = TestResult(
                    endpoint: endpoint,
                    url: url,
                    serverHash: serverHash,
                    expectedHash: expectedHashes[endpoint] ?? "",
                    status: serverHash == result.expectedHash ? "PASSED" : "FAILED",
                    error: nil
                )
                
                if result.status == "PASSED" {
                    passed += 1
                } else {
                    failed += 1
                }
            } else {
                result = TestResult(
                    endpoint: endpoint,
                    url: url,
                    serverHash: nil,
                    expectedHash: expectedHashes[endpoint] ?? "",
                    status: "FAILED",
                    error: "JSON parse error"
                )
                failed += 1
            }
        } else {
            result = TestResult(
                endpoint: endpoint,
                url: url,
                serverHash: nil,
                expectedHash: expectedHashes[endpoint] ?? "",
                status: "FAILED",
                error: "HTTP request failed"
            )
            failed += 1
        }
        
        results.append(result)
        
        // Progress indicator
        if (i + 1) % 10 == 0 {
            print("Tested \(i + 1)/100 endpoints...")
        }
    }
    
    // Create final result
    let totalTests = results.count
    let successRate = String(format: "%.1f%%", Double(passed) / Double(totalTests) * 100)
    
    let finalResult = FinalResult(
        totalTests: totalTests,
        passed: passed,
        failed: failed,
        successRate: successRate,
        timestamp: DateFormatter().string(from: Date()),
        results: results
    )
    
    // Save results to file
    let encoder = JSONEncoder()
    encoder.outputFormatting = .prettyPrinted
    
    do {
        let data = try encoder.encode(finalResult)
        try data.write(to: URL(fileURLWithPath: outputFile))
        print("Results saved to: \(outputFile)")
    } catch {
        print("Error saving results: \(error)")
    }
    
    print("\nTest completed!")
    print("Passed: \(passed)/\(totalTests) (\(successRate))")
    print("Failed: \(failed)/\(totalTests) (\(String(format: "%.1f%%", Double(failed) / Double(totalTests) * 100)))")
}

testWebServer()
