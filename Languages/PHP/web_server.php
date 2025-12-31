<?php
// Hello, World!
echo "Hello, World!\n";

function makeHttpRequest($url) {
    $context = stream_context_create([
        'http' => [
            'timeout' => 5,
            'method' => 'GET'
        ]
    ]);
    
    $response = @file_get_contents($url, false, $context);
    
    if ($response === false) {
        return '';
    }
    
    return $response;
}

function testWebServer() {
    $baseUrl = "http://localhost:3000";
    $compareFile = "../webserver/compare.json";
    $outputFile = "test-result.json";
    
    // Load expected hashes
    if (!file_exists($compareFile)) {
        echo "Error: Could not find $compareFile\n";
        return;
    }
    
    $compareData = json_decode(file_get_contents($compareFile), true);
    if (!$compareData) {
        echo "Error: Could not parse $compareFile\n";
        return;
    }
    
    echo "Testing 100 endpoints...\n";
    
    $results = [];
    $passed = 0;
    $failed = 0;
    
    // Test each endpoint
    for ($i = 0; $i < 100; $i++) {
        $endpoint = "test-$i";
        $url = "$baseUrl/$endpoint";
        
        $response = makeHttpRequest($url);
        
        $result = [
            'endpoint' => $endpoint,
            'url' => $url,
            'expected_hash' => $compareData[$endpoint] ?? ''
        ];
        
        if (empty($response)) {
            $result['status'] = "FAILED";
            $result['error'] = "HTTP request failed";
            $failed++;
        } else {
            $data = json_decode($response, true);
            if ($data && isset($data['hash'])) {
                $result['server_hash'] = $data['hash'];
                
                if ($result['server_hash'] === $result['expected_hash']) {
                    $result['status'] = "PASSED";
                    $passed++;
                } else {
                    $result['status'] = "FAILED";
                    $failed++;
                }
            } else {
                $result['status'] = "FAILED";
                $result['error'] = "JSON parse error";
                $failed++;
            }
        }
        
        $results[] = $result;
        
        // Progress indicator
        if (($i + 1) % 10 == 0) {
            echo "Tested " . ($i + 1) . "/100 endpoints...\n";
        }
    }
    
    // Create final result
    $totalTests = count($results);
    $successRate = number_format(($passed / $totalTests) * 100, 1);
    
    $finalResult = [
        'total_tests' => $totalTests,
        'passed' => $passed,
        'failed' => $failed,
        'success_rate' => $successRate . "%",
        'timestamp' => date('Y-m-d H:i:s'),
        'results' => $results
    ];
    
    // Save results to file
    $outputData = json_encode($finalResult, JSON_PRETTY_PRINT);
    if (file_put_contents($outputFile, $outputData)) {
        echo "Results saved to: $outputFile\n";
    } else {
        echo "Error saving results\n";
    }
    
    echo "\nTest completed!\n";
    echo "Passed: $passed/$totalTests ($successRate%)\n";
    echo "Failed: $failed/$totalTests (" . (100 - $successRate) . "%)\n";
}

testWebServer();
?>
