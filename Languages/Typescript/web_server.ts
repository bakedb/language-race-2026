// Hello, World!
console.log("Hello, World!");

import * as fs from 'fs';
import * as http from 'http';

interface TestResult {
    endpoint: string;
    url: string;
    server_hash?: string;
    expected_hash: string;
    status: string;
    error?: string;
}

interface FinalResult {
    total_tests: number;
    passed: number;
    failed: number;
    success_rate: string;
    timestamp: string;
    results: TestResult[];
}

function makeHttpRequest(url: string): Promise<string> {
    return new Promise((resolve) => {
        const urlObj = new URL(url);
        const options = {
            hostname: urlObj.hostname,
            port: urlObj.port || 80,
            path: urlObj.pathname,
            method: 'GET',
            timeout: 5000
        };
        
        const req = http.request(options, (res) => {
            let data = '';
            
            res.on('data', (chunk) => {
                data += chunk;
            });
            
            res.on('end', () => {
                if (res.statusCode === 200) {
                    resolve(data);
                } else {
                    resolve('');
                }
            });
        });
        
        req.on('error', () => {
            resolve('');
        });
        
        req.on('timeout', () => {
            req.destroy();
            resolve('');
        });
        
        req.end();
    });
}

async function testWebServer(): Promise<void> {
    const baseUrl = "http://localhost:3000";
    const compareFile = "../webserver/compare.json";
    const outputFile = "test-result.json";
    
    // Load expected hashes
    let expectedHashes: { [key: string]: string };
    try {
        const compareData = fs.readFileSync(compareFile, 'utf8');
        expectedHashes = JSON.parse(compareData);
    } catch (error) {
        console.log(`Error: Could not read ${compareFile}`);
        return;
    }
    
    console.log("Testing 100 endpoints...");
    
    const results: TestResult[] = [];
    let passed = 0;
    let failed = 0;
    
    // Test each endpoint
    for (let i = 0; i < 100; i++) {
        const endpoint = `test-${i}`;
        const url = `${baseUrl}/${endpoint}`;
        
        const response = await makeHttpRequest(url);
        
        const result: TestResult = {
            endpoint: endpoint,
            url: url,
            expected_hash: expectedHashes[endpoint] || '',
            status: ''
        };
        
        if (response) {
            try {
                const data = JSON.parse(response);
                result.server_hash = data.hash || '';
                
                if (result.server_hash === result.expected_hash) {
                    result.status = "PASSED";
                    passed++;
                } else {
                    result.status = "FAILED";
                    failed++;
                }
            } catch (error) {
                result.status = "FAILED";
                result.error = "JSON parse error";
                failed++;
            }
        } else {
            result.status = "FAILED";
            result.error = "HTTP request failed";
            failed++;
        }
        
        results.push(result);
        
        // Progress indicator
        if ((i + 1) % 10 === 0) {
            console.log(`Tested ${i + 1}/100 endpoints...`);
        }
    }
    
    // Create final result
    const finalResult: FinalResult = {
        total_tests: results.length,
        passed: passed,
        failed: failed,
        success_rate: `${(passed / results.length * 100).toFixed(1)}%`,
        timestamp: new Date().toISOString(),
        results: results
    };
    
    // Save results to file
    try {
        fs.writeFileSync(outputFile, JSON.stringify(finalResult, null, 2));
        console.log(`Results saved to: ${outputFile}`);
    } catch (error) {
        console.log(`Error saving results: ${error}`);
    }
    
    console.log("\nTest completed!");
    console.log(`Passed: ${passed}/${results.length} (${(passed / results.length * 100).toFixed(1)}%)`);
    console.log(`Failed: ${failed}/${results.length} (${(failed / results.length * 100).toFixed(1)}%)`);
}

testWebServer();
