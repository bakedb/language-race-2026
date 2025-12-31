// Hello, World!
console.log("Hello, World!");

const fs = require('fs');
const http = require('http');

function makeHttpRequest(url) {
    return new Promise((resolve, reject) => {
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

async function testWebServer() {
    const baseUrl = "http://localhost:3000";
    const compareFile = "../webserver/compare.json";
    const outputFile = "test-result.json";
    
    // Load expected hashes
    let expectedHashes;
    try {
        const compareData = fs.readFileSync(compareFile, 'utf8');
        expectedHashes = JSON.parse(compareData);
    } catch (error) {
        console.log(`Error: Could not read ${compareFile}`);
        return;
    }
    
    console.log("Testing 100 endpoints...");
    
    const results = [];
    let passed = 0;
    let failed = 0;
    
    // Test each endpoint
    for (let i = 0; i < 100; i++) {
        const endpoint = `test-${i}`;
        const url = `${baseUrl}/${endpoint}`;
        
        const response = await makeHttpRequest(url);
        
        if (response) {
            try {
                const data = JSON.parse(response);
                const serverHash = data.hash || '';
                const expectedHash = expectedHashes[endpoint] || '';
                
                const status = serverHash === expectedHash ? "PASSED" : "FAILED";
                
                if (status === "PASSED") {
                    passed++;
                } else {
                    failed++;
                }
                
                results.push({
                    endpoint: endpoint,
                    url: url,
                    server_hash: serverHash,
                    expected_hash: expectedHash,
                    status: status
                });
            } catch (error) {
                failed++;
                results.push({
                    endpoint: endpoint,
                    url: url,
                    status: "FAILED",
                    error: "JSON parse error"
                });
            }
        } else {
            failed++;
            results.push({
                endpoint: endpoint,
                url: url,
                status: "FAILED",
                error: "HTTP request failed"
            });
        }
        
        // Progress indicator
        if ((i + 1) % 10 === 0) {
            console.log(`Tested ${i + 1}/100 endpoints...`);
        }
    }
    
    // Create final result
    const finalResult = {
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
        console.log(`Error saving results: ${error.message}`);
    }
    
    console.log("\nTest completed!");
    console.log(`Passed: ${passed}/${results.length} (${(passed / results.length * 100).toFixed(1)}%)`);
    console.log(`Failed: ${failed}/${results.length} (${(failed / results.length * 100).toFixed(1)}%)`);
}

testWebServer();
