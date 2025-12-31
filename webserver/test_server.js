const http = require('http');

// Test function to check server endpoints
function testServer() {
    const baseUrl = 'http://localhost:3000';
    
    console.log('Testing Language Race Web Server...');
    console.log('=====================================');
    
    // Test a few endpoints
    const testEndpoints = [0, 25, 50, 99];
    
    testEndpoints.forEach(index => {
        const url = `${baseUrl}/test-${index}`;
        
        http.get(url, (res) => {
            let data = '';
            
            res.on('data', (chunk) => {
                data += chunk;
            });
            
            res.on('end', () => {
                try {
                    const result = JSON.parse(data);
                    console.log(`✓ test-${index}: ${result.hash}`);
                } catch (e) {
                    console.log(`✗ test-${index}: Invalid JSON response`);
                }
            });
        }).on('error', (err) => {
            console.log(`✗ test-${index}: ${err.message}`);
        });
    });
    
    // Test root endpoint
    http.get(baseUrl, (res) => {
        console.log(`✓ Root endpoint: ${res.statusCode}`);
    }).on('error', (err) => {
        console.log(`✗ Root endpoint: ${err.message}`);
    });
    
    console.log('\nTest requests sent. Check results above.');
    console.log('Make sure the server is running with: npm start');
}

// Run tests if this script is executed directly
if (require.main === module) {
    testServer();
}

module.exports = { testServer };
