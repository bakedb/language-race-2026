const http = require('http');
const fs = require('fs');
const path = require('path');

// Generate deterministic test hash based on index
function generateTestHash(index) {
    const chars = '0123456789abcdef';
    let hash = '';
    let seed = index * 12345 + 67890;
    
    for (let i = 0; i < 64; i++) {
        seed = (seed * 1103515245 + 12345) % 2147483647;
        hash += chars[seed % 16];
    }
    
    return hash;
}

// Create HTTP server
const server = http.createServer((req, res) => {
    // Enable CORS for all requests
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
    
    // Handle preflight requests
    if (req.method === 'OPTIONS') {
        res.writeHead(200);
        res.end();
        return;
    }
    
    const url = req.url;
    
    // Handle test endpoints
    if (url.startsWith('/test-')) {
        const match = url.match(/\/test-(\d+)$/);
        
        if (match) {
            const index = parseInt(match[1]);
            
            if (index >= 0 && index <= 99) {
                const hash = generateTestHash(index);
                const response = JSON.stringify({ hash: hash });
                
                res.writeHead(200, {
                    'Content-Type': 'application/json',
                    'Content-Length': Buffer.byteLength(response)
                });
                res.end(response);
                return;
            }
        }
    }
    
    // Handle root endpoint
    if (url === '/' || url === '/index.html') {
        const filePath = path.join(__dirname, 'index.html');
        
        fs.readFile(filePath, (err, content) => {
            if (err) {
                res.writeHead(404);
                res.end('File not found');
                return;
            }
            
            res.writeHead(200, {
                'Content-Type': 'text/html',
                'Content-Length': Buffer.byteLength(content)
            });
            res.end(content);
        });
        return;
    }
    
    // Handle 404
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end('Not Found');
});

// Generate compare.json with expected hashes
function generateCompareJson() {
    const hashes = {};
    
    for (let i = 0; i < 100; i++) {
        hashes[`test-${i}`] = generateTestHash(i);
    }
    
    fs.writeFileSync(
        path.join(__dirname, 'compare.json'),
        JSON.stringify(hashes, null, 2)
    );
    
    console.log('Generated compare.json with 100 test hashes');
}

// Start server
const PORT = 3000;
const HOST = 'localhost';

server.listen(PORT, HOST, () => {
    console.log(`Language Race Test Server running at http://${HOST}:${PORT}`);
    console.log('Test endpoints: http://localhost:3000/test-0 through test-99');
    
    // Generate compare.json
    generateCompareJson();
});

// Handle graceful shutdown
process.on('SIGINT', () => {
    console.log('\nShutting down server...');
    server.close(() => {
        console.log('Server closed');
        process.exit(0);
    });
});
