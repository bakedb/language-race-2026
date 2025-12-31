// Hello, World!
console.log("Hello, World!");

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

class FileHashResult {
    constructor(filename, filepath, size) {
        this.filename = filename;
        this.filepath = filepath;
        this.size = size;
        this.status = "SUCCESS";
    }
}

class HashResults {
    constructor(directory) {
        this.directory = directory;
        this.results = [];
    }
}

function calculateFileHash(filepath) {
    try {
        const fileBuffer = fs.readFileSync(filepath);
        const hashSum = crypto.createHash('sha256');
        hashSum.update(fileBuffer);
        return hashSum.digest('hex');
    } catch (error) {
        return '';
    }
}

function getFileSize(filepath) {
    try {
        const stats = fs.statSync(filepath);
        return stats.size;
    } catch (error) {
        return 0;
    }
}

function getTxtFiles(directory) {
    try {
        if (!fs.existsSync(directory) || !fs.statSync(directory).isDirectory()) {
            console.log(`Directory not found: ${directory}`);
            return [];
        }
        
        const files = fs.readdirSync(directory)
            .filter(file => file.endsWith('.txt'))
            .map(file => path.join(directory, file))
            .sort();
        
        return files;
    } catch (error) {
        console.log(`Error reading directory: ${error.message}`);
        return [];
    }
}

function getTimestamp() {
    return new Date().toISOString();
}

function hashFilesInDirectory(directory) {
    const files = getTxtFiles(directory);
    
    if (files.length === 0) {
        console.log(`No .txt files found in directory: ${directory}`);
        return;
    }
    
    console.log(`Found ${files.length} files to hash`);
    
    const results = new HashResults(directory);
    results.totalFiles = files.length;
    
    const startTime = Date.now();
    
    for (let i = 0; i < files.length; i++) {
        const filepath = files[i];
        const result = new FileHashResult(
            path.basename(filepath),
            filepath,
            getFileSize(filepath)
        );
        
        const hash = calculateFileHash(filepath);
        if (hash) {
            result.hash = hash;
            result.status = "SUCCESS";
            results.successfulHashes = (results.successfulHashes || 0) + 1;
        } else {
            result.hash = "";
            result.status = "FAILED";
            result.error = "Hash calculation failed";
            results.failedHashes = (results.failedHashes || 0) + 1;
        }
        
        results.results.push(result);
        
        // Progress indicator
        if ((i + 1) % 100 === 0) {
            console.log(`Processed ${i + 1}/${files.length} files...`);
        }
    }
    
    const endTime = Date.now();
    results.processingTimeSeconds = (endTime - startTime) / 1000;
    results.averageTimePerFileMs = (results.processingTimeSeconds / files.length) * 1000;
    results.timestamp = getTimestamp();
    
    // Create JSON output
    const jsonResults = {
        total_files: results.totalFiles,
        successful_hashes: results.successfulHashes || 0,
        failed_hashes: results.failedHashes || 0,
        algorithm: "sha256",
        processing_time_seconds: results.processingTimeSeconds,
        average_time_per_file_ms: results.averageTimePerFileMs,
        timestamp: results.timestamp,
        directory: results.directory,
        results: results.results.map(result => {
            const jsonResult = {
                filename: result.filename,
                filepath: result.filepath,
                size: result.size,
                algorithm: "sha256",
                hash: result.hash,
                status: result.status
            };
            
            if (result.error) {
                jsonResult.error = result.error;
            }
            
            return jsonResult;
        })
    };
    
    // Save to file
    try {
        fs.writeFileSync('hash_results.json', JSON.stringify(jsonResults, null, 2));
        console.log('Results saved to: hash_results.json');
    } catch (error) {
        console.log(`Error saving results: ${error.message}`);
    }
    
    console.log('\nHashing completed!');
    console.log(`Total files: ${results.totalFiles}`);
    console.log(`Successful: ${results.successfulHashes || 0}`);
    console.log(`Failed: ${results.failedHashes || 0}`);
    console.log(`Processing time: ${results.processingTimeSeconds.toFixed(3)} seconds`);
    console.log(`Average time per file: ${results.averageTimePerFileMs.toFixed(2)} ms`);
}

hashFilesInDirectory('../hashfiles');
