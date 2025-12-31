// Hello, World!
console.log("Hello, World!");

import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';

interface FileHashResult {
    filename: string;
    filepath: string;
    size: number;
    hash: string;
    status: string;
    error?: string;
}

interface HashResults {
    total_files: number;
    successful_hashes: number;
    failed_hashes: number;
    algorithm: string;
    processing_time_seconds: number;
    average_time_per_file_ms: number;
    timestamp: string;
    directory: string;
    results: FileHashResult[];
}

class FileHashResultClass implements FileHashResult {
    filename: string;
    filepath: string;
    size: number;
    hash: string = "";
    status: string;
    error?: string;

    constructor(filename: string, filepath: string, size: number) {
        this.filename = filename;
        this.filepath = filepath;
        this.size = size;
        this.status = "SUCCESS";
    }
}

class HashResultsClass implements HashResults {
    total_files: number = 0;
    successful_hashes: number = 0;
    failed_hashes: number = 0;
    algorithm: string = "sha256";
    processing_time_seconds: number = 0;
    average_time_per_file_ms: number = 0;
    timestamp: string = "";
    directory: string;
    results: FileHashResult[];

    constructor(directory: string) {
        this.directory = directory;
        this.results = [];
    }
}

function calculateFileHash(filepath: string): string {
    try {
        const fileBuffer = fs.readFileSync(filepath);
        const hashSum = crypto.createHash('sha256');
        hashSum.update(fileBuffer);
        return hashSum.digest('hex');
    } catch (error) {
        return '';
    }
}

function getFileSize(filepath: string): number {
    try {
        const stats = fs.statSync(filepath);
        return stats.size;
    } catch (error) {
        return 0;
    }
}

function getTxtFiles(directory: string): string[] {
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
        console.log(`Error reading directory: ${error}`);
        return [];
    }
}

function getTimestamp(): string {
    return new Date().toISOString();
}

function hashFilesInDirectory(directory: string): void {
    const files = getTxtFiles(directory);

    if (files.length === 0) {
        console.log(`No .txt files found in directory: ${directory}`);
        return;
    }

    console.log(`Found ${files.length} files to hash`);

    const results = new HashResultsClass(directory);
    results.total_files = files.length;

    const startTime = Date.now();

    for (let i = 0; i < files.length; i++) {
        const filepath = files[i];
        const result = new FileHashResultClass(
            path.basename(filepath),
            filepath,
            getFileSize(filepath)
        );

        const hash = calculateFileHash(filepath);
        if (hash) {
            result.hash = hash;
            result.status = "SUCCESS";
            results.successful_hashes++;
        } else {
            result.hash = "";
            result.status = "FAILED";
            result.error = "Hash calculation failed";
            results.failed_hashes++;
        }

        results.results.push(result);

        // Progress indicator
        if ((i + 1) % 100 === 0) {
            console.log(`Processed ${i + 1}/${files.length} files...`);
        }
    }

    const endTime = Date.now();
    results.processing_time_seconds = (endTime - startTime) / 1000;
    results.average_time_per_file_ms = (results.processing_time_seconds / files.length) * 1000;
    results.timestamp = getTimestamp();

    // Create JSON output
    const jsonResults: HashResults = {
        total_files: results.total_files,
        successful_hashes: results.successful_hashes,
        failed_hashes: results.failed_hashes,
        algorithm: results.algorithm,
        processing_time_seconds: results.processing_time_seconds,
        average_time_per_file_ms: results.average_time_per_file_ms,
        timestamp: results.timestamp,
        directory: results.directory,
        results: results.results.map(result => {
            const jsonResult: FileHashResult = {
                filename: result.filename,
                filepath: result.filepath,
                size: result.size,
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
        console.log(`Error saving results: ${error}`);
    }

    console.log('\nHashing completed!');
    console.log(`Total files: ${results.total_files}`);
    console.log(`Successful: ${results.successful_hashes}`);
    console.log(`Failed: ${results.failed_hashes}`);
    console.log(`Processing time: ${results.processing_time_seconds.toFixed(3)} seconds`);
    console.log(`Average time per file: ${results.average_time_per_file_ms.toFixed(2)} ms`);
}

hashFilesInDirectory('../hashfiles');
