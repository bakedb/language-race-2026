import std.stdio;
import std.json;
import std.file;
import std.path;
import std.digest.sha;
import std.algorithm.sorting;
import std.datetime;
import std.conv;

// Hello, World!
void hello() {
    writeln("Hello, World!");
}

struct FileHashResult {
    string filename;
    string filepath;
    ulong size;
    string hash;
    string status;
    string error;
}

struct HashResults {
    int totalFiles;
    int successfulHashes;
    int failedHashes;
    string algorithm;
    double processingTimeSeconds;
    double averageTimePerFileMs;
    string timestamp;
    string directory;
    FileHashResult[] results;
}

string calculateFileHash(string filepath) {
    try {
        auto content = cast(ubyte[])std.file.read(filepath);
        auto sha256 = SHA256(content);
        return sha256.toHexString();
    } catch (Exception e) {
        return "";
    }
}

ulong getFileSize(string filepath) {
    try {
        return std.file.getSize(filepath);
    } catch (Exception e) {
        return 0;
    }
}

string[] getTxtFiles(string directory) {
    string[] files;
    
    try {
        auto dirEntries = DirEntry(directory);
        foreach (entry; dirEntries.filter!(a => a.isFile && a.name.endsWith(".txt"))) {
            files ~= entry.name;
        }
        sort(files);
    } catch (Exception e) {
        writeln("Error reading directory: ", e.msg);
    }
    
    return files;
}

string getTimestamp() {
    return Clock.currTime().toISOExtString();
}

void hashFilesInDirectory(string directory) {
    auto files = getTxtFiles(directory);
    
    if (files.length == 0) {
        writeln("No .txt files found in directory: ", directory);
        return;
    }
    
    writeln("Found ", files.length, " files to hash");
    
    HashResults results;
    results.totalFiles = cast(int)files.length;
    results.algorithm = "sha256";
    results.directory = directory;
    results.results = new FileHashResult[files.length];
    
    auto startTime = Clock.currTime();
    
    foreach (i, file; files) {
        auto filepath = buildPath(directory, file);
        
        results.results[i] = FileHashResult(
            file,
            filepath,
            getFileSize(filepath),
            "",
            "",
            ""
        );
        
        auto hash = calculateFileHash(filepath);
        if (hash.length > 0) {
            results.results[i].hash = hash;
            results.results[i].status = "SUCCESS";
            results.successfulHashes++;
        } else {
            results.results[i].hash = "";
            results.results[i].status = "FAILED";
            results.results[i].error = "Hash calculation failed";
            results.failedHashes++;
        }
        
        // Progress indicator
        if ((i + 1) % 100 == 0) {
            writeln("Processed ", i + 1, "/", files.length, " files...");
        }
    }
    
    auto endTime = Clock.currTime();
    results.processingTimeSeconds = (endTime - startTime).total!"seconds";
    results.averageTimePerFileMs = (results.processingTimeSeconds / files.length) * 1000;
    results.timestamp = getTimestamp();
    
    // Create JSON output
    JSONValue jsonResults;
    jsonResults["total_files"] = JSONValue(results.totalFiles);
    jsonResults["successful_hashes"] = JSONValue(results.successfulHashes);
    jsonResults["failed_hashes"] = JSONValue(results.failedHashes);
    jsonResults["algorithm"] = JSONValue(results.algorithm);
    jsonResults["processing_time_seconds"] = JSONValue(results.processingTimeSeconds);
    jsonResults["average_time_per_file_ms"] = JSONValue(results.averageTimePerFileMs);
    jsonResults["timestamp"] = JSONValue(results.timestamp);
    jsonResults["directory"] = JSONValue(results.directory);
    
    JSONValue[] jsonResultsArray;
    foreach (result; results.results) {
        JSONValue jsonResult;
        jsonResult["filename"] = JSONValue(result.filename);
        jsonResult["filepath"] = JSONValue(result.filepath);
        jsonResult["size"] = JSONValue(cast(long)result.size);
        jsonResult["algorithm"] = JSONValue("sha256");
        jsonResult["hash"] = JSONValue(result.hash);
        jsonResult["status"] = JSONValue(result.status);
        
        if (result.error.length > 0) {
            jsonResult["error"] = JSONValue(result.error);
        }
        
        jsonResultsArray ~= jsonResult;
    }
    jsonResults["results"] = JSONValue(jsonResultsArray);
    
    // Save to file
    try {
        auto file = File("hash_results.json", "w");
        file.writeln(jsonResults.toPrettyString());
        file.close();
        writeln("Results saved to: hash_results.json");
    } catch (Exception e) {
        writeln("Error saving results: ", e.msg);
    }
    
    writeln("\nHashing completed!");
    writeln("Total files: ", results.totalFiles);
    writeln("Successful: ", results.successfulHashes);
    writeln("Failed: ", results.failedHashes);
    writeln("Processing time: ", format("%.3f", results.processingTimeSeconds), " seconds");
    writeln("Average time per file: ", format("%.2f", results.averageTimePerFileMs), " ms");
}

void main() {
    hello();
    hashFilesInDirectory("../hashfiles");
}
