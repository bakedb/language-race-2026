#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <filesystem>
#include <algorithm>
#include <chrono>
#include <iomanip>
#include <openssl/sha.h>
#include <json/json.h>

// Hello, World!
void hello() {
    std::cout << "Hello, World!" << std::endl;
}

struct FileHashResult {
    std::string filename;
    std::string filepath;
    long size;
    std::string hash;
    std::string status;
    std::string error;
};

struct HashResults {
    int total_files;
    int successful_hashes;
    int failed_hashes;
    double processing_time_seconds;
    double average_time_per_file_ms;
    std::string timestamp;
    std::string directory;
    std::vector<FileHashResult> results;
};

std::string calculate_file_hash(const std::string& filepath) {
    std::ifstream file(filepath, std::ios::binary);
    if (!file.is_open()) {
        return "";
    }
    
    SHA256_CTX sha256;
    SHA256_Init(&sha256);
    
    char buffer[4096];
    while (file.read(buffer, sizeof(buffer))) {
        SHA256_Update(&sha256, buffer, file.gcount());
    }
    
    // Handle any remaining bytes
    if (file.gcount() > 0) {
        SHA256_Update(&sha256, buffer, file.gcount());
    }
    
    unsigned char hash[SHA256_DIGEST_LENGTH];
    SHA256_Final(hash, &sha256);
    
    // Convert to hex string
    std::stringstream ss;
    for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
        ss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(hash[i]);
    }
    
    return ss.str();
}

long get_file_size(const std::string& filepath) {
    std::error_code ec;
    auto size = std::filesystem::file_size(filepath, ec);
    return ec ? 0 : size;
}

std::vector<std::string> get_txt_files(const std::string& directory) {
    std::vector<std::string> files;
    
    try {
        for (const auto& entry : std::filesystem::directory_iterator(directory)) {
            if (entry.is_regular_file() && entry.path().extension() == ".txt") {
                files.push_back(entry.path().string());
            }
        }
    } catch (const std::filesystem::filesystem_error& e) {
        std::cout << "Error reading directory: " << e.what() << std::endl;
        return files;
    }
    
    // Sort files alphabetically
    std::sort(files.begin(), files.end());
    
    return files;
}

std::string get_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    std::stringstream ss;
    ss << std::put_time(std::localtime(&time_t), "%Y-%m-%d %H:%M:%S");
    return ss.str();
}

void hash_files_in_directory(const std::string& directory) {
    auto files = get_txt_files(directory);
    
    if (files.empty()) {
        std::cout << "No .txt files found in directory: " << directory << std::endl;
        return;
    }
    
    std::cout << "Found " << files.size() << " files to hash" << std::endl;
    
    HashResults results;
    results.total_files = files.size();
    results.directory = directory;
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    for (size_t i = 0; i < files.size(); i++) {
        FileHashResult result;
        
        // Extract filename from filepath
        result.filepath = files[i];
        result.filename = std::filesystem::path(files[i]).filename().string();
        result.size = get_file_size(files[i]);
        
        std::string hash = calculate_file_hash(files[i]);
        if (!hash.empty()) {
            result.hash = hash;
            result.status = "SUCCESS";
            results.successful_hashes++;
        } else {
            result.hash = "";
            result.status = "FAILED";
            result.error = "Hash calculation failed";
            results.failed_hashes++;
        }
        
        results.results.push_back(result);
        
        // Progress indicator
        if ((i + 1) % 100 == 0) {
            std::cout << "Processed " << (i + 1) << "/" << files.size() << " files..." << std::endl;
        }
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    results.processing_time_seconds = duration.count() / 1000.0;
    results.average_time_per_file_ms = (results.processing_time_seconds / files.size()) * 1000.0;
    results.timestamp = get_timestamp();
    
    // Create JSON output
    Json::Value json_results;
    json_results["total_files"] = results.total_files;
    json_results["successful_hashes"] = results.successful_hashes;
    json_results["failed_hashes"] = results.failed_hashes;
    json_results["algorithm"] = "sha256";
    json_results["processing_time_seconds"] = results.processing_time_seconds;
    json_results["average_time_per_file_ms"] = results.average_time_per_file_ms;
    json_results["timestamp"] = results.timestamp;
    json_results["directory"] = results.directory;
    
    Json::Value json_results_array(Json::arrayValue);
    
    for (const auto& result : results.results) {
        Json::Value json_result;
        json_result["filename"] = result.filename;
        json_result["filepath"] = result.filepath;
        json_result["size"] = Json::Int64(result.size);
        json_result["algorithm"] = "sha256";
        json_result["hash"] = result.hash;
        json_result["status"] = result.status;
        
        if (!result.error.empty()) {
            json_result["error"] = result.error;
        }
        
        json_results_array.append(json_result);
    }
    
    json_results["results"] = json_results_array;
    
    // Save to file
    std::ofstream output_file("hash_results.json");
    if (output_file.is_open()) {
        Json::StreamWriterBuilder builder;
        std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
        writer->write(json_results, &output_file);
        output_file.close();
        std::cout << "Results saved to: hash_results.json" << std::endl;
    } else {
        std::cout << "Error saving results" << std::endl;
    }
    
    std::cout << "\nHashing completed!" << std::endl;
    std::cout << "Total files: " << results.total_files << std::endl;
    std::cout << "Successful: " << results.successful_hashes << std::endl;
    std::cout << "Failed: " << results.failed_hashes << std::endl;
    std::cout << "Processing time: " << std::fixed << std::setprecision(3) << results.processing_time_seconds << " seconds" << std::endl;
    std::cout << "Average time per file: " << std::fixed << std::setprecision(2) << results.average_time_per_file_ms << " ms" << std::endl;
}

int main() {
    hello();
    hash_files_in_directory("../hashfiles");
    return 0;
}
