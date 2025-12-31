package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:crypto/sha256"
import "core:time"

// Hello, World!
hello :: proc() {
    fmt.println("Hello, World!")
}

File_Hash_Result :: struct {
    filename:    string,
    filepath:    string,
    size:        u64,
    hash:        string,
    status:      string,
    error:       string,
}

Hash_Results :: struct {
    total_files:             int,
    successful_hashes:       int,
    failed_hashes:           int,
    algorithm:               string,
    processing_time_seconds: f64,
    average_time_per_file_ms: f64,
    timestamp:               string,
    directory:               string,
    results:                 [dynamic]File_Hash_Result,
}

calculate_file_hash :: proc(filepath: string) -> string {
    data, ok := os.read_file(filepath)
    if !ok {
        return ""
    }
    
    hash := sha256.hash_bytes(data)
    return strings.hex_from_bytes(hash[:])
}

get_file_size :: proc(filepath: string) -> u64 {
    handle, ok := os.open(filepath)
    if !ok {
        return 0
    }
    defer os.close(handle)
    
    info, ok := os.stat(handle)
    if !ok {
        return 0
    }
    
    return cast(u64)info.size
}

get_txt_files :: proc(directory: string, allocator: context.allocator) -> []string {
    files: [dynamic]string
    
    dir_handle, ok := os.open(directory)
    if !ok {
        fmt.printf("Directory not found: %s\n", directory)
        return files[:]
    }
    defer os.close(dir_handle)
    
    for entry in os.read_dir_entries(dir_handle, allocator) {
        if strings.has_suffix(entry.name, ".txt") {
            filepath := strings.concatenate(directory, "/", entry.name)
            append(&files, filepath)
        }
    }
    
    // Sort files
    sort_strings(files[:])
    return files[:]
}

sort_strings :: proc(files: []string) {
    for i in 0 .. len(files)-1 {
        for j in i+1 .. len(files)-1 {
            if files[i] > files[j] {
                temp := files[i]
                files[i] = files[j]
                files[j] = temp
            }
        }
    }
}

get_timestamp :: proc() -> string {
    return fmt.tprintf("%s", time.now())
}

hash_files_in_directory :: proc(directory: string) {
    files := get_txt_files(directory, context.allocator)
    
    if len(files) == 0 {
        fmt.printf("No .txt files found in directory: %s\n", directory)
        return
    }
    
    fmt.printf("Found %d files to hash\n", len(files))
    
    results: Hash_Results
    results.total_files = len(files)
    results.algorithm = "sha256"
    results.directory = directory
    
    start_time := time.now()
    
    for i in 0 .. len(files)-1 {
        filepath := files[i]
        filename := strings.basename(filepath)
        
        result: File_Hash_Result
        result.filename = filename
        result.filepath = filepath
        result.size = get_file_size(filepath)
        
        hash := calculate_file_hash(filepath)
        if len(hash) > 0 {
            result.hash = hash
            result.status = "SUCCESS"
            results.successful_hashes += 1
        } else {
            result.hash = ""
            result.status = "FAILED"
            result.error = "Hash calculation failed"
            results.failed_hashes += 1
        }
        
        append(&results.results, result)
        
        // Progress indicator
        if (i + 1) % 100 == 0 {
            fmt.printf("Processed %d/%d files...\n", i + 1, len(files))
        }
    }
    
    end_time := time.now()
    duration := time.duration_seconds(end_time - start_time)
    results.processing_time_seconds = duration
    results.average_time_per_file_ms = (duration / f64(len(files))) * 1000.0
    results.timestamp = get_timestamp()
    
    // Save results to file (simplified - would need proper JSON serialization)
    fmt.printf("Results would be saved to: hash_results.json\n")
    fmt.printf("\nHashing completed!\n")
    fmt.printf("Total files: %d\n", results.total_files)
    fmt.printf("Successful: %d\n", results.successful_hashes)
    fmt.printf("Failed: %d\n", results.failed_hashes)
    fmt.printf("Processing time: %.3f seconds\n", results.processing_time_seconds)
    fmt.printf("Average time per file: %.2f ms\n", results.average_time_per_file_ms)
}

main :: proc() {
    hello()
    hash_files_in_directory("../hashfiles")
}
