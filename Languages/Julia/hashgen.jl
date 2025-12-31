# Hello, World!
println("Hello, World!")

using SHA
using JSON

function calculate_file_hash(filepath)
    try
        content = read(filepath)
        bytes2hex(sha256(content))
    catch e
        return ""
    end
end

function get_file_size(filepath)
    try
        filesize(filepath)
    catch e
        return 0
    end
end

function get_txt_files(directory)
    try
        if !isdir(directory)
            println("Directory not found: $directory")
            return String[]
        end
        
        files = filter(f -> endswith(f, ".txt"), readdir(directory, join=true))
        sort(files)
    catch e
        println("Error reading directory: $e")
        return String[]
    end
end

function get_timestamp()
    string(now())
end

function hash_files_in_directory(directory)
    files = get_txt_files(directory)
    
    if isempty(files)
        println("No .txt files found in directory: $directory")
        return
    end
    
    println("Found $(length(files)) files to hash")
    
    results = []
    successful_hashes = 0
    failed_hashes = 0
    
    start_time = time()
    
    for (i, filepath) in enumerate(files)
        filename = basename(filepath)
        size = get_file_size(filepath)
        
        result = Dict(
            "filename" => filename,
            "filepath" => filepath,
            "size" => size,
            "algorithm" => "sha256"
        )
        
        hash = calculate_file_hash(filepath)
        if !isempty(hash)
            result["hash"] = hash
            result["status"] = "SUCCESS"
            successful_hashes += 1
        else
            result["hash"] = ""
            result["status"] = "FAILED"
            result["error"] = "Hash calculation failed"
            failed_hashes += 1
        end
        
        push!(results, result)
        
        # Progress indicator
        if (i % 100) == 0
            println("Processed $i/$(length(files)) files...")
        end
    end
    
    end_time = time()
    processing_time = end_time - start_time
    average_time_per_file = (processing_time / length(files)) * 1000
    
    final_result = Dict(
        "total_files" => length(files),
        "successful_hashes" => successful_hashes,
        "failed_hashes" => failed_hashes,
        "algorithm" => "sha256",
        "processing_time_seconds" => processing_time,
        "average_time_per_file_ms" => average_time_per_file,
        "timestamp" => get_timestamp(),
        "directory" => directory,
        "results" => results
    )
    
    # Save to file
    try
        open("hash_results.json", "w") do f
            JSON.print(f, final_result, 2)
        end
        println("Results saved to: hash_results.json")
    catch e
        println("Error saving results: $e")
    end
    
    println("\nHashing completed!")
    println("Total files: $(length(files))")
    println("Successful: $successful_hashes")
    println("Failed: $failed_hashes")
    println("Processing time: $(round(processing_time, digits=3)) seconds")
    println("Average time per file: $(round(average_time_per_file, digits=2)) ms")
end

hash_files_in_directory("../hashfiles")
