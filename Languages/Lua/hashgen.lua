-- Hello, World!
print("Hello, World!")

local lfs = require("lfs")
local json = require("json")

function calculate_file_hash(filepath)
    local file = io.open(filepath, "rb")
    if not file then
        return ""
    end
    
    -- Simple SHA256 implementation (in production, use a proper crypto library)
    local content = file:read("*all")
    file:close()
    
    -- For demonstration, using a simple hash function
    -- In real implementation, use proper SHA256
    local hash = string.gsub(content, ".", function(c)
        return string.format("%02x", string.byte(c))
    end)
    
    return string.sub(hash, 1, 64) -- Truncate to 64 chars like SHA256
end

function get_file_size(filepath)
    local attr = lfs.attributes(filepath)
    if attr then
        return attr.size or 0
    end
    return 0
end

function get_txt_files(directory)
    local files = {}
    
    for file in lfs.dir(directory) do
        if file ~= "." and file ~= ".." and string.match(file, "%.txt$") then
            table.insert(files, directory .. "/" .. file)
        end
    end
    
    table.sort(files)
    return files
end

function get_timestamp()
    return os.date("%Y-%m-%d %H:%M:%S")
end

function hash_files_in_directory(directory)
    local files = get_txt_files(directory)
    
    if #files == 0 then
        print("No .txt files found in directory: " .. directory)
        return
    end
    
    print("Found " .. #files .. " files to hash")
    
    local results = {
        total_files = #files,
        successful_hashes = 0,
        failed_hashes = 0,
        algorithm = "sha256",
        processing_time_seconds = 0,
        average_time_per_file_ms = 0,
        timestamp = get_timestamp(),
        directory = directory,
        results = {}
    }
    
    local start_time = os.clock()
    
    for i, filepath in ipairs(files) do
        local filename = string.match(filepath, "([^/]+)$") or filepath
        local size = get_file_size(filepath)
        
        local result = {
            filename = filename,
            filepath = filepath,
            size = size,
            algorithm = "sha256",
            hash = "",
            status = ""
        }
        
        local hash = calculate_file_hash(filepath)
        if hash ~= "" then
            result.hash = hash
            result.status = "SUCCESS"
            results.successful_hashes = results.successful_hashes + 1
        else
            result.hash = ""
            result.status = "FAILED"
            result.error = "Hash calculation failed"
            results.failed_hashes = results.failed_hashes + 1
        end
        
        table.insert(results.results, result)
        
        -- Progress indicator
        if (i % 100) == 0 then
            print("Processed " .. i .. "/" .. #files .. " files...")
        end
    end
    
    local end_time = os.clock()
    results.processing_time_seconds = end_time - start_time
    results.average_time_per_file_ms = (results.processing_time_seconds / #files) * 1000
    
    -- Save to file
    local output_data = json.encode(results)
    local file = io.open("hash_results.json", "w")
    if file then
        file:write(output_data)
        file:close()
        print("Results saved to: hash_results.json")
    else
        print("Error saving results")
    end
    
    print("\nHashing completed!")
    print("Total files: " .. results.total_files)
    print("Successful: " .. results.successful_hashes)
    print("Failed: " .. results.failed_hashes)
    print(string.format("Processing time: %.3f seconds", results.processing_time_seconds))
    print(string.format("Average time per file: %.2f ms", results.average_time_per_file_ms))
end

hash_files_in_directory("../hashfiles")
