# Hello, World!
puts "Hello, World!"

require "digest"
require "json"

class FileHashResult
  property filename : String
  property filepath : String
  property size : UInt64
  property hash : String
  property status : String
  property error : String?
  
  def initialize(@filename, @filepath, @size)
    @status = "SUCCESS"
  end
end

class HashResults
  property total_files : Int32
  property successful_hashes : Int32
  property failed_hashes : Int32
  property algorithm : String
  property processing_time_seconds : Float64
  property average_time_per_file_ms : Float64
  property timestamp : String
  property directory : String
  property results : Array(FileHashResult)
  
  def initialize(@directory)
    @results = [] of FileHashResult
    @algorithm = "sha256"
  end
end

def calculate_file_hash(filepath : String) : String
  begin
    content = File.read(filepath)
    Digest::SHA256.hexdigest(content)
  rescue
    ""
  end
end

def get_file_size(filepath : String) : UInt64
  begin
    File.info(filepath).size
  rescue
    0_u64
  end
end

def get_txt_files(directory : String) : Array(String)
  begin
    unless Dir.exists?(directory)
      puts "Directory not found: #{directory}"
      return [] of String
    end
    
    files = Dir.glob(File.join(directory, "*.txt")).sort
    files
  rescue ex
    puts "Error reading directory: #{ex.message}"
    [] of String
  end
end

def get_timestamp : String
  Time.local.to_s("%Y-%m-%d %H:%M:%S")
end

def hash_files_in_directory(directory : String)
  files = get_txt_files(directory)
  
  if files.empty?
    puts "No .txt files found in directory: #{directory}"
    return
  end
  
  puts "Found #{files.size} files to hash"
  
  results = HashResults.new(directory)
  results.total_files = files.size
  
  start_time = Time.monotonic
  
  files.each_with_index do |filepath, i|
    result = FileHashResult.new(
      File.basename(filepath),
      filepath,
      get_file_size(filepath)
    )
    
    hash = calculate_file_hash(filepath)
    if hash.empty?
      result.hash = ""
      result.status = "FAILED"
      result.error = "Hash calculation failed"
      results.failed_hashes += 1
    else
      result.hash = hash
      result.status = "SUCCESS"
      results.successful_hashes += 1
    end
    
    results.results << result
    
    # Progress indicator
    if (i + 1) % 100 == 0
      puts "Processed #{i + 1}/#{files.size} files..."
    end
  end
  
  end_time = Time.monotonic
  results.processing_time_seconds = end_time - start_time
  results.average_time_per_file_ms = (results.processing_time_seconds / files.size) * 1000
  results.timestamp = get_timestamp
  
  # Create final result
  final_result = {
    "total_files"           => results.total_files,
    "successful_hashes"    => results.successful_hashes,
    "failed_hashes"         => results.failed_hashes,
    "algorithm"             => results.algorithm,
    "processing_time_seconds" => results.processing_time_seconds,
    "average_time_per_file_ms" => results.average_time_per_file_ms,
    "timestamp"             => results.timestamp,
    "directory"             => results.directory,
    "results"               => results.results.map do |result|
      hash_result = {
        "filename" => result.filename,
        "filepath" => result.filepath,
        "size"     => result.size,
        "algorithm" => "sha256",
        "hash"     => result.hash,
        "status"   => result.status
      }
      
      if result.error
        hash_result["error"] = result.error
      end
      
      hash_result
    end
  }
  
  # Save to file
  begin
    File.write("hash_results.json", final_result.to_json)
    puts "Results saved to: hash_results.json"
  rescue ex
    puts "Error saving results: #{ex.message}"
  end
  
  puts "\nHashing completed!"
  puts "Total files: #{results.total_files}"
  puts "Successful: #{results.successful_hashes}"
  puts "Failed: #{results.failed_hashes}"
  puts "Processing time: #{results.processing_time_seconds.round(3)} seconds"
  puts "Average time per file: #{results.average_time_per_file_ms.round(2)} ms"
end

hash_files_in_directory("../hashfiles")
