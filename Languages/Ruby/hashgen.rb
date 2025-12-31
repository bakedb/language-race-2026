# Hello, World!
puts "Hello, World!"

require 'digest'
require 'json'
require 'fileutils'

class FileHashResult
  attr_accessor :filename, :filepath, :size, :hash, :status, :error
  
  def initialize(filename, filepath, size)
    @filename = filename
    @filepath = filepath
    @size = size
    @status = "SUCCESS"
  end
end

class HashResults
  attr_accessor :total_files, :successful_hashes, :failed_hashes, :processing_time_seconds, 
                :average_time_per_file_ms, :timestamp, :directory, :results
  
  def initialize(directory)
    @directory = directory
    @results = []
  end
end

def calculate_file_hash(filepath)
  begin
    content = File.read(filepath)
    Digest::SHA256.hexdigest(content)
  rescue => e
    ""
  end
end

def get_file_size(filepath)
  begin
    File.size(filepath)
  rescue => e
    0
  end
end

def get_txt_files(directory)
  begin
    unless Dir.exist?(directory)
      puts "Directory not found: #{directory}"
      return []
    end
    
    files = Dir.glob(File.join(directory, "*.txt")).sort
    files
  rescue => e
    puts "Error reading directory: #{e.message}"
    []
  end
end

def get_timestamp
  Time.now.strftime("%Y-%m-%d %H:%M:%S")
end

def hash_files_in_directory(directory)
  files = get_txt_files(directory)
  
  if files.empty?
    puts "No .txt files found in directory: #{directory}"
    return
  end
  
  puts "Found #{files.length} files to hash"
  
  results = HashResults.new(directory)
  results.total_files = files.length
  
  start_time = Time.now
  
  files.each_with_index do |filepath, i|
    result = FileHashResult.new(
      File.basename(filepath),
      filepath,
      get_file_size(filepath)
    )
    
    hash = calculate_file_hash(filepath)
    if !hash.empty?
      result.hash = hash
      result.status = "SUCCESS"
      results.successful_hashes = (results.successful_hashes || 0) + 1
    else
      result.hash = ""
      result.status = "FAILED"
      result.error = "Hash calculation failed"
      results.failed_hashed = (results.failed_hashes || 0) + 1
    end
    
    results.results << result
    
    # Progress indicator
    if ((i + 1) % 100) == 0
      puts "Processed #{i + 1}/#{files.length} files..."
    end
  end
  
  end_time = Time.now
  results.processing_time_seconds = end_time - start_time
  results.average_time_per_file_ms = (results.processing_time_seconds / files.length) * 1000
  results.timestamp = get_timestamp
  
  # Create JSON output
  json_results = {
    total_files: results.total_files,
    successful_hashes: results.successful_hashes || 0,
    failed_hashes: results.failed_hashes || 0,
    algorithm: "sha256",
    processing_time_seconds: results.processing_time_seconds,
    average_time_per_file_ms: results.average_time_per_file_ms,
    timestamp: results.timestamp,
    directory: results.directory,
    results: results.results.map do |result|
      json_result = {
        filename: result.filename,
        filepath: result.filepath,
        size: result.size,
        algorithm: "sha256",
        hash: result.hash,
        status: result.status
      }
      
      if result.error
        json_result[:error] = result.error
      end
      
      json_result
    end
  }
  
  # Save to file
  begin
    File.write('hash_results.json', JSON.pretty_generate(json_results))
    puts "Results saved to: hash_results.json"
  rescue => e
    puts "Error saving results: #{e.message}"
  end
  
  puts "\nHashing completed!"
  puts "Total files: #{results.total_files}"
  puts "Successful: #{results.successful_hashes || 0}"
  puts "Failed: #{results.failed_hashes || 0}"
  puts "Processing time: #{results.processing_time_seconds.round(3)} seconds"
  puts "Average time per file: #{results.average_time_per_file_ms.round(2)} ms"
end

hash_files_in_directory('../hashfiles')
