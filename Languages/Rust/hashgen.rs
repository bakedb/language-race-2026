use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::Instant;
use sha2::{Sha256, Digest};
use serde::{Deserialize, Serialize};

// Hello, World!
fn hello() {
    println!("Hello, World!");
}

#[derive(Serialize, Deserialize, Debug)]
struct FileHashResult {
    filename: String,
    filepath: String,
    size: u64,
    hash: String,
    status: String,
    error: Option<String>,
}

#[derive(Serialize, Debug)]
struct HashResults {
    total_files: usize,
    successful_hashes: usize,
    failed_hashes: usize,
    algorithm: String,
    processing_time_seconds: f64,
    average_time_per_file_ms: f64,
    timestamp: String,
    directory: String,
    results: Vec<FileHashResult>,
}

fn calculate_file_hash(filepath: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut file = fs::File::open(filepath)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0; 4096];
    
    loop {
        let bytes_read = file.read(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }
    
    Ok(format!("{:x}", hasher.finalize()))
}

fn get_file_size(filepath: &str) -> u64 {
    fs::metadata(filepath).map(|m| m.len()).unwrap_or(0)
}

fn get_txt_files(directory: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();
    
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.is_file() && path.extension().map_or(false, |ext| ext == "txt") {
            files.push(path.to_string_lossy().to_string());
        }
    }
    
    files.sort();
    Ok(files)
}

fn get_timestamp() -> String {
    use chrono::Utc;
    Utc::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

fn hash_files_in_directory(directory: &str) -> Result<(), Box<dyn std::error::Error>> {
    let files = get_txt_files(directory)?;
    
    if files.is_empty() {
        println!("No .txt files found in directory: {}", directory);
        return Ok(());
    }
    
    println!("Found {} files to hash", files.len());
    
    let mut results = HashResults {
        total_files: files.len(),
        successful_hashes: 0,
        failed_hashes: 0,
        algorithm: "sha256".to_string(),
        processing_time_seconds: 0.0,
        average_time_per_file_ms: 0.0,
        timestamp: get_timestamp(),
        directory: directory.to_string(),
        results: Vec::with_capacity(files.len()),
    };
    
    let start_time = Instant::now();
    
    for (i, filepath) in files.iter().enumerate() {
        let result = FileHashResult {
            filename: Path::new(filepath).file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string(),
            filepath: filepath.clone(),
            size: get_file_size(filepath),
            hash: String::new(),
            status: String::new(),
            error: None,
        };
        
        let mut final_result = result;
        
        match calculate_file_hash(filepath) {
            Ok(hash) => {
                final_result.hash = hash;
                final_result.status = "SUCCESS".to_string();
                results.successful_hashes += 1;
            }
            Err(_) => {
                final_result.hash = String::new();
                final_result.status = "FAILED".to_string();
                final_result.error = Some("Hash calculation failed".to_string());
                results.failed_hashes += 1;
            }
        }
        
        results.results.push(final_result);
        
        // Progress indicator
        if (i + 1) % 100 == 0 {
            println!("Processed {}/{} files...", i + 1, files.len());
        }
    }
    
    let end_time = start_time.elapsed();
    results.processing_time_seconds = end_time.as_secs_f64();
    results.average_time_per_file_ms = (results.processing_time_seconds / files.len() as f64) * 1000.0;
    
    // Save to file
    let output_data = serde_json::to_string_pretty(&results)?;
    fs::write("hash_results.json", output_data)?;
    
    println!("Results saved to: hash_results.json");
    println!("\nHashing completed!");
    println!("Total files: {}", results.total_files);
    println!("Successful: {}", results.successful_hashes);
    println!("Failed: {}", results.failed_hashes);
    println!("Processing time: {:.3} seconds", results.processing_time_seconds);
    println!("Average time per file: {:.2} ms", results.average_time_per_file_ms);
    
    Ok(())
}

fn main() {
    hello();
    
    if let Err(e) = hash_files_in_directory("../hashfiles") {
        eprintln!("Error: {}", e);
    }
}
