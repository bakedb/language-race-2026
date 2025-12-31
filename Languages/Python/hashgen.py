print("Hello, World!")

import os
import hashlib
import json
import time
from pathlib import Path

def calculate_file_hash(file_path, algorithm='sha256'):
    """Calculate hash of a file using specified algorithm"""
    hash_obj = hashlib.new(algorithm)
    
    try:
        with open(file_path, 'rb') as f:
            # Read file in chunks to handle large files efficiently
            for chunk in iter(lambda: f.read(4096), b""):
                hash_obj.update(chunk)
        return hash_obj.hexdigest()
    except Exception as e:
        print(f"Error hashing {file_path}: {e}")
        return None

def hash_files_in_directory(directory_path):
    """Hash all files in the specified directory"""
    hashfiles_dir = directory_path
    output_file = "hash_results.json"
    
    if not os.path.exists(hashfiles_dir):
        print(f"Directory not found: {hashfiles_dir}")
        return
    
    print(f"Hashing all files in {hashfiles_dir}...")
    
    # Get all .txt files in the directory
    files = []
    for file_path in Path(hashfiles_dir).glob("*.txt"):
        files.append(file_path)
    
    files.sort()  # Sort for consistent ordering
    
    if not files:
        print("No .txt files found in directory")
        return
    
    print(f"Found {len(files)} files to hash")
    
    results = []
    successful_hashes = 0
    failed_hashes = 0
    
    start_time = time.time()
    
    for i, file_path in enumerate(files):
        file_hash = calculate_file_hash(file_path)
        
        result = {
            "filename": file_path.name,
            "filepath": str(file_path),
            "size": file_path.stat().st_size,
            "algorithm": "sha256"
        }
        
        if file_hash:
            result["hash"] = file_hash
            result["status"] = "SUCCESS"
            successful_hashes += 1
        else:
            result["hash"] = ""
            result["status"] = "FAILED"
            result["error"] = "Hash calculation failed"
            failed_hashes += 1
        
        results.append(result)
        
        # Progress indicator
        if (i + 1) % 100 == 0:
            print(f"Processed {i + 1}/{len(files)} files...")
    
    end_time = time.time()
    processing_time = end_time - start_time
    
    # Create final result
    final_result = {
        "total_files": len(files),
        "successful_hashes": successful_hashes,
        "failed_hashes": failed_hashes,
        "algorithm": "sha256",
        "processing_time_seconds": round(processing_time, 3),
        "average_time_per_file_ms": round((processing_time / len(files)) * 1000, 2),
        "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
        "directory": hashfiles_dir,
        "results": results
    }
    
    # Save results to file
    try:
        with open(output_file, 'w') as f:
            json.dump(final_result, f, indent=2)
        print(f"Results saved to: {output_file}")
    except Exception as e:
        print(f"Error saving results: {e}")
    
    print(f"\nHashing completed!")
    print(f"Total files: {len(files)}")
    print(f"Successful: {successful_hashes}")
    print(f"Failed: {failed_hashes}")
    print(f"Processing time: {processing_time:.3f} seconds")
    print(f"Average time per file: {(processing_time / len(files) * 1000):.2f} ms")

if __name__ == "__main__":
    hash_files_in_directory("../hashfiles")
