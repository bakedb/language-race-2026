<?php
// Hello, World!
echo "Hello, World!\n";

class FileHashResult {
    public $filename;
    public $filepath;
    public $size;
    public $hash;
    public $status;
    public $error;
    
    public function __construct($filename, $filepath, $size) {
        $this->filename = $filename;
        $this->filepath = $filepath;
        $this->size = $size;
        $this->status = "SUCCESS";
    }
}

class HashResults {
    public $total_files;
    public $successful_hashes;
    public $failed_hashes;
    public $processing_time_seconds;
    public $average_time_per_file_ms;
    public $timestamp;
    public $directory;
    public $results;
    
    public function __construct($directory) {
        $this->directory = $directory;
        $this->results = [];
    }
}

function calculateFileHash($filepath) {
    try {
        $content = file_get_contents($filepath);
        if ($content === false) {
            return "";
        }
        return hash('sha256', $content);
    } catch (Exception $e) {
        return "";
    }
}

function getFileSize($filepath) {
    try {
        return filesize($filepath);
    } catch (Exception $e) {
        return 0;
    }
}

function getTxtFiles($directory) {
    try {
        if (!is_dir($directory)) {
            echo "Directory not found: $directory\n";
            return [];
        }
        
        $files = glob($directory . "/*.txt");
        sort($files);
        return $files;
    } catch (Exception $e) {
        echo "Error reading directory: " . $e->getMessage() . "\n";
        return [];
    }
}

function getTimestamp() {
    return date('Y-m-d H:i:s');
}

function hashFilesInDirectory($directory) {
    $files = getTxtFiles($directory);
    
    if (empty($files)) {
        echo "No .txt files found in directory: $directory\n";
        return;
    }
    
    echo "Found " . count($files) . " files to hash\n";
    
    $results = new HashResults($directory);
    $results->total_files = count($files);
    
    $start_time = microtime(true);
    
    foreach ($files as $i => $filepath) {
        $result = new FileHashResult(
            basename($filepath),
            $filepath,
            getFileSize($filepath)
        );
        
        $hash = calculateFileHash($filepath);
        if (!empty($hash)) {
            $result->hash = $hash;
            $result->status = "SUCCESS";
            $results->successful_hashes++;
        } else {
            $result->hash = "";
            $result->status = "FAILED";
            $result->error = "Hash calculation failed";
            $results->failed_hashes++;
        }
        
        $results->results[] = $result;
        
        // Progress indicator
        if (($i + 1) % 100 == 0) {
            echo "Processed " . ($i + 1) . "/" . count($files) . " files...\n";
        }
    }
    
    $end_time = microtime(true);
    $results->processing_time_seconds = $end_time - $start_time;
    $results->average_time_per_file_ms = ($results->processing_time_seconds / count($files)) * 1000;
    $results->timestamp = getTimestamp();
    
    // Create JSON output
    $json_results = [
        'total_files' => $results->total_files,
        'successful_hashes' => $results->successful_hashes,
        'failed_hashes' => $results->failed_hashes,
        'algorithm' => 'sha256',
        'processing_time_seconds' => $results->processing_time_seconds,
        'average_time_per_file_ms' => $results->average_time_per_file_ms,
        'timestamp' => $results->timestamp,
        'directory' => $results->directory,
        'results' => array_map(function($result) {
            $json_result = [
                'filename' => $result->filename,
                'filepath' => $result->filepath,
                'size' => $result->size,
                'algorithm' => 'sha256',
                'hash' => $result->hash,
                'status' => $result->status
            ];
            
            if (!empty($result->error)) {
                $json_result['error'] = $result->error;
            }
            
            return $json_result;
        }, $results->results)
    ];
    
    // Save to file
    try {
        $output = json_encode($json_results, JSON_PRETTY_PRINT);
        file_put_contents('hash_results.json', $output);
        echo "Results saved to: hash_results.json\n";
    } catch (Exception $e) {
        echo "Error saving results: " . $e->getMessage() . "\n";
    }
    
    echo "\nHashing completed!\n";
    echo "Total files: " . $results->total_files . "\n";
    echo "Successful: " . $results->successful_hashes . "\n";
    echo "Failed: " . $results->failed_hashes . "\n";
    echo "Processing time: " . number_format($results->processing_time_seconds, 3) . " seconds\n";
    echo "Average time per file: " . number_format($results->average_time_per_file_ms, 2) . " ms\n";
}

hashFilesInDirectory('../hashfiles');
?>
