using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Linq;
using Newtonsoft.Json;

public class HashGen
{
    public static void Main(string[] args)
    {
        // Hello, World!
        Console.WriteLine("Hello, World!");
        HashFilesInDirectory("../hashfiles");
    }
    
    public class FileHashResult
    {
        public string Filename { get; set; }
        public string Filepath { get; set; }
        public long Size { get; set; }
        public string Hash { get; set; }
        public string Status { get; set; }
        public string Error { get; set; }
        
        public FileHashResult(string filename, string filepath, long size)
        {
            Filename = filename;
            Filepath = filepath;
            Size = size;
            Status = "SUCCESS";
        }
    }
    
    public class HashResults
    {
        public int TotalFiles { get; set; }
        public int SuccessfulHashes { get; set; }
        public int FailedHashes { get; set; }
        public string Algorithm { get; set; }
        public double ProcessingTimeSeconds { get; set; }
        public double AverageTimePerFileMs { get; set; }
        public string Timestamp { get; set; }
        public string Directory { get; set; }
        public List<FileHashResult> Results { get; set; }
        
        public HashResults(string directory)
        {
            Directory = directory;
            Results = new List<FileHashResult>();
            Algorithm = "sha256";
        }
    }
    
    private static string CalculateFileHash(string filepath)
    {
        try
        {
            using (var sha256 = SHA256.Create())
            using (var fileStream = File.OpenRead(filepath))
            {
                byte[] hashBytes = sha256.ComputeHash(fileStream);
                return BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant();
            }
        }
        catch (Exception)
        {
            return "";
        }
    }
    
    private static long GetFileSize(string filepath)
    {
        try
        {
            return new FileInfo(filepath).Length;
        }
        catch (Exception)
        {
            return 0;
        }
    }
    
    private static List<string> GetTxtFiles(string directory)
    {
        try
        {
            if (!Directory.Exists(directory))
            {
                Console.WriteLine($"Directory not found: {directory}");
                return new List<string>();
            }
            
            var files = Directory.GetFiles(directory, "*.txt")
                                .OrderBy(f => f)
                                .ToList();
            return files;
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error reading directory: {e.Message}");
            return new List<string>();
        }
    }
    
    private static string GetTimestamp()
    {
        return DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
    }
    
    private static void HashFilesInDirectory(string directory)
    {
        var files = GetTxtFiles(directory);
        
        if (files.Count == 0)
        {
            Console.WriteLine($"No .txt files found in directory: {directory}");
            return;
        }
        
        Console.WriteLine($"Found {files.Count} files to hash");
        
        var results = new HashResults(directory);
        results.TotalFiles = files.Count;
        
        var startTime = DateTime.Now;
        
        for (int i = 0; i < files.Count; i++)
        {
            var filepath = files[i];
            var result = new FileHashResult(
                Path.GetFileName(filepath),
                filepath,
                GetFileSize(filepath)
            );
            
            var hash = CalculateFileHash(filepath);
            if (!string.IsNullOrEmpty(hash))
            {
                result.Hash = hash;
                result.Status = "SUCCESS";
                results.SuccessfulHashes++;
            }
            else
            {
                result.Hash = "";
                result.Status = "FAILED";
                result.Error = "Hash calculation failed";
                results.FailedHashes++;
            }
            
            results.Results.Add(result);
            
            // Progress indicator
            if ((i + 1) % 100 == 0)
            {
                Console.WriteLine($"Processed {i + 1}/{files.Count} files...");
            }
        }
        
        var endTime = DateTime.Now;
        results.ProcessingTimeSeconds = (endTime - startTime).TotalSeconds;
        results.AverageTimePerFileMs = (results.ProcessingTimeSeconds / files.Count) * 1000;
        results.Timestamp = GetTimestamp();
        
        // Create JSON output
        var jsonResults = new
        {
            total_files = results.TotalFiles,
            successful_hashes = results.SuccessfulHashes,
            failed_hashes = results.FailedHashes,
            algorithm = results.Algorithm,
            processing_time_seconds = results.ProcessingTimeSeconds,
            average_time_per_file_ms = results.AverageTimePerFileMs,
            timestamp = results.Timestamp,
            directory = results.Directory,
            results = results.Results.Select(r => new
            {
                filename = r.Filename,
                filepath = r.Filepath,
                size = r.Size,
                algorithm = "sha256",
                hash = r.Hash,
                status = r.Status,
                error = r.Error
            })
        };
        
        // Save to file
        try
        {
            string output = JsonConvert.SerializeObject(jsonResults, Formatting.Indented);
            File.WriteAllText("hash_results.json", output);
            Console.WriteLine("Results saved to: hash_results.json");
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error saving results: {e.Message}");
        }
        
        Console.WriteLine("\nHashing completed!");
        Console.WriteLine($"Total files: {results.TotalFiles}");
        Console.WriteLine($"Successful: {results.SuccessfulHashes}");
        Console.WriteLine($"Failed: {results.FailedHashes}");
        Console.WriteLine($"Processing time: {results.ProcessingTimeSeconds:F3} seconds");
        Console.WriteLine($"Average time per file: {results.AverageTimePerFileMs:F2} ms");
    }
}
