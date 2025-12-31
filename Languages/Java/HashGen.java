import java.io.*;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

public class HashGen {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        hashFilesInDirectory("../hashfiles");
    }
    
    private static class FileHashResult {
        String filename;
        String filepath;
        long size;
        String hash;
        String status;
        String error;
        
        FileHashResult(String filename, String filepath, long size) {
            this.filename = filename;
            this.filepath = filepath;
            this.size = size;
            this.status = "SUCCESS";
        }
    }
    
    private static class HashResults {
        int totalFiles;
        int successfulHashes;
        int failedHashes;
        double processingTimeSeconds;
        double averageTimePerFileMs;
        String timestamp;
        String directory;
        List<FileHashResult> results;
        
        HashResults(String directory) {
            this.directory = directory;
            this.results = new ArrayList<>();
        }
    }
    
    private static String calculateFileHash(String filepath) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] fileBytes = Files.readAllBytes(Paths.get(filepath));
            byte[] hashBytes = digest.digest(fileBytes);
            
            StringBuilder hexString = new StringBuilder();
            for (byte b : hashBytes) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            
            return hexString.toString();
        } catch (NoSuchAlgorithmException | IOException e) {
            return "";
        }
    }
    
    private static long getFileSize(String filepath) {
        try {
            return Files.size(Paths.get(filepath));
        } catch (IOException e) {
            return 0;
        }
    }
    
    private static List<String> getTxtFiles(String directory) {
        try {
            Path dirPath = Paths.get(directory);
            if (!Files.exists(dirPath) || !Files.isDirectory(dirPath)) {
                System.out.println("Directory not found: " + directory);
                return Collections.emptyList();
            }
            
            return Files.list(dirPath)
                    .filter(path -> Files.isRegularFile(path) && path.toString().endsWith(".txt"))
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.out.println("Error reading directory: " + e.getMessage());
            return Collections.emptyList();
        }
    }
    
    private static String getTimestamp() {
        return new java.util.Date().toString();
    }
    
    private static String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
    
    private static void hashFilesInDirectory(String directory) {
        List<String> files = getTxtFiles(directory);
        
        if (files.isEmpty()) {
            System.out.println("No .txt files found in directory: " + directory);
            return;
        }
        
        System.out.println("Found " + files.size() + " files to hash");
        
        HashResults results = new HashResults(directory);
        results.totalFiles = files.size();
        
        long startTime = System.currentTimeMillis();
        
        for (int i = 0; i < files.size(); i++) {
            String filepath = files.get(i);
            FileHashResult result = new FileHashResult(
                Paths.get(filepath).getFileName().toString(),
                filepath,
                getFileSize(filepath)
            );
            
            String hash = calculateFileHash(filepath);
            if (!hash.isEmpty()) {
                result.hash = hash;
                result.status = "SUCCESS";
                results.successfulHashes++;
            } else {
                result.hash = "";
                result.status = "FAILED";
                result.error = "Hash calculation failed";
                results.failedHashes++;
            }
            
            results.results.add(result);
            
            // Progress indicator
            if ((i + 1) % 100 == 0) {
                System.out.println("Processed " + (i + 1) + "/" + files.size() + " files...");
            }
        }
        
        long endTime = System.currentTimeMillis();
        results.processingTimeSeconds = (endTime - startTime) / 1000.0;
        results.averageTimePerFileMs = (results.processingTimeSeconds / files.size()) * 1000.0;
        results.timestamp = getTimestamp();
        
        // Create JSON output
        StringBuilder jsonBuilder = new StringBuilder();
        jsonBuilder.append("{\n");
        jsonBuilder.append("  \"total_files\": ").append(results.totalFiles).append(",\n");
        jsonBuilder.append("  \"successful_hashes\": ").append(results.successfulHashes).append(",\n");
        jsonBuilder.append("  \"failed_hashes\": ").append(results.failedHashes).append(",\n");
        jsonBuilder.append("  \"algorithm\": \"sha256\",\n");
        jsonBuilder.append("  \"processing_time_seconds\": ").append(results.processingTimeSeconds).append(",\n");
        jsonBuilder.append("  \"average_time_per_file_ms\": ").append(results.averageTimePerFileMs).append(",\n");
        jsonBuilder.append("  \"timestamp\": \"").append(results.timestamp).append("\",\n");
        jsonBuilder.append("  \"directory\": \"").append(results.directory).append("\",\n");
        jsonBuilder.append("  \"results\": [\n");
        
        for (int i = 0; i < results.results.size(); i++) {
            FileHashResult result = results.results.get(i);
            jsonBuilder.append("    {\n");
            jsonBuilder.append("      \"filename\": \"").append(escapeJson(result.filename)).append("\",\n");
            jsonBuilder.append("      \"filepath\": \"").append(escapeJson(result.filepath)).append("\",\n");
            jsonBuilder.append("      \"size\": ").append(result.size).append(",\n");
            jsonBuilder.append("      \"algorithm\": \"sha256\",\n");
            jsonBuilder.append("      \"hash\": \"").append(result.hash).append("\",\n");
            jsonBuilder.append("      \"status\": \"").append(result.status).append("\"");
            
            if (result.error != null) {
                jsonBuilder.append(",\n      \"error\": \"").append(escapeJson(result.error)).append("\"");
            }
            
            jsonBuilder.append("\n    }");
            
            if (i < results.results.size() - 1) {
                jsonBuilder.append(",");
            }
            jsonBuilder.append("\n");
        }
        
        jsonBuilder.append("  ]\n");
        jsonBuilder.append("}");
        
        // Save to file
        try (FileWriter file = new FileWriter("hash_results.json")) {
            file.write(jsonBuilder.toString());
            System.out.println("Results saved to: hash_results.json");
        } catch (IOException e) {
            System.out.println("Error saving results: " + e.getMessage());
        }
        
        System.out.println("\nHashing completed!");
        System.out.println("Total files: " + results.totalFiles);
        System.out.println("Successful: " + results.successfulHashes);
        System.out.println("Failed: " + results.failedHashes);
        System.out.printf("Processing time: %.3f seconds\n", results.processingTimeSeconds);
        System.out.printf("Average time per file: %.2f ms\n", results.averageTimePerFileMs);
    }
}
