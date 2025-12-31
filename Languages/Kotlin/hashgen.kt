import java.io.File
import java.security.MessageDigest
import java.util.*
import org.json.JSONObject
import org.json.JSONArray

// Hello, World!
fun hello() {
    println("Hello, World!")
}

data class FileHashResult(
    val filename: String,
    val filepath: String,
    val size: Long,
    val hash: String,
    val status: String,
    val error: String?
)

data class HashResults(
    val totalFiles: Int,
    val successfulHashes: Int,
    val failedHashes: Int,
    val algorithm: String,
    val processingTimeSeconds: Double,
    val averageTimePerFileMs: Double,
    val timestamp: String,
    val directory: String,
    val results: List<FileHashResult>
)

fun calculateFileHash(filepath: String): String {
    return try {
        val file = File(filepath)
        val bytes = file.readBytes()
        val digest = MessageDigest.getInstance("SHA-256")
        val hash = digest.digest(bytes)
        
        hash.joinToString("") { "%02x".format(it) }
    } catch (e: Exception) {
        ""
    }
}

fun getFileSize(filepath: String): Long {
    return try {
        File(filepath).length()
    } catch (e: Exception) {
        0L
    }
}

fun getTxtFiles(directory: String): List<String> {
    return try {
        val dir = File(directory)
        if (!dir.exists() || !dir.isDirectory) {
            println("Directory not found: $directory")
            return emptyList()
        }
        
        dir.listFiles { file -> file.isFile && file.name.endsWith(".txt") }
            ?.map { it.absolutePath }
            ?.sorted()
            ?: emptyList()
    } catch (e: Exception) {
        println("Error reading directory: ${e.message}")
        emptyList()
    }
}

fun getTimestamp(): String {
    return Date().toString()
}

fun hashFilesInDirectory(directory: String) {
    val files = getTxtFiles(directory)
    
    if (files.isEmpty()) {
        println("No .txt files found in directory: $directory")
        return
    }
    
    println("Found ${files.size} files to hash")
    
    val results = mutableListOf<FileHashResult>()
    var successfulHashes = 0
    var failedHashes = 0
    
    val startTime = System.currentTimeMillis()
    
    files.forEachIndexed { index, filepath ->
        val file = File(filepath)
        val result = FileHashResult(
            filename = file.name,
            filepath = filepath,
            size = getFileSize(filepath),
            hash = "",
            status = "",
            error = null
        )
        
        val hash = calculateFileHash(filepath)
        val finalResult = if (hash.isNotEmpty()) {
            result.copy(
                hash = hash,
                status = "SUCCESS"
            ).also { successfulHashes++ }
        } else {
            result.copy(
                hash = "",
                status = "FAILED",
                error = "Hash calculation failed"
            ).also { failedHashes++ }
        }
        
        results.add(finalResult)
        
        // Progress indicator
        if ((index + 1) % 100 == 0) {
            println("Processed ${index + 1}/${files.size} files...")
        }
    }
    
    val endTime = System.currentTimeMillis()
    val processingTime = (endTime - startTime) / 1000.0
    val averageTimePerFile = (processingTime / files.size) * 1000
    
    val finalResults = HashResults(
        totalFiles = files.size,
        successfulHashes = successfulHashes,
        failedHashes = failedHashes,
        algorithm = "sha256",
        processingTimeSeconds = processingTime,
        averageTimePerFileMs = averageTimePerFile,
        timestamp = getTimestamp(),
        directory = directory,
        results = results
    )
    
    // Create JSON output
    val jsonResults = JSONObject().apply {
        put("total_files", finalResults.totalFiles)
        put("successful_hashes", finalResults.successfulHashes)
        put("failed_hashes", finalResults.failedHashes)
        put("algorithm", finalResults.algorithm)
        put("processing_time_seconds", finalResults.processingTimeSeconds)
        put("average_time_per_file_ms", finalResults.averageTimePerFileMs)
        put("timestamp", finalResults.timestamp)
        put("directory", finalResults.directory)
        
        val resultsArray = JSONArray()
        finalResults.results.forEach { result ->
            val resultJson = JSONObject().apply {
                put("filename", result.filename)
                put("filepath", result.filepath)
                put("size", result.size)
                put("algorithm", "sha256")
                put("hash", result.hash)
                put("status", result.status)
                result.error?.let { put("error", it) }
            }
            resultsArray.put(resultJson)
        }
        put("results", resultsArray)
    }
    
    // Save to file
    try {
        val outputFile = File("hash_results.json")
        outputFile.writeText(jsonResults.toString(2))
        println("Results saved to: hash_results.json")
    } catch (e: Exception) {
        println("Error saving results: ${e.message}")
    }
    
    println("\nHashing completed!")
    println("Total files: ${finalResults.totalFiles}")
    println("Successful: ${finalResults.successfulHashes}")
    println("Failed: ${finalResults.failedHashes}")
    println("Processing time: ${String.format("%.3f", finalResults.processingTimeSeconds)} seconds")
    println("Average time per file: ${String.format("%.2f", finalResults.averageTimePerFileMs)} ms")
}

fun main() {
    hello()
    hashFilesInDirectory("../hashfiles")
}
