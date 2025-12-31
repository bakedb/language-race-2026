import Foundation
import CommonCrypto

// Hello, World!
print("Hello, World!")

struct FileHashResult: Codable {
    let filename: String
    let filepath: String
    let size: Int64
    let hash: String
    let status: String
    let error: String?
    
    enum CodingKeys: String, CodingKey {
        case filename, filepath, size, status, error
        case hash
    }
}

struct HashResults: Codable {
    let totalFiles: Int
    let successfulHashes: Int
    let failedHashes: Int
    let algorithm: String
    let processingTimeSeconds: Double
    let averageTimePerFileMs: Double
    let timestamp: String
    let directory: String
    let results: [FileHashResult]
    
    enum CodingKeys: String, CodingKey {
        case totalFiles, successfulHashes, failedHashes, results
        case algorithm, timestamp, directory
        case processingTimeSeconds, averageTimePerFileMs
    }
}

func calculateFileHash(filepath: String) -> String? {
    guard let data = try? Data(contentsOf: URL(fileURLWithPath: filepath)) else {
        return nil
    }
    
    var hash = [UInt8](repeating: 0, count: Int(CC_SHA256_DIGEST_LENGTH))
    
    data.withUnsafeBytes { bytes in
        guard let baseAddress = bytes.baseAddress else { return }
        CC_SHA256(baseAddress, CC_LONG(data.count), &hash)
    }
    
    return hash.map { String(format: "%02x", $0) }.joined()
}

func getFileSize(filepath: String) -> Int64 {
    do {
        let attributes = try FileManager.default.attributesOfItem(atPath: filepath)
        return attributes[.size] as? Int64 ?? 0
    } catch {
        return 0
    }
}

func getTxtFiles(directory: String) -> [String] {
    let fileManager = FileManager.default
    
    guard fileManager.fileExists(atPath: directory) else {
        print("Directory not found: \(directory)")
        return []
    }
    
    do {
        let files = try fileManager.contentsOfDirectory(atPath: directory)
            .filter { $0.hasSuffix(".txt") }
            .map { "\(directory)/\($0)" }
            .sorted()
        return files
    } catch {
        print("Error reading directory: \(error)")
        return []
    }
}

func getTimestamp() -> String {
    let formatter = DateFormatter()
    formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
    return formatter.string(from: Date())
}

func hashFilesInDirectory(directory: String) {
    let files = getTxtFiles(directory: directory)
    
    if files.isEmpty {
        print("No .txt files found in directory: \(directory)")
        return
    }
    
    print("Found \(files.count) files to hash")
    
    var results = [FileHashResult]()
    var successfulHashes = 0
    var failedHashes = 0
    
    let startTime = Date()
    
    for (index, filepath) in files.enumerated() {
        let filename = URL(fileURLWithPath: filepath).lastPathComponent
        let size = getFileSize(filepath: filepath)
        
        var result: FileHashResult
        
        if let hash = calculateFileHash(filepath: filepath) {
            result = FileHashResult(
                filename: filename,
                filepath: filepath,
                size: size,
                hash: hash,
                status: "SUCCESS",
                error: nil
            )
            successfulHashes += 1
        } else {
            result = FileHashResult(
                filename: filename,
                filepath: filepath,
                size: size,
                hash: "",
                status: "FAILED",
                error: "Hash calculation failed"
            )
            failedHashes += 1
        }
        
        results.append(result)
        
        // Progress indicator
        if (index + 1) % 100 == 0 {
            print("Processed \(index + 1)/\(files.count) files...")
        }
    }
    
    let endTime = Date()
    let processingTime = endTime.timeIntervalSince(startTime)
    let averageTimePerFile = (processingTime / Double(files.count)) * 1000
    
    let finalResults = HashResults(
        totalFiles: files.count,
        successfulHashes: successfulHashes,
        failedHashes: failedHashes,
        algorithm: "sha256",
        processingTimeSeconds: processingTime,
        averageTimePerFileMs: averageTimePerFile,
        timestamp: getTimestamp(),
        directory: directory,
        results: results
    )
    
    // Save to file
    let encoder = JSONEncoder()
    encoder.outputFormatting = .prettyPrinted
    
    do {
        let data = try encoder.encode(finalResults)
        try data.write(to: URL(fileURLWithPath: "hash_results.json"))
        print("Results saved to: hash_results.json")
    } catch {
        print("Error saving results: \(error)")
    }
    
    print("\nHashing completed!")
    print("Total files: \(finalResults.totalFiles)")
    print("Successful: \(finalResults.successfulHashes)")
    print("Failed: \(finalResults.failedHashes)")
    print(String(format: "Processing time: %.3f seconds", finalResults.processingTimeSeconds))
    print(String(format: "Average time per file: %.2f ms", finalResults.averageTimePerFileMs))
}

hashFilesInDirectory(directory: "../hashfiles")
