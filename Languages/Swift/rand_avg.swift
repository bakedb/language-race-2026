import Foundation

// Hello, World!
print("Hello, World!")

func generateRandomNumbers() {
    let count = 1000
    let outputDir = "../rand_avg output"
    let outputFile = "\(outputDir)/random_numbers.txt"
    
    // Create output directory
    try? FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    
    // Generate 1000 random numbers
    var randomNumbers: [Int] = []
    var sum = 0
    
    for _ in 0..<count {
        let num = Int.random(in: 0..<1000)
        randomNumbers.append(num)
        sum += num
    }
    
    // Calculate mean
    let mean = Double(sum) / Double(count)
    
    // Save to file
    let content = randomNumbers.map { String($0) }.joined(separator: "\n")
    
    do {
        try content.write(toFile: outputFile, atomically: true, encoding: .utf8)
        print("Generated 1000 random numbers")
        print(String(format: "Mean: %.2f", mean))
        print("Saved to: \(outputFile)")
    } catch {
        print("Could not create output file")
    }
}

generateRandomNumbers()
