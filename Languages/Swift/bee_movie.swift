import Foundation

// Hello, World!
print("Hello, World!")

struct LetterCount {
    let letter: Character
    let count: Int
}

func analyzeBeeMovie() {
    let scriptPath = "../beemoviescript.txt"
    
    guard let content = try? String(contentsOfFile: scriptPath) else {
        print("File not found: \(scriptPath)")
        return
    }
    
    print("Bee Movie Script:")
    print("--------------------------------------------------")
    
    let lines = content.components(separatedBy: .newlines)
    var letterCounts: [Character: Int] = [:]
    var totalLetters = 0
    
    // Print each line and count letters
    for line in lines {
        print(line)
        
        for char in line {
            if char.isLetter {
                let lower = char.lowercased()
                letterCounts[lower, default: 0] += 1
                totalLetters += 1
            }
        }
    }
    
    print("--------------------------------------------------")
    print("Analysis complete.")
    
    if totalLetters > 0 {
        // Convert to array and sort
        let sorted = letterCounts.map { (letter, count) in
            LetterCount(letter: letter, count: count)
        }.sorted { $0.count > $1.count }
        
        print("\nTop 3 most commonly used letters:")
        for (index, item) in sorted.prefix(3).enumerated() {
            print("\(index + 1). '\(item.letter)': \(item.count) times")
        }
    } else {
        print("No letters found in the script.")
    }
}

analyzeBeeMovie()
