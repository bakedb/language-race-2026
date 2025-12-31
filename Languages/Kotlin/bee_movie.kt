import java.io.File

// Hello, World!
fun hello() {
    println("Hello, World!")
}

data class LetterCount(val letter: Char, val count: Int)

fun analyzeBeeMovie() {
    val scriptPath = "../beemoviescript.txt"
    val file = File(scriptPath)
    
    if (!file.exists()) {
        println("File not found: $scriptPath")
        return
    }
    
    println("Bee Movie Script:")
    println("--------------------------------------------------")
    
    val letterCounts = mutableMapOf<Char, Int>()
    var totalLetters = 0
    
    // Read file line by line and print each line
    file.forEachLine { line ->
        println(line)
        
        // Count letters
        for (char in line) {
            if (char.isLetter()) {
                val lower = char.lowercaseChar()
                letterCounts[lower] = letterCounts.getOrDefault(lower, 0) + 1
                totalLetters++
            }
        }
    }
    
    println("--------------------------------------------------")
    println("Analysis complete.")
    
    if (totalLetters > 0) {
        // Sort by count (descending)
        val sorted = letterCounts.entries
            .sortedByDescending { it.value }
            .take(3)
        
        println("\nTop 3 most commonly used letters:")
        sorted.forEachIndexed { index, entry ->
            println("${index + 1}. '${entry.key}': ${entry.value} times")
        }
    } else {
        println("No letters found in the script.")
    }
}

fun main() {
    hello()
    analyzeBeeMovie()
}
