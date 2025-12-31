import java.io.File
import kotlin.random.Random

// Hello, World!
fun hello() {
    println("Hello, World!")
}

fun generateRandomNumbers() {
    val count = 1000
    val outputDir = "../rand_avg output"
    val outputFile = "$outputDir/random_numbers.txt"
    
    // Create output directory
    File(outputDir).mkdirs()
    
    // Generate 1000 random numbers
    val randomNumbers = mutableListOf<Int>()
    var sum = 0L
    
    repeat(count) {
        val num = Random.nextInt(0, 1000)
        randomNumbers.add(num)
        sum += num
    }
    
    // Calculate mean
    val mean = sum.toDouble() / count
    
    // Save to file
    try {
        File(outputFile).writeText(randomNumbers.joinToString("\n"))
        
        println("Generated 1000 random numbers")
        println("Mean: ${String.format("%.2f", mean)}")
        println("Saved to: $outputFile")
    } catch (e: Exception) {
        println("Could not create output file")
    }
}

fun main() {
    hello()
    generateRandomNumbers()
}
