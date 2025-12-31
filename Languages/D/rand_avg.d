import std.stdio;
import std.random;
import std.file;
import std.path;
import std.format;

// Hello, World!
void hello() {
    writeln("Hello, World!");
}

void generateRandomNumbers() {
    enum count = 1000;
    string outputDir = "../rand_avg output";
    string outputFile = outputDir ~ "/random_numbers.txt";
    
    // Create output directory
    if (!exists(outputDir)) {
        mkdirRecurse(outputDir);
    }
    
    // Random number generator
    auto rng = Random(unpredictableSeed);
    
    // Generate 1000 random numbers
    int[] randomNumbers;
    randomNumbers.length = count;
    long sum = 0;
    
    foreach (ref num; randomNumbers) {
        num = uniform(0, 1000, rng);
        sum += num;
    }
    
    // Calculate mean
    double mean = cast(double)sum / count;
    
    // Save to file
    auto file = File(outputFile, "w");
    foreach (num; randomNumbers) {
        file.writefln("%s", num);
    }
    file.close();
    
    writeln("Generated 1000 random numbers");
    writefln("Mean: %.2f", mean);
    writeln("Saved to: ", outputFile);
}

void main() {
    hello();
    generateRandomNumbers();
}
