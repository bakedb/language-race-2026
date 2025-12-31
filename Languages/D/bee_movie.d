import std.stdio;
import std.stdio;
import std.algorithm;
import std.ascii;
import std.path;
import std.file;

struct LetterCount {
    char letter;
    int count;
}

// Hello, World!
void hello() {
    writeln("Hello, World!");
}

void analyzeBeeMovie() {
    string scriptPath = "../beemoviescript.txt";
    
    if (!exists(scriptPath)) {
        writeln("File not found: ", scriptPath);
        return;
    }
    
    writeln("Bee Movie Script:");
    writeln("--------------------------------------------------");
    
    int[26] letterCounts;
    letterCounts[] = 0;
    int totalLetters = 0;
    
    // Read file line by line and print each line
    File file = File(scriptPath, "r");
    foreach (line; file.byLine()) {
        writeln(line);
        
        // Count letters
        foreach (char c; line) {
            if (isAlpha(c)) {
                char lower = toLower(c);
                letterCounts[lower - 'a']++;
                totalLetters++;
            }
        }
    }
    file.close();
    
    writeln("--------------------------------------------------");
    writeln("Analysis complete.");
    
    if (totalLetters > 0) {
        // Create array of LetterCount structs
        LetterCount[26] counts;
        foreach (i, ref count; counts) {
            count = LetterCount(cast(char)('a' + i), letterCounts[i]);
        }
        
        // Sort by count (descending)
        sort!((a, b) => a.count > b.count)(counts);
        
        writeln("\nTop 3 most commonly used letters:");
        foreach (i; 0..3) {
            if (counts[i].count > 0) {
                writefln("%d. '%s': %d times", i + 1, counts[i].letter, counts[i].count);
            }
        }
    } else {
        writeln("No letters found in the script.");
    }
}

void main() {
    hello();
    analyzeBeeMovie();
}
