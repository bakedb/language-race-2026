package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:ascii"

LetterCount :: struct {
    letter: u8,
    count:  int,
}

// Hello, World!
hello :: proc() {
    fmt.println("Hello, World!")
}

analyze_bee_movie :: proc() {
    script_path := "../beemoviescript.txt"
    
    data, ok := os.read_file(script_path)
    if !ok {
        fmt.printf("File not found: %s\n", script_path)
        return
    }
    
    fmt.println("Bee Movie Script:")
    fmt.println("--------------------------------------------------")
    
    letter_counts: [26]int
    total_letters: int
    
    lines := strings.split(data, "\n")
    
    // Print each line and count letters
    for line in lines {
        fmt.println(line)
        
        // Count letters
        for char in line {
            if ascii.is_alpha(byte(char)) {
                lower := ascii.to_lower(byte(char))
                letter_counts[lower - 'a'] += 1
                total_letters += 1
            }
        }
    }
    
    fmt.println("--------------------------------------------------")
    fmt.println("Analysis complete.")
    
    if total_letters > 0 {
        // Create array of LetterCount structs
        counts: [26]LetterCount
        for i in 0..<26 {
            counts[i] = LetterCount{letter = 'a' + u8(i), count = letter_counts[i]}
        }
        
        // Sort by count (descending)
        sort_by_field(counts[:], LetterCount.count, .descending)
        
        fmt.println("\nTop 3 most commonly used letters:")
        for i in 0..<3 {
            if counts[i].count > 0 {
                fmt.printf("%d: '%c': %d times\n", i + 1, counts[i].letter, counts[i].count)
            }
        }
    } else {
        fmt.println("No letters found in the script.")
    }
}

main :: proc() {
    hello()
    analyze_bee_movie()
}
