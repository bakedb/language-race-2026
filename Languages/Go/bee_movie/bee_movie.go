package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"unicode"
)

// Hello, World!
func hello() {
	fmt.Println("Hello, World!")
}

type LetterCount struct {
	Letter rune
	Count  int
}

func analyzeBeeMovie() {
	scriptPath := "../beemoviescript.txt"

	file, err := os.Open(scriptPath)
	if err != nil {
		fmt.Printf("File not found: %s\n", scriptPath)
		return
	}
	defer file.Close()

	fmt.Println("Bee Movie Script:")
	fmt.Println("--------------------------------------------------")

	letterCounts := make(map[rune]int)
	totalLetters := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Println(line)

		// Count letters
		for _, char := range line {
			if unicode.IsLetter(char) {
				lower := unicode.ToLower(char)
				letterCounts[lower]++
				totalLetters++
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}

	fmt.Println("--------------------------------------------------")
	fmt.Println("Analysis complete.")

	if totalLetters > 0 {
		// Convert map to slice and sort
		var counts []LetterCount
		for letter, count := range letterCounts {
			counts = append(counts, LetterCount{letter, count})
		}

		sort.Slice(counts, func(i, j int) bool {
			return counts[i].Count > counts[j].Count
		})

		fmt.Println("\nTop 3 most commonly used letters:")
		for i := 0; i < 3 && i < len(counts); i++ {
			fmt.Printf("%d. '%c': %d times\n", i+1, counts[i].Letter, counts[i].Count)
		}
	} else {
		fmt.Println("No letters found in the script.")
	}
}

func main() {
	hello()
	analyzeBeeMovie()
}
