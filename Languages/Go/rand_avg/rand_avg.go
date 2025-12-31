package main

import (
	"fmt"
	"math/rand"
	"os"
	"time"
)

// Hello, World!
func hello() {
	fmt.Println("Hello, World!")
}

func generateRandomNumbers() {
	const count = 1000
	const outputDir = "../../rand_avg output"
	const outputFile = outputDir + "/random_numbers.txt"

	// Create output directory
	if err := os.MkdirAll(outputDir, 0755); err != nil {
		fmt.Println("Could not create output directory")
		return
	}

	// Seed random number generator
	rand.Seed(time.Now().UnixNano())

	// Generate 1000 random numbers
	randomNumbers := make([]int, count)
	sum := 0

	for i := 0; i < count; i++ {
		num := rand.Intn(1000)
		randomNumbers[i] = num
		sum += num
	}

	// Calculate mean
	mean := float64(sum) / float64(count)

	// Save to file
	file, err := os.Create(outputFile)
	if err != nil {
		fmt.Println("Could not create output file")
		return
	}
	defer file.Close()

	for _, num := range randomNumbers {
		_, err := file.WriteString(fmt.Sprintf("%d\n", num))
		if err != nil {
			fmt.Println("Error writing to file")
			return
		}
	}

	fmt.Println("Generated 1000 random numbers")
	fmt.Printf("Mean: %.2f\n", mean)
	fmt.Printf("Saved to: %s\n", outputFile)
}

func main() {
	hello()
	generateRandomNumbers()
}
