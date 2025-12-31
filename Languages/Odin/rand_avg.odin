package main

import "core:fmt"
import "core:os"
import "core:math/rand"
import "core:time"

// Hello, World!
hello :: proc() {
	fmt.println("Hello, World!")
}

generate_random_numbers :: proc() {
	count := 1000
	output_dir := "../rand_avg output"
	output_file := output_dir + "/random_numbers.txt"
	
	// Create output directory
	os.create_directory_all(output_dir) or_return
	
	// Random number generator
	rng := rand.init(uint64(time.now()))
	
	// Generate 1000 random numbers
	sum := 0
	
	file_handle := os.create(output_file) or_return
	defer os.close(file_handle)
	
	for i in 0..<count {
		num := rand.int_max(rng, 999)
		sum += num
		fmt.fprintln(file_handle, num)
	}
	
	// Calculate mean
	mean := f32(sum) / f32(count)
	
	fmt.println("Generated 1000 random numbers")
	fmt.printf("Mean: %.2f\n", mean)
	fmt.printf("Saved to: %s\n", output_file)
}

main :: proc() {
	hello()
	generate_random_numbers()
}
