# Hello, World!
cat("Hello, World!\n")

generate_random_numbers <- function() {
  count <- 1000
  output_dir <- "../rand_avg output"
  output_file <- file.path(output_dir, "random_numbers.txt")
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate 1000 random numbers
  random_numbers <- sample(0:999, count, replace = TRUE)
  sum <- sum(random_numbers)
  mean <- sum / count
  
  # Save to file
  writeLines(as.character(random_numbers), output_file)
  
  cat("Generated 1000 random numbers\n")
  cat(paste("Mean:", round(mean, 2), "\n"))
  cat(paste("Saved to:", output_file, "\n"))
}

generate_random_numbers()
