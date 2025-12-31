# Hello, World!
cat("Hello, World!\n")

analyze_bee_movie <- function() {
  script_path <- "../beemoviescript.txt"
  
  if (!file.exists(script_path)) {
    cat("File not found:", script_path, "\n")
    return()
  }
  
  cat("Bee Movie Script:\n")
  cat("--------------------------------------------------\n")
  
  # Read file line by line and print each line
  lines <- readLines(script_path)
  
  # Print each line
  for (line in lines) {
    cat(line, "\n")
  }
  
  cat("--------------------------------------------------\n")
  cat("Analysis complete.\n")
  
  # Count letters
  all_text <- paste(lines, collapse = "")
  letters <- strsplit(all_text, NULL)[[1]]
  letters <- letters[grepl("[a-zA-Z]", letters)]
  lower_letters <- tolower(letters)
  
  if (length(letters) > 0) {
    letter_counts <- table(lower_letters)
    sorted_counts <- sort(letter_counts, decreasing = TRUE)
    top_3 <- head(sorted_counts, 3)
    
    cat("\nTop 3 most commonly used letters:\n")
    for (i in 1:length(top_3)) {
      letter <- names(top_3)[i]
      count <- top_3[i]
      cat(i, ". '", letter, "': ", count, " times\n", sep = "")
    }
  } else {
    cat("No letters found in the script.\n")
  }
}

analyze_bee_movie()
