# Hello, World!
cat("Hello, World!\n")

library(digest)
library(jsonlite)

calculate_file_hash <- function(filepath) {
  tryCatch({
    content <- readBin(filepath, "raw", n = 1e6)
    digest(content, algo = "sha256", serialize = FALSE)
  }, error = function(e) {
    return("")
  })
}

get_file_size <- function(filepath) {
  tryCatch({
    file.info(filepath)$size %||% 0
  }, error = function(e) {
    return(0)
  })
}

get_txt_files <- function(directory) {
  tryCatch({
    if (!dir.exists(directory)) {
      cat("Directory not found:", directory, "\n")
      return(character(0))
    }
    
    files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
    sort(files)
  }, error = function(e) {
    cat("Error reading directory:", e$message, "\n")
    return(character(0))
  })
}

get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

hash_files_in_directory <- function(directory) {
  files <- get_txt_files(directory)
  
  if (length(files) == 0) {
    cat("No .txt files found in directory:", directory, "\n")
    return()
  }
  
  cat("Found", length(files), "files to hash\n")
  
  results <- list()
  successful_hashes <- 0
  failed_hashes <- 0
  
  start_time <- Sys.time()
  
  for (i in seq_along(files)) {
    filepath <- files[i]
    filename <- basename(filepath)
    size <- get_file_size(filepath)
    
    result <- list(
      filename = filename,
      filepath = filepath,
      size = size,
      algorithm = "sha256"
    )
    
    hash <- calculate_file_hash(filepath)
    if (nchar(hash) > 0) {
      result$hash <- hash
      result$status <- "SUCCESS"
      successful_hashes <- successful_hashes + 1
    } else {
      result$hash <- ""
      result$status <- "FAILED"
      result$error <- "Hash calculation failed"
      failed_hashes <- failed_hashes + 1
    }
    
    results[[i]] <- result
    
    # Progress indicator
    if (i %% 100 == 0) {
      cat("Processed", i, "/", length(files), "files...\n")
    }
  }
  
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  average_time_per_file <- (processing_time / length(files)) * 1000
  
  final_result <- list(
    total_files = length(files),
    successful_hashes = successful_hashes,
    failed_hashes = failed_hashes,
    algorithm = "sha256",
    processing_time_seconds = processing_time,
    average_time_per_file_ms = average_time_per_file,
    timestamp = get_timestamp(),
    directory = directory,
    results = results
  )
  
  # Save to file
  tryCatch({
    write_json(final_result, "hash_results.json", pretty = TRUE, auto_unbox = TRUE)
    cat("Results saved to: hash_results.json\n")
  }, error = function(e) {
    cat("Error saving results:", e$message, "\n")
  })
  
  cat("\nHashing completed!\n")
  cat("Total files:", length(files), "\n")
  cat("Successful:", successful_hashes, "\n")
  cat("Failed:", failed_hashes, "\n")
  cat("Processing time:", round(processing_time, 3), "seconds\n")
  cat("Average time per file:", round(average_time_per_file, 2), "ms\n")
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

hash_files_in_directory("../hashfiles")
