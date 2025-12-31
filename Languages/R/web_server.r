# Hello, World!
cat("Hello, World!\n")

library(httr)
library(jsonlite)

make_http_request <- function(url) {
  tryCatch({
    response <- GET(url, timeout(5))
    if (status_code(response) == 200) {
      return(content(response, "text", encoding = "UTF-8"))
    } else {
      return("")
    }
  }, error = function(e) {
    return("")
  })
}

test_web_server <- function() {
  base_url <- "http://localhost:3000"
  compare_file <- "../webserver/compare.json"
  output_file <- "test-result.json"
  
  # Load expected hashes
  if (!file.exists(compare_file)) {
    cat("Error: Could not find", compare_file, "\n")
    return()
  }
  
  expected_hashes <- fromJSON(compare_file)
  
  cat("Testing 100 endpoints...\n")
  
  results <- list()
  passed <- 0
  failed <- 0
  
  # Test each endpoint
  for (i in 0:99) {
    endpoint <- paste0("test-", i)
    url <- paste0(base_url, "/", endpoint)
    
    response <- make_http_request(url)
    
    result <- list(
      endpoint = endpoint,
      url = url,
      expected_hash = expected_hashes[[endpoint]] %||% ""
    )
    
    if (response == "") {
      result$status <- "FAILED"
      result$error <- "HTTP request failed"
      failed <- failed + 1
    } else {
      tryCatch({
        data <- fromJSON(response)
        server_hash <- data$hash %||% ""
        result$server_hash <- server_hash
        
        if (server_hash == result$expected_hash) {
          result$status <- "PASSED"
          passed <- passed + 1
        } else {
          result$status <- "FAILED"
          failed <- failed + 1
        }
      }, error = function(e) {
        result$status <- "FAILED"
        result$error <- "JSON parse error"
        failed <- failed + 1
      })
    }
    
    results[[i + 1]] <- result
    
    # Progress indicator
    if ((i + 1) %% 10 == 0) {
      cat("Tested", i + 1, "/100 endpoints...\n")
    }
  }
  
  # Create final result
  total_tests <- length(results)
  success_rate <- round(passed / total_tests * 100, 1)
  
  final_result <- list(
    total_tests = total_tests,
    passed = passed,
    failed = failed,
    success_rate = paste0(success_rate, "%"),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    results = results
  )
  
  # Save results to file
  tryCatch({
    write_json(final_result, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Results saved to:", output_file, "\n")
  }, error = function(e) {
    cat("Error saving results:", e$message, "\n")
  })
  
  cat("\nTest completed!\n")
  cat("Passed:", passed, "/", total_tests, "(", success_rate, "%)\n")
  cat("Failed:", failed, "/", total_tests, "(", (100 - success_rate), "%)\n")
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

test_web_server()
