# Hello, World!
cat("Hello, World!\n")

cat("\nProcessing math equations...\n")

evaluate_expression <- function(expr) {
  # Remove extra whitespace
  expr <- trimws(expr)
  
  # Use eval to solve the math equation (safe in this controlled environment)
  tryCatch({
    eval(parse(text = expr))
  }, error = function(e) {
    "Error"
  })
}

solve_equation <- function(line) {
  equation <- line
  
  # Remove "= ?" part
  if (grepl("=", equation)) {
    equation <- strsplit(equation, "=")[[1]][1]
    equation <- trimws(equation)
  }
  
  result <- evaluate_expression(equation)
  cat(paste(equation, "=", result, "\n"))
}

process_file <- function(filename) {
  if (!file.exists(filename)) {
    cat("Could not open file:", filename, "\n")
    return()
  }
  
  lines <- readLines(filename)
  
  for (line in lines) {
    # Skip empty lines and markdown headers
    if (nchar(trimws(line)) == 0 || grepl("^#", line)) {
      next
    }
    
    # Handle markdown list items
    start <- if (grepl("^- ", line)) {
      substring(line, 3)
    } else {
      line
    }
    
    if (grepl("=", start)) {
      solve_equation(start)
    }
  }
}

# Process all files
process_file("../test_data/math_equations.txt")
process_file("../test_data/math_equations.md")
process_file("../test_data/math_equations.json")
process_file("../test_data/math_equations.yaml")
process_file("../test_data/math_equations")
