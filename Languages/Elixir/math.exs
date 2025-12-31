# Hello, World!
IO.puts "Hello, World!"

IO.puts "\nProcessing math equations..."

def evaluate_expression(expr) do
  # Remove extra whitespace
  expr = String.trim(expr)
  
  # Use Code.eval_string to evaluate the expression
  try do
    {result, _} = Code.eval_string(expr)
    result
  rescue
    _ -> "Error"
  end
end

def solve_equation(line) do
  equation = line
  
  # Remove "= ?" part
  equation = 
    if String.contains?(equation, "=") do
      String.split(equation, "=") |> List.first() |> String.trim()
    else
      equation
    end
  
  result = evaluate_expression(equation)
  IO.puts("#{equation} = #{result}")
end

def process_file(filename) do
  unless File.exists?(filename) do
    IO.puts "Could not open file: #{filename}"
    :ok
  end
  
  File.stream!(filename)
  |> Enum.each(fn line ->
    # Skip empty lines and markdown headers
    if line != "\n" and not String.starts_with?(String.trim(line), "#") do
      # Handle markdown list items
      start = 
        if String.starts_with?(line, "- ") do
          String.slice(line, 2..-1)
        else
          line
        end
      
      if String.contains?(start, "=") do
        solve_equation(start)
      end
    end
  end)
end

# Process all files
process_file("../test_data/math_equations.txt")
process_file("../test_data/math_equations.md")
process_file("../test_data/math_equations.json")
process_file("../test_data/math_equations.yaml")
process_file("../test_data/math_equations")
