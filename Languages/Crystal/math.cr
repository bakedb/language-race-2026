# Hello, World!
puts "Hello, World!"

puts "\nProcessing math equations..."

def evaluate_expression(expr)
  # Remove extra whitespace
  expr = expr.strip
  
  # Use eval to solve the math equation (safe in this controlled environment)
  begin
    eval(expr)
  rescue
    "Error"
  end
end

def solve_equation(line)
  equation = line.dup
  
  # Remove "= ?" part
  if equation.includes?('=')
    equation = equation.split('=')[0].strip
  end
  
  result = evaluate_expression(equation)
  puts "#{equation} = #{result}"
end

def process_file(filename)
  unless File.exists?(filename)
    puts "Could not open file: #{filename}"
    return
  end
  
  File.each_line(filename) do |line|
    # Skip empty lines and markdown headers
    next if line.strip.empty? || line.starts_with?('#')
    
    # Handle markdown list items
    start = if line.starts_with?("- ")
              line[2..-1]
            else
              line
            end
    
    if start.includes?('=')
      solve_equation(start)
    end
  end
end

# Process all files
process_file("../test_data/math_equations.txt")
process_file("../test_data/math_equations.md")
process_file("../test_data/math_equations.json")
process_file("../test_data/math_equations.yaml")
process_file("../test_data/math_equations")
