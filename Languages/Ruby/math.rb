# Hello, World!
puts "Hello, World!"

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
  if equation.include?('=')
    equation = equation.split('=')[0].strip
  end
  
  result = evaluate_expression(equation)
  puts "#{equation} = #{result}"
end

def process_file(filename)
  begin
    File.readlines(filename).each do |line|
      # Skip empty lines and markdown headers
      next if line.strip.empty? || line.start_with?('#')
      
      # Handle markdown list items
      start = if line.start_with?('- ')
                line[2..-1]
              else
                line
              end
      
      if start.include?('=')
        solve_equation(start)
      end
    end
  rescue Errno::ENOENT
    puts "Could not open file: #{filename}"
  end
end

puts "\nProcessing math equations..."

process_file("../test_data/math_equations.txt")
process_file("../test_data/math_equations.md")
process_file("../test_data/math_equations.json")
process_file("../test_data/math_equations.yaml")
process_file("../test_data/math_equations")
