# Hello, World!
puts "Hello, World!"

def generate_random_numbers
  count = 1000
  output_dir = "../rand_avg output"
  output_file = File.join(output_dir, "random_numbers.txt")
  
  # Create output directory
  Dir.mkdir(output_dir) unless Dir.exists?(output_dir)
  
  # Generate 1000 random numbers
  random_numbers = [] of Int32
  sum = 0
  
  count.times do
    num = rand(1000)
    random_numbers << num
    sum += num
  end
  
  # Calculate mean
  mean = sum.to_f / count
  
  # Save to file
  File.write(output_file, random_numbers.join("\n"))
  
  puts "Generated 1000 random numbers"
  puts "Mean: #{mean.round(2)}"
  puts "Saved to: #{output_file}"
end

generate_random_numbers
