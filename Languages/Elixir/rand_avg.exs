# Hello, World!
IO.puts "Hello, World!"

def generate_random_numbers do
  count = 1000
  output_dir = "../rand_avg output"
  output_file = Path.join(output_dir, "random_numbers.txt")
  
  # Create output directory
  File.mkdir_p!(output_dir)
  
  # Generate 1000 random numbers
  random_numbers = 
    for _ <- 1..count do
      :rand.uniform(1000) - 1
    end
  
  sum = Enum.sum(random_numbers)
  mean = sum / count
  
  # Save to file
  content = Enum.join(random_numbers, "\n")
  File.write!(output_file, content)
  
  IO.puts "Generated 1000 random numbers"
  IO.puts "Mean: #{Float.round(mean, 2)}"
  IO.puts "Saved to: #{output_file}"
end

# Seed random number generator
:rand.seed(:exs1024, :erlang.timestamp())

generate_random_numbers()
