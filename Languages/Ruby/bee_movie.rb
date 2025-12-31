# Hello, World!
puts "Hello, World!"

def analyze_bee_movie
  script_path = "../beemoviescript.txt"
  
  unless File.exist?(script_path)
    puts "File not found: #{script_path}"
    return
  end
  
  puts "Bee Movie Script:"
  puts "--------------------------------------------------"
  
  letter_counts = Hash.new(0)
  total_letters = 0
  
  # Read file line by line and print each line
  File.readlines(script_path).each do |line|
    puts line
    
    # Count letters
    line.each_char do |char|
      if char.match?(/[a-zA-Z]/)
        lower = char.downcase
        letter_counts[lower] += 1
        total_letters += 1
      end
    end
  end
  
  puts "--------------------------------------------------"
  puts "Analysis complete."
  
  if total_letters > 0
    # Sort by count (descending)
    sorted = letter_counts.sort_by { |letter, count| -count }
    
    puts "\nTop 3 most commonly used letters:"
    sorted.take(3).each_with_index do |(letter, count), index|
      puts "#{index + 1}. '#{letter}': #{count} times"
    end
  else
    puts "No letters found in the script."
  end
end

analyze_bee_movie
