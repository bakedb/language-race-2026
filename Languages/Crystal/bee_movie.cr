# Hello, World!
puts "Hello, World!"

def analyze_bee_movie
  script_path = "../beemoviescript.txt"
  
  unless File.exists?(script_path)
    puts "File not found: #{script_path}"
    return
  end
  
  puts "Bee Movie Script:"
  puts "--------------------------------------------------"
  
  letter_counts = Hash(Char, Int32).new(0)
  total_letters = 0
  
  # Read file line by line and print each line
  File.each_line(script_path) do |line|
    puts line
    
    # Count letters
    line.each_char do |char|
      if char.ascii_letter?
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
    sorted = letter_counts.to_a.sort_by { |letter, count| -count }
    
    puts "\nTop 3 most commonly used letters:"
    sorted.first(3).each_with_index do |(letter, count), index|
      puts "#{index + 1}. '#{letter}': #{count} times"
    end
  else
    puts "No letters found in the script."
  end
end

analyze_bee_movie
