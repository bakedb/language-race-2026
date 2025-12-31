# Hello, World!
IO.puts "Hello, World!"

def analyze_bee_movie do
  script_path = "../beemoviescript.txt"
  
  unless File.exists?(script_path) do
    IO.puts "File not found: #{script_path}"
    return
  end
  
  IO.puts "Bee Movie Script:"
  IO.puts "--------------------------------------------------"
  
  # Read file line by line and print each line
  content = File.read!(script_path)
  lines = String.split(content, "\n")
  
  # Count letters
  letter_counts = 
    content
    |> String.graphemes()
    |> Enum.reduce(%{}, fn char, acc ->
      if String.match?(char, ~r/[a-zA-Z]/) do
        lower = String.downcase(char)
        Map.update(acc, lower, 1, &(&1 + 1))
      else
        acc
      end
    end)
  
  # Print each line
  Enum.each(lines, &IO.puts/1)
  
  IO.puts "--------------------------------------------------"
  IO.puts "Analysis complete."
  
  if map_size(letter_counts) > 0 do
    # Sort by count (descending) and take top 3
    top_3 = 
      letter_counts
      |> Enum.sort_by(fn {_letter, count} -> -count end)
      |> Enum.take(3)
    
    IO.puts "\nTop 3 most commonly used letters:"
    Enum.with_index(top_3, 1)
    |> Enum.each(fn {index, {letter, count}} ->
      IO.puts "#{index}. '#{letter}': #{count} times"
    end)
  else
    IO.puts "No letters found in the script."
  end
end

analyze_bee_movie()
