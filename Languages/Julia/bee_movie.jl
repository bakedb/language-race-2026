# Hello, World!
println("Hello, World!")

function analyze_bee_movie()
    script_path = "../beemoviescript.txt"
    
    if !isfile(script_path)
        println("File not found: $script_path")
        return
    end
    
    println("Bee Movie Script:")
    println("--------------------------------------------------")
    
    # Read file line by line and print each line
    lines = readlines(script_path)
    
    # Print each line
    for line in lines
        println(line)
    end
    
    # Count letters
    all_chars = join(lines, "")
    letters = filter(isletter, all_chars)
    lower_letters = lowercase.(letters)
    
    println("--------------------------------------------------")
    println("Analysis complete.")
    
    if length(letters) > 0
        letter_counts = countmap(lower_letters)
        sorted_counts = sort(collect(letter_counts), by = x -> x[2], rev = true)
        top_3 = first(sorted_counts, 3)
        
        println("\nTop 3 most commonly used letters:")
        for (i, (letter, count)) in enumerate(top_3)
            println("$i. '$letter': $count times")
        end
    else
        println("No letters found in the script.")
    end
end

analyze_bee_movie()
