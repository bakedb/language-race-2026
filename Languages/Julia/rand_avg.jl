# Hello, World!
println("Hello, World!")

function generate_random_numbers()
    count = 1000
    output_dir = "../rand_avg output"
    output_file = joinpath(output_dir, "random_numbers.txt")
    
    # Create output directory
    mkpath(output_dir)
    
    # Generate 1000 random numbers
    random_numbers = rand(0:999, count)
    sum = sum(random_numbers)
    mean = sum / count
    
    # Save to file
    writedlm(output_file, random_numbers)
    
    println("Generated 1000 random numbers")
    println("Mean: $(round(mean, digits=2))")
    println("Saved to: $output_file")
end

generate_random_numbers()
