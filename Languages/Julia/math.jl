# Hello, World!
println("Hello, World!")

println("\nProcessing math equations...")

function evaluate_expression(expr)
    # Remove extra whitespace
    expr = strip(expr)
    
    # Use eval to solve the math equation (safe in this controlled environment)
    try
        return eval(Meta.parse(expr))
    catch
        return "Error"
    end
end

function solve_equation(line)
    equation = line
    
    # Remove "= ?" part
    if occursin("=", equation)
        equation = split(equation, "=")[1]
        equation = strip(equation)
    end
    
    result = evaluate_expression(equation)
    println("$equation = $result")
end

function process_file(filename)
    if !isfile(filename)
        println("Could not open file: $filename")
        return
    end
    
    lines = readlines(filename)
    
    for line in lines
        # Skip empty lines and markdown headers
        if isempty(strip(line)) || startswith(line, "#")
            continue
        end
        
        # Handle markdown list items
        start = if startswith(line, "- ")
            line[3:end]
        else
            line
        end
        
        if occursin("=", start)
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
