-- Hello, World!
print("Hello, World!")

print("\nProcessing math equations...")

function evaluate_expression(expr)
    -- Remove extra whitespace
    expr = expr:match("^%s*(.-)%s*$") or expr
    
    -- Use load to evaluate the expression safely
    local func, err = load("return " .. expr)
    if func then
        local success, result = pcall(func)
        if success then
            return result
        end
    end
    return "Error"
end

function solve_equation(line)
    local equation = line
    
    -- Remove "= ?" part
    local equals_pos = equation:find("=")
    if equals_pos then
        equation = equation:sub(1, equals_pos - 1)
    end
    
    -- Trim whitespace
    equation = equation:match("^%s*(.-)%s*$") or equation
    
    local result = evaluate_expression(equation)
    print(equation .. " = " .. tostring(result))
end

function process_file(filename)
    local file = io.open(filename, "r")
    if not file then
        print("Could not open file: " .. filename)
        return
    end
    
    for line in file:lines() do
        -- Skip empty lines and markdown headers
        if line ~= "" and not line:match("^#") then
            -- Handle markdown list items
            local start = line
            if line:match("^%- ") then
                start = line:sub(3)
            end
            
            if start:find("=") then
                solve_equation(start)
            end
        end
    end
    
    file:close()
end

-- Process all files
process_file("../test_data/math_equations.txt")
process_file("../test_data/math_equations.md")
process_file("../test_data/math_equations.json")
process_file("../test_data/math_equations.yaml")
process_file("../test_data/math_equations")
