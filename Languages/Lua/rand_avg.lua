-- Hello, World!
print("Hello, World!")

function generate_random_numbers()
    local count = 1000
    local output_dir = "../rand_avg output"
    local output_file = output_dir .. "/random_numbers.txt"
    
    -- Create output directory
    os.execute("mkdir -p '" .. output_dir .. "'")
    
    -- Generate 1000 random numbers
    local random_numbers = {}
    local sum = 0
    
    for i = 1, count do
        local num = math.random(0, 999)
        table.insert(random_numbers, num)
        sum = sum + num
    end
    
    -- Calculate mean
    local mean = sum / count
    
    -- Save to file
    local file = io.open(output_file, "w")
    if file then
        for _, num in ipairs(random_numbers) do
            file:write(num .. "\n")
        end
        file:close()
        
        print("Generated 1000 random numbers")
        print(string.format("Mean: %.2f", mean))
        print("Saved to: " .. output_file)
    else
        print("Could not create output file")
    end
end

generate_random_numbers()
