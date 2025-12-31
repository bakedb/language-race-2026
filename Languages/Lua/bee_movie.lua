-- Hello, World!
print("Hello, World!")

function analyze_bee_movie()
    local script_path = "../beemoviescript.txt"
    
    local file = io.open(script_path, "r")
    if not file then
        print("File not found: " .. script_path)
        return
    end
    
    print("Bee Movie Script:")
    print("--------------------------------------------------")
    
    local letter_counts = {}
    local total_letters = 0
    
    -- Read file line by line and print each line
    for line in file:lines() do
        print(line)
        
        -- Count letters
        for char in line:gmatch(".") do
            if char:match("[a-zA-Z]") then
                local lower = char:lower()
                letter_counts[lower] = (letter_counts[lower] or 0) + 1
                total_letters = total_letters + 1
            end
        end
    end
    
    file:close()
    
    print("--------------------------------------------------")
    print("Analysis complete.")
    
    if total_letters > 0 then
        -- Convert to array and sort
        local sorted = {}
        for letter, count in pairs(letter_counts) do
            table.insert(sorted, {letter = letter, count = count})
        end
        
        table.sort(sorted, function(a, b) return a.count > b.count end)
        
        print("\nTop 3 most commonly used letters:")
        for i = 1, math.min(3, #sorted) do
            local item = sorted[i]
            print(string.format("%d. '%s': %d times", i, item.letter, item.count))
        end
    else
        print("No letters found in the script.")
    end
end

analyze_bee_movie()
