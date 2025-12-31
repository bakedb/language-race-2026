<?php
// Hello, World!
echo "Hello, World!\n";

function analyzeBeeMovie() {
    $scriptPath = "../beemoviescript.txt";
    
    if (!file_exists($scriptPath)) {
        echo "File not found: $scriptPath\n";
        return;
    }
    
    echo "Bee Movie Script:\n";
    echo "--------------------------------------------------\n";
    
    $letterCounts = array_fill_keys(range('a', 'z'), 0);
    $totalLetters = 0;
    
    // Read file line by line and print each line
    $lines = file($scriptPath, FILE_IGNORE_NEW_LINES);
    foreach ($lines as $line) {
        echo $line . "\n";
        
        // Count letters
        for ($i = 0; $i < strlen($line); $i++) {
            $char = $line[$i];
            if (ctype_alpha($char)) {
                $lower = strtolower($char);
                $letterCounts[$lower]++;
                $totalLetters++;
            }
        }
    }
    
    echo "--------------------------------------------------\n";
    echo "Analysis complete.\n";
    
    if ($totalLetters > 0) {
        // Sort by count (descending)
        arsort($letterCounts);
        
        echo "\nTop 3 most commonly used letters:\n";
        $count = 0;
        foreach ($letterCounts as $letter => $count) {
            if ($count > 0 && $count < 3) {
                echo ($count + 1) . ". '$letter': $count times\n";
                $count++;
            }
            if ($count >= 3) break;
        }
    } else {
        echo "No letters found in the script.\n";
    }
}

analyzeBeeMovie();
?>
