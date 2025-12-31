<?php
// Hello, World!
echo "Hello, World!\n";

function generateRandomNumbers() {
    $count = 1000;
    $outputDir = "../rand_avg output";
    $outputFile = $outputDir . "/random_numbers.txt";
    
    // Create output directory
    if (!file_exists($outputDir)) {
        mkdir($outputDir, 0755, true);
    }
    
    // Generate 1000 random numbers
    $randomNumbers = [];
    $sum = 0;
    
    for ($i = 0; $i < $count; $i++) {
        $num = rand(0, 999);
        $randomNumbers[] = $num;
        $sum += $num;
    }
    
    // Calculate mean
    $mean = $sum / $count;
    
    // Save to file
    $content = implode("\n", $randomNumbers);
    file_put_contents($outputFile, $content);
    
    echo "Generated 1000 random numbers\n";
    echo "Mean: " . number_format($mean, 2) . "\n";
    echo "Saved to: $outputFile\n";
}

generateRandomNumbers();
?>
