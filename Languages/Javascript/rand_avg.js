// Hello, World!
console.log("Hello, World!");

const fs = require('fs');
const path = require('path');

function generateRandomNumbers() {
    const count = 1000;
    const outputDir = "../rand_avg output";
    const outputFile = path.join(outputDir, "random_numbers.txt");
    
    // Create output directory
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }
    
    // Generate 1000 random numbers
    const randomNumbers = [];
    let sum = 0;
    
    for (let i = 0; i < count; i++) {
        const num = Math.floor(Math.random() * 1000);
        randomNumbers.push(num);
        sum += num;
    }
    
    // Calculate mean
    const mean = sum / count;
    
    // Save to file
    try {
        const content = randomNumbers.join('\n');
        fs.writeFileSync(outputFile, content);
        
        console.log("Generated 1000 random numbers");
        console.log(`Mean: ${mean.toFixed(2)}`);
        console.log(`Saved to: ${outputFile}`);
    } catch (error) {
        console.log("Could not create output file");
    }
}

generateRandomNumbers();
