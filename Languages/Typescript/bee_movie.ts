// Hello, World!
console.log("Hello, World!");

import * as fs from 'fs';

interface LetterCount {
    letter: string;
    count: number;
}

function analyzeBeeMovie(): void {
    const scriptPath = '../beemoviescript.txt';
    
    if (!fs.existsSync(scriptPath)) {
        console.log(`File not found: ${scriptPath}`);
        return;
    }
    
    console.log("Bee Movie Script:");
    console.log("--------------------------------------------------");
    
    const content = fs.readFileSync(scriptPath, 'utf8');
    const lines = content.split('\n');
    
    // Print each line
    lines.forEach(line => {
        console.log(line);
    });
    
    console.log("--------------------------------------------------");
    console.log("Analysis complete.");
    
    // Count letter frequencies
    const letterCounts: { [key: string]: number } = {};
    let totalLetters = 0;
    
    for (const line of lines) {
        for (const char of line) {
            if (/[a-zA-Z]/.test(char)) {
                const lower = char.toLowerCase();
                letterCounts[lower] = (letterCounts[lower] || 0) + 1;
                totalLetters++;
            }
        }
    }
    
    if (totalLetters > 0) {
        // Convert to array and sort
        const sorted: LetterCount[] = Object.entries(letterCounts)
            .map(([letter, count]) => ({ letter, count }))
            .sort((a, b) => b.count - a.count)
            .slice(0, 3);
        
        console.log("\nTop 3 most commonly used letters:");
        sorted.forEach((item, index) => {
            console.log(`${index + 1}. '${item.letter}': ${item.count} times`);
        });
    } else {
        console.log("No letters found in the script.");
    }
}

analyzeBeeMovie();
