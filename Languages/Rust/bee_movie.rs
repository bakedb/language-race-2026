use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

// Hello, World!
fn hello() {
    println!("Hello, World!");
}

#[derive(Debug)]
struct LetterCount {
    letter: char,
    count: usize,
}

fn analyze_bee_movie() -> Result<(), Box<dyn std::error::Error>> {
    let script_path = "../beemoviescript.txt";
    let file = File::open(script_path)?;
    
    println!("Bee Movie Script:");
    println!("--------------------------------------------------");
    
    let mut letter_counts = HashMap::new();
    let mut total_letters = 0;
    
    // Read file line by line and print each line
    for line in io::BufReader::new(file).lines() {
        let line = line?;
        println!("{}", line);
        
        // Count letters
        for char in line.chars() {
            if char.is_alphabetic() {
                let lower = char.to_ascii_lowercase();
                *letter_counts.entry(lower).or_insert(0) += 1;
                total_letters += 1;
            }
        }
    }
    
    println!("--------------------------------------------------");
    println!("Analysis complete.");
    
    if total_letters > 0 {
        // Convert HashMap to vector and sort
        let mut counts: Vec<LetterCount> = letter_counts
            .into_iter()
            .map(|(letter, count)| LetterCount { letter, count })
            .collect();
        
        counts.sort_by(|a, b| b.count.cmp(&a.count));
        
        println!("\nTop 3 most commonly used letters:");
        for (i, lc) in counts.iter().take(3).enumerate() {
            println!("{}. '{}': {} times", i + 1, lc.letter, lc.count);
        }
    } else {
        println!("No letters found in the script.");
    }
    
    Ok(())
}

fn main() {
    hello();
    
    if let Err(e) = analyze_bee_movie() {
        eprintln!("Error: {}", e);
    }
}
