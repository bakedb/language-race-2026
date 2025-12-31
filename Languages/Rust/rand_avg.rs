use std::fs;
use std::io::{self, Write};
use rand::Rng;

// Hello, World!
fn hello() {
    println!("Hello, World!");
}

fn generate_random_numbers() -> Result<(), Box<dyn std::error::Error>> {
    const COUNT: usize = 1000;
    const OUTPUT_DIR: &str = "../rand_avg output";
    const OUTPUT_FILE: &str = "../rand_avg output/random_numbers.txt";
    
    // Create output directory
    fs::create_dir_all(OUTPUT_DIR)?;
    
    // Generate 1000 random numbers
    let mut random_numbers = Vec::new();
    let mut rng = rand::thread_rng();
    let mut sum = 0.0;
    
    for _ in 0..COUNT {
        let num: i32 = rng.gen_range(0..1000);
        random_numbers.push(num);
        sum += num as f64;
    }
    
    // Calculate mean
    let mean = sum / COUNT as f64;
    
    // Save to file
    let mut file = fs::File::create(OUTPUT_FILE)?;
    for num in &random_numbers {
        writeln!(file, "{}", num)?;
    }
    
    println!("Generated 1000 random numbers");
    println!("Mean: {:.2}", mean);
    println!("Saved to: {}", OUTPUT_FILE);
    
    Ok(())
}

fn main() {
    hello();
    
    if let Err(e) = generate_random_numbers() {
        eprintln!("Error: {}", e);
    }
}
