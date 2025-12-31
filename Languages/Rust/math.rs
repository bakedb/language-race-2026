use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Hello, World!
fn hello() {
    println!("Hello, World!");
}

fn evaluate_expression(expr: &str) -> Result<f64, Box<dyn std::error::Error>> {
    let parts: Vec<&str> expr.trim().split_whitespace().collect();
    
    if parts.len() < 3 {
        return Err("Invalid expression".into());
    }
    
    let a: f64 = parts[0].parse()?;
    let op = parts[1];
    let b: f64 = parts[2].parse()?;
    
    match op {
        "+" => Ok(a + b),
        "-" => Ok(a - b),
        "*" => Ok(a * b),
        "/" => {
            if b == 0.0 {
                Err("Division by zero".into())
            } else {
                Ok(a / b)
            }
        }
        _ => Err("Unknown operator".into()),
    }
}

fn solve_equation(line: &str) {
    let mut equation = line.to_string();
    
    // Remove "= ?" part
    if let Some(equals_pos) = equation.find('=') {
        equation.truncate(equals_pos);
    }
    
    // Trim whitespace
    equation = equation.trim().to_string();
    
    match evaluate_expression(&equation) {
        Ok(result) => println!("{} = {:.2}", equation, result),
        Err(_) => println!("{} = Error", equation),
    }
}

fn process_file(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let path = Path::new(filename);
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    
    for line in reader.lines() {
        let line = line?;
        
        // Skip empty lines and markdown headers
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        
        // Handle markdown list items
        let start = if line.starts_with("- ") {
            &line[2..]
        } else {
            &line
        };
        
        if start.contains('=') {
            solve_equation(start);
        }
    }
    
    Ok(())
}

fn main() {
    hello();
    
    println!("\nProcessing math equations...");
    
    let files = [
        "../test_data/math_equations.txt",
        "../test_data/math_equations.md",
        "../test_data/math_equations.json",
        "../test_data/math_equations.yaml",
        "../test_data/math_equations",
    ];
    
    for file in &files {
        if let Err(e) = process_file(file) {
            println!("Could not process file {}: {}", file, e);
        }
    }
}
