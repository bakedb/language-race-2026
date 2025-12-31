use std::collections::HashMap;
use std::fs;
use std::time::Duration;
use serde::{Deserialize, Serialize};
use serde_json;

// Hello, World!
fn hello() {
    println!("Hello, World!");
}

#[derive(Serialize, Deserialize, Debug)]
struct TestResult {
    endpoint: String,
    url: String,
    server_hash: String,
    expected_hash: String,
    status: String,
    error: Option<String>,
}

#[derive(Serialize, Debug)]
struct FinalResult {
    total_tests: usize,
    passed: usize,
    failed: usize,
    success_rate: String,
    timestamp: String,
    results: Vec<TestResult>,
}

fn load_compare_json(filename: &str) -> Result<HashMap<String, String>, Box<dyn std::error::Error>> {
    let data = fs::read_to_string(filename)?;
    let compare_data: HashMap<String, String> = serde_json::from_str(&data)?;
    Ok(compare_data)
}

fn make_http_request(url: &str) -> Result<String, Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::new();
    let response = client.get(url)
        .timeout(Duration::from_secs(5))
        .send()?;
    
    if response.status().as_u16() != 200 {
        return Err(format!("HTTP {}", response.status()).into());
    }
    
    let body = response.text()?;
    Ok(body)
}

fn test_web_server() -> Result<(), Box<dyn std::error::Error>> {
    let base_url = "http://localhost:3000";
    let compare_file = "../webserver/compare.json";
    let output_file = "test-result.json";
    
    // Load expected hashes
    let expected_hashes = load_compare_json(compare_file)?;
    
    println!("Testing 100 endpoints...");
    
    let mut results = Vec::new();
    let mut passed = 0;
    let mut failed = 0;
    
    // Test each endpoint
    for i in 0..100 {
        let endpoint = format!("test-{}", i);
        let url = format!("{}/{}", base_url, endpoint);
        
        let mut result = TestResult {
            endpoint: endpoint.clone(),
            url: url.clone(),
            server_hash: String::new(),
            expected_hash: expected_hashes.get(&endpoint).unwrap_or(&String::new()).clone(),
            status: String::new(),
            error: None,
        };
        
        match make_http_request(&url) {
            Ok(response) => {
                match serde_json::from_str::<HashMap<String, String>>(&response) {
                    Ok(response_data) => {
                        result.server_hash = response_data.get("hash").unwrap_or(&String::new()).clone();
                        if result.server_hash == result.expected_hash {
                            result.status = "PASSED".to_string();
                            passed += 1;
                        } else {
                            result.status = "FAILED".to_string();
                            failed += 1;
                        }
                    }
                    Err(_) => {
                        result.status = "FAILED".to_string();
                        result.error = Some("JSON parse error".to_string());
                        failed += 1;
                    }
                }
            }
            Err(e) => {
                result.status = "FAILED".to_string();
                result.error = Some(e.to_string());
                failed += 1;
            }
        }
        
        results.push(result);
        
        // Progress indicator
        if (i + 1) % 10 == 0 {
            println!("Tested {} endpoints...", i + 1);
        }
    }
    
    // Create final result
    let success_rate = (passed as f64 / results.len() as f64) * 100.0;
    let final_result = FinalResult {
        total_tests: results.len(),
        passed,
        failed,
        success_rate: format!("{:.1}%", success_rate),
        timestamp: chrono::Utc::now().format("%Y-%m-%d %H:%M:%S").to_string(),
        results,
    };
    
    // Save results to file
    let output_data = serde_json::to_string_pretty(&final_result)?;
    fs::write(output_file, output_data)?;
    
    println!("Results saved to: {}", output_file);
    println!("\nTest completed!");
    println!("Passed: {}/{} ({:.1}%)", passed, results.len(), success_rate);
    println!("Failed: {}/{} ({:.1}%)", failed, results.len(), 100.0 - success_rate);
    
    Ok(())
}

fn main() {
    hello();
    
    if let Err(e) = test_web_server() {
        eprintln!("Error: {}", e);
    }
}
