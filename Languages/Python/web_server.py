# Return beemoviescript.txt line by line, and then return the 3 most commonly used letters.
print("Hello, World!")

import json
import requests
import time

def test_web_server():
    base_url = "http://localhost:3000"
    compare_file = "../webserver/compare.json"
    output_file = "test-result.json"
    
    # Load expected hashes
    try:
        with open(compare_file, 'r') as f:
            expected_hashes = json.load(f)
    except FileNotFoundError:
        print(f"Error: Could not find {compare_file}")
        return
    
    results = []
    passed = 0
    failed = 0
    
    print(f"Testing {len(expected_hashes)} endpoints...")
    
    # Test each endpoint
    for i in range(100):
        endpoint = f"test-{i}"
        url = f"{base_url}/{endpoint}"
        
        try:
            # Make HTTP request
            response = requests.get(url, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                server_hash = data.get('hash', '')
                expected_hash = expected_hashes.get(endpoint, '')
                
                # Compare hashes
                if server_hash == expected_hash:
                    status = "PASSED"
                    passed += 1
                else:
                    status = "FAILED"
                    failed += 1
                
                results.append({
                    "endpoint": endpoint,
                    "url": url,
                    "server_hash": server_hash,
                    "expected_hash": expected_hash,
                    "status": status
                })
            else:
                status = "FAILED"
                failed += 1
                results.append({
                    "endpoint": endpoint,
                    "url": url,
                    "status": status,
                    "error": f"HTTP {response.status_code}"
                })
                
        except requests.exceptions.RequestException as e:
            status = "FAILED"
            failed += 1
            results.append({
                "endpoint": endpoint,
                "url": url,
                "status": status,
                "error": str(e)
            })
        
        # Progress indicator
        if (i + 1) % 10 == 0:
            print(f"Tested {i + 1}/100 endpoints...")
    
    # Create final result
    final_result = {
        "total_tests": len(results),
        "passed": passed,
        "failed": failed,
        "success_rate": f"{(passed / len(results) * 100):.1f}%",
        "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
        "results": results
    }
    
    # Save results to file
    with open(output_file, 'w') as f:
        json.dump(final_result, f, indent=2)
    
    print(f"\nTest completed!")
    print(f"Passed: {passed}/{len(results)} ({(passed / len(results) * 100):.1f}%)")
    print(f"Failed: {failed}/{len(results)} ({(failed / len(results) * 100):.1f}%)")
    print(f"Results saved to: {output_file}")

if __name__ == "__main__":
    test_web_server()
