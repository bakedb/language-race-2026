# Hello, World!
puts "Hello, World!"

require "http/client"
require "json"

def make_http_request(url : String) : String?
  client = HTTP::Client.new
  client.read_timeout = 5.seconds
  
  begin
    response = client.get(url)
    if response.status_code == 200
      response.body
    else
      nil
    end
  rescue
    nil
  end
end

def test_web_server
  base_url = "http://localhost:3000"
  compare_file = "../webserver/compare.json"
  output_file = "test-result.json"
  
  # Load expected hashes
  unless File.exists?(compare_file)
    puts "Error: Could not find #{compare_file}"
    return
  end
  
  expected_hashes = Hash(String, String).from_json(File.read(compare_file))
  
  puts "Testing 100 endpoints..."
  
  results = [] of Hash(String, String | Nil)
  passed = 0
  failed = 0
  
  # Test each endpoint
  (0..99).each do |i|
    endpoint = "test-#{i}"
    url = "#{base_url}/#{endpoint}"
    
    response = make_http_request(url)
    
    result = Hash(String, String | Nil).new
    result["endpoint"] = endpoint
    result["url"] = url
    result["expected_hash"] = expected_hashes[endpoint]? || ""
    
    if response.nil?
      result["status"] = "FAILED"
      result["error"] = "HTTP request failed"
      failed += 1
    else
      begin
        data = Hash(String, String).from_json(response)
        server_hash = data["hash"]? || ""
        result["server_hash"] = server_hash
        
        if server_hash == result["expected_hash"]
          result["status"] = "PASSED"
          passed += 1
        else
          result["status"] = "FAILED"
          failed += 1
        end
      rescue JSON::ParseException
        result["status"] = "FAILED"
        result["error"] = "JSON parse error"
        failed += 1
      end
    end
    
    results << result
    
    # Progress indicator
    if (i + 1) % 10 == 0
      puts "Tested #{i + 1}/100 endpoints..."
    end
  end
  
  # Create final result
  total_tests = results.size
  success_rate = (passed.to_f / total_tests * 100).round(1)
  
  final_result = {
    "total_tests"    => total_tests,
    "passed"         => passed,
    "failed"         => failed,
    "success_rate"   => "#{success_rate}%",
    "timestamp"      => Time.local.to_s("%Y-%m-%d %H:%M:%S"),
    "results"        => results
  }
  
  # Save results to file
  begin
    File.write(output_file, final_result.to_json)
    puts "Results saved to: #{output_file}"
  rescue ex
    puts "Error saving results: #{ex.message}"
  end
  
  puts "\nTest completed!"
  puts "Passed: #{passed}/#{total_tests} (#{success_rate}%)"
  puts "Failed: #{failed}/#{total_tests} (#{(100 - success_rate)}%)"
end

test_web_server
