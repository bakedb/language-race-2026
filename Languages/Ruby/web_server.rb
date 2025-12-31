# Hello, World!
puts "Hello, World!"

require 'net/http'
require 'json'
require 'uri'

def make_http_request(url)
  uri = URI(url)
  http = Net::HTTP.new(uri.host, uri.port)
  http.read_timeout = 5
  http.open_timeout = 5
  
  request = Net::HTTP::Get.new(uri)
  response = http.request(request)
  
  if response.code == '200'
    response.body
  else
    ''
  end
rescue
  ''
end

def test_web_server
  base_url = "http://localhost:3000"
  compare_file = "../webserver/compare.json"
  output_file = "test-result.json"
  
  # Load expected hashes
  begin
    expected_hashes = JSON.parse(File.read(compare_file))
  rescue => e
    puts "Error: Could not read #{compare_file}: #{e.message}"
    return
  end
  
  puts "Testing 100 endpoints..."
  
  results = []
  passed = 0
  failed = 0
  
  # Test each endpoint
  (0..99).each do |i|
    endpoint = "test-#{i}"
    url = "#{base_url}/#{endpoint}"
    
    response = make_http_request(url)
    
    result = {
      endpoint: endpoint,
      url: url,
      expected_hash: expected_hashes[endpoint] || ''
    }
    
    if response.empty?
      result[:status] = "FAILED"
      result[:error] = "HTTP request failed"
      failed += 1
    else
      begin
        data = JSON.parse(response)
        server_hash = data['hash'] || ''
        result[:server_hash] = server_hash
        
        if server_hash == result[:expected_hash]
          result[:status] = "PASSED"
          passed += 1
        else
          result[:status] = "FAILED"
          failed += 1
        end
      rescue JSON::ParserError
        result[:status] = "FAILED"
        result[:error] = "JSON parse error"
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
  final_result = {
    total_tests: results.length,
    passed: passed,
    failed: failed,
    success_rate: "#{(passed.to_f / results.length * 100).round(1)}%",
    timestamp: Time.now.strftime("%Y-%m-%d %H:%M:%S"),
    results: results
  }
  
  # Save results to file
  begin
    File.write(output_file, JSON.pretty_generate(final_result))
    puts "Results saved to: #{output_file}"
  rescue => e
    puts "Error saving results: #{e.message}"
  end
  
  puts "\nTest completed!"
  puts "Passed: #{passed}/#{results.length} (#{(passed.to_f / results.length * 100).round(1)}%)"
  puts "Failed: #{failed}/#{results.length} (#{(failed.to_f / results.length * 100).round(1)}%)"
end

test_web_server
