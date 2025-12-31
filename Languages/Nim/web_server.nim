# Hello, World!
echo "Hello, World!"

import json, httpclient, os, times, strutils

type
  TestResult = object
    endpoint: string
    url: string
    server_hash: string
    expected_hash: string
    status: string
    error: string

  FinalResult = object
    total_tests: int
    passed: int
    failed: int
    success_rate: string
    timestamp: string
    results: seq[TestResult]

proc loadCompareJson(filename: string): JsonNode =
  try:
    let content = readFile(filename)
    parseJson(content)
  except:
    echo "Error: Could not read " & filename
    nil

proc makeHttpRequest(url: string): string =
  var client = newHttpClient(timeout = 5000)
  try:
    let response = client.get(url)
    if response.status == "200 OK":
      return response.body
    else:
      return ""
  except:
    return ""

proc testWebServer() =
  let baseUrl = "http://localhost:3000"
  let compareFile = "../webserver/compare.json"
  let outputFile = "test-result.json"
  
  # Load expected hashes
  let expectedHashes = loadCompareJson(compareFile)
  if expectedHashes == nil:
    return
  
  echo "Testing 100 endpoints..."
  
  var results: seq[TestResult]
  var passed = 0
  var failed = 0
  
  # Test each endpoint
  for i in 0..<100:
    let endpoint = "test-" & $i
    let url = baseUrl & "/" & endpoint
    
    let response = makeHttpRequest(url)
    
    var result = TestResult(
      endpoint: endpoint,
      url: url,
      expected_hash: expectedHashes{endpoint}.getStr(""),
      status: "",
      error: ""
    )
    
    if response == "":
      result.status = "FAILED"
      result.error = "HTTP request failed"
      inc(failed)
    else:
      try:
        let data = parseJson(response)
        let serverHash = data{"hash"}.getStr("")
        result.server_hash = serverHash
        
        if serverHash == result.expected_hash:
          result.status = "PASSED"
          inc(passed)
        else:
          result.status = "FAILED"
          inc(failed)
      except:
        result.status = "FAILED"
        result.error = "JSON parse error"
        inc(failed)
    
    results.add(result)
    
    # Progress indicator
    if (i + 1) mod 10 == 0:
      echo "Tested " & $(i + 1) & "/100 endpoints..."
  
  # Create final result
  let totalTests = results.len
  let successRate = formatFloat(passed.float / totalTests.float * 100, ffDecimal, 1) & "%"
  
  let finalResult = FinalResult(
    total_tests: totalTests,
    passed: passed,
    failed: failed,
    success_rate: successRate,
    timestamp: getTime().format("yyyy-MM-dd HH:mm:ss"),
    results: results
  )
  
  # Save results to file
  try:
    let output = pretty(finalResult, 4)
    writeFile(outputFile, output)
    echo "Results saved to: " & outputFile
  except:
    echo "Error saving results"
  
  echo "\nTest completed!"
  echo "Passed: " & $passed & "/" & $totalTests & " (" & successRate & ")"
  echo "Failed: " & $failed & "/" & $totalTests & " (" & formatFloat(failed.float / totalTests.float * 100, ffDecimal, 1) & "%)"

testWebServer()
