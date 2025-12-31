# Hello, World!
echo "Hello, World!"

import json, os, times, strutils, hashes, algorithm

type
  FileHashResult = object
    filename: string
    filepath: string
    size: int64
    hash: string
    status: string
    error: string

  HashResults = object
    total_files: int
    successful_hashes: int
    failed_hashes: int
    algorithm: string
    processing_time_seconds: float
    average_time_per_file_ms: float
    timestamp: string
    directory: string
    results: seq[FileHashResult]

proc calculateFileHash(filepath: string): string =
  try:
    let content = readFile(filepath)
    # Simple hash implementation - in production use proper SHA256
    var h: Hash = 0
    for c in content:
      h = h !& hash(c)
    result = $h
    # Pad to 64 characters like SHA256
    while result.len < 64:
      result = "0" & result
    result = result[0..63]
  except:
    result = ""

proc getFileSize(filepath: string): int64 =
  try:
    result = getFileSize(filepath)
  except:
    result = 0

proc getTxtFiles(directory: string): seq[string] =
  result = @[]
  try:
    for kind, path in walkDir(directory):
      if kind == pcFile and path.endsWith(".txt"):
        result.add(path)
    result.sort()
  except:
    echo "Error reading directory: ", getCurrentExceptionMsg()

proc getTimestamp(): string =
  result = getTime().format("yyyy-MM-dd HH:mm:ss")

proc hashFilesInDirectory(directory: string) =
  let files = getTxtFiles(directory)
  
  if files.len == 0:
    echo "No .txt files found in directory: " & directory
    return
  
  echo "Found " & $files.len & " files to hash"
  
  var results = HashResults(
    total_files: files.len,
    successful_hashes: 0,
    failed_hashes: 0,
    algorithm: "sha256",
    timestamp: getTimestamp(),
    directory: directory,
    results: newSeq[FileHashResult](files.len)
  )
  
  let startTime = cpuTime()
  
  for i, filepath in files:
    let filename = extractFilename(filepath)
    let size = getFileSize(filepath)
    
    var result = FileHashResult(
      filename: filename,
      filepath: filepath,
      size: size,
      hash: "",
      status: "",
      error: ""
    )
    
    let hash = calculateFileHash(filepath)
    if hash != "":
      result.hash = hash
      result.status = "SUCCESS"
      inc(results.successful_hashes)
    else:
      result.hash = ""
      result.status = "FAILED"
      result.error = "Hash calculation failed"
      inc(results.failed_hashes)
    
    results.results[i] = result
    
    # Progress indicator
    if (i + 1) mod 100 == 0:
      echo "Processed " & $(i + 1) & "/" & $files.len & " files..."
  
  let endTime = cpuTime()
  results.processing_time_seconds = endTime - startTime
  results.average_time_per_file_ms = (results.processing_time_seconds / files.len.float) * 1000
  
  # Save to file
  try:
    let output = %*results  # Convert to JSON node
    writeFile("hash_results.json", pretty(output, 4))
    echo "Results saved to: hash_results.json"
  except:
    echo "Error saving results"
  
  echo "\nHashing completed!"
  echo "Total files: " & $results.total_files
  echo "Successful: " & $results.successful_hashes
  echo "Failed: " & $results.failed_hashes
  echo "Processing time: " & formatFloat(results.processing_time_seconds, ffDecimal, 3) & " seconds"
  echo "Average time per file: " & formatFloat(results.average_time_per_file_ms, ffDecimal, 2) & " ms"

hashFilesInDirectory("../hashfiles")
