# Hello, World!
echo "Hello, World!"

import os, random, strutils

proc generateRandomNumbers() =
  let count = 1000
  let outputDir = "../rand_avg output"
  let outputFile = outputDir / "random_numbers.txt"
  
  # Create output directory
  createDir(outputDir)
  
  # Generate 1000 random numbers
  var randomNumbers: seq[int]
  var sum = 0
  
  for i in 0..<count:
    let num = rand(999)
    randomNumbers.add(num)
    sum += num
  
  # Calculate mean
  let mean = sum.float / count.float
  
  # Save to file
  let file = open(outputFile, fmWrite)
  for num in randomNumbers:
    file.writeLine($num)
  file.close()
  
  echo "Generated 1000 random numbers"
  echo "Mean: " & formatFloat(mean, ffDecimal, 2)
  echo "Saved to: " & outputFile

randomize()
generateRandomNumbers()
