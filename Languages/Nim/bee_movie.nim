# Hello, World!
echo "Hello, World!"

import os, strutils, algorithm, tables

type
  LetterCount = ref object
    letter: char
    count: int

proc analyzeBeeMovie() =
  let scriptPath = "../beemoviescript.txt"
  
  if not fileExists(scriptPath):
    echo "File not found: " & scriptPath
    return
  
  echo "Bee Movie Script:"
  echo "--------------------------------------------------"
  
  var letterCounts = initCountTable[char]()
  var totalLetters = 0
  
  # Read file line by line and print each line
  for line in scriptPath.lines():
    echo line
    
    # Count letters
    for char in line:
      if char.isAlphaAscii():
        let lower = char.toLowerAscii()
        letterCounts.inc(lower)
        totalLetters += 1
  
  echo "--------------------------------------------------"
  echo "Analysis complete."
  
  if totalLetters > 0:
    # Convert to array and sort
    var counts: seq[LetterCount]
    for letter, count in letterCounts.pairs():
      counts.add(LetterCount(letter: letter, count: count))
    
    counts.sort(proc(a, b: LetterCount): int = cmp(b.count, a.count))
    
    echo "\nTop 3 most commonly used letters:"
    for i in 0..<min(3, counts.len):
      let item = counts[i]
      echo $(i+1) & ". '" & item.letter & "': " & $item.count & " times"
  else:
    echo "No letters found in the script."

analyzeBeeMovie()
