print("Hello, World!")

import os
from collections import Counter

def analyze_bee_movie():
    script_path = "../beemoviescript.txt"
    
    if not os.path.exists(script_path):
        print(f"File not found: {script_path}")
        return
    
    # Read file line by line and print each line
    print("Bee Movie Script:")
    print("-" * 50)
    
    with open(script_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        for line in lines:
            print(line.rstrip())
    
    print("-" * 50)
    print("Analysis complete.")
    
    # Count letter frequencies
    all_text = ''.join(lines)
    letters = [char.lower() for char in all_text if char.isalpha()]
    
    if letters:
        letter_counts = Counter(letters)
        top_3 = letter_counts.most_common(3)
        
        print("\nTop 3 most commonly used letters:")
        for i, (letter, count) in enumerate(top_3, 1):
            print(f"{i}. '{letter}': {count} times")
    else:
        print("No letters found in the script.")

if __name__ == "__main__":
    analyze_bee_movie()
