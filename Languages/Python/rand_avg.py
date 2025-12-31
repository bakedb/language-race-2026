print("Hello, World!")

import random
import os

def generate_random_numbers():
    # Generate 1000 random numbers from 0 to 999
    random_numbers = [random.randint(0, 999) for _ in range(1000)]
    
    # Create output directory if it doesn't exist
    output_dir = "../rand_avg output"
    os.makedirs(output_dir, exist_ok=True)
    
    # Save to file
    output_file = os.path.join(output_dir, "random_numbers.txt")
    with open(output_file, 'w') as f:
        for num in random_numbers:
            f.write(f"{num}\n")
    
    # Calculate mean
    mean = sum(random_numbers) / len(random_numbers)
    
    print(f"Generated 1000 random numbers")
    print(f"Mean: {mean:.2f}")
    print(f"Saved to: {output_file}")

if __name__ == "__main__":
    generate_random_numbers()
