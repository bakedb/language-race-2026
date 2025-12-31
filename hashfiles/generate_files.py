#!/usr/bin/env python3
import os
import random
import string

def generate_random_content(length):
    """Generate random content of specified length"""
    chars = string.ascii_letters + string.digits + string.punctuation + ' \n\t'
    return ''.join(random.choice(chars) for _ in range(length))

def create_test_files():
    """Create 1000 test files with varied content"""
    hashfiles_dir = "."
    
    # Different content types and patterns
    content_templates = [
        lambda: f"Test file {random.randint(1, 1000)} with random content: {generate_random_content(100)}",
        lambda: f"Lorem ipsum dolor sit amet, consectetur adipiscing elit. {generate_random_content(50)}",
        lambda: f"Data entry {random.randint(1, 999)}: {generate_random_content(200)}",
        lambda: f"Hash test #{random.randint(1, 1000)}\nContent: {generate_random_content(150)}\nEnd of file",
        lambda: f"Binary simulation: {''.join(chr(random.randint(0, 255)) for _ in range(100))}",
        lambda: f"JSON-like: {{\"id\": {random.randint(1, 1000)}, \"data\": \"{generate_random_content(80)}\"}}",
        lambda: f"XML-like: <item id=\"{random.randint(1, 1000)}\">{generate_random_content(120)}</item>",
        lambda: f"CSV header: id,name,value\n{random.randint(1, 1000)},item_{random.randint(1, 1000)},{random.random():.2f}",
        lambda: f"Log entry: [{random.randint(1, 24):02d}:{random.randint(0, 59):02d}] {generate_random_content(180)}",
        lambda: f"Config: setting_{random.randint(1, 100)} = {generate_random_content(90)}"
    ]
    
    print(f"Creating 1000 test files in {hashfiles_dir}...")
    
    for i in range(1000):
        filename = f"file_{i:04d}.txt"
        filepath = os.path.join(hashfiles_dir, filename)
        
        # Vary content type and size
        template = random.choice(content_templates)
        content = template()
        
        # Sometimes add extra content to vary file sizes
        if random.random() < 0.3:
            content += f"\nAdditional content: {generate_random_content(random.randint(50, 200))}"
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        
        if (i + 1) % 100 == 0:
            print(f"Created {i + 1} files...")
    
    print("All 1000 files created successfully!")

if __name__ == "__main__":
    create_test_files()
