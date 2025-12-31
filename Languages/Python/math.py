print("Hello, World!")

import re
import json
import yaml
import os

def solve_equation(equation):
    # Remove the "= ?" part and evaluate the expression
    equation = equation.replace("= ?", "").strip()
    
    # Use eval to solve the math equation (safe in this controlled environment)
    try:
        result = eval(equation)
        return f"{equation} = {result}"
    except:
        return f"{equation} = Error"

def process_txt_file(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    results = []
    for line in lines:
        line = line.strip()
        if line and '=' in line:
            results.append(solve_equation(line))
    
    return results

def process_md_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Extract equations from markdown
    lines = content.split('\n')
    results = []
    
    for line in lines:
        line = line.strip()
        if line.startswith('- ') and '=' in line:
            equation = line[2:]  # Remove "- "
            results.append(solve_equation(equation))
    
    return results

def process_json_file(filepath):
    with open(filepath, 'r') as f:
        data = json.load(f)
    
    results = []
    for item in data['equations']:
        equation = f"{item['expression']} = ?"
        results.append(solve_equation(equation))
    
    return results

def process_yaml_file(filepath):
    with open(filepath, 'r') as f:
        data = yaml.safe_load(f)
    
    results = []
    for item in data['equations']:
        equation = f"{item['expression']} = ?"
        results.append(solve_equation(equation))
    
    return results

def process_raw_file(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    results = []
    for line in lines:
        line = line.strip()
        if line and '=' in line:
            results.append(solve_equation(line))
    
    return results

def main():
    test_data_dir = "../test_data"
    
    # Process all files
    files = [
        ("math_equations.txt", process_txt_file),
        ("math_equations.md", process_md_file),
        ("math_equations.json", process_json_file),
        ("math_equations.yaml", process_yaml_file),
        ("math_equations", process_raw_file)
    ]
    
    all_results = []
    
    for filename, processor in files:
        filepath = os.path.join(test_data_dir, filename)
        if os.path.exists(filepath):
            print(f"\nProcessing {filename}:")
            results = processor(filepath)
            all_results.extend(results)
            for result in results:
                print(result)
    
    print(f"\nTotal equations solved: {len(all_results)}")

if __name__ == "__main__":
    main()
