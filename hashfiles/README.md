# Hash Files Directory

This directory contains 1000 test files for the Language Race hashing task.

## Files

- **file_0000.txt** through **file_0999.txt**: 1000 test files with varied content
- **generate_files.py**: Python script to generate the test files

## File Content

Each file contains varied content including:
- Random text data
- Simulated log entries
- JSON-like structures
- XML-like markup
- CSV data
- Configuration snippets
- Binary simulation data

File sizes range from ~40 bytes to ~430 bytes to provide variety for hashing performance testing.

## Usage

Language programs should:
1. Read all 1000 files in this directory
2. Calculate hash values for each file (using appropriate hash algorithm like SHA-256, MD5, etc.)
3. Output the hash results

## File Count Verification

```bash
ls file_*.txt | wc -l
# Should output: 1000
```

## Regenerating Files

To regenerate the test files with different content:

```bash
python3 generate_files.py
```

This will overwrite all existing files with new randomly generated content.
