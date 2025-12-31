package main

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"os"
	path "path/filepath"
	"sort"
	"time"
)

// Hello, World!
func hello() {
	fmt.Println("Hello, World!")
}

type FileHashResult struct {
	Filename string `json:"filename"`
	Filepath string `json:"filepath"`
	Size     int64  `json:"size"`
	Hash     string `json:"hash"`
	Status   string `json:"status"`
	Error    string `json:"error,omitempty"`
}

type HashResults struct {
	TotalFiles            int              `json:"total_files"`
	SuccessfulHashes      int              `json:"successful_hashes"`
	FailedHashes          int              `json:"failed_hashes"`
	Algorithm             string           `json:"algorithm"`
	ProcessingTimeSeconds float64          `json:"processing_time_seconds"`
	AverageTimePerFileMs  float64          `json:"average_time_per_file_ms"`
	Timestamp             string           `json:"timestamp"`
	Directory             string           `json:"directory"`
	Results               []FileHashResult `json:"results"`
}

func calculateFileHash(filepath string) (string, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", err
	}

	return hex.EncodeToString(hasher.Sum(nil)), nil
}

func getFileSize(filepath string) int64 {
	info, err := os.Stat(filepath)
	if err != nil {
		return 0
	}
	return info.Size()
}

func getTxtFiles(directory string) ([]string, error) {
	var files []string

	entries, err := os.ReadDir(directory)
	if err != nil {
		return files, err
	}

	for _, entry := range entries {
		if !entry.IsDir() && path.Ext(entry.Name()) == ".txt" {
			files = append(files, path.Join(directory, entry.Name()))
		}
	}

	sort.Strings(files)
	return files, nil
}

func getTimestamp() string {
	return time.Now().Format("2006-01-02 15:04:05")
}

func hashFilesInDirectory(directory string) error {
	files, err := getTxtFiles(directory)
	if err != nil {
		return fmt.Errorf("error reading directory: %v", err)
	}

	if len(files) == 0 {
		fmt.Printf("No .txt files found in directory: %s\n", directory)
		return nil
	}

	fmt.Printf("Found %d files to hash\n", len(files))

	results := HashResults{
		Directory: directory,
		Algorithm: "sha256",
		Results:   make([]FileHashResult, 0, len(files)),
	}

	startTime := time.Now()

	for i, filepath := range files {
		result := FileHashResult{
			Filepath: filepath,
			Filename: path.Base(filepath),
			Size:     getFileSize(filepath),
		}

		hash, err := calculateFileHash(filepath)
		if err != nil {
			result.Hash = ""
			result.Status = "FAILED"
			result.Error = "Hash calculation failed"
			results.FailedHashes++
		} else {
			result.Hash = hash
			result.Status = "SUCCESS"
			results.SuccessfulHashes++
		}

		results.Results = append(results.Results, result)

		// Progress indicator
		if (i+1)%100 == 0 {
			fmt.Printf("Processed %d/%d files...\n", i+1, len(files))
		}
	}

	endTime := time.Now()
	processingTime := endTime.Sub(startTime).Seconds()
	results.ProcessingTimeSeconds = processingTime
	results.AverageTimePerFileMs = (processingTime / float64(len(files))) * 1000
	results.Timestamp = getTimestamp()
	results.TotalFiles = len(files)

	// Save to file
	outputData, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return fmt.Errorf("error marshaling results: %v", err)
	}

	if err := os.WriteFile("hash_results.json", outputData, 0644); err != nil {
		return fmt.Errorf("error saving results: %v", err)
	}

	fmt.Printf("Results saved to: hash_results.json\n")
	fmt.Printf("\nHashing completed!\n")
	fmt.Printf("Total files: %d\n", results.TotalFiles)
	fmt.Printf("Successful: %d\n", results.SuccessfulHashes)
	fmt.Printf("Failed: %d\n", results.FailedHashes)
	fmt.Printf("Processing time: %.3f seconds\n", results.ProcessingTimeSeconds)
	fmt.Printf("Average time per file: %.2f ms\n", results.AverageTimePerFileMs)

	return nil
}

func main() {
	hello()
	if err := hashFilesInDirectory("../hashfiles"); err != nil {
		fmt.Printf("Error: %v\n", err)
	}
}
