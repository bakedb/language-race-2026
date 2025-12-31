package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

// Hello, World!
func hello() {
	fmt.Println("Hello, World!")
}

type TestResult struct {
	Endpoint     string `json:"endpoint"`
	URL          string `json:"url"`
	ServerHash   string `json:"server_hash"`
	ExpectedHash string `json:"expected_hash"`
	Status       string `json:"status"`
	Error        string `json:"error,omitempty"`
}

type FinalResult struct {
	TotalTests  int          `json:"total_tests"`
	Passed      int          `json:"passed"`
	Failed      int          `json:"failed"`
	SuccessRate string       `json:"success_rate"`
	Timestamp   string       `json:"timestamp"`
	Results     []TestResult `json:"results"`
}

func loadCompareJSON(filename string) (map[string]string, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("could not read %s: %v", filename, err)
	}

	var compareData map[string]string
	if err := json.Unmarshal(data, &compareData); err != nil {
		return nil, fmt.Errorf("could not parse JSON: %v", err)
	}

	return compareData, nil
}

func makeHTTPRequest(url string) (string, error) {
	client := &http.Client{
		Timeout: 5 * time.Second,
	}

	resp, err := client.Get(url)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return "", fmt.Errorf("HTTP %d", resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	return string(body), nil
}

func testWebServer() {
	baseURL := "http://localhost:3000"
	compareFile := "../../webserver/compare.json"
	outputFile := "test-result.json"

	// Load expected hashes
	expectedHashes, err := loadCompareJSON(compareFile)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	fmt.Println("Testing 100 endpoints...")

	var results []TestResult
	passed := 0
	failed := 0

	// Test each endpoint
	for i := 0; i < 100; i++ {
		endpoint := fmt.Sprintf("test-%d", i)
		url := fmt.Sprintf("%s/%s", baseURL, endpoint)

		response, err := makeHTTPRequest(url)

		var result TestResult
		result.Endpoint = endpoint
		result.URL = url
		result.ExpectedHash = expectedHashes[endpoint]

		if err != nil {
			result.Status = "FAILED"
			result.Error = err.Error()
			failed++
		} else {
			var responseData map[string]string
			if err := json.Unmarshal([]byte(response), &responseData); err != nil {
				result.Status = "FAILED"
				result.Error = "JSON parse error"
				failed++
			} else {
				result.ServerHash = responseData["hash"]
				if result.ServerHash == result.ExpectedHash {
					result.Status = "PASSED"
					passed++
				} else {
					result.Status = "FAILED"
					failed++
				}
			}
		}

		results = append(results, result)

		// Progress indicator
		if (i+1)%10 == 0 {
			fmt.Printf("Tested %d/100 endpoints...\n", i+1)
		}
	}

	// Create final result
	successRate := float64(passed) / float64(len(results)) * 100
	finalResult := FinalResult{
		TotalTests:  len(results),
		Passed:      passed,
		Failed:      failed,
		SuccessRate: fmt.Sprintf("%.1f%%", successRate),
		Timestamp:   time.Now().Format("2006-01-02 15:04:05"),
		Results:     results,
	}

	// Save results to file
	outputData, err := json.MarshalIndent(finalResult, "", "  ")
	if err != nil {
		fmt.Printf("Error marshaling results: %v\n", err)
		return
	}

	if err := os.WriteFile(outputFile, outputData, 0644); err != nil {
		fmt.Printf("Error saving results: %v\n", err)
		return
	}

	fmt.Printf("Results saved to: %s\n", outputFile)
	fmt.Printf("\nTest completed!\n")
	fmt.Printf("Passed: %d/%d (%.1f%%)\n", passed, len(results), successRate)
	fmt.Printf("Failed: %d/%d (%.1f%%)\n", failed, len(results), 100-successRate)
}

func main() {
	hello()
	testWebServer()
}
