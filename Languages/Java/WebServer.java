import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.*;
import org.json.JSONObject;
import org.json.JSONArray;

public class WebServer {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        testWebServer();
    }
    
    private static Map<String, String> loadCompareJson(String filename) {
        try {
            File file = new File(filename);
            if (!file.exists()) {
                System.out.println("Error: Could not find " + filename);
                return Collections.emptyMap();
            }
            
            StringBuilder content = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    content.append(line);
                }
            }
            
            JSONObject json = new JSONObject(content.toString());
            Map<String, String> result = new HashMap<>();
            
            for (String key : json.keySet()) {
                result.put(key, json.getString(key));
            }
            
            return result;
        } catch (IOException e) {
            System.out.println("Error reading compare.json: " + e.getMessage());
            return Collections.emptyMap();
        }
    }
    
    private static String makeHttpRequest(String urlString) {
        try {
            URL url = new URL(urlString);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(5000);
            
            int responseCode = connection.getResponseCode();
            
            if (responseCode == 200) {
                BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                StringBuilder response = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    response.append(line);
                }
                reader.close();
                return response.toString();
            } else {
                return "";
            }
        } catch (Exception e) {
            return "";
        }
    }
    
    private static void testWebServer() {
        String baseUrl = "http://localhost:3000";
        Map<String, String> expectedHashes = loadCompareJson("../webserver/compare.json");
        
        if (expectedHashes.isEmpty()) {
            return;
        }
        
        System.out.println("Testing 100 endpoints...");
        
        int passed = 0;
        int failed = 0;
        JSONArray results = new JSONArray();
        
        // Test each endpoint
        for (int i = 0; i < 100; i++) {
            String endpoint = "test-" + i;
            String url = baseUrl + "/" + endpoint;
            
            String response = makeHttpRequest(url);
            
            if (!response.isEmpty()) {
                try {
                    JSONObject jsonResponse = new JSONObject(response);
                    String serverHash = jsonResponse.getString("hash");
                    String expectedHash = expectedHashes.getOrDefault(endpoint, "");
                    
                    JSONObject result = new JSONObject();
                    result.put("endpoint", endpoint);
                    result.put("url", url);
                    result.put("server_hash", serverHash);
                    result.put("expected_hash", expectedHash);
                    
                    if (serverHash.equals(expectedHash)) {
                        result.put("status", "PASSED");
                        passed++;
                    } else {
                        result.put("status", "FAILED");
                        failed++;
                    }
                    
                    results.put(result);
                } catch (Exception e) {
                    JSONObject result = new JSONObject();
                    result.put("endpoint", endpoint);
                    result.put("url", url);
                    result.put("status", "FAILED");
                    result.put("error", "JSON parse error");
                    results.put(result);
                    failed++;
                }
            } else {
                JSONObject result = new JSONObject();
                result.put("endpoint", endpoint);
                result.put("url", url);
                result.put("status", "FAILED");
                result.put("error", "HTTP request failed");
                results.put(result);
                failed++;
            }
            
            // Progress indicator
            if ((i + 1) % 10 == 0) {
                System.out.println("Tested " + (i + 1) + "/100 endpoints...");
            }
        }
        
        // Create final result
        JSONObject finalResult = new JSONObject();
        finalResult.put("total_tests", 100);
        finalResult.put("passed", passed);
        finalResult.put("failed", failed);
        finalResult.put("success_rate", String.format("%.1f%%", (double) passed / 100 * 100));
        finalResult.put("timestamp", new java.util.Date().toString());
        finalResult.put("results", results);
        
        // Save results to file
        try (FileWriter file = new FileWriter("test-result.json")) {
            file.write(finalResult.toString(2));
            System.out.println("Results saved to: test-result.json");
        } catch (IOException e) {
            System.out.println("Error saving results: " + e.getMessage());
        }
        
        System.out.println("\nTest completed!");
        System.out.printf("Passed: %d/100 (%.1f%%)\n", passed, (double) passed / 100 * 100);
        System.out.printf("Failed: %d/100 (%.1f%%)\n", failed, (double) failed / 100 * 100);
    }
}
