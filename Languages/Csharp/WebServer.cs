using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using Newtonsoft.Json;

public class WebServer
{
    public static void Main(string[] args)
    {
        // Hello, World!
        Console.WriteLine("Hello, World!");
        TestWebServer();
    }
    
    private static Dictionary<string, string> LoadCompareJson(string filename)
    {
        try
        {
            if (!File.Exists(filename))
            {
                Console.WriteLine($"Error: Could not find {filename}");
                return new Dictionary<string, string>();
            }
            
            string content = File.ReadAllText(filename);
            var result = JsonConvert.DeserializeObject<Dictionary<string, string>>(content);
            return result ?? new Dictionary<string, string>();
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error reading compare.json: {e.Message}");
            return new Dictionary<string, string>();
        }
    }
    
    private static string MakeHttpRequest(string url)
    {
        try
        {
            var request = WebRequest.Create(url) as HttpWebRequest;
            request.Method = "GET";
            request.Timeout = 5000;
            
            using (var response = request.GetResponse() as HttpWebResponse)
            {
                if (response.StatusCode == HttpStatusCode.OK)
                {
                    using (var reader = new StreamReader(response.GetResponseStream()))
                    {
                        return reader.ReadToEnd();
                    }
                }
            }
        }
        catch
        {
            // Handle any exceptions
        }
        
        return "";
    }
    
    private static void TestWebServer()
    {
        string baseUrl = "http://localhost:3000";
        var expectedHashes = LoadCompareJson("../webserver/compare.json");
        
        Console.WriteLine("Testing 100 endpoints...");
        
        var results = new List<object>();
        int passed = 0;
        int failed = 0;
        
        // Test each endpoint
        for (int i = 0; i < 100; i++)
        {
            string endpoint = $"test-{i}";
            string url = $"{baseUrl}/{endpoint}";
            
            string response = MakeHttpRequest(url);
            
            var result = new Dictionary<string, object>
            {
                ["endpoint"] = endpoint,
                ["url"] = url,
                ["expected_hash"] = expectedHashes.GetValueOrDefault(endpoint, "")
            };
            
            if (string.IsNullOrEmpty(response))
            {
                result["status"] = "FAILED";
                result["error"] = "HTTP request failed";
                failed++;
            }
            else
            {
                try
                {
                    var responseData = JsonConvert.DeserializeObject<Dictionary<string, string>>(response);
                    string serverHash = responseData?.GetValueOrDefault("hash", "") ?? "";
                    result["server_hash"] = serverHash;
                    
                    if (serverHash == result["expected_hash"].ToString())
                    {
                        result["status"] = "PASSED";
                        passed++;
                    }
                    else
                    {
                        result["status"] = "FAILED";
                        failed++;
                    }
                }
                catch
                {
                    result["status"] = "FAILED";
                    result["error"] = "JSON parse error";
                    failed++;
                }
            }
            
            results.Add(result);
            
            // Progress indicator
            if ((i + 1) % 10 == 0)
            {
                Console.WriteLine($"Tested {i + 1}/100 endpoints...");
            }
        }
        
        // Create final result
        var finalResult = new Dictionary<string, object>
        {
            ["total_tests"] = results.Count,
            ["passed"] = passed,
            ["failed"] = failed,
            ["success_rate"] = $"{(double)passed / results.Count * 100:F1}%",
            ["timestamp"] = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss"),
            ["results"] = results
        };
        
        // Save results to file
        try
        {
            string output = JsonConvert.SerializeObject(finalResult, Formatting.Indented);
            File.WriteAllText("test-result.json", output);
            Console.WriteLine("Results saved to: test-result.json");
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error saving results: {e.Message}");
        }
        
        Console.WriteLine("\nTest completed!");
        Console.WriteLine($"Passed: {passed}/{results.Count} ({(double)passed / results.Count * 100:F1}%)");
        Console.WriteLine($"Failed: {failed}/{results.Count} ({(double)failed / results.Count * 100:F1}%)");
    }
}
