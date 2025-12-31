import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.util.*
import org.json.JSONObject

// Hello, World!
fun hello() {
    println("Hello, World!")
}

data class TestResult(
    val endpoint: String,
    val url: String,
    val serverHash: String?,
    val expectedHash: String,
    val status: String,
    val error: String?
)

data class FinalResult(
    val totalTests: Int,
    val passed: Int,
    val failed: Int,
    val successRate: String,
    val timestamp: String,
    val results: List<TestResult>
)

fun loadCompareJson(filename: String): Map<String, String>? {
    return try {
        val file = File(filename)
        if (!file.exists()) {
            println("Error: Could not find $filename")
            return null
        }
        
        val content = file.readText()
        val json = JSONObject(content)
        val result = mutableMapOf<String, String>()
        
        for (key in json.keys()) {
            result[key] = json.getString(key)
        }
        
        result
    } catch (e: Exception) {
        println("Error reading compare.json: ${e.message}")
        null
    }
}

fun makeHttpRequest(urlString: String): String? {
    return try {
        val url = URL(urlString)
        val connection = url.openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connectTimeout = 5000
        connection.readTimeout = 5000
        
        if (connection.responseCode == 200) {
            connection.inputStream.bufferedReader().use { reader ->
                reader.readText()
            }
        } else {
            null
        }
    } catch (e: Exception) {
        null
    }
}

fun testWebServer() {
    val baseUrl = "http://localhost:3000"
    val expectedHashes = loadCompareJson("../webserver/compare.json") ?: return
    
    println("Testing 100 endpoints...")
    
    val results = mutableListOf<TestResult>()
    var passed = 0
    var failed = 0
    
    // Test each endpoint
    for (i in 0 until 100) {
        val endpoint = "test-$i"
        val url = "$baseUrl/$endpoint"
        
        val response = makeHttpRequest(url)
        
        val result = TestResult(
            endpoint = endpoint,
            url = url,
            serverHash = null,
            expectedHash = expectedHashes[endpoint] ?: "",
            status = "",
            error = null
        )
        
        if (response == null) {
            result.copy(
                status = "FAILED",
                error = "HTTP request failed"
            ).also { results.add(it) }
            failed++
        } else {
            try {
                val data = JSONObject(response)
                val serverHash = data.optString("hash", "")
                
                val finalResult = result.copy(
                    serverHash = serverHash,
                    status = if (serverHash == result.expectedHash) "PASSED" else "FAILED"
                )
                
                if (finalResult.status == "PASSED") {
                    passed++
                } else {
                    failed++
                }
                
                results.add(finalResult)
            } catch (e: Exception) {
                result.copy(
                    status = "FAILED",
                    error = "JSON parse error"
                ).also { results.add(it) }
                failed++
            }
        }
        
        // Progress indicator
        if ((i + 1) % 10 == 0) {
            println("Tested ${i + 1}/100 endpoints...")
        }
    }
    
    // Create final result
    val totalTests = results.size
    val successRate = (passed.toDouble() / totalTests * 100).format(1)
    
    val finalResult = FinalResult(
        totalTests = totalTests,
        passed = passed,
        failed = failed,
        successRate = "$successRate%",
        timestamp = Date().toString(),
        results = results
    )
    
    // Save results to file
    try {
        val outputFile = File("test-result.json")
        val jsonResult = JSONObject()
        jsonResult.put("total_tests", finalResult.totalTests)
        jsonResult.put("passed", finalResult.passed)
        jsonResult.put("failed", finalResult.failed)
        jsonResult.put("success_rate", finalResult.successRate)
        jsonResult.put("timestamp", finalResult.timestamp)
        
        val resultsArray = org.json.JSONArray()
        for (result in finalResult.results) {
            val resultJson = JSONObject()
            resultJson.put("endpoint", result.endpoint)
            resultJson.put("url", result.url)
            resultJson.put("server_hash", result.serverHash)
            resultJson.put("expected_hash", result.expectedHash)
            resultJson.put("status", result.status)
            result.error?.let { resultJson.put("error", it) }
            resultsArray.put(resultJson)
        }
        jsonResult.put("results", resultsArray)
        
        outputFile.writeText(jsonResult.toString(2))
        println("Results saved to: test-result.json")
    } catch (e: Exception) {
        println("Error saving results: ${e.message}")
    }
    
    println("\nTest completed!")
    println("Passed: $passed/$totalTests ($successRate%)")
    println("Failed: $failed/$totalTests (${(100 - successRate)}%)")
}

fun Double.format(digits: Int) = "%.${digits}f".format(this)

fun main() {
    hello()
    testWebServer()
}
