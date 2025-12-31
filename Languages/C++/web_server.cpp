#include <iostream>
#include <fstream>
#include <string>
#include <curl/curl.h>
#include <json/json.h>
#include <iomanip>

// Hello, World!
void hello() {
    std::cout << "Hello, World!" << std::endl;
}

struct ResponseData {
    std::string data;
};

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t total_size = size * nmemb;
    ResponseData *response = static_cast<ResponseData*>(userp);
    response->data.append(static_cast<char*>(contents), total_size);
    return total_size;
}

Json::Value loadCompareJson(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cout << "Error: Could not open " << filename << std::endl;
        return Json::Value();
    }
    
    Json::Value root;
    Json::CharReaderBuilder builder;
    std::string errors;
    
    if (!Json::parseFromStream(builder, file, &root, &errors)) {
        std::cout << "Error parsing JSON: " << errors << std::endl;
        return Json::Value();
    }
    
    return root;
}

void testWebServer() {
    CURL *curl;
    CURLcode res;
    
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if (!curl) {
        std::cout << "Error: Failed to initialize curl" << std::endl;
        return;
    }
    
    // Load expected hashes
    Json::Value compareData = loadCompareJson("../webserver/compare.json");
    if (compareData.isNull()) {
        curl_easy_cleanup(curl);
        curl_global_cleanup();
        return;
    }
    
    std::cout << "Testing 100 endpoints..." << std::endl;
    
    int passed = 0;
    int failed = 0;
    const std::string baseUrl = "http://localhost:3000";
    
    // Test each endpoint
    for (int i = 0; i < 100; i++) {
        std::string endpoint = "test-" + std::to_string(i);
        std::string url = baseUrl + "/" + endpoint;
        
        ResponseData response;
        
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        
        res = curl_easy_perform(curl);
        
        if (res == CURLE_OK) {
            long response_code;
            curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
            
            if (response_code == 200) {
                // Parse JSON response
                Json::Value responseData;
                Json::CharReaderBuilder builder;
                std::string errors;
                std::istringstream stream(response.data);
                
                if (Json::parseFromStream(builder, stream, &responseData, &errors)) {
                    std::string serverHash = responseData.get("hash", "").asString();
                    std::string expectedHash = compareData.get(endpoint, "").asString();
                    
                    if (serverHash == expectedHash) {
                        passed++;
                    } else {
                        failed++;
                    }
                } else {
                    failed++;
                }
            } else {
                failed++;
            }
        } else {
            failed++;
        }
        
        // Progress indicator
        if ((i + 1) % 10 == 0) {
            std::cout << "Tested " << i + 1 << "/100 endpoints..." << std::endl;
        }
    }
    
    // Create result JSON
    Json::Value result;
    result["total_tests"] = 100;
    result["passed"] = passed;
    result["failed"] = failed;
    
    double successRate = static_cast<double>(passed) / 100.0 * 100.0;
    std::ostringstream rateStream;
    rateStream << std::fixed << std::setprecision(1) << successRate << "%";
    result["success_rate"] = rateStream.str();
    
    // Save results
    std::ofstream outputFile("test-result.json");
    if (outputFile.is_open()) {
        Json::StreamWriterBuilder builder;
        std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
        writer->write(result, &outputFile);
        outputFile.close();
        std::cout << "Results saved to: test-result.json" << std::endl;
    }
    
    std::cout << "\nTest completed!" << std::endl;
    std::cout << "Passed: " << passed << "/100 (" << std::fixed << std::setprecision(1) << successRate << "%)" << std::endl;
    std::cout << "Failed: " << failed << "/100 (" << std::fixed << std::setprecision(1) << (100.0 - successRate) << "%)" << std::endl;
    
    // Cleanup
    curl_easy_cleanup(curl);
    curl_global_cleanup();
}

int main() {
    hello();
    testWebServer();
    return 0;
}
