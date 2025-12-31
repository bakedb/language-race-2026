#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include <json-c/json.h>

#define MAX_URL_LENGTH 256
#define MAX_RESPONSE_LENGTH 1024
#define BASE_URL "http://localhost:3000"

// Hello, World!
void hello() {
    printf("Hello, World!\n");
}

struct ResponseData {
    char data[MAX_RESPONSE_LENGTH];
    size_t size;
};

size_t write_callback(void *contents, size_t size, size_t nmemb, struct ResponseData *response) {
    size_t total_size = size * nmemb;
    if (response->size + total_size < MAX_RESPONSE_LENGTH - 1) {
        memcpy(response->data + response->size, contents, total_size);
        response->size += total_size;
        response->data[response->size] = '\0';
    }
    return total_size;
}

json_object* load_compare_json(const char* filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        printf("Error: Could not open %s\n", filename);
        return NULL;
    }
    
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    char *buffer = malloc(file_size + 1);
    fread(buffer, 1, file_size, fp);
    buffer[file_size] = '\0';
    fclose(fp);
    
    json_object *obj = json_tokener_parse(buffer);
    free(buffer);
    return obj;
}

void test_web_server() {
    CURL *curl;
    CURLcode res;
    
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if (!curl) {
        printf("Error: Failed to initialize curl\n");
        return;
    }
    
    // Load expected hashes
    json_object *compare_obj = load_compare_json("../webserver/compare.json");
    if (!compare_obj) {
        curl_easy_cleanup(curl);
        curl_global_cleanup();
        return;
    }
    
    printf("Testing 100 endpoints...\n");
    
    int passed = 0;
    int failed = 0;
    
    // Test each endpoint
    for (int i = 0; i < 100; i++) {
        char url[MAX_URL_LENGTH];
        char endpoint[32];
        snprintf(endpoint, sizeof(endpoint), "test-%d", i);
        snprintf(url, sizeof(url), "%s/%s", BASE_URL, endpoint);
        
        struct ResponseData response = {0};
        
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        
        res = curl_easy_perform(curl);
        
        if (res == CURLE_OK) {
            long response_code;
            curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
            
            if (response_code == 200) {
                // Parse JSON response
                json_object *response_obj = json_tokener_parse(response.data);
                if (response_obj) {
                    json_object *hash_obj;
                    if (json_object_object_get_ex(response_obj, "hash", &hash_obj)) {
                        const char *server_hash = json_object_get_string(hash_obj);
                        
                        // Get expected hash
                        json_object *expected_obj;
                        const char *expected_hash = "";
                        if (json_object_object_get_ex(compare_obj, endpoint, &expected_obj)) {
                            expected_hash = json_object_get_string(expected_obj);
                        }
                        
                        if (strcmp(server_hash, expected_hash) == 0) {
                            passed++;
                        } else {
                            failed++;
                        }
                    }
                    json_object_put(response_obj);
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
            printf("Tested %d/100 endpoints...\n", i + 1);
        }
    }
    
    // Create result JSON
    json_object *result_obj = json_object_new_object();
    json_object_object_add(result_obj, "total_tests", json_object_new_int(100));
    json_object_object_add(result_obj, "passed", json_object_new_int(passed));
    json_object_object_add(result_obj, "failed", json_object_new_int(failed));
    
    double success_rate = (double)passed / 100.0 * 100.0;
    char rate_str[32];
    snprintf(rate_str, sizeof(rate_str), "%.1f%%", success_rate);
    json_object_object_add(result_obj, "success_rate", json_object_new_string(rate_str));
    
    // Save results
    FILE *output_fp = fopen("test-result.json", "w");
    if (output_fp) {
        const char *result_str = json_object_to_json_string_ext(result_obj, JSON_C_TO_STRING_PRETTY);
        fprintf(output_fp, "%s", result_str);
        fclose(output_fp);
        printf("Results saved to: test-result.json\n");
    }
    
    printf("\nTest completed!\n");
    printf("Passed: %d/100 (%.1f%%)\n", passed, success_rate);
    printf("Failed: %d/100 (%.1f%%)\n", failed, 100.0 - success_rate);
    
    // Cleanup
    json_object_put(result_obj);
    json_object_put(compare_obj);
    curl_easy_cleanup(curl);
    curl_global_cleanup();
}

int main() {
    hello();
    test_web_server();
    return 0;
}
