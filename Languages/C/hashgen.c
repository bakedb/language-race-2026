#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <time.h>
#include <openssl/sha.h>
#include <json-c/json.h>

#define MAX_PATH_LENGTH 512
#define MAX_FILES 2000
#define CHUNK_SIZE 4096

// Hello, World!
void hello() {
    printf("Hello, World!\n");
}

typedef struct {
    char filename[256];
    char filepath[MAX_PATH_LENGTH];
    long size;
    char hash[65];  // SHA256 produces 64 chars + null terminator
    char status[16];
    char error[128];
} FileHashResult;

typedef struct {
    int total_files;
    int successful_hashes;
    int failed_hashes;
    double processing_time_seconds;
    double average_time_per_file_ms;
    char timestamp[32];
    char directory[MAX_PATH_LENGTH];
    FileHashResult results[MAX_FILES];
    int result_count;
} HashResults;

char* calculate_file_hash(const char* filepath) {
    static char hash_output[65];
    FILE *file;
    SHA256_CTX sha256;
    unsigned char hash[SHA256_DIGEST_LENGTH];
    unsigned char buffer[CHUNK_SIZE];
    size_t bytes_read;
    
    file = fopen(filepath, "rb");
    if (!file) {
        strcpy(hash_output, "");
        return hash_output;
    }
    
    SHA256_Init(&sha256);
    
    while ((bytes_read = fread(buffer, 1, CHUNK_SIZE, file)) > 0) {
        SHA256_Update(&sha256, buffer, bytes_read);
    }
    
    fclose(file);
    
    SHA256_Final(hash, &sha256);
    
    // Convert hash to hex string
    for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
        sprintf(hash_output + (i * 2), "%02x", hash[i]);
    }
    hash_output[64] = '\0';
    
    return hash_output;
}

long get_file_size(const char* filepath) {
    struct stat st;
    if (stat(filepath, &st) == 0) {
        return st.st_size;
    }
    return 0;
}

int get_txt_files(const char* directory, char files[MAX_FILES][MAX_PATH_LENGTH]) {
    DIR *dir;
    struct dirent *entry;
    int count = 0;
    
    dir = opendir(directory);
    if (!dir) {
        return 0;
    }
    
    while ((entry = readdir(dir)) != NULL && count < MAX_FILES) {
        if (strstr(entry->d_name, ".txt") != NULL) {
            snprintf(files[count], MAX_PATH_LENGTH, "%s/%s", directory, entry->d_name);
            count++;
        }
    }
    
    closedir(dir);
    
    // Sort files alphabetically
    for (int i = 0; i < count - 1; i++) {
        for (int j = i + 1; j < count; j++) {
            if (strcmp(files[i], files[j]) > 0) {
                char temp[MAX_PATH_LENGTH];
                strcpy(temp, files[i]);
                strcpy(files[i], files[j]);
                strcpy(files[j], temp);
            }
        }
    }
    
    return count;
}

void hash_files_in_directory(const char* directory) {
    char files[MAX_FILES][MAX_PATH_LENGTH];
    int file_count = get_txt_files(directory, files);
    
    if (file_count == 0) {
        printf("No .txt files found in directory: %s\n", directory);
        return;
    }
    
    printf("Found %d files to hash\n", file_count);
    
    HashResults results = {0};
    results.result_count = 0;
    results.total_files = file_count;
    strcpy(results.directory, directory);
    strcpy(results.algorithm, "sha256");
    
    clock_t start_time = clock();
    
    for (int i = 0; i < file_count; i++) {
        FileHashResult *result = &results.results[results.result_count];
        
        // Extract filename from filepath
        char *filename = strrchr(files[i], '/');
        if (filename) {
            filename++;  // Skip the '/'
        } else {
            filename = files[i];
        }
        
        strcpy(result->filename, filename);
        strcpy(result->filepath, files[i]);
        result->size = get_file_size(files[i]);
        
        char* hash = calculate_file_hash(files[i]);
        if (strlen(hash) > 0) {
            strcpy(result->hash, hash);
            strcpy(result->status, "SUCCESS");
            results.successful_hashes++;
        } else {
            strcpy(result->hash, "");
            strcpy(result->status, "FAILED");
            strcpy(result->error, "Hash calculation failed");
            results.failed_hashes++;
        }
        
        results.result_count++;
        
        // Progress indicator
        if ((i + 1) % 100 == 0) {
            printf("Processed %d/%d files...\n", i + 1, file_count);
        }
    }
    
    clock_t end_time = clock();
    results.processing_time_seconds = ((double)(end_time - start_time)) / CLOCKS_PER_SEC;
    results.average_time_per_file_ms = (results.processing_time_seconds / file_count) * 1000.0;
    
    // Get timestamp
    time_t now = time(NULL);
    struct tm *tm_info = localtime(&now);
    strftime(results.timestamp, sizeof(results.timestamp), "%Y-%m-%d %H:%M:%S", tm_info);
    
    // Create JSON output
    json_object *json_results = json_object_new_object();
    json_object_object_add(json_results, "total_files", json_object_new_int(results.total_files));
    json_object_object_add(json_results, "successful_hashes", json_object_new_int(results.successful_hashes));
    json_object_object_add(json_results, "failed_hashes", json_object_new_int(results.failed_hashes));
    json_object_object_add(json_results, "algorithm", json_object_new_string("sha256"));
    json_object_object_add(json_results, "processing_time_seconds", json_object_new_double(results.processing_time_seconds));
    json_object_object_add(json_results, "average_time_per_file_ms", json_object_new_double(results.average_time_per_file_ms));
    json_object_object_add(json_results, "timestamp", json_object_new_string(results.timestamp));
    json_object_object_add(json_results, "directory", json_object_new_string(results.directory));
    
    json_object *json_results_array = json_object_new_array();
    
    for (int i = 0; i < results.result_count; i++) {
        FileHashResult *result = &results.results[i];
        json_object *json_result = json_object_new_object();
        
        json_object_object_add(json_result, "filename", json_object_new_string(result->filename));
        json_object_object_add(json_result, "filepath", json_object_new_string(result->filepath));
        json_object_object_add(json_result, "size", json_object_new_int64(result->size));
        json_object_object_add(json_result, "algorithm", json_object_new_string("sha256"));
        json_object_object_add(json_result, "hash", json_object_new_string(result->hash));
        json_object_object_add(json_result, "status", json_object_new_string(result->status));
        
        if (strlen(result->error) > 0) {
            json_object_object_add(json_result, "error", json_object_new_string(result->error));
        }
        
        json_object_array_add(json_results_array, json_result);
    }
    
    json_object_object_add(json_results, "results", json_results_array);
    
    // Save to file
    FILE *output_file = fopen("hash_results.json", "w");
    if (output_file) {
        fprintf(output_file, "%s", json_object_to_json_string_ext(json_results, JSON_C_TO_STRING_PRETTY));
        fclose(output_file);
        printf("Results saved to: hash_results.json\n");
    } else {
        printf("Error saving results\n");
    }
    
    json_object_put(json_results);
    
    printf("\nHashing completed!\n");
    printf("Total files: %d\n", results.total_files);
    printf("Successful: %d\n", results.successful_hashes);
    printf("Failed: %d\n", results.failed_hashes);
    printf("Processing time: %.3f seconds\n", results.processing_time_seconds);
    printf("Average time per file: %.2f ms\n", results.average_time_per_file_ms);
}

int main() {
    hello();
    hash_files_in_directory("../hashfiles");
    return 0;
}
