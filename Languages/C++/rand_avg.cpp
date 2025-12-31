#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <numeric>
#include <iomanip>
#include <filesystem>

// Hello, World!
void hello() {
    std::cout << "Hello, World!" << std::endl;
}

void generate_random_numbers() {
    std::vector<int> random_numbers;
    const int count = 1000;
    const std::string output_dir = "../rand_avg output";
    const std::string output_file = output_dir + "/random_numbers.txt";
    
    // Create output directory
    std::filesystem::create_directories(output_dir);
    
    // Random number generator
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(0, 999);
    
    // Generate 1000 random numbers
    for (int i = 0; i < count; ++i) {
        random_numbers.push_back(distrib(gen));
    }
    
    // Calculate mean
    double sum = std::accumulate(random_numbers.begin(), random_numbers.end(), 0.0);
    double mean = sum / count;
    
    // Save to file
    std::ofstream file(output_file);
    if (!file.is_open()) {
        std::cout << "Could not create output file" << std::endl;
        return;
    }
    
    for (int num : random_numbers) {
        file << num << "\n";
    }
    
    file.close();
    
    std::cout << "Generated 1000 random numbers" << std::endl;
    std::cout << "Mean: " << std::fixed << std::setprecision(2) << mean << std::endl;
    std::cout << "Saved to: " << output_file << std::endl;
}

int main() {
    hello();
    generate_random_numbers();
    return 0;
}
