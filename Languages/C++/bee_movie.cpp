#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <iomanip>

// Hello, World!
void hello() {
    std::cout << "Hello, World!" << std::endl;
}

struct LetterCount {
    char letter;
    int count;
};

void analyze_bee_movie() {
    const std::string script_path = "../beemoviescript.txt";
    std::ifstream file(script_path);
    
    if (!file.is_open()) {
        std::cout << "File not found: " << script_path << std::endl;
        return;
    }
    
    std::cout << "Bee Movie Script:" << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;
    
    std::string line;
    int letter_counts[26] = {0};
    int total_letters = 0;
    
    // Read file line by line and print each line
    while (std::getline(file, line)) {
        std::cout << line << std::endl;
        
        // Count letters
        for (char c : line) {
            if (std::isalpha(c)) {
                char lower = std::tolower(c);
                letter_counts[lower - 'a']++;
                total_letters++;
            }
        }
    }
    
    file.close();
    
    std::cout << "--------------------------------------------------" << std::endl;
    std::cout << "Analysis complete." << std::endl;
    
    if (total_letters > 0) {
        // Create vector of LetterCount structs
        std::vector<LetterCount> counts;
        for (int i = 0; i < 26; i++) {
            counts.push_back({static_cast<char>('a' + i), letter_counts[i]});
        }
        
        // Sort by count (descending)
        std::sort(counts.begin(), counts.end(), 
                  [](const LetterCount& a, const LetterCount& b) {
                      return a.count > b.count;
                  });
        
        std::cout << "\nTop 3 most commonly used letters:" << std::endl;
        for (int i = 0; i < 3 && counts[i].count > 0; i++) {
            std::cout << i + 1 << ". '" << counts[i].letter << "': " 
                      << counts[i].count << " times" << std::endl;
        }
    } else {
        std::cout << "No letters found in the script." << std::endl;
    }
}

int main() {
    hello();
    analyze_bee_movie();
    return 0;
}
