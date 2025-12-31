#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 1024
#define ALPHABET_SIZE 26

// Hello, World!
void hello() {
    printf("Hello, World!\n");
}

typedef struct {
    char letter;
    int count;
} LetterCount;

void analyze_bee_movie() {
    const char* script_path = "../beemoviescript.txt";
    FILE* file = fopen(script_path, "r");
    
    if (!file) {
        printf("File not found: %s\n", script_path);
        return;
    }
    
    printf("Bee Movie Script:\n");
    printf("--------------------------------------------------\n");
    
    char line[MAX_LINE_LENGTH];
    int letter_counts[ALPHABET_SIZE] = {0};
    int total_letters = 0;
    
    // Read file line by line and print each line
    while (fgets(line, sizeof(line), file)) {
        printf("%s", line);
        
        // Count letters
        for (int i = 0; line[i] != '\0'; i++) {
            if (isalpha(line[i])) {
                char lower = tolower(line[i]);
                letter_counts[lower - 'a']++;
                total_letters++;
            }
        }
    }
    
    fclose(file);
    
    printf("--------------------------------------------------\n");
    printf("Analysis complete.\n");
    
    if (total_letters > 0) {
        // Create array of LetterCount structs
        LetterCount counts[ALPHABET_SIZE];
        for (int i = 0; i < ALPHABET_SIZE; i++) {
            counts[i].letter = 'a' + i;
            counts[i].count = letter_counts[i];
        }
        
        // Sort by count (descending)
        for (int i = 0; i < ALPHABET_SIZE - 1; i++) {
            for (int j = 0; j < ALPHABET_SIZE - i - 1; j++) {
                if (counts[j].count < counts[j + 1].count) {
                    LetterCount temp = counts[j];
                    counts[j] = counts[j + 1];
                    counts[j + 1] = temp;
                }
            }
        }
        
        printf("\nTop 3 most commonly used letters:\n");
        for (int i = 0; i < 3 && counts[i].count > 0; i++) {
            printf("%d. '%c': %d times\n", i + 1, counts[i].letter, counts[i].count);
        }
    } else {
        printf("No letters found in the script.\n");
    }
}

int main() {
    hello();
    analyze_bee_movie();
    return 0;
}
