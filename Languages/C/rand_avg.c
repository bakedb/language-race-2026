#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/stat.h>

// Hello, World!
void hello() {
    printf("Hello, World!\n");
}

void generate_random_numbers() {
    int random_numbers[1000];
    int i;
    long long sum = 0;
    double mean;
    FILE *file;
    const char *output_dir = "../rand_avg output";
    const char *output_file = "../rand_avg output/random_numbers.txt";
    
    // Create output directory
    mkdir(output_dir, 0755);
    
    // Seed random number generator
    srand(time(NULL));
    
    // Generate 1000 random numbers
    for (i = 0; i < 1000; i++) {
        random_numbers[i] = rand() % 1000;
        sum += random_numbers[i];
    }
    
    // Calculate mean
    mean = (double)sum / 1000.0;
    
    // Save to file
    file = fopen(output_file, "w");
    if (file == NULL) {
        printf("Could not create output file\n");
        return;
    }
    
    for (i = 0; i < 1000; i++) {
        fprintf(file, "%d\n", random_numbers[i]);
    }
    
    fclose(file);
    
    printf("Generated 1000 random numbers\n");
    printf("Mean: %.2f\n", mean);
    printf("Saved to: %s\n", output_file);
}

int main() {
    hello();
    generate_random_numbers();
    return 0;
}
