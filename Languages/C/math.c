#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Hello, World!
void hello() {
    printf("Hello, World!\n");
}

// Simple expression evaluator
double evaluate_expression(const char* expr) {
    char* expr_copy = strdup(expr);
    char* token = strtok(expr_copy, " ");
    
    if (!token) return 0.0;
    
    double a = atof(token);
    
    token = strtok(NULL, " ");
    if (!token) {
        free(expr_copy);
        return a;
    }
    
    char op = token[0];
    
    token = strtok(NULL, " ");
    if (!token) {
        free(expr_copy);
        return a;
    }
    
    double b = atof(token);
    
    double result;
    switch (op) {
        case '+': result = a + b; break;
        case '-': result = a - b; break;
        case '*': result = a * b; break;
        case '/': result = b != 0 ? a / b : 0.0; break;
        default: result = 0.0; break;
    }
    
    free(expr_copy);
    return result;
}

void solve_equation(const char* line) {
    char equation[256];
    strncpy(equation, line, sizeof(equation) - 1);
    equation[sizeof(equation) - 1] = '\0';
    
    // Remove "= ?" part
    char* equals_pos = strstr(equation, "=");
    if (equals_pos) {
        *equals_pos = '\0';
    }
    
    // Trim whitespace
    char* end = equation + strlen(equation) - 1;
    while (end > equation && isspace((unsigned char)*end)) end--;
    *(end + 1) = '\0';
    
    double result = evaluate_expression(equation);
    printf("%s = %.2f\n", equation, result);
}

void process_file(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        printf("Could not open file: %s\n", filename);
        return;
    }
    
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;
        
        // Skip empty lines and markdown headers
        if (strlen(line) == 0 || line[0] == '#') continue;
        
        // Handle markdown list items
        char* start = line;
        if (strncmp(line, "- ", 2) == 0) {
            start = line + 2;
        }
        
        if (strstr(start, "=")) {
            solve_equation(start);
        }
    }
    
    fclose(file);
}

int main() {
    hello();
    
    printf("\nProcessing math equations...\n");
    
    process_file("../test_data/math_equations.txt");
    process_file("../test_data/math_equations.md");
    process_file("../test_data/math_equations.json");
    process_file("../test_data/math_equations.yaml");
    process_file("../test_data/math_equations");
    
    return 0;
}
