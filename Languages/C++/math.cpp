#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>

// Hello, World!
void hello() {
    std::cout << "Hello, World!" << std::endl;
}

double evaluate_expression(const std::string& expr) {
    std::istringstream iss(expr);
    std::string token;
    std::vector<std::string> tokens;
    
    while (iss >> token) {
        tokens.push_back(token);
    }
    
    if (tokens.empty()) return 0.0;
    
    double a = std::stod(tokens[0]);
    
    if (tokens.size() == 1) return a;
    
    char op = tokens[1][0];
    double b = std::stod(tokens[2]);
    
    switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/': return b != 0 ? a / b : 0.0;
        default: return 0.0;
    }
}

void solve_equation(const std::string& line) {
    std::string equation = line;
    
    // Remove "= ?" part
    size_t equals_pos = equation.find('=');
    if (equals_pos != std::string::npos) {
        equation = equation.substr(0, equals_pos);
    }
    
    // Trim whitespace
    equation.erase(0, equation.find_first_not_of(" \t"));
    equation.erase(equation.find_last_not_of(" \t") + 1);
    
    double result = evaluate_expression(equation);
    std::cout << equation << " = " << std::fixed << std::setprecision(2) << result << std::endl;
}

void process_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cout << "Could not open file: " << filename << std::endl;
        return;
    }
    
    std::string line;
    while (std::getline(file, line)) {
        // Skip empty lines and markdown headers
        if (line.empty() || line[0] == '#') continue;
        
        // Handle markdown list items
        std::string start = line;
        if (line.substr(0, 2) == "- ") {
            start = line.substr(2);
        }
        
        if (start.find('=') != std::string::npos) {
            solve_equation(start);
        }
    }
    
    file.close();
}

int main() {
    hello();
    
    std::cout << "\nProcessing math equations..." << std::endl;
    
    process_file("../test_data/math_equations.txt");
    process_file("../test_data/math_equations.md");
    process_file("../test_data/math_equations.json");
    process_file("../test_data/math_equations.yaml");
    process_file("../test_data/math_equations");
    
    return 0;
}
