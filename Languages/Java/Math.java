import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Math {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        
        System.out.println("\nProcessing math equations...");
        
        processFile("../test_data/math_equations.txt");
        processFile("../test_data/math_equations.md");
        processFile("../test_data/math_equations.json");
        processFile("../test_data/math_equations.yaml");
        processFile("../test_data/math_equations");
    }
    
    private static double evaluateExpression(String expr) {
        // Remove extra whitespace
        expr = expr.trim();
        
        // Use regex to parse simple expressions
        Pattern pattern = Pattern.compile("([\\d.]+)\\s*([+\\-*/])\\s*([\\d.]+)");
        Matcher matcher = pattern.matcher(expr);
        
        if (matcher.find()) {
            double a = Double.parseDouble(matcher.group(1));
            char op = matcher.group(2).charAt(0);
            double b = Double.parseDouble(matcher.group(3));
            
            switch (op) {
                case '+': return a + b;
                case '-': return a - b;
                case '*': return a * b;
                case '/': return b != 0 ? a / b : 0.0;
                default: return 0.0;
            }
        }
        
        return 0.0;
    }
    
    private static void solveEquation(String line) {
        String equation = line;
        
        // Remove "= ?" part
        int equalsPos = equation.indexOf('=');
        if (equalsPos != -1) {
            equation = equation.substring(0, equalsPos);
        }
        
        // Trim whitespace
        equation = equation.trim();
        
        double result = evaluateExpression(equation);
        System.out.printf("%s = %.2f%n", equation, result);
    }
    
    private static void processFile(String filename) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                // Skip empty lines and markdown headers
                if (line.isEmpty() || line.startsWith("#")) continue;
                
                // Handle markdown list items
                String start = line;
                if (line.startsWith("- ")) {
                    start = line.substring(2);
                }
                
                if (start.contains("=")) {
                    solveEquation(start);
                }
            }
        } catch (IOException e) {
            System.out.println("Could not open file: " + filename);
        }
    }
}
