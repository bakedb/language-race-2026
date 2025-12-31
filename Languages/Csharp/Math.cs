using System;
using System.IO;
using System.Text.RegularExpressions;

public class Math
{
    public static void Main(string[] args)
    {
        // Hello, World!
        Console.WriteLine("Hello, World!");
        
        Console.WriteLine("\nProcessing math equations...");
        
        ProcessFile("../test_data/math_equations.txt");
        ProcessFile("../test_data/math_equations.md");
        ProcessFile("../test_data/math_equations.json");
        ProcessFile("../test_data/math_equations.yaml");
        ProcessFile("../test_data/math_equations");
    }
    
    private static double EvaluateExpression(string expr)
    {
        // Remove extra whitespace
        expr = expr.Trim();
        
        // Use regex to parse simple expressions
        var pattern = @"([\d.]+)\s*([+\-*/])\s*([\d.]+)";
        var match = Regex.Match(expr, pattern);
        
        if (match.Success)
        {
            double a = double.Parse(match.Groups[1].Value);
            char op = match.Groups[2].Value[0];
            double b = double.Parse(match.Groups[3].Value);
            
            switch (op)
            {
                case '+': return a + b;
                case '-': return a - b;
                case '*': return a * b;
                case '/': return b != 0 ? a / b : 0.0;
                default: return 0.0;
            }
        }
        
        return 0.0;
    }
    
    private static void SolveEquation(string line)
    {
        string equation = line;
        
        // Remove "= ?" part
        int equalsPos = equation.IndexOf('=');
        if (equalsPos != -1)
        {
            equation = equation.Substring(0, equalsPos);
        }
        
        // Trim whitespace
        equation = equation.Trim();
        
        double result = EvaluateExpression(equation);
        Console.WriteLine($"{equation} = {result:F2}");
    }
    
    private static void ProcessFile(string filename)
    {
        try
        {
            string[] lines = File.ReadAllLines(filename);
            
            foreach (string line in lines)
            {
                // Skip empty lines and markdown headers
                if (string.IsNullOrWhiteSpace(line) || line.StartsWith("#"))
                    continue;
                
                // Handle markdown list items
                string start = line;
                if (line.StartsWith("- "))
                {
                    start = line.Substring(2);
                }
                
                if (start.Contains("="))
                {
                    SolveEquation(start);
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"Could not open file: {filename}");
        }
    }
}
