using System;
using System.IO;
using System.Collections.Generic;

public class RandAvg
{
    public static void Main(string[] args)
    {
        // Hello, World!
        Console.WriteLine("Hello, World!");
        GenerateRandomNumbers();
    }
    
    private static void GenerateRandomNumbers()
    {
        const int count = 1000;
        const string outputDir = "../rand_avg output";
        const string outputFile = outputDir + "/random_numbers.txt";
        
        // Create output directory
        if (!Directory.Exists(outputDir))
        {
            Directory.CreateDirectory(outputDir);
        }
        
        // Random number generator
        Random random = new Random();
        
        // Generate 1000 random numbers
        List<int> randomNumbers = new List<int>();
        long sum = 0;
        
        for (int i = 0; i < count; i++)
        {
            int num = random.Next(0, 1000);
            randomNumbers.Add(num);
            sum += num;
        }
        
        // Calculate mean
        double mean = (double)sum / count;
        
        // Save to file
        try
        {
            using (StreamWriter writer = new StreamWriter(outputFile))
            {
                foreach (int num in randomNumbers)
                {
                    writer.WriteLine(num);
                }
            }
            
            Console.WriteLine("Generated 1000 random numbers");
            Console.WriteLine($"Mean: {mean:F2}");
            Console.WriteLine($"Saved to: {outputFile}");
        }
        catch (Exception e)
        {
            Console.WriteLine("Could not create output file");
        }
    }
}
