using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

public class BeeMovie
{
    public static void Main(string[] args)
    {
        // Hello, World!
        Console.WriteLine("Hello, World!");
        AnalyzeBeeMovie();
    }
    
    private static void AnalyzeBeeMovie()
    {
        string scriptPath = "../beemoviescript.txt";
        
        if (!File.Exists(scriptPath))
        {
            Console.WriteLine($"File not found: {scriptPath}");
            return;
        }
        
        Console.WriteLine("Bee Movie Script:");
        Console.WriteLine("--------------------------------------------------");
        
        int[] letterCounts = new int[26];
        int totalLetters = 0;
        
        try
        {
            string[] lines = File.ReadAllLines(scriptPath);
            
            // Print each line
            foreach (string line in lines)
            {
                Console.WriteLine(line);
                
                // Count letters
                foreach (char c in line)
                {
                    if (char.IsLetter(c))
                    {
                        char lower = char.ToLower(c);
                        letterCounts[lower - 'a']++;
                        totalLetters++;
                    }
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error reading file: {e.Message}");
            return;
        }
        
        Console.WriteLine("--------------------------------------------------");
        Console.WriteLine("Analysis complete.");
        
        if (totalLetters > 0)
        {
            // Create list of letter counts
            var counts = new List<LetterCount>();
            for (int i = 0; i < 26; i++)
            {
                counts.Add(new LetterCount((char)('a' + i), letterCounts[i]));
            }
            
            // Sort by count (descending)
            counts = counts.OrderByDescending(x => x.Count).ToList();
            
            Console.WriteLine("\nTop 3 most commonly used letters:");
            for (int i = 0; i < 3 && counts[i].Count > 0; i++)
            {
                Console.WriteLine($"{i + 1}. '{counts[i].Letter}': {counts[i].Count} times");
            }
        }
        else
        {
            Console.WriteLine("No letters found in the script.");
        }
    }
    
    class LetterCount
    {
        public char Letter { get; }
        public int Count { get; }
        
        public LetterCount(char letter, int count)
        {
            Letter = letter;
            Count = count;
        }
    }
}
