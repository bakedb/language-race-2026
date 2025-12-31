import java.io.*;
import java.util.*;

public class BeeMovie {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        analyzeBeeMovie();
    }
    
    private static void analyzeBeeMovie() {
        String scriptPath = "../beemoviescript.txt";
        File file = new File(scriptPath);
        
        if (!file.exists()) {
            System.out.println("File not found: " + scriptPath);
            return;
        }
        
        System.out.println("Bee Movie Script:");
        System.out.println("--------------------------------------------------");
        
        int[] letterCounts = new int[26];
        int totalLetters = 0;
        
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
                
                // Count letters
                for (char c : line.toCharArray()) {
                    if (Character.isLetter(c)) {
                        char lower = Character.toLowerCase(c);
                        letterCounts[lower - 'a']++;
                        totalLetters++;
                    }
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }
        
        System.out.println("--------------------------------------------------");
        System.out.println("Analysis complete.");
        
        if (totalLetters > 0) {
            // Create list of letter counts
            List<LetterCount> counts = new ArrayList<>();
            for (int i = 0; i < 26; i++) {
                counts.add(new LetterCount((char)('a' + i), letterCounts[i]));
            }
            
            // Sort by count (descending)
            counts.sort((a, b) -> Integer.compare(b.count, a.count));
            
            System.out.println("\nTop 3 most commonly used letters:");
            for (int i = 0; i < 3 && counts.get(i).count > 0; i++) {
                LetterCount lc = counts.get(i);
                System.out.printf("%d. '%c': %d times%n", i + 1, lc.letter, lc.count);
            }
        } else {
            System.out.println("No letters found in the script.");
        }
    }
    
    static class LetterCount {
        char letter;
        int count;
        
        LetterCount(char letter, int count) {
            this.letter = letter;
            this.count = count;
        }
    }
}
