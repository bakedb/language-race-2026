import java.io.*;
import java.util.*;
import java.nio.file.*;

public class RandAvg {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        generateRandomNumbers();
    }
    
    private static void generateRandomNumbers() {
        List<Integer> randomNumbers = new ArrayList<>();
        final int count = 1000;
        final String outputDir = "../rand_avg output";
        final String outputFile = outputDir + "/random_numbers.txt";
        
        // Create output directory
        try {
            Files.createDirectories(Paths.get(outputDir));
        } catch (IOException e) {
            System.out.println("Could not create output directory");
            return;
        }
        
        // Random number generator
        Random random = new Random();
        
        // Generate 1000 random numbers
        for (int i = 0; i < count; i++) {
            randomNumbers.add(random.nextInt(1000));
        }
        
        // Calculate mean
        double sum = 0;
        for (int num : randomNumbers) {
            sum += num;
        }
        double mean = sum / count;
        
        // Save to file
        try (PrintWriter writer = new PrintWriter(new FileWriter(outputFile))) {
            for (int num : randomNumbers) {
                writer.println(num);
            }
        } catch (IOException e) {
            System.out.println("Could not create output file");
            return;
        }
        
        System.out.println("Generated 1000 random numbers");
        System.out.printf("Mean: %.2f%n", mean);
        System.out.println("Saved to: " + outputFile);
    }
}
