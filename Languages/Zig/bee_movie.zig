const std = @import("std");
const fs = std.fs;
const print = std.debug.print;

// Hello, World!
fn hello() void {
    print("Hello, World!\n");
}

const LetterCount = struct {
    letter: u8,
    count: usize,
};

fn analyzeBeeMovie() !void {
    const script_path = "../beemoviescript.txt";
    
    const file = fs.cwd().openFile(script_path, .{}) catch {
        print("File not found: {s}\n", .{script_path});
        return;
    };
    defer file.close();
    
    print("Bee Movie Script:\n");
    print("--------------------------------------------------\n");
    
    var letter_counts: [26]usize = std.mem.zeroes([26]usize);
    var total_letters: usize = 0;
    
    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();
    
    var line_buf: [1024]u8 = undefined;
    while (reader.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        print("{s}\n", .{line});
        
        // Count letters
        for (line) |byte| {
            if (std.ascii.isAlphabetic(byte)) {
                const lower = std.ascii.toLower(byte);
                letter_counts[lower - 'a'] += 1;
                total_letters += 1;
            }
        }
    } else |err| {
        print("Error reading file: {}\n", .{err});
        return;
    }
    
    print("--------------------------------------------------\n");
    print("Analysis complete.\n");
    
    if (total_letters > 0) {
        // Create array of LetterCount structs
        var counts: [26]LetterCount = undefined;
        for (letter_counts, 0..) |count, i| {
            counts[i] = LetterCount{ .letter = @intCast(i + 'a'), .count = count };
        }
        
        // Sort by count (descending)
        std.sort.sort(LetterCount, &counts, {}, struct {
            fn lessThan(context: void, a: LetterCount, b: LetterCount) bool {
                return a.count > b.count;
            }
        }.lessThan);
        
        print("\nTop 3 most commonly used letters:\n");
        for (counts[0..3]) |item, i| {
            if (item.count > 0) {
                print("{}: '{c}': {} times\n", .{ i + 1, item.letter, item.count });
            }
        }
    } else {
        print("No letters found in the script.\n");
    }
}

pub fn main() void {
    hello();
    
    if (analyzeBeeMovie()) |err| {
        print("Error: {}\n", .{err});
    } else |_| {
        // Success
    }
}
