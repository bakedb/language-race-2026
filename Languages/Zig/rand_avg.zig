const std = @import("std");
const fs = std.fs;
const print = std.debug.print;

// Hello, World!
fn hello() void {
    print("Hello, World!\n");
}

fn generateRandomNumbers() !void {
    const count = 1000;
    const output_dir = "../rand_avg output";
    const output_file = "../rand_avg output/random_numbers.txt";
    
    // Create output directory
    try fs.cwd().makePath(output_dir);
    
    // Random number generator
    var prng = std.rand.DefaultPrng.init(@intCast(std.time.timestamp()));
    var random = prng.random();
    
    // Generate 1000 random numbers
    var sum: u64 = 0;
    
    const file = try fs.cwd().createFile(output_file, .{});
    defer file.close();
    
    var writer = file.writer();
    
    for (0..count) |_| {
        const num = random.intRangeAtMost(u32, 0, 999);
        sum += num;
        try writer.print("{}\n", .{num});
    }
    
    // Calculate mean
    const mean = @as(f64, @floatFromInt(sum)) / @as(f64, @floatFromInt(count));
    
    print("Generated 1000 random numbers\n");
    print("Mean: {d:.2}\n", .{mean});
    print("Saved to: {s}\n", .{output_file});
}

pub fn main() void {
    hello();
    
    if (generateRandomNumbers()) |err| {
        print("Error: {}\n", .{err});
    } else |_| {
        // Success
    }
}
