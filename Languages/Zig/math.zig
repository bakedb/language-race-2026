const std = @import("std");
const fs = std.fs;
const print = std.debug.print;

// Hello, World!
fn hello() void {
    print("Hello, World!\n");
}

fn evaluateExpression(expr: []const u8) f64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    
    var parts = std.mem.tokenize(u8, expr, " ");
    
    const a_str = parts.next() orelse return 0.0;
    const op_str = parts.next() orelse return 0.0;
    const b_str = parts.next() orelse return 0.0;
    
    const a = std.fmt.parseFloat(f64, a_str) catch 0.0;
    const b = std.fmt.parseFloat(f64, b_str) catch 0.0;
    
    switch (op_str[0]) {
        '+' => return a + b,
        '-' => return a - b,
        '*' => return a * b,
        '/' => {
            if (b != 0.0) return a / b;
            return 0.0;
        },
        else => return 0.0,
    }
}

fn solveEquation(line: []const u8) void {
    var equation = line;
    
    // Remove "= ?" part
    if (std.mem.indexOf(u8, equation, "=")) |equals_pos| {
        equation = equation[0..equals_pos];
    }
    
    // Trim whitespace
    equation = std.mem.trim(u8, equation, " \t\r\n");
    
    const result = evaluateExpression(equation);
    print("{s} = {d:.2}\n", .{ equation, result });
}

fn processFile(filename: []const u8) void {
    const file = fs.cwd().openFile(filename, .{}) catch {
        print("Could not open file: {s}\n", .{filename});
        return;
    };
    defer file.close();
    
    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();
    
    var line_buf: [1024]u8 = undefined;
    while (reader.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        // Skip empty lines and markdown headers
        if (line.len == 0 or line[0] == '#') continue;
        
        // Handle markdown list items
        var start = line;
        if (line.len >= 2 and line[0] == '-' and line[1] == ' ') {
            start = line[2..];
        }
        
        if (std.mem.indexOf(u8, start, "=")) |_| {
            solveEquation(start);
        }
    } else |err| {
        print("Error reading file: {}\n", .{err});
    }
}

pub fn main() void {
    hello();
    
    print("\nProcessing math equations...\n");
    
    processFile("../test_data/math_equations.txt");
    processFile("../test_data/math_equations.md");
    processFile("../test_data/math_equations.json");
    processFile("../test_data/math_equations.yaml");
    processFile("../test_data/math_equations");
}
