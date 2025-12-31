const std = @import("std");
const crypto = @import("crypto");
const json = @import("json.zig");

// Hello, World!
fn hello() void {
    std.debug.print("Hello, World!\n");
}

const FileHashResult = struct {
    filename: []const u8,
    filepath: []const u8,
    size: u64,
    hash: []const u8,
    status: []const u8,
    error: ?[]const u8,
};

const HashResults = struct {
    total_files: usize,
    successful_hashes: usize,
    failed_hashes: usize,
    algorithm: []const u8,
    processing_time_seconds: f64,
    average_time_per_file_ms: f64,
    timestamp: []const u8,
    directory: []const u8,
    results: []const FileHashResult,
};

fn calculateFileHash(allocator: std.mem.Allocator, filepath: []const u8) ![]const u8 {
    const file = std.fs.cwd().openFile(filepath, .{}) catch return "";
    defer file.close();
    
    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return "";
    defer allocator.free(content);
    
    var hash: [crypto.hash.sha256.digest_length]u8 = undefined;
    crypto.hash.sha256.hash(content, &hash);
    
    var hex_str = try allocator.alloc(u8, hash.len * 2);
    for (hash, 0..) |byte, i| {
        std.fmt.formatIntBuf(hex_str[i * 2 .. i * 2 + 2], byte, 16, .lower, .{ .fill = '0', .width = 2 });
    }
    
    return hex_str;
}

fn getFileSize(filepath: []const u8) u64 {
    const file = std.fs.cwd().openFile(filepath, .{}) catch return 0;
    defer file.close();
    return file.stat().size;
}

fn getTxtFiles(allocator: std.mem.Allocator, directory: []const u8) ![][]const u8 {
    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();
    
    var dir = try std.fs.cwd().openDir(directory, .{ .iterate = true });
    defer dir.close();
    
    var iterator = dir.iterate();
    while (iterator.next() catch |err| switch (err) {
        error.AccessDenied => continue,
        else => return err,
    }) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".txt")) {
            const filepath = try std.fs.path.join(allocator, &.{ directory, entry.name });
            try files.append(filepath);
        }
    }
    
    // Sort files
    std.sort.insertion([]const u8, files.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.lessThan(u8, lhs, rhs);
        }
    }.lessThan);
    
    return files.toOwnedSlice();
}

fn getTimestamp(allocator: std.mem.Allocator) ![]const u8 {
    const timestamp = std.time.timestamp();
    return std.fmt.allocPrint(allocator, "{}", .{timestamp});
}

fn hashFilesInDirectory(allocator: std.mem.Allocator, directory: []const u8) !void {
    const files = try getTxtFiles(allocator, directory);
    defer allocator.free(files);
    
    if (files.len == 0) {
        std.debug.print("No .txt files found in directory: {s}\n", .{directory});
        return;
    }
    
    std.debug.print("Found {d} files to hash\n", .{files.len});
    
    var results = std.ArrayList(FileHashResult).init(allocator);
    defer results.deinit();
    
    var successful_hashes: usize = 0;
    var failed_hashes: usize = 0;
    
    const start_time = std.time.nanoTimestamp();
    
    for (files, 0..) |filepath, i| {
        const filename = std.fs.path.basename(filepath);
        const size = getFileSize(filepath);
        
        const hash = calculateFileHash(allocator, filepath) catch "";
        defer if (hash.len > 0) allocator.free(hash);
        
        var result = FileHashResult{
            .filename = try allocator.dupe(u8, filename),
            .filepath = try allocator.dupe(u8, filepath),
            .size = size,
            .hash = if (hash.len > 0) try allocator.dupe(u8, hash) else "",
            .status = if (hash.len > 0) "SUCCESS" else "FAILED",
            .error = if (hash.len == 0) "Hash calculation failed" else null,
        };
        
        if (hash.len > 0) {
            successful_hashes += 1;
        } else {
            failed_hashes += 1;
        }
        
        try results.append(result);
        
        // Progress indicator
        if ((i + 1) % 100 == 0) {
            std.debug.print("Processed {d}/{d} files...\n", .{ i + 1, files.len });
        }
    }
    
    const end_time = std.time.nanoTimestamp();
    const processing_time = @as(f64, @floatFromInt(end_time - start_time)) / 1_000_000_000.0;
    const average_time_per_file = (processing_time / @as(f64, @floatFromInt(files.len))) * 1000.0;
    
    const timestamp = try getTimestamp(allocator);
    defer allocator.free(timestamp);
    
    // Create final results structure
    const final_results = HashResults{
        .total_files = files.len,
        .successful_hashes = successful_hashes,
        .failed_hashes = failed_hashes,
        .algorithm = "sha256",
        .processing_time_seconds = processing_time,
        .average_time_per_file_ms = average_time_per_file,
        .timestamp = timestamp,
        .directory = directory,
        .results = results.toOwnedSlice(),
    };
    
    // Save to file (simplified - would need proper JSON serialization)
    std.debug.print("Results would be saved to: hash_results.json\n");
    std.debug.print("\nHashing completed!\n");
    std.debug.print("Total files: {d}\n", .{final_results.total_files});
    std.debug.print("Successful: {d}\n", .{final_results.successful_hashes});
    std.debug.print("Failed: {d}\n", .{final_results.failed_hashes});
    std.debug.print("Processing time: {d:.3} seconds\n", .{final_results.processing_time_seconds});
    std.debug.print("Average time per file: {d:.2} ms\n", .{final_results.average_time_per_file_ms});
    
    // Cleanup
    for (final_results.results) |result| {
        allocator.free(result.filename);
        allocator.free(result.filepath);
        allocator.free(result.hash);
    }
    allocator.free(final_results.results);
}

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    hello();
    
    hashFilesInDirectory(allocator, "../hashfiles") catch |err| {
        std.debug.print("Error: {}\n", .{err});
    };
}
