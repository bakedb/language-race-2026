const std = @import("std");
const http = @cImport(@cInclude("curl/curl.h"));
const json = @import("json.zig");

// Hello, World!
fn hello() void {
    std.debug.print("Hello, World!\n");
}

const TestResult = struct {
    endpoint: []const u8,
    url: []const u8,
    server_hash: ?[]const u8,
    expected_hash: []const u8,
    status: []const u8,
    error: ?[]const u8,
};

const FinalResult = struct {
    total_tests: usize,
    passed: usize,
    failed: usize,
    success_rate: []const u8,
    timestamp: []const u8,
    results: []const TestResult,
};

fn loadCompareJson(allocator: std.mem.Allocator, filename: []const u8) !?json.ObjectMap {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("Error: Could not find {s}\n", .{filename});
            return null;
        },
        else => return err,
    };
    defer file.close();
    
    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return err,
    };
    defer allocator.free(content);
    
    var parsed = json.parseFromSlice(json.ObjectMap, allocator, content, .{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            std.debug.print("Error parsing JSON: {}\n", .{err});
            return null;
        },
    };
    defer parsed.deinit();
    
    var result = parsed.value.clone(allocator) catch return error.OutOfMemory;
    return result;
}

fn makeHttpRequest(allocator: std.mem.Allocator, url: []const u8) !?[]const u8 {
    var curl = http.curl_easy_init();
    if (curl == null) return null;
    defer http.curl_easy_cleanup(curl);
    
    var response_data = std.ArrayList(u8).init(allocator);
    defer response_data.deinit();
    
    _ = http.curl_easy_setopt(curl, http.CURLOPT_URL, url.ptr);
    _ = http.curl_easy_setopt(curl, http.CURLOPT_TIMEOUT, 5L);
    _ = http.curl_easy_setopt(curl, http.CURLOPT_WRITEFUNCTION, writeCallback);
    _ = http.curl_easy_setopt(curl, http.CURLOPT_WRITEDATA, &response_data);
    
    const result = http.curl_easy_perform(curl);
    if (result != http.CURLE_OK) return null;
    
    var response_code: c_long = 0;
    _ = http.curl_easy_getinfo(curl, http.CURLINFO_RESPONSE_CODE, &response_code);
    if (response_code != 200) return null;
    
    return response_data.toOwnedSlice();
}

export fn writeCallback(contents: *c_void, size: usize, nmemb: usize, userp: *c_void) callconv(.C) usize {
    var response_data = @as(*std.ArrayList(u8), @ptrCast(@alignCast(userp)));
    const total_size = size * nmemb;
    response_data.writer().writeAll(@as([*]const u8, @ptrCast(contents))[0..total_size]) catch return 0;
    return total_size;
}

fn testWebServer(allocator: std.mem.Allocator) !void {
    const base_url = "http://localhost:3000";
    const compare_file = "../webserver/compare.json";
    const output_file = "test-result.json";
    
    // Load expected hashes
    const expected_hashes = (try loadCompareJson(allocator, compare_file)) orelse return;
    defer expected_hashes.deinit();
    
    std.debug.print("Testing 100 endpoints...\n");
    
    var results = std.ArrayList(TestResult).init(allocator);
    defer results.deinit();
    
    var passed: usize = 0;
    var failed: usize = 0;
    
    // Test each endpoint
    for (0..100) |i| {
        const endpoint = try std.fmt.allocPrint(allocator, "test-{d}", .{i});
        defer allocator.free(endpoint);
        
        const url = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ base_url, endpoint });
        defer allocator.free(url);
        
        const response = makeHttpRequest(allocator, url) catch null;
        defer if (response) |r| allocator.free(r);
        
        var result = TestResult{
            .endpoint = endpoint,
            .url = url,
            .server_hash = null,
            .expected_hash = expected_hashes.get(endpoint).?.string,
            .status = "",
            .error = null,
        };
        
        if (response) |resp| {
            var parsed = json.parseFromSlice(json.ObjectMap, allocator, resp, .{}) catch {
                result.status = "FAILED";
                result.error = "JSON parse error";
                failed += 1;
                try results.append(result);
                continue;
            };
            defer parsed.deinit();
            
            const server_hash = parsed.value.get("hash").?.string;
            result.server_hash = server_hash;
            
            if (std.mem.eql(u8, server_hash, result.expected_hash)) {
                result.status = "PASSED";
                passed += 1;
            } else {
                result.status = "FAILED";
                failed += 1;
            }
        } else {
            result.status = "FAILED";
            result.error = "HTTP request failed";
            failed += 1;
        }
        
        try results.append(result);
        
        // Progress indicator
        if ((i + 1) % 10 == 0) {
            std.debug.print("Tested {d}/100 endpoints...\n", .{i + 1});
        }
    }
    
    // Create final result
    const total_tests = results.items.len;
    const success_rate = try std.fmt.allocPrint(allocator, "{d:.1}%", .{@as(f64, @floatFromInt(passed)) / @as(f64, @floatFromInt(total_tests)) * 100.0});
    defer allocator.free(success_rate);
    
    const timestamp = std.time.timestamp();
    const time_str = try std.fmt.allocPrint(allocator, "{d}", .{timestamp});
    defer allocator.free(time_str);
    
    const final_result = FinalResult{
        .total_tests = total_tests,
        .passed = passed,
        .failed = failed,
        .success_rate = success_rate,
        .timestamp = time_str,
        .results = results.toOwnedSlice(),
    };
    
    // Save results to file (simplified - would need proper JSON serialization)
    std.debug.print("Results would be saved to: {s}\n", .{output_file});
    std.debug.print("\nTest completed!\n");
    std.debug.print("Passed: {d}/{d} ({s})\n", .{ passed, total_tests, success_rate });
    std.debug.print("Failed: {d}/{d} ({d:.1}%)\n", .{ failed, total_tests, @as(f64, @floatFromInt(failed)) / @as(f64, @floatFromInt(total_tests)) * 100.0 });
}

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    hello();
    
    // Initialize curl
    if (http.curl_global_init(http.CURL_GLOBAL_DEFAULT) != http.CURLE_OK) {
        std.debug.print("Failed to initialize curl\n");
        return;
    }
    defer http.curl_global_cleanup();
    
    testWebServer(allocator) catch |err| {
        std.debug.print("Error: {}\n", .{err});
    };
}
