const std = @import("std");
const ArrayList = std.ArrayList;

const SwapInfo = struct {
    pid: usize,
    size: isize,
    comm: []const u8,
};
const SwapInfoList = ArrayList(SwapInfo);
const UNITS: [4]u8 = .{ 'K', 'M', 'G', 'T' };

fn getSwapSizeForProcess(allocator: std.mem.Allocator, pid: usize) !isize {
    var sum: isize = 0;

    var b: [40]u8 = undefined;
    const smaps_path = try std.fmt.bufPrint(&b, "/proc/{d}/smaps", .{pid});
    const smaps_file = try std.fs.cwd().openFile(smaps_path, .{ .mode = .read_only });
    defer smaps_file.close();

    try smaps_file.seekTo(0);
    const content = try smaps_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(content);

    var iter = std.mem.split(u8, content, "\n");
    while (iter.next()) |line| {
        if (std.mem.startsWith(u8, line, "Swap:")) {
            const string = line[5..(line.len-3)];
            const value = std.mem.trim(u8, string, " ");
            const size = try std.fmt.parseInt(isize, value, 10);
            sum += size;
        }
    }
    return sum * 1024;
}

fn sanitizeCommandLine(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    const len = str.len;
    var result = if ((len > 0) and (str[len-1] == '\x00'))
            str[0..(len-1)]
        else
            str;
    var buf = try allocator.alloc(u8, result.len);
    _ = std.mem.replace(u8, result, "\x00", " ", buf[0..]);
    return buf[0..result.len];
}

test "sanitize command line" {
    const allocator = std.testing.allocator;
    const expect = std.testing.expect;
    var str = "This\x00is the\x00good!\x00\x00";
    var result = try sanitizeCommandLine(allocator, str[0..]);
    defer allocator.free(result);
    try expect(std.mem.eql(u8, result, "This is the good! "));
}

fn getCommandLineForProcess(allocator: std.mem.Allocator, pid: usize) ![]const u8 {
    var b: [40]u8 = undefined;
    const cmdline_path = try std.fmt.bufPrint(&b, "/proc/{d}/cmdline", .{pid});
    const cmdline_file = try std.fs.cwd().openFile(cmdline_path, .{ .mode = .read_only });
    defer cmdline_file.close();

    try cmdline_file.seekTo(0);
    const content = try cmdline_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(content);

    return try sanitizeCommandLine(allocator, content);
}

fn getSwapInfoList(allocator: std.mem.Allocator, results: *SwapInfoList) !void {
    const dir = try std.fs.cwd().openIterableDir("/proc", .{});
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        // NOTE: skip unreadable files
        const pid = std.fmt.parseInt(usize, entry.name, 10) catch continue;
        const size = getSwapSizeForProcess(allocator, pid) catch continue;
        if (size > 0) {
            const comm = getCommandLineForProcess(allocator, pid) catch continue;
            const info = SwapInfo{
                .pid = pid,
                .size = size,
                .comm = comm,
            };
            try results.append(info);
        }
    }
}

fn lessThan(_:void, lhs: SwapInfo, rhs: SwapInfo) bool {
    return lhs.size < rhs.size;
}

fn formatFileSize(allocator: std.mem.Allocator, size: isize) ![]const u8 {
    var left = @as(f64, @floatFromInt(size));
    var unit: i4 = -1;
    while ((left > 1100) and (unit < 3)) {
        left /= 1024;
        unit +=1;
    }
    if (unit == -1) {
        return try std.fmt.allocPrint(allocator, "{d}B", .{size});
    } else {
        if (size < 0) left = -left;
        const index: u4 = @as(*u4, @ptrCast(&unit)).*;
        return try std.fmt.allocPrint(allocator, "{d:.1}{c}iB", .{left, UNITS[index]});
    }
}

fn perform(allocator: std.mem.Allocator) !void {
    var results = SwapInfoList.init(allocator);
    try getSwapInfoList(allocator, &results);
    defer results.deinit();

    std.sort.block(SwapInfo, results.items, {}, lessThan);
    var total: isize = 0;

    const stdout = std.io.getStdOut().writer();

    try stdout.print("{s:>7} {s:>9} {s}\n", .{"PID", "SWAP", "COMMAND"});
    for (results.items) |info| {
        defer allocator.free(info.comm);

        const size_h = try formatFileSize(allocator, info.size);
        defer allocator.free(size_h);

        try stdout.print("{d:>7} {s:>9} {s}\n", .{info.pid, size_h, info.comm});
        total += info.size;
    }
    const file_size_text = try formatFileSize(allocator, total);
    defer allocator.free(file_size_text);
    try stdout.print("Total: {s:>10}\n", .{file_size_text});
}

test "perform" {
    const allocator = std.testing.allocator;

    try perform(allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    try perform(allocator);
}
