const std = @import("std");
const ArrayList = std.ArrayList;

const UNITS: [4]u8 = .{ 'K', 'M', 'G', 'T' };

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn formatFileSize(size: isize) []const u8 {
    var left = @intToFloat(f64, size);
    var unit: i4 = -1;
    while ((left > 1100) and (unit < 3)) {
        left /= 1024;
        unit +=1;
    }
    if (unit == -1) {
        return std.fmt.allocPrint(allocator, "{d}B", .{size}) catch unreachable;
    } else {
        if (size < 0) left = -left;
        const index: u4 = @ptrCast(*u4, &unit).*;
        return std.fmt.allocPrint(allocator, "{d:.1}{c}iB", .{left, UNITS[index]}) catch unreachable;
    }
}

const SwapInfo = struct {
    pid: usize,
    size: isize,
    comm: []const u8,
};

fn getSwapFor(pid: usize) isize {
    var sum: isize = 0;

    var b: [40]u8 = undefined;
    const smaps_path = std.fmt.bufPrint(&b, "/proc/{d}/smaps", .{pid}) catch unreachable;
    const smaps_file = std.fs.cwd().openFile(smaps_path, .{ .read = true, .write = false }) catch return 0;
    defer smaps_file.close();

    smaps_file.seekTo(0) catch return 0;
    var content = smaps_file.readToEndAlloc(allocator, std.math.maxInt(usize)) catch return 0;
    var iter = std.mem.split(u8, content, "\n");
    while (iter.next()) |line| {
        if (std.mem.startsWith(u8, line, "Swap:")) {
            const string = line[5..(line.len-3)];
            const value = std.mem.trim(u8, string, " ");
            const size = std.fmt.parseInt(isize, value, 10) catch return 0;
            sum += size;
        }
    }
    return sum * 1024;
}

fn sanitizeComm(str: []const u8) []const u8 {
    const len = str.len;
    var result = if ((len > 0) and (str[len-1] == '\x00'))
            str[0..(len-1)]
        else
            str;
    var buf = allocator.alloc(u8, result.len) catch unreachable;
    _ = std.mem.replace(u8, result, "\x00", " ", buf[0..]);
    return buf[0..result.len];
}

test "sanitize comm" {
    const expect = std.testing.expect;
    var str = "This\x00is the\x00good!\x00\x00";
    var result = sanitizeComm(str[0..]);
    defer allocator.free(result);
    try expect(std.mem.eql(u8, result, "This is the good! "));
}

fn getCommFor(pid: usize) []const u8 {
    var b: [40]u8 = undefined;
    const cmdline_path = std.fmt.bufPrint(&b, "/proc/{d}/cmdline", .{pid}) catch unreachable;
    const cmdline_file = std.fs.cwd().openFile(cmdline_path, .{ .read = true, .write = false }) catch return "";
    defer cmdline_file.close();

    cmdline_file.seekTo(0) catch return "";
    const content = cmdline_file.readToEndAlloc(allocator, std.math.maxInt(usize)) catch return "";
    return sanitizeComm(content);
}

fn getSwap() ArrayList(SwapInfo) {
    var results = ArrayList(SwapInfo).init(allocator);

    const dir = std.fs.cwd().openDir("/proc", .{ .iterate = true }) catch unreachable;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        const pid = std.fmt.parseInt(usize, entry.name, 10) catch continue;
        const size = getSwapFor(pid);
        if (size > 0) {
            const comm = getCommFor(pid);
            const info = SwapInfo{
                .pid = pid,
                .size = size,
                .comm = comm,
            };
            results.append(info) catch {};
        }
    }
    return results;
}

fn lessThan(_:void, lhs: SwapInfo, rhs: SwapInfo) bool {
    return lhs.size < rhs.size;
}

pub fn main() !void {
    var results = getSwap();
    std.sort.sort(SwapInfo, results.items, {}, lessThan);
    //defer results.deinit();
    var total: isize = 0;

    const stdout = std.io.getStdOut().writer();

    try stdout.print("{s:>7} {s:>9} {s}\n", .{"PID", "SWAP", "COMMAND"});
    for (results.items) |info| {
        //defer allocator.free(info.comm);
        const size_h = formatFileSize(info.size);
        try stdout.print("{d:>7} {s:>9} {s}\n", .{info.pid, size_h, info.comm});
        total += info.size;
    }
    try stdout.print("Total: {s:>10}\n", .{formatFileSize(total)});
}
