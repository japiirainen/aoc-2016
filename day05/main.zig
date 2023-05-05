const std = @import("std");

fn join(allocator: std.mem.Allocator, fullStr: anytype, x: anytype, comptime fmt: []const u8) !void {
    var y = try std.fmt.allocPrint(allocator, fmt, .{x});
    fullStr.* = try std.mem.join(allocator, "", &[_][]const u8{ fullStr.*, y });
}

fn toHex(allocator: std.mem.Allocator, x: anytype) ![]u8 {
    return try std.fmt.allocPrint(allocator, "{s}", .{std.fmt.fmtSliceHexLower(&x)});
}

fn part1(allocator: std.mem.Allocator, input: []const u8, debug: bool) ![]u8 {
    var password: []u8 = "";

    var i: i64 = 0;
    while (i < 100_000_000) : (i += 1) {
        var x: [std.crypto.hash.Md5.digest_length]u8 = undefined;
        var pw = input;
        _ = try join(allocator, &pw, i, "{d}");
        _ = std.crypto.hash.Md5.hash(pw, &x, .{});

        var hex = try toHex(allocator, x);

        if (std.mem.startsWith(u8, hex, "00000")) {
            try join(allocator, &password, hex[5], "{c}");
            if (debug) {
                std.debug.print("iteration = {d}\n", .{i});
                std.debug.print("hashed = {s}\n", .{pw});
                std.debug.print("hex = {s}\n", .{hex});
                std.debug.print("hex[5] = {c}\n", .{hex[5]});
                std.debug.print("password = {s}\n", .{password});
                std.debug.print("\n", .{});
            }
            if (password.len == 8) {
                break;
            }
        }
    }

    var b: [1024]u8 = undefined;

    return try std.fmt.bufPrint(&b, "{s}", .{password});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input: []const u8 = "ugkcyxxp";
    // const input: []const u8 = "abc";

    const debug = true;

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Solving for input : {s}\nDebug = {any}\n\n", .{input, debug});

    try stdout.print("part 1 : {s}\n", .{try part1(allocator, input, debug)});
}
