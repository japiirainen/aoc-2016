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
            if (password.len == 8)
                break;
        }
    }

    return try std.fmt.allocPrint(allocator, "{s}", .{password});
}

fn part2(allocator: std.mem.Allocator, input: []const u8, debug: bool) ![8:0]u8 {
    var password = "????????".*;

    var i: i64 = 0;
    while (i < 100_000_000_000_000) : (i += 1) {
        var x: [std.crypto.hash.Md5.digest_length]u8 = undefined;
        var pw = input;
        _ = try join(allocator, &pw, i, "{d}");
        _ = std.crypto.hash.Md5.hash(pw, &x, .{});

        var hex = try toHex(allocator, x);

        if (std.mem.startsWith(u8, hex, "00000")) {
            var idx = try std.fmt.allocPrint(allocator, "{c}", .{hex[5]});
            var index = try std.fmt.parseInt(usize, idx, 16);

            if (index < @as(u8, 8)) {
                var atIdx = try std.fmt.allocPrint(allocator, "{c}", .{password[index]});
                if (std.mem.eql(u8, atIdx, "?"))
                    password[index] = hex[6];

                    if (debug) {
                        std.debug.print("iteration = {d}\n", .{i});
                        std.debug.print("hashed = {s}\n", .{pw});
                        std.debug.print("hex = {s}\n", .{hex});
                        std.debug.print("hex[5] = {c}\n", .{hex[5]});
                        std.debug.print("password = {s}\n", .{password});
                        std.debug.print("\n", .{});
                    }

                if (!std.mem.containsAtLeast(u8, &password, 1, "?"))
                    break;
            }
        }
    }

    return password;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input: []const u8 = "ugkcyxxp";
    const debug = false;

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Solving for input : {s}\nDebug = {any}\n\n", .{input, debug});

    var timer = try std.time.Timer.start();

    const p1 = try part1(allocator, input, debug);

    const t0 = timer.lap() / 1000;
    _ = timer.reset();

    const p2 = try part2(allocator, input, debug);
    const t1 = timer.lap() / 1000;

    try stdout.print("----- results: -----\n", .{});
    try stdout.print("part 1 in {d}µs : {s}\n", .{t0, p1});
    try stdout.print("part 2 in {d}µs : {s}\n", .{t1, p2});
}
