const std = @import("std");

const Room = struct {
    name: [][]const u8,
    sectorId: usize,
    checksum: []const u8,

    fn init(name: [][]const u8, sectorId: usize, checksum: []const u8) Room {
        return Room {
            .name = name,
            .sectorId = sectorId,
            .checksum = checksum
        };
    }
};

fn parseRoom(allocator: std.mem.Allocator, line: []u8) !Room {
    var splits = std.mem.split(u8, line, "[");
    var start = splits.next().?;
    var end = splits.next().?;
    var checksum = end[0..(end.len - 1)];

    var startSplits = std.mem.split(u8, start, "-");

    var xs = std.ArrayList([]const u8).init(allocator);
    defer xs.deinit();

    while (startSplits.next()) |s|
        try xs.append(s);

    var xsSlice = xs.toOwnedSlice();

    var sectorId = try std.fmt.parseInt(usize, xsSlice[xsSlice.len - 1], 10);

    var name = xsSlice[0..(xsSlice.len - 1)];

    return Room.init(name, sectorId, checksum);
}

fn readRooms(allocator: std.mem.Allocator, filePath: []u8) ![]Room {
    const file = try std.fs.cwd().openFile(filePath, .{});
    defer file.close();

    var bufReader = std.io.bufferedReader(file.reader());
    var inStream = bufReader.reader();
    var buf: [1024]u8 = undefined;

    var rooms = std.ArrayList(Room).init(allocator);

    while (try inStream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try rooms.append(try parseRoom(allocator, line));
    }

    return rooms.toOwnedSlice();
}

fn solveFile(allocator: std.mem.Allocator, filePath: []u8) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Solving for file : {s}\n", .{filePath});

    const rooms = try readRooms(allocator, filePath);
    for (rooms) |r|
        std.debug.print("{?}", .{r});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    for (args[1..]) |arg| {
        try solveFile(allocator, arg);
    }
}

