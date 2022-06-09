const std = @import("std");

fn runFile(path: []const u8) void {
    std.debug.print("path to run: {s}\n", .{path});
}

fn runPrompt() void {
    std.debug.print("run the interpreter\n", .{});
}

pub fn main() anyerror!void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    // args[0] is our executable
    if (args.len > 2) {
        std.debug.print("Usage: lexzical [script]\n", .{});
    } else if (args.len == 2) {
        runFile(args[1]);
    } else {
        runPrompt();
    }
}
