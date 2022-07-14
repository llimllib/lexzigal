// zig run main.zig -- test.lox
const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");

fn run(allocator: std.mem.Allocator, buf: []const u8) !tokenizer.Scanner {
    var scanner = tokenizer.Scanner.init(allocator, buf);
    var tokens = try scanner.scan();

    try tokenizer.printTokens(tokens);

    var parse = parser.Parser.init(allocator, tokens);
    var expr = parse.expression();

    std.debug.print("expr: {s}\n", .{expr});

    return scanner;
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    var input = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(input);

    var scanner = try run(allocator, input);
    if (scanner.hadError) {
        std.process.exit(65);
    }
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var repl_buf: [1024]u8 = undefined;

    while (true) {
        try stdout.print("lox> ", .{});
        if (try stdin.readUntilDelimiterOrEof(&repl_buf, '\n')) |line| {
            if (line.len == 0) {
                return;
            }
            _ = try run(allocator, line);
        }
    }
}

pub fn main() anyerror!void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len > 2) {
        std.debug.print("Usage: lexzical [script]\n", .{});
    } else if (args.len == 2) {
        try runFile(gpa, args[1]);
    } else {
        try runPrompt(gpa);
    }
}
