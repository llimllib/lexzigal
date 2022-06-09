// zig run main_3.zig -- test.lox
const std = @import("std");

const TokenType = enum {
    // single-character tokens
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    // 1-2 char tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // literals
    identifier,
    string,
    number,

    // keywords
    keyword_and,
    keyword_class,
    keyword_else,
    keyword_false,
    keyword_fun,
    keyword_for,
    keyword_if,
    keyword_nil,
    keyword_or,
    keyword_print,
    keyword_return,
    keyword_super,
    keyword_this,
    keyword_true,
    keyword_var,
    keyword_while,

    eof,
};

const Lexemes = enum {
    bytes,
    int,
    float,
    empty,
};

const Token = struct {
    typ: TokenType,
    // I think the natural way to do a zig token would be to return a slice
    // into the buffer? But instead I'm going to follow the book and represent
    // the lexeme as a union
    lexeme: union(Lexemes) {
        bytes: []const u8,
        int: i64,
        float: f64,
        empty: void,
    },
    line: u64,
};

const TokenList = std.ArrayList(*Token);

const Scanner = struct {
    allocator: std.mem.Allocator,
    buf: []const u8,
    start: u64 = 0,
    current: u64 = 0,
    line: u64 = 1,
    tokens: TokenList,
    hadError: bool = false,

    pub fn init(allocator: std.mem.Allocator, buf: []const u8) Scanner {
        return Scanner{
            .allocator = allocator,
            .buf = buf,
            .tokens = TokenList.init(allocator),
        };
    }

    pub fn scan(self: *Scanner) !TokenList {
        while (self.current < self.buf.len) {
            self.start = self.current;
            try self.next();
        }

        try self.tokens.append(&Token{ .typ = .eof, .lexeme = .empty, .line = self.line });
        return self.tokens;
    }

    pub fn next(self: *Scanner) !void {
        var c = self.buf[self.current];
        self.current += 1;
        switch (c) {
            '(' => {
                try self.addTok(.left_paren);
            },
            ')' => {
                try self.addTok(.right_paren);
            },
            '{' => {
                try self.addTok(.left_brace);
            },
            '}' => {
                try self.addTok(.right_brace);
            },
            ',' => {
                try self.addTok(.comma);
            },
            '.' => {
                try self.addTok(.dot);
            },
            '-' => {
                try self.addTok(.minus);
            },
            '+' => {
                try self.addTok(.plus);
            },
            ';' => {
                try self.addTok(.semicolon);
            },
            '*' => {
                try self.addTok(.star);
            },
            '!' => {
                try if (self.match('=')) self.addTok(.bang_equal) else self.addTok(.bang);
            },
            '=' => {
                try if (self.match('=')) self.addTok(.equal_equal) else self.addTok(.equal);
            },
            '<' => {
                try if (self.match('=')) self.addTok(.less_equal) else self.addTok(.less);
            },
            '>' => {
                try if (self.match('=')) self.addTok(.greater_equal) else self.addTok(.greater);
            },
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and self.current != self.buf.len) {
                        self.current += 1;
                    }
                } else {
                    try self.addTok(.slash);
                }
            },
            ' ', '\t', '\r' => {},
            '\n' => {
                self.line += 1;
            },
            else => {
                self.hadError = true;
                // Nystrom has the scanner call out to the Lox class, which has
                // a static error reporting method. That doesn't make sense in
                // this program though. Maybe we should just save errors and
                // then the caller can be responsible for printing them? For
                // now we can just print an error.
                std.debug.print("Error lexing line {d} at char {c}\n", .{ self.line, self.buf[self.current - 1] });
            },
        }
    }

    // return the character at the current index but don't consume it
    fn peek(self: *Scanner) u8 {
        if (self.current == self.buf.len) return 0;
        return self.buf[self.current];
    }

    // if the next character is `expected`, return true and consume; otherwise
    // return false
    fn match(self: *Scanner, expected: u8) bool {
        if (self.current == self.buf.len) return false;
        if (self.buf[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn addTok(self: *Scanner, typ: TokenType) !void {
        var tok = try self.allocator.create(Token);
        tok.* = Token{ .typ = typ, .lexeme = .empty, .line = 1 };
        try self.tokens.append(tok);
    }
};

fn printTokens(tl: TokenList) !void {
    const stdout = std.io.getStdOut().writer();
    for (tl.items) |tok| {
        try stdout.print("{s} ", .{tok.typ});
        // TODO switch on the lexeme union and print it out
    }
    try stdout.print("\n", .{});
}

fn run(allocator: std.mem.Allocator, buf: []const u8) !Scanner {
    var scanner = Scanner.init(allocator, buf);
    var tokens = try scanner.scan();

    try printTokens(tokens);

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
