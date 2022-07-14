const std = @import("std");

pub const TokenType = enum {
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

pub const Lexemes = enum {
    bytes,
    float,
    empty,
};

pub const Token = struct {
    typ: TokenType,
    // I think the natural way to do a zig token would be to return a slice
    // into the buffer? But instead I'm going to follow the book and represent
    // the lexeme as a union
    lexeme: union(Lexemes) {
        bytes: []const u8,
        float: f64,
        empty: void,
    },
    line: u64,
};

pub const TokenList = std.ArrayList(*Token);

pub const Scanner = struct {
    allocator: std.mem.Allocator,
    buf: []const u8,
    start: u64 = 0,
    current: u64 = 0,
    line: u64 = 1,
    tokens: TokenList,
    hadError: bool = false,

    pub const keywords = std.ComptimeStringMap(TokenType, .{
        .{ "and", .keyword_and },
        .{ "class", .keyword_class },
        .{ "else", .keyword_else },
        .{ "false", .keyword_false },
        .{ "for", .keyword_for },
        .{ "fun", .keyword_fun },
        .{ "if", .keyword_if },
        .{ "nil", .keyword_nil },
        .{ "or", .keyword_or },
        .{ "print", .keyword_print },
        .{ "return", .keyword_return },
        .{ "super", .keyword_super },
        .{ "this", .keyword_this },
        .{ "true", .keyword_true },
        .{ "var", .keyword_var },
        .{ "while", .keyword_while },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

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

        try self.addTok(.eof);
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
            '"' => {
                while (self.peek() != '"' and self.current != self.buf.len) {
                    if (self.peek() == '\n') {
                        self.line += 1;
                    }
                    self.current += 1;
                }

                if (self.current == self.buf.len) {
                    std.debug.print("Unterminated string\n", .{});
                    return;
                }

                self.current += 1;

                var tok = try self.allocator.create(Token);
                tok.* = Token{
                    .typ = .string,
                    .lexeme = .{ .bytes = self.buf[self.start + 1 .. self.current - 1] },
                    .line = self.line,
                };
                try self.tokens.append(tok);
            },
            '0'...'9' => {
                while (digit(self.peek())) {
                    self.current += 1;
                }

                if (self.peek() == '.' and digit(self.peekNext())) {
                    self.current += 1;
                    while (digit(self.peek())) {
                        self.current += 1;
                    }
                }

                var tok = try self.allocator.create(Token);
                tok.* = Token{
                    .typ = .number,
                    .lexeme = .{ .float = try std.fmt.parseFloat(f64, self.buf[self.start..self.current]) },
                    .line = self.line,
                };
                try self.tokens.append(tok);
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (alphanumeric(self.peek())) {
                    self.current += 1;
                }

                var typ = if (getKeyword(self.buf[self.start..self.current])) |typ| typ else TokenType.identifier;

                var tok = try self.allocator.create(Token);
                tok.* = Token{
                    .typ = typ,
                    .lexeme = .{ .bytes = self.buf[self.start..self.current] },
                    .line = self.line,
                };

                try self.tokens.append(tok);
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

    fn alphanumeric(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => true,
            else => false,
        };
    }

    fn digit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    // return the character at the current index but don't consume it
    fn peek(self: *Scanner) u8 {
        if (self.current == self.buf.len) return 0;
        return self.buf[self.current];
    }

    // return the character at the next index but don't consume it
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.buf.len) return 0;
        return self.buf[self.current + 1];
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
        tok.* = Token{ .typ = typ, .lexeme = .empty, .line = self.line };
        try self.tokens.append(tok);
    }
};

pub fn printTokens(tl: TokenList) !void {
    const stdout = std.io.getStdOut().writer();
    for (tl.items) |tok| {
        try stdout.print("{s} ", .{tok.typ});
        switch (tok.lexeme) {
            Lexemes.bytes => try stdout.print("\"{s}\" ", .{tok.lexeme.bytes}),
            Lexemes.float => try stdout.print("{d} ", .{tok.lexeme.float}),
            Lexemes.empty => {},
        }
    }
    try stdout.print("\n", .{});
}
