const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const TokenType = tokenizer.TokenType;

pub const ExprTypes = enum {
    literal,
    unary,
    binary,
    grouping,
};

pub const LiteralTypes = enum {
    boolType,
    nilType,
    stringType,
    numberType,
};

pub const BinaryExpr = struct {
    left: *Expr,
    operator: *tokenizer.Token,
    right: *Expr,
};

pub const UnaryExpr = struct {
    operator: *tokenizer.Token,
    right: *Expr,
};

pub const GroupingExpr = struct {
    expr: *Expr,
};

pub const LiteralExpr = struct {
    value: union(LiteralTypes) {
        boolType: bool,
        // we don't actually need anything here, this will always be true.
        // Better way to represent this?
        nilType: bool,
        stringType: []const u8,
        numberType: f64,
    },
};

pub const Expr = struct {
    data: union(ExprTypes) {
        literal: LiteralExpr,
        unary: UnaryExpr,
        binary: BinaryExpr,
        grouping: GroupingExpr,
    },
};

// recursive functions cannot infer error sets
pub const ParserError = error{
    ParserError,
    InvalidPrimary,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: tokenizer.TokenList,
    current: usize = 0,

    pub fn init(allocator: std.mem.Allocator, tokens: tokenizer.TokenList) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
        };
    }

    pub fn expression(self: *Parser) ParserError!Expr {
        return try self.equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(self: *Parser) ParserError!Expr {
        var expr = try self.comparison();
        while (self.match2(.bang_equal, .equal_equal)) {
            var operator = self.tokens.items[self.current - 1];
            var right = try self.comparison();
            expr = Expr{
                .data = .{
                    .binary = BinaryExpr{
                        .left = &expr,
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }
        return expr;
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(self: *Parser) ParserError!Expr {
        var expr = try self.term();
        while (self.match4(.greater, .greater_equal, .less, .less_equal)) {
            var operator = self.tokens.items[self.current - 1];
            var right = try self.term();
            expr = Expr{
                .data = .{
                    .binary = BinaryExpr{
                        .left = &expr,
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }
        return expr;
    }

    // term → factor ( ( "-" | "+" ) factor )* ;
    fn term(self: *Parser) ParserError!Expr {
        var expr = try self.factor();
        while (self.match2(.minus, .plus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = try self.factor();
            expr = Expr{
                .data = .{
                    .binary = BinaryExpr{
                        .left = &expr,
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }
        return expr;
    }

    // // factor → unary ( ( "/" | "*" ) unary )* ;
    fn factor(self: *Parser) ParserError!Expr {
        var expr = try self.unary();
        while (self.match2(.minus, .plus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = try self.unary();
            expr = Expr{
                .data = .{
                    .binary = BinaryExpr{
                        .left = &expr,
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }
        return expr;
    }

    // unary → ( "!" | "-" ) unary | primary ;
    fn unary(self: *Parser) ParserError!Expr {
        if (self.match2(.bang, .minus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = try self.unary();
            return Expr{
                .data = .{
                    .unary = UnaryExpr{
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }

        return try self.primary();
    }

    // // primary → NUMBER | STRING | "true" | "false" | "nil"
    // //           | "(" expression ")" ;
    fn primary(self: *Parser) ParserError!Expr {
        if (self.match(.keyword_false)) return boolLiteral(false);
        if (self.match(.keyword_true)) return boolLiteral(true);
        if (self.match(.keyword_nil)) return nilLiteral();
        if (self.match(.number))
            return numericLiteral(self.tokens.items[self.current - 1].lexeme.float);
        if (self.match(.string))
            return stringLiteral(self.tokens.items[self.current - 1].lexeme.bytes);
        if (self.match(.left_paren)) {
            var expr = try self.expression();
            // TODO: throw an error if the next token isn't a right paren
            try self.consume(.right_paren, "Need to figure out error messages");
            return Expr{
                .data = .{ .grouping = GroupingExpr{ .expr = &expr } },
            };
        }

        return ParserError.InvalidPrimary;
    }

    fn consume(self: *Parser, typ: TokenType, msg: []const u8) ParserError!void {
        if (self.check(typ)) {
            self.current += 1;
            return;
        }

        // zig doesn't allow you to associate a string with an error, so I
        // probably need to use some heavier-weight mechanism here?
        // see https://github.com/ziglang/zig/issues/2647
        std.debug.print("{s}\n", .{msg});
        return ParserError.ParserError;
    }

    fn stringLiteral(val: []const u8) Expr {
        return Expr{
            .data = .{
                .literal = LiteralExpr{
                    .value = .{
                        .stringType = val,
                    },
                },
            },
        };
    }

    fn numericLiteral(val: f64) Expr {
        return Expr{
            .data = .{
                .literal = LiteralExpr{
                    .value = .{
                        .numberType = val,
                    },
                },
            },
        };
    }

    fn nilLiteral() Expr {
        return Expr{
            .data = .{
                .literal = LiteralExpr{
                    .value = .{
                        .nilType = true,
                    },
                },
            },
        };
    }

    fn boolLiteral(val: bool) Expr {
        return Expr{
            .data = .{
                .literal = LiteralExpr{
                    .value = .{
                        .boolType = val,
                    },
                },
            },
        };
    }

    // TODO accept a vector instead (?)
    fn match(self: *Parser, typ1: TokenType) bool {
        if (self.check(typ1)) {
            if (self.current != self.tokens.items.len) self.current += 1;
            return true;
        }
        return false;
    }

    fn match2(self: *Parser, typ1: TokenType, typ2: TokenType) bool {
        if (self.check(typ1) or self.check(typ2)) {
            if (self.current != self.tokens.items.len) self.current += 1;
            return true;
        }
        return false;
    }

    fn match3(self: *Parser, typ1: TokenType, typ2: TokenType, typ3: TokenType) bool {
        if (self.check(typ1) or self.check(typ2) or self.check(typ3)) {
            if (self.current != self.tokens.items.len) self.current += 1;
            return true;
        }
        return false;
    }

    fn match4(self: *Parser, typ1: TokenType, typ2: TokenType, typ3: TokenType, typ4: TokenType) bool {
        if (self.check(typ1) or self.check(typ2) or self.check(typ3) or self.check(typ4)) {
            if (self.current != self.tokens.items.len) self.current += 1;
            return true;
        }
        return false;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        if (self.current == self.tokens.items.len) {
            return false;
        }
        return self.tokens.items[self.current].typ == typ;
    }
};
