const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const TokenType = tokenizer.TokenType;

pub const ExprTypes = enum {
    literal,
    unary,
    binary,
};

pub const LiteralTypes = enum {
    boolType,
    nullType,
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

pub const LiteralExpr = struct {
    value: union(LiteralTypes) {
        boolType: bool,
        // we don't actually need anything here, this will always be true.
        // Better way to represent this?
        nullType: bool,
        stringType: []const u8,
        numberType: f64,
    },
};

pub const Expr = struct {
    data: union(ExprTypes) {
        literal: LiteralExpr,
        unary: UnaryExpr,
        binary: BinaryExpr,
    },
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

    pub fn expression(self: *Parser) Expr {
        return self.equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(self: *Parser) Expr {
        var expr = self.comparison();
        while (self.match2(.bang_equal, .equal_equal)) {
            var operator = self.tokens.items[self.current - 1];
            var right = self.comparison();
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
    fn comparison(self: *Parser) Expr {
        var expr = self.term();
        while (self.match4(.greater, .greater_equal, .less, .less_equal)) {
            var operator = self.tokens.items[self.current - 1];
            var right = self.term();
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
    fn term(self: *Parser) Expr {
        var expr = self.factor();
        while (self.match2(.minus, .plus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = self.factor();
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
    fn factor(self: *Parser) Expr {
        var expr = self.unary();
        while (self.match2(.minus, .plus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = self.unary();
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
    fn unary(self: *Parser) Expr {
        if (self.match2(.bang, .minus)) {
            var operator = self.tokens.items[self.current - 1];
            var right = self.unary();
            return Expr{
                .data = .{
                    .unary = UnaryExpr{
                        .operator = operator,
                        .right = &right,
                    },
                },
            };
        }

        return self.primary();
    }

    // // primary → NUMBER | STRING | "true" | "false" | "nil"
    // //           | "(" expression ")" ;
    fn primary(self: *Parser) Expr {
        if (self.match(.keyword_false))
            return Expr{
                .data = .{
                    .literal = LiteralExpr{
                        .value = .{
                            .boolType = false,
                        },
                    },
                },
            };
        if (self.match(.keyword_true))
            return Expr{
                .data = .{
                    .literal = LiteralExpr{
                        .value = .{
                            .boolType = true,
                        },
                    },
                },
            };
        //TODO handle string, number, paren cases
        return Expr{
            .data = .{
                .literal = LiteralExpr{
                    .value = .{
                        .nullType = true,
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
