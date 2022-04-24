use std::collections::HashMap;

// non-alphanumeric symbols
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOTEQ: &str = "!=";
pub const GTOREQ: &str = ">=";
pub const LTOREQ: &str = "<=";
pub const NULL: &str = "NULL";
pub const INT: &str = "@int";
pub const STRING: &str = "@string";
pub const BOOLEAN: &str = "@bool";
pub const ARRAY: &str = "@array";
pub const MAP: &str = "@map";

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenTypeKind {
    UNKNOWN,
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT,   // 1343456

    // Delimiters
    COMMA,     // ","
    SEMICOLON, // ";"
    LPAREN,    // "("
    RPAREN,    // ")"
    LBRACE,    // "{"
    RBRACE,    // "}"

    // Operators
    ASSIGN,   //  "="
    PLUS,     // "+"
    MINUS,    // "-"
    BANG,     //  "!"
    ASTERISK, // "*"
    SLASH,    // "/"
    LT,       // "<"
    GT,       //  ">"
    EQ,       // "=="
    NOTEQ,    //"!="
    GTOREQ,   // ">="
    LTOREQ,   // "<="

    // Keywords
    FUNCTION, // "FUNCTION"
    LET,      // "LET"
    TRUE,     // "TRUE"
    FALSE,    // "FALSE"
    IF,       // "IF"
    ELSE,     // "ELSE"
    RETURN,   // "RETURN"

    // types
    INTTYPE,
    DECIMALTYPE,
    STRINGTYPE,
    BOOLEANTYPE,
    ARRAYTYPE,
    MAPTYPE,
}

/// Token is an identifiable single unit
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenTypeKind,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenTypeKind, literal: String) -> Token {
        Self {
            token_type,
            literal,
        }
    }
}

pub fn keyword_lookup(ident: String) -> Option<TokenTypeKind> {
    let keywords = get_keywords();
    let key = ident.as_str();
    match keywords.get(key) {
        Some(t) => {
            let t0 = t.to_owned();
            Some(t0)
        }
        None => None,
    }
}

fn get_keywords() -> HashMap<&'static str, TokenTypeKind> {
    HashMap::from([
        ("fn", TokenTypeKind::FUNCTION),
        ("let", TokenTypeKind::LET),
        ("true", TokenTypeKind::TRUE),
        ("false", TokenTypeKind::FALSE),
        ("if", TokenTypeKind::IF),
        ("else", TokenTypeKind::ELSE),
        ("return", TokenTypeKind::RETURN),
    ])
}
