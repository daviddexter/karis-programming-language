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
pub const QUOTEMARK: &str = "'";

pub const INT: &str = "@int";
pub const STRING: &str = "@string";
pub const BOOLEAN: &str = "@bool";
pub const LET: &str = "let";
pub const FUNCTION: &str = "fn";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const RETURN: &str = "return";

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IndentifierKind {
    UNKNOWN,
    EOF,

    // Identifiers + literals
    VARIABLE,      // add, foobar, x, y, ...
    INTLITERAL,    // 1343456, 1.22, 23.781
    STRINGLITERAL, // "alice", "in", "wonderland"
    INTTYPE,       // @int
    STRINGTYPE,    // @string
    BOOLEANTYPE,   //@bool

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
}

/// Token is an identifiable single unit
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: IndentifierKind,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: IndentifierKind, literal: String) -> Token {
        Self {
            token_type,
            literal,
        }
    }
}
