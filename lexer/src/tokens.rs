use serde::Serialize;

/// non-alphanumeric symbols
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const LSQUAREBRACE: &str = "[";
pub const RSQUAREBRACE: &str = "]";
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const MODULUS: &str = "%";
pub const AT: &str = "@";
pub const SLASH: &str = "/";
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOTEQ: &str = "!=";
pub const GTOREQ: &str = ">=";
pub const LTOREQ: &str = "<=";
pub const AND: &str = "&&";
pub const OR: &str = "||";
pub const PIPE: &str = "|";
pub const AMPERSAND: &str = "&";
pub const NULL: &str = "NULL";
pub const QUOTEMARK: &str = "'";
pub const HASH: &str = "#";

pub const INT: &str = "@int";
pub const STRING: &str = "@string";
pub const BOOLEAN: &str = "@bool";
pub const UNIT: &str = "@unit";
pub const MAIN: &str = "@main";
pub const END: &str = "@end";
pub const LET: &str = "let";
pub const FUNCTION: &str = "fn";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const RETURN: &str = "return";
pub const FORMAT: &str = "format";
pub const PRINT: &str = "print";

#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, Serialize)]
pub enum IdentifierKind {
    #[default]
    UNKNOWN,

    EOF, // end of file. Nothoing more to read
    EOS, // end of statement for block

    // Identifiers + literals
    VARIABLE,       // add, foobar, x, y, ...
    INTLITERAL,     // 1343456, 1.22, 23.781
    STRINGLITERAL,  // "alice", "in", "wonderland"
    BOOLEANLITERAL, // "true", "false

    INTTYPE,     // @int
    STRINGTYPE,  // @string
    BOOLEANTYPE, //@bool
    ARRAYTYPE,   // [ <> ] e.g [ @int ], [ @string ]
    UNITTYPE,    //@unit

    // Delimiters
    COMMA,        // ","
    SEMICOLON,    // ";"
    LPAREN,       // "("
    RPAREN,       // ")"
    LBRACE,       // "{"
    RBRACE,       // "}"
    LSQUAREBRACE, // "["
    RSQUAREBRACE, // "]"

    HASH, // "#"}"

    // Operators
    ASSIGN,   //  "="
    PLUS,     // "+"
    MINUS,    // "-"
    ASTERISK, // "*"
    SLASH,    // "/"
    BANG,     //  "!" // logical negation
    LT,       // "<"
    GT,       //  ">"
    EQ,       // "=="
    NOTEQ,    //"!="
    GTOREQ,   // ">="
    LTOREQ,   // "<="
    AND,      // "&&"
    OR,       // "||"
    LAND,     // "&" // logical AND
    LOR,      // "|" // logical OR
    MODULUS,  // "%"

    // Keywords
    FUNCTION, // "FUNCTION"
    LET,      // "LET"
    TRUE,     // "TRUE"
    FALSE,    // "FALSE"
    IF,       // "IF"
    ELSE,     // "ELSE"
    RETURN,   // "RETURN"
    FORMAT,   // "FORMAT"
    PRINT,    // "PRINT"

    MAIN,
    END,
    CALLER, // A function call
    BLOCK,
    GROUPING,
    ARRAY,
}

/// Token is an identifiable single unit
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: IdentifierKind,
    pub literal: String,
    pub line_number: usize,
    pub column_number: usize,
}

impl Token {
    pub fn new(
        token_type: IdentifierKind,
        literal: String,
        line_number: usize,
        column_number: usize,
    ) -> Token {
        Self {
            token_type,
            literal,
            line_number,
            column_number,
        }
    }
}
