use errors::errors;

use crate::tokens;

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    /// input that is fed to the lexer for tokenization
    input: String,

    /// points to the current char in the input string. This is the same as the col of the input string
    position: usize,

    line_number: usize,

    /// points to the next character in the input string after the current char
    read_position: usize,

    /// the literal value of the current character
    ch: Option<String>,

    /// indicates the starting point when an identifier is been read
    identifier_start_read_position: isize,

    /// indicates the previous character in the input string after the current
    last_read_token: Option<tokens::Token>,

    /// end of statement
    eos: bool,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input,
            position: 0,
            line_number: 1,
            read_position: 0,
            ch: None,
            identifier_start_read_position: -1,
            last_read_token: None,
            eos: false,
        }
    }

    pub fn generate_and_print(&mut self) {
        while self.position < self.input.len() || self.eos {
            match self.read_tokens() {
                Ok(token) => {
                    println!("{:?}", token);
                }
                Err(err) => eprintln!("{:?}", err),
            }
        }
        println!("\n")
    }

    #[allow(clippy::never_loop)]
    pub fn generate(&mut self) -> Result<tokens::Token, errors::KarisError> {
        'gen: loop {
            if self.position < self.input.len() || self.eos {
                return self.read_tokens();
            }
            break 'gen;
        }

        Ok(tokens::Token::new(
            tokens::IdentifierKind::EOF,
            String::new(),
            self.line_number,
            self.position,
        ))
    }

    pub fn read_tokens(&mut self) -> Result<tokens::Token, errors::KarisError> {
        match self.eos {
            true => {
                self.eos = false;
                Ok(tokens::Token::new(
                    tokens::IdentifierKind::EOS,
                    String::from(""),
                    self.line_number,
                    self.position,
                ))
            }
            false => {
                let res = self.new_token();
                if let Ok(tok) = &res {
                    if tok.token_type == tokens::IdentifierKind::SEMICOLON {
                        let default = tokens::Token::new(
                            tokens::IdentifierKind::UNKNOWN,
                            String::new(),
                            self.line_number,
                            self.position,
                        );

                        let last_tok = self.last_read_token.as_ref().unwrap_or(&default);

                        match last_tok.token_type {
                            tokens::IdentifierKind::INTLITERAL
                            | tokens::IdentifierKind::STRINGLITERAL
                            | tokens::IdentifierKind::TRUE
                            | tokens::IdentifierKind::FALSE
                            | tokens::IdentifierKind::RPAREN
                            | tokens::IdentifierKind::RBRACE
                            | tokens::IdentifierKind::END => {
                                self.eos = true;
                            }
                            _ => {
                                self.eos = false;
                            }
                        }
                    }

                    self.last_read_token = Some(tok.clone());
                }
                res
            }
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = Some(String::from(tokens::NULL));
        } else {
            let item = self.input.get(self.read_position..self.read_position + 0x1);
            if let Some(val) = item {
                self.ch = Some(String::from(val));
            }
        }

        self.read_position += 0x1;
    }

    fn new_token(&mut self) -> Result<tokens::Token, errors::KarisError> {
        if self.read_position == 0x0 {
            self.read_char();
        }

        // remove whitespace
        self.skip_whitespace_and_string_escape();

        match &self.ch {
            Some(ch) => {
                let ch_owned = ch.clone();

                let tok = match ch.as_str() {
                    tokens::COMMA => Ok(tokens::Token::new(
                        tokens::IdentifierKind::COMMA,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::SEMICOLON => Ok(tokens::Token::new(
                        tokens::IdentifierKind::SEMICOLON,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::LPAREN => Ok(tokens::Token::new(
                        tokens::IdentifierKind::LPAREN,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::RPAREN => Ok(tokens::Token::new(
                        tokens::IdentifierKind::RPAREN,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::LBRACE => Ok(tokens::Token::new(
                        tokens::IdentifierKind::LBRACE,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::RBRACE => Ok(tokens::Token::new(
                        tokens::IdentifierKind::RBRACE,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),

                    // LSQUAREBRACE and RSQUAREBRACE define an array of a specific type enclosed within.
                    // While whitespace have no intrinsic meaning to Karis language, they are vital in the definition
                    // of an array. Whitespace is required to seperate the square braces on either side from the enclosed content
                    // For example:
                    //  [ @int ] is a valid array definition
                    //  [@int] is not a valid array definition
                    //
                    // However the lexer will accept [@int ] since there is whitespace before the tail the square brace.
                    // [ @int] is not a valid array definition
                    tokens::LSQUAREBRACE => Ok(tokens::Token::new(
                        tokens::IdentifierKind::LSQUAREBRACE,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),

                    tokens::RSQUAREBRACE => Ok(tokens::Token::new(
                        tokens::IdentifierKind::RSQUAREBRACE,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::ASTERISK => Ok(tokens::Token::new(
                        tokens::IdentifierKind::ASTERISK,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::SLASH => Ok(tokens::Token::new(
                        tokens::IdentifierKind::SLASH,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),

                    tokens::PLUS => Ok(tokens::Token::new(
                        tokens::IdentifierKind::PLUS,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::MINUS => Ok(tokens::Token::new(
                        tokens::IdentifierKind::MINUS,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::MODULUS => Ok(tokens::Token::new(
                        tokens::IdentifierKind::MODULUS,
                        ch_owned,
                        self.line_number,
                        self.position,
                    )),
                    tokens::NULL => Ok(tokens::Token::new(
                        tokens::IdentifierKind::EOF,
                        String::new(),
                        self.line_number,
                        self.position,
                    )),

                    // if the current char is `=`, check if the next character is `=`, therefore asserting
                    // whether the combination is `==`
                    tokens::ASSIGN => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::EQ, tokens::EQ);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::EQ,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::ASSIGN,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::ASSIGN,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    tokens::PIPE => match self.forward_is_any_token(vec![tokens::PIPE]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::PIPE, tokens::PIPE);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::OR,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::LOR,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }

                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::LOR,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    tokens::AMPERSAND => match self.forward_is_any_token(vec![tokens::AMPERSAND]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::AMPERSAND, tokens::AMPERSAND);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::AND,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::LAND,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::LAND,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    // if the current char is `!`, check if the next character is `=`, therefore asserting
                    // whether the combination is `!=`
                    tokens::BANG => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::BANG, tokens::ASSIGN);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::NOTEQ,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::BANG,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::BANG,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    // if the current char is `<`, check if he next character is `=`, therefore asserting
                    // whether the combination is `<=`
                    tokens::LT => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::LT, tokens::ASSIGN);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::LTOREQ,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::LT,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::LT,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    // if the current char is `>`, check is the next character if `=`, therefore asserting
                    // whether the combination is `>=`
                    tokens::GT => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                let lit = format!("{:?}{:?}", tokens::GT, tokens::ASSIGN);
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::GTOREQ,
                                    lit,
                                    self.line_number,
                                    self.position,
                                ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IdentifierKind::GT,
                                    ch_owned,
                                    self.line_number,
                                    self.position,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IdentifierKind::GT,
                            ch_owned,
                            self.line_number,
                            self.position,
                        )),
                    },

                    _ => {
                        // update identifier_start_read_position so that we know where we
                        // are when slicing to retrieve the exact identifier.
                        // This is usually the same as the current position that we are at
                        if self.identifier_start_read_position == -0x1 {
                            self.identifier_start_read_position = self.position as isize;
                        }
                        self.extract_token_from_alphabet()
                    }
                };
                self.move_current_position_and_read();
                tok
            }
            None => self.unknown_token_error(" "),
        }
    }

    fn move_current_position_and_read(&mut self) {
        self.position += 1;
        self.read_char()
    }

    fn skip_whitespace_and_string_escape(&mut self) {
        let current = self.ch.as_ref().unwrap();

        if is_newline(current.as_bytes().first().unwrap()) {
            self.line_number += 0x1;
            self.move_current_position_and_read();
            return self.skip_whitespace_and_string_escape();
        }

        if is_space(current.as_bytes().first().unwrap())
            || is_newline(current.as_bytes().first().unwrap())
        {
            self.move_current_position_and_read();
            self.skip_whitespace_and_string_escape()
        }
    }

    /// asserts that a token within the bounds of the `position` and `read_position` is
    /// the same as tokens provided in the `example` vector
    fn forward_is_any_token(&self, examples: Vec<&str>) -> Option<bool> {
        // peek forward
        let item = self.input.get(self.position + 1..self.read_position + 1);
        let mut r = false;
        for x in examples.iter() {
            if item == Some(*x) {
                r = true;
                break;
            } else {
                continue;
            }
        }
        Some(r)
    }

    fn unknown_token_error(&self, ident: &str) -> Result<tokens::Token, errors::KarisError> {
        Err(errors::KarisError {
            error_type: errors::KarisErrorType::UnknownToken,
            message: format!("identifier not known : {}", ident),
        })
    }

    /// move the length of a string literal untill encounter a closing quatation
    fn traverse_to_closing_quotations(&self, begin: usize) -> usize {
        let next = self.input.get(begin..begin + 0x1).unwrap();
        if is_quotation_mark(next) {
            // this is actually the index where the closing quote is located.
            begin
        } else {
            self.traverse_to_closing_quotations(begin + 0x1)
        }
    }

    fn extract_token_from_alphabet(&mut self) -> Result<tokens::Token, errors::KarisError> {
        // peek forward
        let next_char_fn =
            |begin: usize, end: usize, default| self.input.get(begin..end).unwrap_or(default);

        let is_func_identifier_fn = |next_char: &str| {
            // next_char will be converted to it's unicode representation
            // `n` => 110
            // `N` => 78

            // the unicode of the current character.
            // `f` => 102
            // `F` => 70
            let current_char = self
                .input
                .get(self.position..self.read_position)
                .unwrap()
                .as_bytes()
                .first()
                .unwrap();

            let next_two_step_char =
                next_char_fn(self.position + 0x2, self.read_position + 0x2, tokens::NULL);

            (*current_char == 0x46 || *current_char == 0x66)
                && (*next_char.as_bytes().first().unwrap() == 0x6e
                    || *next_char.as_bytes().first().unwrap() == 0x4e)
                && (next_two_step_char == tokens::LPAREN)
        };

        match &self.ch {
            Some(current_literal) => {
                let next_char = next_char_fn(
                    self.position + 0x1,
                    self.read_position + 0x1,
                    tokens::SEMICOLON,
                );

                // if encountered an opening quatation mark, moving along the sequence until a closing quatation mark is encountered
                if is_quotation_mark(current_literal) {
                    let string_literal_ending =
                        self.traverse_to_closing_quotations(self.position + 0x1);

                    let string_literal = self
                        .input
                        .get(self.position + 0x1..string_literal_ending)
                        .unwrap();

                    // reset identifier start read position
                    self.identifier_start_read_position = -0x1;
                    self.position = string_literal_ending;
                    self.read_position = string_literal_ending + 0x1;

                    Ok(tokens::Token::new(
                        tokens::IdentifierKind::STRINGLITERAL,
                        String::from(string_literal),
                        self.line_number,
                        self.position,
                    ))
                } else if current_literal == tokens::AT
                    && next_char_fn(self.position, self.read_position + 0x4, tokens::NULL)
                        == tokens::MAIN
                {
                    // read the identifier then update the literal and the token
                    match self
                        .input
                        .get(self.identifier_start_read_position as usize..self.read_position + 0x4)
                    {
                        Some(ident) => {
                            // reset identifier start read position
                            self.identifier_start_read_position = -0x1;

                            self.position += 0x4;
                            self.read_position += 0x4;

                            let ident_owned = String::from(ident);
                            Ok(tokens::Token::new(
                                tokens::IdentifierKind::MAIN,
                                ident_owned,
                                self.line_number,
                                self.position,
                            ))
                        }
                        None => self.unknown_token_error(tokens::NULL),
                    }
                } else if current_literal == tokens::AT
                    && next_char_fn(self.position, self.read_position + 0x3, tokens::NULL)
                        == tokens::END
                {
                    // read the identifier then update the literal and the token
                    match self
                        .input
                        .get(self.identifier_start_read_position as usize..self.read_position + 0x3)
                    {
                        Some(ident) => {
                            // reset identifier start read position
                            self.identifier_start_read_position = -0x1;
                            self.position += 0x3;
                            self.read_position += 0x3;

                            let ident_owned = String::from(ident);
                            Ok(tokens::Token::new(
                                tokens::IdentifierKind::END,
                                ident_owned,
                                self.line_number,
                                self.position,
                            ))
                        }
                        None => self.unknown_token_error(tokens::NULL),
                    }
                }
                // handles multi-line scenarios and edge cases
                else if is_space(next_char.as_bytes().first().unwrap())
                    || next_char == tokens::COMMA
                    || next_char == tokens::LBRACE
                    || next_char == tokens::RPAREN
                    || next_char == tokens::AT
                    || next_char == tokens::SEMICOLON
                    || next_char == tokens::LPAREN
                {
                    // read the identifier then update the literal and the token
                    match self
                        .input
                        .get(self.identifier_start_read_position as usize..self.read_position)
                    {
                        Some(ident) => {
                            let ident_owned = String::from(ident);

                            let tok = match ident {
                                tokens::INT => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::INTTYPE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::STRING => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::STRINGTYPE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::BOOLEAN => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::BOOLEANTYPE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::UNIT => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::UNITTYPE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::LET => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::LET,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::FUNCTION => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::FUNCTION,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::TRUE => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::TRUE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::FALSE => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::FALSE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::IF => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::IF,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),

                                tokens::ELSE => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::ELSE,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::RETURN => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::RETURN,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::FORMAT => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::FORMAT,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                tokens::PRINT => Ok(tokens::Token::new(
                                    tokens::IdentifierKind::PRINT,
                                    String::from(ident),
                                    self.line_number,
                                    self.position,
                                )),
                                _ => {
                                    if is_intergers_only(ident) {
                                        Ok(tokens::Token::new(
                                            tokens::IdentifierKind::INTLITERAL,
                                            ident_owned,
                                            self.line_number,
                                            self.position,
                                        ))
                                    } else if is_alphanumeric_only(ident) {
                                        if next_char == tokens::LPAREN {
                                            Ok(tokens::Token::new(
                                                tokens::IdentifierKind::CALLER,
                                                ident_owned,
                                                self.line_number,
                                                self.position,
                                            ))
                                        } else {
                                            Ok(tokens::Token::new(
                                                tokens::IdentifierKind::VARIABLE,
                                                ident_owned,
                                                self.line_number,
                                                self.position,
                                            ))
                                        }
                                    } else {
                                        self.unknown_token_error(ident)
                                    }
                                }
                            };

                            // reset identifier start read position
                            self.identifier_start_read_position = -0x1;
                            tok
                        }
                        None => self.unknown_token_error(tokens::NULL),
                    }
                } else if is_func_identifier_fn(next_char) {
                    let ident = self
                        .input
                        .get(self.identifier_start_read_position as usize..self.read_position + 1)
                        .unwrap();

                    if ident == tokens::FUNCTION {
                        // reset identifier start read position
                        self.identifier_start_read_position = -0x1;
                        // move the cursor forward
                        self.position += 1;
                        self.read_position += 1;
                        Ok(tokens::Token::new(
                            tokens::IdentifierKind::FUNCTION,
                            String::from(ident),
                            self.line_number,
                            self.position,
                        ))
                    } else {
                        self.unknown_token_error(ident)
                    }
                } else {
                    self.move_current_position_and_read();
                    self.extract_token_from_alphabet()
                }
            }
            None => self.unknown_token_error(tokens::NULL),
        }
    }
}

// check if the identifier is composed of both intergers and alphabet characters
// example:
//  fname, fname0, fname2, f_name, f_name1,lname123
fn is_alphanumeric_only(i: &str) -> bool {
    let mut not_valid_count = 0;
    for x in i.as_bytes() {
        if !is_allowed_alphanumeric_and_char(*x) {
            not_valid_count += 1;
        }
    }
    not_valid_count == 0
}

// checks if the identifier is composed of intergers only
// these intergers will be parsed correctly when performing arthemetic operations
fn is_intergers_only(i: &str) -> bool {
    for x in i.as_bytes() {
        if !is_digit(*x) {
            return false;
        } else {
            continue;
        }
    }
    true
}

fn is_quotation_mark(i: &str) -> bool {
    for x in i.as_bytes() {
        if *x == 0x22 || *x == 0x27 {
            return true;
        } else {
            continue;
        }
    }
    false
}

/// check if byte is ASCII alphanumeric: A-Z, a-z, 0-9, _,
/// meaning variable names can take the form of
/// name, name123,name_123, NAME,NAME123,NAME_123
fn is_allowed_alphanumeric_and_char(chr: u8) -> bool {
    is_alphabetic(chr) || is_digit(chr) || is_underscore(chr) || is_hash(chr)
}

// checks if byte is ASCII : space or tab
fn is_space(chr: &u8) -> bool {
    *chr == b' ' || *chr == b'\t'
}

// checks if byte is ASCII : newline or backslash
fn is_newline(chr: &u8) -> bool {
    *chr == b'\n' || *chr == 0x5C
}

/// check if byte is ASCII alphabetic: A-Z, a-z
fn is_alphabetic(chr: u8) -> bool {
    (0x41..=0x5A).contains(&chr) || (0x61..=0x7A).contains(&chr)
}

/// check if byte is ASCII digit: 0-9
fn is_digit(chr: u8) -> bool {
    (0x30..=0x39).contains(&chr)
}

/// check if byte is an ASCII underscore
fn is_underscore(chr: u8) -> bool {
    chr == 0x5f
}

/// check if byte is an ASCII #
fn is_hash(chr: u8) -> bool {
    chr == 0x23
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn should_read_char() {
        let mut lx = Lexer::new(String::from(",;(){}=+-!*/<>"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::MINUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::BANG
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::ASTERISK
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::SLASH
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::GT
        );
    }

    #[test]
    fn should_read_equal_token0() {
        let mut lx0 = Lexer::new(String::from("; == && || & |"));
        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::EQ
        );
        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::AND
        );

        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::OR
        );

        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::LAND
        );

        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IdentifierKind::LOR
        );
    }

    #[test]
    fn should_read_equal_token1() {
        let mut lx = Lexer::new(String::from("=="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::EQ
        );
    }

    #[test]
    fn should_read_equal_token2() {
        let mut lx = Lexer::new(String::from(">="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::GTOREQ
        );
    }

    #[test]
    fn should_read_equal_token3() {
        let mut lx = Lexer::new(String::from("!=+>="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::NOTEQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::GTOREQ
        );
    }

    #[test]
    fn should_read_equal_token4() {
        let mut lx = Lexer::new(String::from("==+>=!()=<>*<=-/~ -"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::EQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::GTOREQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::BANG
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::GT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::ASTERISK
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::LTOREQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::MINUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::SLASH
        );
        assert!(lx.new_token().is_err());
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IdentifierKind::MINUS
        );
    }

    #[test]
    fn should_read_equal_token5a() {
        let mut lx = Lexer::new(String::from("let a = 1 + 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_equal_token5b() {
        let mut lx = Lexer::new(String::from("let a = 1 - 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::MINUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_equal_token6() {
        let mut lx = Lexer::new(String::from("let person_age @int = 1 + 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_int_typing() {
        let mut lx = Lexer::new(String::from("@int"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
    }

    #[test]
    fn should_read_equal_string_typing() {
        let mut lx = Lexer::new(String::from("@string"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGTYPE
        );
    }

    #[test]
    fn should_read_equal_bool_typing() {
        let mut lx = Lexer::new(String::from("@bool"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::BOOLEANTYPE
        );
    }

    #[test]
    fn should_read_equal_unit_typing() {
        let mut lx = Lexer::new(String::from("@unit"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::UNITTYPE
        );
    }

    #[test]
    fn should_read_equal_main_typing() {
        let mut lx = Lexer::new(String::from("@main"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::MAIN
        );
    }

    #[test]
    fn should_read_equal_end_typing() {
        let mut lx = Lexer::new(String::from("@end"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::END
        );
    }

    #[test]
    fn should_read_let_keyword() {
        let mut lx = Lexer::new(String::from("let"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
    }

    #[test]
    fn should_read_true_keyword() {
        let mut lx = Lexer::new(String::from("true"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::TRUE
        );
    }

    #[test]
    fn should_read_false_keyword() {
        let mut lx = Lexer::new(String::from("false"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FALSE
        );
    }

    #[test]
    fn should_read_if_keyword() {
        let mut lx = Lexer::new(String::from("if"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::IF
        );
    }

    #[test]
    fn should_read_else_keyword() {
        let mut lx = Lexer::new(String::from("else"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ELSE
        );
    }

    #[test]
    fn should_read_return_keyword() {
        let mut lx = Lexer::new(String::from("return"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
    }

    #[test]
    fn should_read_if_else_return_keywords() {
        let mut lx = Lexer::new(String::from(
            "
        if {
            return;
        }else{
            return;
        }


        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::IF
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ELSE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
    }

    #[test]
    fn should_read_equal_token7() {
        let mut lx = Lexer::new(String::from("let name = \"alice\"; "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_token8() {
        let mut lx = Lexer::new(String::from("print(\"Name #name\"); "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PRINT
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_func_0() {
        let mut lx = Lexer::new(String::from("fn() {} "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOF
        );
    }

    #[test]
    fn should_read_func_1() {
        let mut lx = Lexer::new(String::from("func() {} "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::CALLER
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOF
        );
    }

    #[test]
    fn should_read_func_2() {
        let mut lx = Lexer::new(String::from("let num @int = fn() { return 1; }"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOF
        );
    }

    #[test]
    fn should_read_func_3() {
        let mut lx = Lexer::new(String::from(
            "let add @int = fn(x @int, y @int) { return x + y; }",
        ));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOF
        );
    }

    #[test]
    fn should_return_err() {
        let mut lx = Lexer::new(String::from("~"));
        assert!(lx.read_tokens().is_err(),);
    }

    #[test]
    fn should_read_modulus() {
        let mut lx = Lexer::new(String::from("%"));
        assert!(lx.read_tokens().is_ok(),);
    }

    #[test]
    fn should_read_multiline0() {
        let mut lx = Lexer::new(String::from(
            "
        let sum @int = 1 + 2;

        let person_age @int = 25;

        let is_active @bool = true;

        let is_live @bool = false;

        ",
        ));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::TRUE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FALSE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline1() {
        let mut lx = Lexer::new(String::from(
            "
        let add @int = fn(x @int, y @int){
            return x + y;
        };

        add(1,3);

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::CALLER
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline2() {
        let mut lx = Lexer::new(String::from(
            "
        let greater @int = fn(x @int, y @int) {
            if x > y {
                return x;
            }
            return y;
        };        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::IF
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::GT
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline3() {
        let mut lx = Lexer::new(String::from(
            "
        let greeter @unit = fn(name @string) {
            print(\"Hi you\");
            let msg @string = format(\"Hi you #name\");
        };

        greeter();

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::UNITTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::PRINT
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGTYPE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FORMAT
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::CALLER
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
    }

    #[test]
    fn should_read_multiline_main() {
        let mut lx = Lexer::new(String::from(
            "
        @main fn(){
            let x @int = 5;
            let name @string = \"Karis\";

        }@end;

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::MAIN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::END
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EOS
        );
    }

    #[test]
    fn should_read_array1() {
        let mut lx = Lexer::new(String::from(
            "
        let numbers [ @int ];
        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RSQUAREBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_array2() {
        let mut lx = Lexer::new(String::from(
            "
        let numbers [ @int];
        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );
        assert_eq!(lx.read_tokens().is_err(), true);
    }

    #[test]
    fn should_read_array3() {
        let mut lx = Lexer::new(String::from(
            "
        let numbers [ @int ] = [ 1, 2, 3 ];
        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::COMMA
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_array4() {
        let mut lx = Lexer::new(String::from(
            "
        let take_items [ @int ] = fn (items [ @int ]){
            return items;
        };

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::ASSIGN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::FUNCTION
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTTYPE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RSQUAREBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RETURN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_try_to_read_golang_program() {
        let mut lx = Lexer::new(String::from(
            "
            package main

            import \"fmt\"

            func main() {

                if 7 % 2 == 0 {
                    fmt.Println(\"7 is even\")
                } else {
                    fmt.Println(\"7 is odd\")
                }

                if 8 % 4 == 0 {
                    fmt.Println(\"8 is divisible by 4\")
                }

                if num := 9; num < 0 {
                    fmt.Println(num, \"is negative\")
                } else if num < 10 {
                    fmt.Println(num, \"has 1 digit\")
                } else {
                    fmt.Println(num, \"has multiple digits\")
                }
            }

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert!(lx.read_tokens().is_err());

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::VARIABLE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::CALLER
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::IF
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::MODULUS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::EQ
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::INTLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LBRACE
        );

        assert!(lx.read_tokens().is_err(),);

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::LPAREN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::STRINGLITERAL
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IdentifierKind::RPAREN
        );
    }
}
