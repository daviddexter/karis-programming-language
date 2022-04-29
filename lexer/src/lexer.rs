use crate::errors;
use crate::tokens;

pub struct Lexer {
    // input that is fed to the lexer for tokenization
    input: String,

    // points to the current char in the input string
    position: usize,

    // points to the next character in the input string after the current char
    read_position: usize,

    // the literal value of the current character
    ch: Option<String>,

    // indicates the starting point when an identifier is been read
    identifier_start_read_position: isize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            identifier_start_read_position: -1,
        }
    }

    pub fn generate(&mut self) {
        while self.position < self.input.len() {
            match self.new_token() {
                Ok(token) => {
                    println!("{:?}", token);
                }
                Err(err) => eprintln!("{:?}", err),
            }
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = Some(String::from(tokens::NULL));
        } else {
            let item = self.input.get(self.read_position..self.read_position + 1);
            match item {
                Some(val) => {
                    self.ch = Some(String::from(val));
                }
                None => {}
            }
        }

        self.read_position += 1;
    }

    pub fn new_token(&mut self) -> Result<tokens::Token, errors::LexerError> {
        if self.read_position == 0 {
            self.read_char();
        }

        self.skip_whitespace();

        match &self.ch {
            Some(ch) => {
                let ch_owned = ch.clone();

                let tok = match ch.as_str() {
                    tokens::COMMA => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::COMMA, ch_owned))
                    }
                    tokens::SEMICOLON => Ok(tokens::Token::new(
                        tokens::IndentifierKind::SEMICOLON,
                        ch_owned,
                    )),
                    tokens::LPAREN => Ok(tokens::Token::new(
                        tokens::IndentifierKind::LPAREN,
                        ch_owned,
                    )),
                    tokens::RPAREN => Ok(tokens::Token::new(
                        tokens::IndentifierKind::RPAREN,
                        ch_owned,
                    )),
                    tokens::LBRACE => Ok(tokens::Token::new(
                        tokens::IndentifierKind::LBRACE,
                        ch_owned,
                    )),
                    tokens::RBRACE => Ok(tokens::Token::new(
                        tokens::IndentifierKind::RBRACE,
                        ch_owned,
                    )),
                    tokens::ASSIGN => match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(tokens::IndentifierKind::EQ, ch_owned))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::ASSIGN,
                                    ch_owned,
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IndentifierKind::ASSIGN,
                            ch_owned,
                        )),
                    },
                    tokens::PLUS => Ok(tokens::Token::new(tokens::IndentifierKind::PLUS, ch_owned)),
                    tokens::MINUS => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::MINUS, ch_owned))
                    }
                    tokens::BANG => match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(tokens::IndentifierKind::NOTEQ, ch_owned))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned)),
                    },
                    tokens::ASTERISK => Ok(tokens::Token::new(
                        tokens::IndentifierKind::ASTERISK,
                        ch_owned,
                    )),
                    tokens::SLASH => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::SLASH, ch_owned))
                    }
                    tokens::LT => match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::LTOREQ,
                                    ch_owned,
                                ))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::LT, ch_owned))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::LT, ch_owned)),
                    },
                    tokens::GT => match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::GTOREQ,
                                    ch_owned,
                                ))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::GT, ch_owned))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::GT, ch_owned)),
                    },
                    tokens::NULL => Ok(tokens::Token::new(
                        tokens::IndentifierKind::EOF,
                        String::new(),
                    )),
                    _ => {
                        // update identifier_start_read_position so that we know where we
                        // are when slicing to retrieve the exact identifier.
                        // This is usually the same as the current position that we are at
                        if self.identifier_start_read_position == -1 {
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

    fn skip_whitespace(&mut self) {
        let v = self.ch.as_ref().unwrap();
        if is_space(v.as_bytes().first().unwrap()) || is_newline(v.as_bytes().first().unwrap()) {
            self.move_current_position_and_read();
            self.skip_whitespace()
        }
    }

    fn forward_is_equal_token(&self) -> Option<bool> {
        // peek forward
        let item = self.input.get(self.position + 1..self.read_position + 1);
        match item {
            Some(val) => {
                if val == tokens::ASSIGN {
                    Some(true)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    fn unknown_token_error(&self, ident: &str) -> Result<tokens::Token, errors::LexerError> {
        Err(errors::LexerError {
            error_type: errors::LexerErrorType::UnknownToken,
            message: format!("identifier not known : {}", ident),
        })
    }

    fn extract_token_from_alphabet(&mut self) -> Result<tokens::Token, errors::LexerError> {
        // peek forward
        let next_char_fn =
            |begin: usize, end: usize| self.input.get(begin..end).unwrap_or(tokens::SEMICOLON);

        let is_func_identifier = |next_char: &str| {
            let current_char = self
                .input
                .get(self.position..self.read_position)
                .unwrap()
                .as_bytes()
                .first()
                .unwrap();

            let next_two_step_char = self
                .input
                .get(self.position + 2..self.read_position + 2)
                .unwrap_or(tokens::NULL);

            (*current_char == 0x46 || *current_char == 0x66)
                && (*next_char.as_bytes().first().unwrap() == 0x6e
                    || *next_char.as_bytes().first().unwrap() == 0x4e)
                && (next_two_step_char == tokens::LPAREN)
        };

        match &self.ch {
            Some(literal) => {
                let next_char = next_char_fn(self.position + 1, self.read_position + 1);

                if is_quotation_mark(literal) && is_alphanumeric_only(next_char) {
                    self.move_current_position_and_read();
                    self.extract_token_from_alphabet()
                } else if is_alphanumeric_only(literal) && is_quotation_mark(next_char) {
                    todo!("implementing this")
                } else if is_space(next_char.as_bytes().first().unwrap())
                    || next_char == tokens::COMMA
                    || next_char == tokens::RPAREN
                    || next_char == tokens::SEMICOLON
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
                                    tokens::IndentifierKind::INTTYPE,
                                    String::from(ident),
                                )),
                                tokens::STRING => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::INTTYPE,
                                    String::from(ident),
                                )),
                                tokens::BOOLEAN => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::BOOLEANTYPE,
                                    String::from(ident),
                                )),
                                tokens::LET => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::LET,
                                    String::from(ident),
                                )),
                                tokens::FUNCTION => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::FUNCTION,
                                    String::from(ident),
                                )),
                                tokens::TRUE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::TRUE,
                                    String::from(ident),
                                )),
                                tokens::FALSE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::FALSE,
                                    String::from(ident),
                                )),
                                tokens::IF => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::IF,
                                    String::from(ident),
                                )),

                                tokens::ELSE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::ELSE,
                                    String::from(ident),
                                )),
                                tokens::RETURN => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::RETURN,
                                    String::from(ident),
                                )),
                                _ => {
                                    if is_intergers_only(ident) {
                                        Ok(tokens::Token::new(
                                            tokens::IndentifierKind::INTLITERAL,
                                            ident_owned,
                                        ))
                                    } else if is_alphanumeric_only(ident) {
                                        Ok(tokens::Token::new(
                                            tokens::IndentifierKind::VARIABLE,
                                            ident_owned,
                                        ))
                                    } else {
                                        self.unknown_token_error(ident)
                                    }
                                }
                            };

                            // reset identifier start read position
                            self.identifier_start_read_position = -0x1;
                            tok
                        }
                        None => self.unknown_token_error(""),
                    }
                } else if is_func_identifier(next_char) {
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
                            tokens::IndentifierKind::FUNCTION,
                            String::from(ident),
                        ))
                    } else {
                        self.unknown_token_error(ident)
                    }
                } else {
                    self.move_current_position_and_read();
                    self.extract_token_from_alphabet()
                }
            }
            None => self.unknown_token_error(""),
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
        if *x == 0x22 {
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
    is_alphabetic(chr) || is_digit(chr) || is_underscore(chr)
}

// checks if byte is ASCII : space or tab
fn is_space(chr: &u8) -> bool {
    *chr == b' ' || *chr == b'\t'
}

// checks if byte is ASCII : space or tab
fn is_newline(chr: &u8) -> bool {
    *chr == b'\n'
}

/// check if byte is ASCII alphabetic: A-Z, a-z
fn is_alphabetic(chr: u8) -> bool {
    (0x41..=0x5A).contains(&chr) || (0x61..=0x7A).contains(&chr)
}

/// check if byte is ASCII digit: 0-9
fn is_digit(chr: u8) -> bool {
    (0x30..=0x39).contains(&chr)
}

// check if byte is an ASCII underscore
fn is_underscore(chr: u8) -> bool {
    chr == 0x5f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_read_char() {
        let mut lx = Lexer::new(String::from(",;(){}=+-!*/<>"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::COMMA
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::MINUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::BANG
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASTERISK
        );
    }

    #[test]
    fn should_read_equal_token0() {
        let mut lx0 = Lexer::new(String::from("; =="));
        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx0.new_token().unwrap().token_type,
            tokens::IndentifierKind::EQ
        );
    }

    #[test]
    fn should_read_equal_token1() {
        let mut lx = Lexer::new(String::from("=="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::EQ
        );
    }

    #[test]
    fn should_read_equal_token2() {
        let mut lx = Lexer::new(String::from(">="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GTOREQ
        );
    }

    #[test]
    fn should_read_equal_token3() {
        let mut lx = Lexer::new(String::from("!=+>="));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::NOTEQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GTOREQ
        );
    }

    #[test]
    fn should_read_equal_token4() {
        let mut lx = Lexer::new(String::from("==+>=!()=<>*<=-/# -"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::EQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GTOREQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::BANG
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASTERISK
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LTOREQ
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::MINUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SLASH
        );
        assert!(lx.new_token().is_err());
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::MINUS
        );
    }

    #[test]
    fn should_read_equal_token5() {
        let mut lx = Lexer::new(String::from("let a = 1 + 2;"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_token6() {
        let mut lx = Lexer::new(String::from("let person_age @int = 1 + 2;"));
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_return_err() {
        let mut lx = Lexer::new(String::from("#"));
        assert!(lx.new_token().is_err(),);
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
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::TRUE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );

        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::FALSE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_multiline1() {
        let mut lx = Lexer::new(String::from(
            "
        let add = fn(x @int, y @int) {
            return x + y;
        };
        ",
        ));

        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::FUNCTION
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::COMMA
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_multiline2() {
        let mut lx = Lexer::new(String::from(
            "
        let greater = fn(x @int, y @int) {
            if x > y {
                return x;
            }
            return y;
        };
        ",
        ));

        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::FUNCTION
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::COMMA
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::IF
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }
}
