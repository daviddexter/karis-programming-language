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

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = Some(String::from(tokens::NULL));
        } else {
            let item = self.input.get(self.read_position..self.read_position + 1);
            match item {
                Some(val) => {
                    println!("line 38: {:?}", val);
                    self.ch = Some(String::from(val));
                }
                None => {}
            }
        }

        self.read_position += 1;
    }

    pub fn new_token(&mut self) -> tokens::Token {
        if self.read_position == 0 {
            self.read_char();
        }

        self.skip_whitespace();

        match &self.ch {
            Some(ch) => {
                let ch_owned = ch.clone();
                let tok = if ch == tokens::COMMA {
                    tokens::Token::new(tokens::IndentifierKind::COMMA, ch_owned)
                } else if ch == tokens::SEMICOLON {
                    tokens::Token::new(tokens::IndentifierKind::SEMICOLON, ch_owned)
                } else if ch == tokens::LPAREN {
                    tokens::Token::new(tokens::IndentifierKind::LPAREN, ch_owned)
                } else if ch == tokens::RPAREN {
                    tokens::Token::new(tokens::IndentifierKind::RPAREN, ch_owned)
                } else if ch == tokens::LBRACE {
                    tokens::Token::new(tokens::IndentifierKind::LBRACE, ch_owned)
                } else if ch == tokens::RBRACE {
                    tokens::Token::new(tokens::IndentifierKind::RBRACE, ch_owned)
                } else if ch == tokens::ASSIGN {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::IndentifierKind::EQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::IndentifierKind::ASSIGN, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::IndentifierKind::ASSIGN, ch_owned),
                    }
                } else if ch == tokens::PLUS {
                    tokens::Token::new(tokens::IndentifierKind::PLUS, ch_owned)
                } else if ch == tokens::MINUS {
                    tokens::Token::new(tokens::IndentifierKind::MINUS, ch_owned)
                } else if ch == tokens::BANG {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::IndentifierKind::NOTEQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned),
                    }
                } else if ch == tokens::ASTERISK {
                    tokens::Token::new(tokens::IndentifierKind::ASTERISK, ch_owned)
                } else if ch == tokens::SLASH {
                    tokens::Token::new(tokens::IndentifierKind::SLASH, ch_owned)
                } else if ch == tokens::LT {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::IndentifierKind::LTOREQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::IndentifierKind::LT, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::IndentifierKind::LT, ch_owned),
                    }
                } else if ch == tokens::GT {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::IndentifierKind::GTOREQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::IndentifierKind::GT, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::IndentifierKind::GT, ch_owned),
                    }
                } else if ch == tokens::NULL {
                    tokens::Token::new(tokens::IndentifierKind::EOF, String::new())
                } else {
                    // update identifier_start_read_position so that we know where we
                    // are when slicing to retrieve the exact identifier.
                    // This is usually the same as the current position that we are at
                    if self.identifier_start_read_position == -1 {
                        self.identifier_start_read_position = self.position as isize;
                    }
                    self.extract_token_from_alphabet()
                };

                println!(
                    "line 138  position {:?} read position {:?} token found {:?}",
                    self.position, self.read_position, tok,
                );

                self.move_current_position_and_read();
                tok
            }
            None => tokens::Token::new(tokens::IndentifierKind::UNKNOWN, String::new()),
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

    fn extract_token_from_alphabet(&mut self) -> tokens::Token {
        // peek forward
        let next_char_fn = |begin: usize, end: usize| self.input.get(begin..end).unwrap();

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
                .unwrap();

            println!(
                "Line 196 Current Char {:?} : Next Char {:?} : Next two step chars {:?}",
                self.input.get(self.position..self.read_position).unwrap(),
                next_char,
                next_two_step_char
            );

            if (*current_char == 0x46 || *current_char == 0x66)
                && (*next_char.as_bytes().first().unwrap() == 0x6e
                    || *next_char.as_bytes().first().unwrap() == 0x4e)
                && (next_two_step_char == tokens::LPAREN)
            {
                return true;
            } else {
                return false;
            }
        };

        match &self.ch {
            Some(literal) => {
                let next_char = next_char_fn(self.position + 1, self.read_position + 1);

                println!("line 184 next char: {:?}", next_char);
                if is_space(next_char.as_bytes().first().unwrap())
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
                            println!(
                                "line 202 {:?} position {:?} read position {:?} identifier position {:?} identifier {:?}",
                                literal,
                                self.position,
                                self.read_position,
                                self.identifier_start_read_position, ident
                            );
                            let ident_owned = ident.clone().to_string();
                            let tok = if ident == tokens::INT {
                                tokens::Token::new(tokens::IndentifierKind::INTTYPE, ident_owned)
                            } else if ident == tokens::STRING {
                                tokens::Token::new(tokens::IndentifierKind::STRINGTYPE, ident_owned)
                            } else if ident == tokens::BOOLEAN {
                                tokens::Token::new(
                                    tokens::IndentifierKind::BOOLEANTYPE,
                                    ident_owned,
                                )
                            } else if ident == tokens::LET {
                                tokens::Token::new(tokens::IndentifierKind::LET, ident_owned)
                            } else if ident == tokens::FUNCTION {
                                tokens::Token::new(tokens::IndentifierKind::FUNCTION, ident_owned)
                            } else if ident == tokens::TRUE {
                                tokens::Token::new(tokens::IndentifierKind::TRUE, ident_owned)
                            } else if ident == tokens::FALSE {
                                tokens::Token::new(tokens::IndentifierKind::FALSE, ident_owned)
                            } else if ident == tokens::IF {
                                tokens::Token::new(tokens::IndentifierKind::IF, ident_owned)
                            } else if ident == tokens::ELSE {
                                tokens::Token::new(tokens::IndentifierKind::ELSE, ident_owned)
                            } else if ident == tokens::RETURN {
                                tokens::Token::new(tokens::IndentifierKind::RETURN, ident_owned)
                            } else if ident == tokens::INT {
                                tokens::Token::new(tokens::IndentifierKind::INTTYPE, ident_owned)
                            } else if ident == tokens::STRING {
                                tokens::Token::new(tokens::IndentifierKind::STRINGTYPE, ident_owned)
                            } else if ident == tokens::BOOLEAN {
                                tokens::Token::new(
                                    tokens::IndentifierKind::BOOLEANTYPE,
                                    ident_owned,
                                )
                            } else if is_intergers_only(ident) {
                                tokens::Token::new(tokens::IndentifierKind::INT, ident_owned)
                            } else if is_alphanumeric_only(ident) {
                                tokens::Token::new(tokens::IndentifierKind::VARIABLE, ident_owned)
                            } else {
                                tokens::Token::new(tokens::IndentifierKind::UNKNOWN, String::new())
                            };

                            // reset identifier start read position
                            self.identifier_start_read_position = -0x1;
                            tok
                        }
                        None => tokens::Token::new(tokens::IndentifierKind::UNKNOWN, String::new()),
                    }
                } else if is_func_identifier(next_char) {
                    let ident = self
                        .input
                        .get(self.identifier_start_read_position as usize..self.read_position + 1)
                        .unwrap();
                    // println!("line 274 funcs chars: {:?}", x);
                    if ident == tokens::FUNCTION {
                        // reset identifier start read position
                        self.identifier_start_read_position = -0x1;
                        // move the cursor forward
                        self.position = self.position + 1;
                        self.read_position = self.read_position + 1;
                        tokens::Token::new(tokens::IndentifierKind::FUNCTION, String::from(ident))
                    } else {
                        tokens::Token::new(tokens::IndentifierKind::UNKNOWN, String::new())
                    }
                } else {
                    println!(
                        "line 224 {:?} position {:?} read position {:?} identifier position {:?}",
                        literal,
                        self.position,
                        self.read_position,
                        self.identifier_start_read_position
                    );
                    self.move_current_position_and_read();
                    self.extract_token_from_alphabet()
                }
            }
            None => tokens::Token::new(tokens::IndentifierKind::UNKNOWN, String::new()),
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
    if not_valid_count != 0 {
        false
    } else {
        true
    }
}

// checks if the identifier is composed of intergers only
// these intergers will be parsed correctly when performing arthemetic operations
fn is_intergers_only(i: &str) -> bool {
    let mut not_valid_count = 0;
    for x in i.as_bytes() {
        if !is_digit(*x) {
            not_valid_count += 1;
        }
    }
    if not_valid_count != 0 {
        false
    } else {
        true
    }
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
    (chr >= 0x41 && chr <= 0x5A) || (chr >= 0x61 && chr <= 0x7A)
}

/// check if byte is ASCII digit: 0-9
fn is_digit(chr: u8) -> bool {
    chr >= 0x30 && chr <= 0x39
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
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::COMMA);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LBRACE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RBRACE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::MINUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::BANG);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASTERISK);
    }

    #[test]
    fn should_read_equal_token0() {
        let mut lx0 = Lexer::new(String::from("; =="));
        assert_eq!(
            lx0.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(lx0.new_token().token_type, tokens::IndentifierKind::EQ);
    }

    #[test]
    fn should_read_equal_token1() {
        let mut lx = Lexer::new(String::from("=="));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::EQ);
    }

    #[test]
    fn should_read_equal_token2() {
        let mut lx = Lexer::new(String::from(">="));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::GTOREQ);
    }

    #[test]
    fn should_read_equal_token3() {
        let mut lx = Lexer::new(String::from("!=+>="));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::NOTEQ);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::GTOREQ);
    }

    #[test]
    fn should_read_equal_token4() {
        let mut lx = Lexer::new(String::from("==+>=!()=<>*<=-/# -"));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::EQ);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::GTOREQ);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::BANG);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LT);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::GT);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASTERISK);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LTOREQ);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::MINUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::SLASH);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::UNKNOWN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::MINUS);
    }

    #[test]
    fn should_read_equal_token5() {
        let mut lx = Lexer::new(String::from("let a = 1 + 2;"));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_token6() {
        let mut lx = Lexer::new(String::from("let person_age @int = 1 + 2;"));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INTTYPE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_multiline() {
        let mut lx = Lexer::new(String::from(
            "
        let sum @int = 1 + 2;

        let person_age @int = 25;

        let is_active @bool = true;

        let is_live @bool = false;
        
        ",
        ));
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INTTYPE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INTTYPE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::INT);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );

        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::TRUE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );

        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::FALSE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_multiline0() {
        let mut lx = Lexer::new(String::from(
            "
        let add = fn(x, y) {
            return x + y;
        };
        ",
        ));

        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LET);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::FUNCTION);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::COMMA);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RPAREN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::LBRACE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RETURN);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::VARIABLE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(lx.new_token().token_type, tokens::IndentifierKind::RBRACE);
        assert_eq!(
            lx.new_token().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }
}
