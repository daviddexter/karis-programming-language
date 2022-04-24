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
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        }
    }

    pub fn read_char(&mut self) {
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

    pub fn new_token(&mut self) -> tokens::Token {
        if self.read_position == 0 {
            self.read_char();
        }

        match &self.ch {
            Some(ch) => {
                let ch_owned = ch.clone();
                let tok = if ch == tokens::COMMA {
                    tokens::Token::new(tokens::TokenTypeKind::COMMA, ch_owned)
                } else if ch == tokens::SEMICOLON {
                    tokens::Token::new(tokens::TokenTypeKind::SEMICOLON, ch_owned)
                } else if ch == tokens::LPAREN {
                    tokens::Token::new(tokens::TokenTypeKind::LPAREN, ch_owned)
                } else if ch == tokens::RPAREN {
                    tokens::Token::new(tokens::TokenTypeKind::RPAREN, ch_owned)
                } else if ch == tokens::LBRACE {
                    tokens::Token::new(tokens::TokenTypeKind::LBRACE, ch_owned)
                } else if ch == tokens::RBRACE {
                    tokens::Token::new(tokens::TokenTypeKind::RBRACE, ch_owned)
                } else if ch == tokens::ASSIGN {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::TokenTypeKind::EQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::TokenTypeKind::ASSIGN, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::TokenTypeKind::ASSIGN, ch_owned),
                    }
                } else if ch == tokens::PLUS {
                    tokens::Token::new(tokens::TokenTypeKind::PLUS, ch_owned)
                } else if ch == tokens::MINUS {
                    tokens::Token::new(tokens::TokenTypeKind::MINUS, ch_owned)
                } else if ch == tokens::BANG {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::TokenTypeKind::NOTEQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::TokenTypeKind::BANG, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::TokenTypeKind::BANG, ch_owned),
                    }
                } else if ch == tokens::ASTERISK {
                    tokens::Token::new(tokens::TokenTypeKind::ASTERISK, ch_owned)
                } else if ch == tokens::SLASH {
                    tokens::Token::new(tokens::TokenTypeKind::SLASH, ch_owned)
                } else if ch == tokens::LT {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::TokenTypeKind::LTOREQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::TokenTypeKind::LT, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::TokenTypeKind::LT, ch_owned),
                    }
                } else if ch == tokens::GT {
                    match self.forward_is_equal_token() {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                tokens::Token::new(tokens::TokenTypeKind::GTOREQ, ch_owned)
                            } else {
                                tokens::Token::new(tokens::TokenTypeKind::GT, ch_owned)
                            }
                        }
                        None => tokens::Token::new(tokens::TokenTypeKind::GT, ch_owned),
                    }
                } else if ch == tokens::NULL {
                    tokens::Token::new(tokens::TokenTypeKind::EOF, String::new())
                } else {
                    tokens::Token::new(tokens::TokenTypeKind::UNKNOWN, String::new())
                };

                self.move_current_position_and_read();
                tok
            }
            None => tokens::Token::new(tokens::TokenTypeKind::UNKNOWN, String::new()),
        }
    }

    fn move_current_position_and_read(&mut self) {
        self.position += 1;
        self.read_char();
    }

    fn forward_is_equal_token(&self) -> Option<bool> {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_read_char() {
        let mut lx = Lexer::new(String::from(",;(){}=+-!*/<>"));
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::COMMA);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::SEMICOLON);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::LPAREN);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::RPAREN);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::LBRACE);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::RBRACE);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::ASSIGN);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::PLUS);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::MINUS);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::BANG);
        assert_eq!(lx.new_token().token_type, tokens::TokenTypeKind::ASTERISK);
    }

    #[test]
    fn should_read_equal_token() {
        let mut lx0 = Lexer::new(String::from(";=="));
        assert_eq!(lx0.new_token().token_type, tokens::TokenTypeKind::SEMICOLON);
        assert_eq!(lx0.new_token().token_type, tokens::TokenTypeKind::EQ);

        let mut lx1 = Lexer::new(String::from("=="));
        assert_eq!(lx1.new_token().token_type, tokens::TokenTypeKind::EQ);

        let mut lx2 = Lexer::new(String::from(">="));
        assert_eq!(lx2.new_token().token_type, tokens::TokenTypeKind::GTOREQ);

        let mut lx3 = Lexer::new(String::from("!=+>="));
        assert_eq!(lx3.new_token().token_type, tokens::TokenTypeKind::NOTEQ);
        assert_eq!(lx3.new_token().token_type, tokens::TokenTypeKind::PLUS);
        assert_eq!(lx3.new_token().token_type, tokens::TokenTypeKind::GTOREQ);

        let mut lx4 = Lexer::new(String::from("==+>=!()=<>*<=-/#"));
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::EQ);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::PLUS);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::GTOREQ);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::BANG);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::LPAREN);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::RPAREN);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::ASSIGN);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::LT);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::GT);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::ASTERISK);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::LTOREQ);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::MINUS);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::SLASH);
        assert_eq!(lx4.new_token().token_type, tokens::TokenTypeKind::UNKNOWN);

        let mut lx5 = Lexer::new(String::from("let a = 1 + 2;"));
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::LET);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::IDENT);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::EQ);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::INT);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::PLUS);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::INT);
        assert_eq!(lx5.new_token().token_type, tokens::TokenTypeKind::SEMICOLON);
    }
}
