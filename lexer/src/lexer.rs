use errors::errors;

use crate::tokens;

#[derive(Debug, Clone)]
pub struct Lexer {
    // input that is fed to the lexer for tokenization
    input: String,

    // points to the current char in the input string. This is the same as the col of the input string
    position: usize,

    line_number: usize,

    // points to the next character in the input string after the current char
    read_position: usize,

    // the literal value of the current character
    ch: Option<String>,

    // indicates the starting point when an identifier is been read
    identifier_start_read_position: isize,

    // indicates the previous character in the input string after the current
    last_read_token: Option<tokens::Token>,

    // end of statement
    eos: bool,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input,
            position: 0,
            line_number:1,
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
            tokens::IndentifierKind::EOF,
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
                    tokens::IndentifierKind::EOS,
                    String::from(""),
                    self.line_number,  
                    self.position, 
                ))
            }
            false => {
                let res = self.new_token();
                if let Ok(tok) = &res {
                    if tok.token_type == tokens::IndentifierKind::SEMICOLON {
                        let default =
                            tokens::Token::new(
                                tokens::IndentifierKind::UNKNOWN,
                                 String::new(),
                                 self.line_number,
                                 self.position, 
                            );

                        let last_tok = self.last_read_token.as_ref().unwrap_or(&default);

                        match last_tok.token_type {
                            tokens::IndentifierKind::INTLITERAL
                            | tokens::IndentifierKind::STRINGLITERAL
                            | tokens::IndentifierKind::TRUE
                            | tokens::IndentifierKind::FALSE
                            | tokens::IndentifierKind::RPAREN
                            | tokens::IndentifierKind::RBRACE 
                            | tokens::IndentifierKind::END => {
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
            match item {
                Some(val) => {
                    self.ch = Some(String::from(val));
                }
                None => {}
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
                    tokens::COMMA => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::COMMA, ch_owned,self.line_number,
                            self.position, ))
                    }
                    tokens::SEMICOLON => Ok(tokens::Token::new(
                        tokens::IndentifierKind::SEMICOLON,
                        ch_owned,self.line_number,
                        self.position, 
                    )),
                    tokens::LPAREN => Ok(tokens::Token::new(
                        tokens::IndentifierKind::LPAREN,
                        ch_owned,self.line_number,
                        self.position, 
                    )),
                    tokens::RPAREN => Ok(tokens::Token::new(
                        tokens::IndentifierKind::RPAREN,
                        ch_owned,self.line_number,
                        self.position, 
                    )),
                    tokens::LBRACE => Ok(tokens::Token::new(
                        tokens::IndentifierKind::LBRACE,
                        ch_owned,self.line_number,
                        self.position, 
                    )),
                    tokens::RBRACE => Ok(tokens::Token::new(
                        tokens::IndentifierKind::RBRACE,
                        ch_owned,self.line_number,
                        self.position, 
                    )),

                    // is the current char is `=`, check is the next character is `=`, therefore asserting
                    // whether the combination is `==`
                    tokens::ASSIGN => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(tokens::IndentifierKind::EQ, ch_owned,self.line_number,
                                    self.position, ))
                            } else {
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::ASSIGN,
                                    ch_owned,self.line_number,
                                    self.position, 
                                ))
                            }
                        }
                        None => Ok(tokens::Token::new(
                            tokens::IndentifierKind::ASSIGN,
                            ch_owned,self.line_number,
                            self.position, 
                        )),
                    },
                    tokens::PLUS => Ok(tokens::Token::new(tokens::IndentifierKind::PLUS, ch_owned,self.line_number,
                        self.position, )),
                    tokens::MINUS => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::MINUS, ch_owned,self.line_number,
                            self.position, ))
                    }

                     // is the current char is `!`, check is the next character is `=`, therefore asserting
                    // whether the combination is `!=`
                    tokens::BANG => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(tokens::IndentifierKind::NOTEQ, ch_owned,self.line_number,
                                    self.position, ))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned,self.line_number,
                                    self.position, ))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::BANG, ch_owned,self.line_number,
                            self.position, )),
                    },
                    tokens::ASTERISK => Ok(tokens::Token::new(
                        tokens::IndentifierKind::ASTERISK,
                        ch_owned,self.line_number,self.position, 
                    )),
                    tokens::SLASH => {
                        Ok(tokens::Token::new(tokens::IndentifierKind::SLASH, ch_owned,self.line_number,
                            self.position, ))
                    }

                    // is the current char is `<`, check is the next character is `=`, therefore asserting
                    // whether the combination is `<=`
                    tokens::LT => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::LTOREQ,
                                    ch_owned,self.line_number,
                                    self.position, 
                                ))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::LT, ch_owned,self.line_number,
                                    self.position, ))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::LT, ch_owned,self.line_number,
                            self.position, )),
                    },

                    // is the current char is `>`, check is the next character is `=`, therefore asserting
                    // whether the combination is `>=`
                    tokens::GT => match self.forward_is_any_token(vec![tokens::ASSIGN]) {
                        Some(val) => {
                            if val {
                                self.move_current_position_and_read();
                                Ok(tokens::Token::new(
                                    tokens::IndentifierKind::GTOREQ,
                                    ch_owned,self.line_number,
                                    self.position, 
                                ))
                            } else {
                                Ok(tokens::Token::new(tokens::IndentifierKind::GT, ch_owned,self.line_number,
                                    self.position, ))
                            }
                        }
                        None => Ok(tokens::Token::new(tokens::IndentifierKind::GT, ch_owned,self.line_number,
                            self.position, )),
                    },                    
                    tokens::NULL => Ok(tokens::Token::new(
                        tokens::IndentifierKind::EOF,
                        String::new(),self.line_number,
                        self.position, 
                    )),
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
        
        if is_newline(current.as_bytes().first().unwrap()){
            self.line_number+=0x1;
            self.move_current_position_and_read();
            return self.skip_whitespace_and_string_escape();
        }

        if is_space(current.as_bytes().first().unwrap()) || 
            is_newline(current.as_bytes().first().unwrap())          {
            self.move_current_position_and_read();
            self.skip_whitespace_and_string_escape()
        }
    }

    // asserts that a token within the bounds of the `position` and `read_position` is
    // the same as tokens provided in the `example` vector
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

    // move the length of a string literal untill encounter a closing quatation
    fn traverse_to_closing_quotations(&self,begin:usize) -> usize {  
        let next = self.input.get(begin..begin + 0x1).unwrap();
        if is_quotation_mark(next){
            begin
        }else{
            self.traverse_to_closing_quotations(begin + 0x1)
        }        
    }

    fn extract_token_from_alphabet(&mut self) -> Result<tokens::Token, errors::KarisError> {
        // peek forward
        let next_char_fn =
            |begin: usize, end: usize, default| self.input.get(begin..end).unwrap_or(default);

        let is_func_identifier = |next_char: &str| {
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

                // if encountered an opengin quatation mark, moving along the sequence until a closing quatation mark is encountered
                if is_quotation_mark(current_literal){
                    let string_literal_ending = self.traverse_to_closing_quotations(self.position+ 0x1);
                    let string_literal = self.input.get(self.position+0x1..string_literal_ending).unwrap();                    
                    // reset identifier start read position
                    self.identifier_start_read_position = -0x1;
                    self.position = string_literal_ending;
                    self.read_position = string_literal_ending + 0x1;                    
                    Ok(tokens::Token::new(
                                tokens::IndentifierKind::STRINGLITERAL,
                                String::from(string_literal),self.line_number,
                                self.position, 
                    ))                       
                } else if current_literal == tokens::AT
                    && next_char_fn(self.position, self.read_position + 0x4, tokens::NULL)
                        == tokens::MAIN {
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
                                tokens::IndentifierKind::MAIN,
                                ident_owned,self.line_number,
                                self.position, 
                            ))
                            
                        }
                        None => self.unknown_token_error(tokens::NULL),
                    }
                } else if current_literal == tokens::AT && 
                    next_char_fn(self.position, self.read_position + 0x3, tokens::NULL) == tokens::END  {
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
                                tokens::IndentifierKind::END,
                                ident_owned,self.line_number,
                                self.position, 
                            ))
                        }
                        None => self.unknown_token_error(tokens::NULL),
                    }
                }else if is_space(next_char.as_bytes().first().unwrap())
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
                                    tokens::IndentifierKind::INTTYPE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::STRING => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::STRINGTYPE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::BOOLEAN => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::BOOLEANTYPE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::LET => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::LET,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::FUNCTION => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::FUNCTION,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::TRUE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::TRUE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::FALSE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::FALSE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::IF => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::IF,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),

                                tokens::ELSE => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::ELSE,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::RETURN => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::RETURN,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::FORMAT => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::FORMAT,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                tokens::PRINT => Ok(tokens::Token::new(
                                    tokens::IndentifierKind::PRINT,
                                    String::from(ident),self.line_number,
                                    self.position, 
                                )),
                                _ => {
                                    if is_intergers_only(ident) {
                                        Ok(tokens::Token::new(
                                            tokens::IndentifierKind::INTLITERAL,
                                            ident_owned,self.line_number,
                                            self.position, 
                                        ))
                                    } else if is_alphanumeric_only(ident) {
                                        Ok(tokens::Token::new(
                                            tokens::IndentifierKind::VARIABLE,
                                            ident_owned,self.line_number,
                                            self.position, 
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
                        None => self.unknown_token_error(tokens::NULL),
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
                            String::from(ident),self.line_number,
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

// check if byte is an ASCII #
fn is_hash(chr: u8) -> bool {
    chr == 0x23
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
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::SLASH
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::LT
        );
        assert_eq!(
            lx.new_token().unwrap().token_type,
            tokens::IndentifierKind::GT
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
        let mut lx = Lexer::new(String::from("==+>=!()=<>*<=-/~ -"));
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
    fn should_read_equal_token5a() {
        let mut lx = Lexer::new(String::from("let a = 1 + 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_read_equal_token5b() {
        let mut lx = Lexer::new(String::from("let a = 1 - 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::MINUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_read_equal_token6() {
        let mut lx = Lexer::new(String::from("let person_age @int = 1 + 2;"));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_token7() {
        let mut lx = Lexer::new(String::from("let name = \"alice\"; "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
    }

    #[test]
    fn should_read_equal_token8() {
        let mut lx = Lexer::new(String::from("print(\"Name #name\"); "));
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::PRINT
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );        
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );  
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_return_err() {
        let mut lx = Lexer::new(String::from("~"));
        assert!(lx.read_tokens().is_err(),);
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
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::TRUE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::BOOLEANTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::FALSE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline1() {
        let mut lx = Lexer::new(String::from(
            "
        let add = fn(x @int, y @int) @int{
            return x + y;
        };

        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::COMMA
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::PLUS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline2() {
        let mut lx = Lexer::new(String::from(
            "
        let greater = fn(x @int, y @int) @int {
            if x > y {
                return x;
            }
            return y;
        };        ",
        ));

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::COMMA
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::IF
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::GT
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RETURN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }

    #[test]
    fn should_read_multiline3() {
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
            tokens::IndentifierKind::MAIN
        );

        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::FUNCTION
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LPAREN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RPAREN
        );
        
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::INTLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::LET
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::VARIABLE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::STRINGTYPE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::ASSIGN
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::STRINGLITERAL
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::RBRACE
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::END
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::SEMICOLON
        );
        assert_eq!(
            lx.read_tokens().unwrap().token_type,
            tokens::IndentifierKind::EOS
        );
    }
}
