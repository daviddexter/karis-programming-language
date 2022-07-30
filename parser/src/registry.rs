use hashbrown::HashMap;
use lexer::tokens::{IdentifierKind, Token};

use crate::objects::{ParserType, TypingKind};

#[derive(Debug, Default, Clone)]
pub struct TokenRegistry {
    pub registry: HashMap<IdentifierKind, ParserType>,
}

impl TokenRegistry {
    pub fn new() -> TokenRegistry {
        let mut tg = TokenRegistry::default();
        tg.get_token_registry();
        tg
    }

    pub fn retrieve_from_registry(&self, symbol: IdentifierKind) -> Option<&ParserType> {
        self.registry.get(&symbol)
    }

    fn get_token_registry(&mut self) {
        self.consumable(IdentifierKind::SEMICOLON);
        self.consumable(IdentifierKind::EOS);
        self.consumable(IdentifierKind::EOF);
        self.consumable(IdentifierKind::RBRACE);
        self.consumable(IdentifierKind::RPAREN);

        self.add_let_binding();
        self.add_main_binding();
        self.add_int_literal();
        self.add_variable_literal();
        self.add_string_literal();
        self.add_boolean_true_literal();
        self.add_boolean_false_literal();
        self.add_return();

        self.add_left_brace_statement();
        self.add_if_statement();
        self.add_else_statement();

        self.add_minus_or_plus_as_prefix(IdentifierKind::MINUS);
        self.add_minus_or_plus_as_prefix(IdentifierKind::PLUS);

        self.add_assign();

        self.add_infix(IdentifierKind::PLUS, 30);
        self.add_infix(IdentifierKind::MINUS, 30);
        self.add_infix(IdentifierKind::MODULUS, 30);
        self.add_infix(IdentifierKind::ASTERISK, 35);
        self.add_infix(IdentifierKind::SLASH, 40);

        self.add_opening_parenthesis(IdentifierKind::LPAREN, 50);
        self.add_closing_parenthesis(IdentifierKind::RPAREN, 55);

        self.add_infix(IdentifierKind::GT, 60);
        self.add_infix(IdentifierKind::GTOREQ, 60);
        self.add_infix(IdentifierKind::LT, 60);
        self.add_infix(IdentifierKind::LTOREQ, 60);
        self.add_infix(IdentifierKind::EQ, 60);
        self.add_infix(IdentifierKind::OR, 60);
        self.add_infix(IdentifierKind::AND, 60);

        self.add_call_declaration();
        self.add_function_declaration();
        self.add_builtin_function(IdentifierKind::PRINT, 70);
        self.add_builtin_function(IdentifierKind::FORMAT, 70);
    }

    pub(crate) fn typing_kind(tok: &Token) -> TypingKind {
        match tok.token_type {
            IdentifierKind::INTTYPE => TypingKind::Int,
            IdentifierKind::BOOLEANTYPE => TypingKind::Boolean,
            IdentifierKind::STRINGTYPE => TypingKind::String,
            _ => TypingKind::Unknown,
        }
    }

    fn consumable(&mut self, symbol: IdentifierKind) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_consumable),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(symbol, obj);
    }

    fn add_int_literal(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_int_literal),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::INTLITERAL, obj);
    }

    fn add_variable_literal(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_variable),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::VARIABLE, obj);
    }

    fn add_string_literal(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::STRINGLITERAL, obj);
    }

    fn add_boolean_true_literal(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_boolean_literal),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::TRUE, obj);
    }

    fn add_boolean_false_literal(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_boolean_literal),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::FALSE, obj);
    }

    fn add_return(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_return),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::RETURN, obj);
    }

    fn add_let_binding(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_let_expressions),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::LET, obj);
    }

    fn add_main_binding(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_main_expressions),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::MAIN, obj);
    }

    fn add_left_brace_statement(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_block),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::LBRACE, obj);
    }

    fn add_if_statement(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_if_else_expressions),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::IF, obj);
    }

    fn add_else_statement(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_if_else_expressions),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::ELSE, obj);
    }

    fn add_minus_or_plus_as_prefix(&mut self, symbol: IdentifierKind) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_minus_or_plus_as_prefix),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(symbol, obj);
    }

    fn add_assign(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_assign_operator),
            binding_power: Some(30),
        };
        self.register(IdentifierKind::ASSIGN, obj);
    }

    fn add_infix(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_infix_operator),
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_function_declaration(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_function_definition),
            led_fn: None,
            binding_power: Some(70),
        };
        self.register(IdentifierKind::FUNCTION, obj);
    }

    // parses a funtion call expression
    // A function call first identifier is of the type `CALLER`
    fn add_call_declaration(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_function_call),
            led_fn: None,
            binding_power: Some(70),
        };
        self.register(IdentifierKind::CALLER, obj);
    }

    fn add_builtin_function(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_function_call),
            led_fn: None,
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_opening_parenthesis(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_opening_parenthesis),
            led_fn: Some(Self::parse_infix_operator),
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_closing_parenthesis(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_closing_parenthesis),
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn register(&mut self, symbol: IdentifierKind, obj: ParserType) {
        if let Some((_key, val)) = self.registry.get_key_value_mut(&symbol) {
            if let Some(f) = obj.nud_fn {
                val.nud_fn = Some(f);
            }

            if let Some(f) = obj.led_fn {
                val.led_fn = Some(f);
            }

            if let Some(f) = obj.binding_power {
                val.binding_power = Some(f);
            }
        } else {
            self.registry.insert(symbol, obj);
        }
    }
}
