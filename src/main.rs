#![allow(dead_code, unused_variables)]

#[derive(Clone, PartialEq)]
enum TokenKind {
    EndOfFile,

    Identifier(String),
    Integer(u64),
    Float(f64),
    String(String),

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,

    Equals,

    Colon,
    Semicolon,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::EndOfFile         => write!(f, "EndOfFile"),

            TokenKind::Identifier(_name) => write!(f, "Identifier"),
            TokenKind::Integer(_value)   => write!(f, "Integer"),
            TokenKind::Float(_value)     => write!(f, "Float"),
            TokenKind::String(_value)    => write!(f, "String"),

            TokenKind::Plus              => write!(f, "+"),
            TokenKind::Minus             => write!(f, "-"),
            TokenKind::Asterisk          => write!(f, "*"),
            TokenKind::Slash             => write!(f, "/"),
            TokenKind::Percent           => write!(f, "%"),

            TokenKind::Equals            => write!(f, "="),

            TokenKind::Colon             => write!(f, ":"),
            TokenKind::Semicolon         => write!(f, ";"),
        }
    }
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    position: usize,
    line: usize,
    column: usize,
    length: usize,
}

impl Token {
    fn new(kind: TokenKind, position: usize, line: usize, column: usize, length: usize) -> Token {
        return Token {
            kind,
            position,
            line,
            column,
            length,
        };
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Kind: '{}', Position: {}, Line: {}, Column: {}, Length: {}", self.kind, self.position, self.line, self.column, self.length).unwrap();
        match &self.kind {
            TokenKind::Identifier(name) => write!(f, ", Name: {}", name),
            TokenKind::Integer(value)   => write!(f, ", Value: {}", value),
            TokenKind::Float(value)     => write!(f, ", Value: {}", value),
            TokenKind::String(value)    => write!(f, ", Value: {}", value),
            _                           => write!(f, ""),
        }
    }
}

struct Lexer {
    source: String,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &String) -> Lexer {
        return Lexer {
            source: source.clone(),
            position: 0,
            line: 1,
            column: 1,
        };
    }

    fn peek_char(&self, offset: usize) -> char {
        if self.position + offset >= self.source.len() {
            return '\0';
        }
        return self.source.chars().nth(self.position + offset).unwrap();
    }

    fn current_char(&self) -> char {
        return self.peek_char(0);
    }

    fn next_char(&mut self) -> char {
        let current = self.current_char();
        self.position += 1;
        self.column += 1;
        if current == '\n' {
            self.line += 1;
            self.column = 1;
        }
        return current;
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            let start_position = self.position;
            let start_line = self.line;
            let start_column = self.column;

            let lex_chars = |lexer: &mut Lexer, kind: TokenKind, length: usize| -> Token {
                for _i in 0..length {
                    lexer.next_char();
                }
                return Token::new(kind, start_position, start_line, start_column, length);
            };

            match self.current_char() {
                '\0' => return lex_chars(self, TokenKind::EndOfFile, 0),

                '+' => return lex_chars(self, TokenKind::Plus, 1),
                '-' => return lex_chars(self, TokenKind::Minus, 1),
                '*' => return lex_chars(self, TokenKind::Asterisk, 1),
                '/' => return lex_chars(self, TokenKind::Slash, 1),
                '%' => return lex_chars(self, TokenKind::Percent, 1),

                '=' => return lex_chars(self, TokenKind::Equals, 1),

                ':' => return lex_chars(self, TokenKind::Colon, 1),
                ';' => return lex_chars(self, TokenKind::Semicolon, 1),

                ' ' | '\t' | '\n' | '\r' => {
                    self.next_char();
                },

                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut length: usize = 0;
                    let mut name = String::new();

                    loop {
                        match self.current_char() {
                            '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' => {
                                length += 1;
                                name.push(self.next_char());
                            },

                            _ => break,
                        }
                    }

                    return Token::new(TokenKind::Identifier(name), start_position, start_line, start_column, length);
                },

                '0'..='9' => {
                    let mut length: usize = 0;
                    let mut int_value: u64 = 0;

                    let base: u64 = if self.current_char() == '0' {
                        self.next_char();
                        match self.current_char() {
                            'x' => { self.next_char(); 16 },
                            'b' => { self.next_char(); 2 },
                            _ => 10
                        }
                    } else {
                        10
                    };

                    assert!(base <= 36);

                    loop {
                        match self.current_char() {
                            '0'..='9' => {
                                length += 1;

                                int_value *= base;
                                int_value += match self.current_char() {
                                    '0'..='9' => {
                                        self.current_char() as u64 - '0' as u64
                                    },

                                    'a'..='z' => {
                                        self.current_char() as u64 - 'a' as u64 + 10
                                    },

                                    'A'..='Z' => {
                                        self.current_char() as u64 - 'A' as u64 + 10
                                    },

                                    _ => panic!(),
                                };

                                self.next_char();
                            },

                            '_' => {
                                length += 1;
                                self.next_char();
                            },

                            '.' => {
                                self.next_char();

                                let mut float_value = int_value as f64;
                                let mut denominator: u64 = 1;

                                loop {
                                    match self.current_char() {
                                        '0'..='9' => {
                                            length += 1;

                                            denominator *= base;
                                            float_value += match self.current_char() {
                                                '0'..='9' => {
                                                    self.current_char() as u64 - '0' as u64
                                                },

                                                'a'..='z' => {
                                                    self.current_char() as u64 - 'a' as u64 + 10
                                                },

                                                'A'..='Z' => {
                                                    self.current_char() as u64 - 'A' as u64 + 10
                                                },

                                                _ => panic!(),
                                            } as f64 / denominator as f64;

                                            self.next_char();
                                        },

                                        '_' => {
                                            length += 1;
                                            self.next_char();
                                        },

                                        '.' => {
                                            panic!(); // TODO: Error message
                                        },

                                        _ => break,
                                    }
                                }

                                return Token::new(TokenKind::Float(float_value), start_position, start_line, start_column, length);
                            },

                            _ => break,
                        }
                    }

                    return Token::new(TokenKind::Integer(int_value), start_position, start_line, start_column, length);
                },

                '"' => {
                    self.next_char();

                    let mut length = 0;
                    let mut string = String::new();

                    loop {
                        match self.current_char() {
                            '"' | '\0' => break,

                            '\\' => {
                                length += 2;
                                self.next_char();

                                match self.current_char() {
                                    '\\' => { self.next_char(); string.push('\\'); },
                                    '"' => { self.next_char(); string.push('"'); },
                                    '0' => { self.next_char(); string.push('\0'); },
                                    't' => { self.next_char(); string.push('\t'); },
                                    'n' => { self.next_char(); string.push('\n'); },
                                    'r' => { self.next_char(); string.push('\r'); },

                                    _ => panic!(),
                                }
                            },

                            _ => {
                                length += 1;
                                string.push(self.next_char());
                            },
                        }
                    }

                    assert_eq!(self.next_char(), '"'); // TODO: Error message

                    return Token::new(TokenKind::String(string), start_position, start_line, start_column, length);
                },

                _ => panic!(), // TODO: Error message
            };
        }
    }
}

enum Ast {
    Statement(AstStatement),
}

enum AstStatement {
    Expression(AstExpression),
}

enum AstExpression {
    BinaryExpression(Box<AstBinaryExpression>),
    UnaryExpression(Box<AstUnaryExpression>),
}

struct AstUnaryExpression {
    operator: Token,
    operand: AstExpression,
}

impl AstUnaryExpression {
    pub fn new(operator: Token, operand: AstExpression) -> AstExpression {
        return AstExpression::UnaryExpression(Box::new(AstUnaryExpression {
            operator,
            operand,
        }));
    }
}

struct AstBinaryExpression {
    left: AstExpression,
    operator: Token,
    right: AstExpression,
}

impl AstBinaryExpression {
    pub fn new(left: AstExpression, operator: Token, right: AstExpression) -> AstExpression {
        return AstExpression::BinaryExpression(Box::new(AstBinaryExpression {
            left,
            operator,
            right,
        }));
    }
}

struct Parser {
    source: String,
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(source: &String) -> Parser {
        let mut tokens = Vec::<Token>::new();

        let mut lexer = Lexer::new(source);
        loop {
            let token = lexer.next_token();

            let end: bool = token.kind == TokenKind::EndOfFile;

            tokens.push(token);

            if end {
                break;
            }
        }

        return Parser {
            source: source.clone(),
            tokens,
            position: 0,
        };
    }

    fn peek_token(&self, offset: usize) -> &Token {
        if self.position + offset >= self.tokens.len() {
            return self.tokens.get(self.tokens.len() - 1).unwrap();
        }
        return self.tokens.get(self.position + offset).unwrap();
    }

    fn current_token(&self) -> &Token {
        return self.peek_token(0);
    }

    fn next_token(&mut self) -> &Token {
        let token = self.current_token();
        self.position += 1;
        return token;
    }

    fn parse_expression(&mut self) -> AstExpression {
        return self.parse_binary_expression(0);
    }

    fn parse_primary_expression(&mut self) -> AstExpression {
        unimplemented!();
    }

    fn parse_binary_expression(&mut self, presedence: u8) -> AstExpression {
        let unary_presedence = Parser::get_unary_presedence(self.current_token());
        let left = if unary_presedence != 0 && unary_presedence > presedence {
            let operator = self.next_token();
            let operand = self.parse_binary_expression(unary_presedence);
            AstUnaryExpression::new(operator.clone(), operand)
        } else {
            self.parse_expression()
        };

        loop {
        }
    }

    fn get_unary_presedence(token: &Token) -> u8 {
        match &token.kind {
            TokenKind::Plus |
            TokenKind::Minus => return 3,

            _ => return 0,
        }
    }

    fn get_binary_presedence(token: &Token) -> u8 {
        match &token.kind {
            TokenKind::Asterisk |
            TokenKind::Slash |
            TokenKind::Percent => return 2,

            TokenKind::Plus |
            TokenKind::Minus => return 1,

            _ => return 0,
        }
    }
}

fn main() {
    let source = String::from("1 + 2.3 * 0x45 / 0b10;\n");

    if true {
        let mut lexer = Lexer::new(&source);

        loop {
            let token = lexer.next_token();
            println!("{}", token);

            if token.kind == TokenKind::EndOfFile {
                break;
            }
        }

        println!();
    }

    let mut parser = Parser::new(&source);
    let expression = parser.parse_expression();
}
