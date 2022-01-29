use itertools::{multipeek, MultiPeek, PeekingNext};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::{self, CharIndices};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("and", TokenKind::And);
        m.insert("class", TokenKind::Class);
        m.insert("else", TokenKind::Else);
        m.insert("false", TokenKind::False);
        m.insert("for", TokenKind::For);
        m.insert("fun", TokenKind::Fun);
        m.insert("if", TokenKind::If);
        m.insert("nil", TokenKind::Nil);
        m.insert("or", TokenKind::Or);
        m.insert("print", TokenKind::Print);
        m.insert("return", TokenKind::Return);
        m.insert("super", TokenKind::Super);
        m.insert("this", TokenKind::This);
        m.insert("true", TokenKind::True);
        m.insert("var", TokenKind::Var);
        m.insert("while", TokenKind::While);
        m
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub literal: Literal<'a>,
    pub line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]

pub enum Literal<'a> {
    String(&'a str),
    Number(f64),
    None,
}

pub struct Tokenizer<'a> {
    src: &'a str,
    current: usize,
    line: usize,
    start: usize,
    char_slices: MultiPeek<CharSlices<'a>>,
    sent_eof: bool,
}

struct CharSlices<'a> {
    char_indices: CharIndices<'a>,
    len: usize,
    prev: Option<(usize, char)>,
}

impl<'a> CharSlices<'a> {
    fn new(s: &'a str) -> Self {
        let mut char_indices = s.char_indices();
        let prev = char_indices.next();

        Self {
            char_indices,
            len: s.len(),
            prev,
        }
    }
}

impl<'a> Iterator for CharSlices<'a> {
    type Item = (usize, usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self.prev.take() {
            Some(prev) => {
                if let Some(next) = self.char_indices.next() {
                    self.prev = Some(next);
                    Some((prev.0, next.0, prev.1))
                } else {
                    Some((prev.0, self.len, prev.1))
                }
            }
            None => None,
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            src,
            current: 0,
            line: 1,
            start: 0,
            char_slices: multipeek(CharSlices::new(src)),
            sent_eof: false,
        }
    }

    #[inline]
    fn at_eof(&self) -> bool {
        self.current >= self.src.len()
    }

    #[inline]
    fn next_char(&mut self) -> Option<char> {
        match self.char_slices.next() {
            Some((_, end, ch)) => {
                self.current = end;
                Some(ch)
            }
            None => None,
        }
    }

    #[inline]
    fn peek_char(&mut self) -> Option<char> {
        self.char_slices.reset_peek();
        self.peek_char_inner()
    }

    #[inline]
    fn peek_char_inner(&mut self) -> Option<char> {
        self.char_slices.peek().map(|&x| x.2)
    }

    #[inline]
    fn peek_char_next(&mut self) -> Option<(char, Option<char>)> {
        self.char_slices.reset_peek();
        self.peek_char_inner()
            .map(|ch| (ch, self.peek_char_inner()))
    }

    #[inline]
    fn next_if(&mut self, ch: char) -> Option<char> {
        match self.char_slices.peeking_next(|&x| x.2 == ch) {
            Some((_, end, ch)) => {
                self.current = end;
                Some(ch)
            }
            None => None,
        }
    }

    fn string(&mut self) -> Option<Token<'a>> {
        while let Some(ch) = self.peek_char() {
            if ch == '"' {
                break;
            }
            if ch == '\n' {
                self.line += 1;
            }
            self.next_char();
        }

        if self.at_eof() {
            None
        } else {
            // Consume the closing ".
            self.next_char();
            Some(Token {
                kind: TokenKind::String,
                lexeme: self.lexeme(),
                line: self.line,
                literal: Literal::String(unsafe {
                    str::from_utf8_unchecked(&self.src.as_bytes()[self.start + 1..self.current - 1])
                }),
            })
        }
    }

    fn number(&mut self) -> Token<'a> {
        while let Some('0'..='9') = self.peek_char() {
            self.next_char();
        }

        if let Some(('.', Some('0'..='9'))) = self.peek_char_next() {
            self.next_char();
            while let Some('0'..='9') = self.peek_char() {
                self.next_char();
            }
        }

        Token {
            kind: TokenKind::Number,
            lexeme: self.lexeme(),
            line: self.line,
            literal: Literal::Number(self.lexeme().parse::<f64>().unwrap()),
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while let Some('0'..='9' | 'a'..='z' | 'A'..='Z') = self.peek_char() {
            self.next_char();
        }

        let lexeme = self.lexeme();
        Token {
            kind: *KEYWORDS.get(lexeme).unwrap_or(&TokenKind::Identifier),
            lexeme,
            line: self.line,
            literal: Literal::None,
        }
    }

    #[inline]
    fn lexeme(&self) -> &'a str {
        unsafe { str::from_utf8_unchecked(&self.src.as_bytes()[self.start..self.current]) }
    }

    #[inline]
    fn simple_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: self.lexeme(),
            line: self.line,
            literal: Literal::None,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_eof() {
            return if self.sent_eof {
                None
            } else {
                self.sent_eof = true;
                Some(Token {
                    kind: TokenKind::Eof,
                    lexeme: "",
                    line: self.line,
                    literal: Literal::None,
                })
            };
        }

        loop {
            self.start = self.current;
            match self.next_char() {
                Some(ch) => match ch {
                    '(' => return Some(self.simple_token(TokenKind::LeftParen)),
                    ')' => return Some(self.simple_token(TokenKind::RightParen)),
                    '{' => return Some(self.simple_token(TokenKind::LeftBrace)),
                    '}' => return Some(self.simple_token(TokenKind::RightBrace)),
                    ',' => return Some(self.simple_token(TokenKind::Comma)),
                    '.' => return Some(self.simple_token(TokenKind::Dot)),
                    '-' => return Some(self.simple_token(TokenKind::Minus)),
                    '+' => return Some(self.simple_token(TokenKind::Plus)),
                    ';' => return Some(self.simple_token(TokenKind::Semicolon)),
                    '*' => return Some(self.simple_token(TokenKind::Star)),
                    '!' => {
                        return Some(match self.next_if('=') {
                            Some(_) => self.simple_token(TokenKind::BangEqual),
                            None => self.simple_token(TokenKind::Bang),
                        })
                    }
                    '=' => {
                        return Some(match self.next_if('=') {
                            Some(_) => self.simple_token(TokenKind::EqualEqual),
                            None => self.simple_token(TokenKind::Equal),
                        })
                    }
                    '<' => {
                        return Some(match self.next_if('=') {
                            Some(_) => self.simple_token(TokenKind::LessEqual),
                            None => self.simple_token(TokenKind::Less),
                        })
                    }
                    '>' => {
                        return Some(match self.next_if('=') {
                            Some(_) => self.simple_token(TokenKind::GreaterEqual),
                            None => self.simple_token(TokenKind::Greater),
                        })
                    }
                    '"' => return self.string(),
                    '/' => {
                        if let Some('/') = self.peek_char() {
                            self.next_char();
                            loop {
                                match self.peek_char() {
                                    Some('\n') => break,
                                    Some(_) => {
                                        self.next_char();
                                    }
                                    None => break,
                                }
                            }
                        } else {
                            return Some(self.simple_token(TokenKind::Slash));
                        }
                    }
                    'a'..='z' | 'A'..='Z' => return Some(self.identifier()),
                    '0'..='9' => return Some(self.number()),
                    ' ' | '\r' | '\t' => {}
                    '\n' => {
                        self.line += 1;
                    }
                    _ => break None,
                },
                None => {
                    self.sent_eof = true;
                    return Some(Token {
                        kind: TokenKind::Eof,
                        lexeme: "",
                        line: self.line,
                        literal: Literal::None,
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char_slices() {
        let mut char_slices = CharSlices::new("hello");
        assert_eq!(Some((0, 1, 'h')), char_slices.next());
        assert_eq!(Some((1, 2, 'e')), char_slices.next());
        assert_eq!(Some((2, 3, 'l')), char_slices.next());
        assert_eq!(Some((3, 4, 'l')), char_slices.next());
        assert_eq!(Some((4, 5, 'o')), char_slices.next());
        assert_eq!(None, char_slices.next());
    }

    #[test]
    fn test_tokenizer() {
        let src = r#"(){}
!!====
"hello" ""
var x = "hello"
var y = 3.6
// This is a comment.
"world""#;

        let mut tok = Tokenizer::new(src);
        assert_eq!(
            Some(Token {
                kind: TokenKind::LeftParen,
                lexeme: "(",
                line: 1,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::RightParen,
                lexeme: ")",
                line: 1,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::LeftBrace,
                lexeme: "{",
                line: 1,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::RightBrace,
                lexeme: "}",
                line: 1,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Bang,
                lexeme: "!",
                line: 2,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::BangEqual,
                lexeme: "!=",
                line: 2,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::EqualEqual,
                lexeme: "==",
                line: 2,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Equal,
                lexeme: "=",
                line: 2,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::String,
                lexeme: "\"hello\"",
                line: 3,
                literal: Literal::String("hello"),
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::String,
                lexeme: "\"\"",
                line: 3,
                literal: Literal::String(""),
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Var,
                lexeme: "var",
                line: 4,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Identifier,
                lexeme: "x",
                line: 4,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Equal,
                lexeme: "=",
                line: 4,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::String,
                lexeme: "\"hello\"",
                line: 4,
                literal: Literal::String("hello"),
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Var,
                lexeme: "var",
                line: 5,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Identifier,
                lexeme: "y",
                line: 5,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Equal,
                lexeme: "=",
                line: 5,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Number,
                lexeme: "3.6",
                line: 5,
                literal: Literal::Number(3.6),
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::String,
                lexeme: "\"world\"",
                line: 7,
                literal: Literal::String("world"),
            }),
            tok.next()
        );
        assert_eq!(
            Some(Token {
                kind: TokenKind::Eof,
                lexeme: "",
                line: 7,
                literal: Literal::None,
            }),
            tok.next()
        );
        assert_eq!(None, tok.next());
    }
}
