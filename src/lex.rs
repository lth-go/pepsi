use core::panic;
use std::io::{Bytes, Read};
use std::iter::Peekable;
use std::mem;

#[derive(Debug, PartialEq)]
pub enum Token {
    And,             // and
    Break,           // break
    Do,              // do
    Else,            // else
    Elseif,          // elseif
    End,             // end
    False,           // false
    For,             // for
    Function,        // function
    Goto,            // goto
    If,              // if
    In,              // in
    Local,           // local
    Nil,             // nil
    Not,             // not
    Or,              // or
    Repeat,          // repeat
    Return,          // return
    Then,            // then
    True,            // true
    Until,           // until
    While,           // while
    Add,             // +
    Sub,             // -
    Mul,             // *
    Div,             // /
    Mod,             // %
    Pow,             // ^
    Len,             // #
    BitAnd,          // &
    BitNot,          // ~
    BitOr,           // |
    ShiftL,          // <<
    ShiftR,          // >>
    Idiv,            // //
    Equal,           // ==
    NotEq,           // ~=
    LesEq,           // <=
    GreEq,           // >=
    Less,            // <
    Greater,         // >
    Assign,          // =
    ParL,            // (
    ParR,            // )
    CurlyL,          // {
    CurlyR,          // }
    SqurL,           // [
    SquarR,          // ]
    DoubColon,       // ::
    SemiColon,       // ;
    Colon,           // :
    Comma,           // ,
    Dot,             // .
    Concat,          // ..
    Dots,            // ...
    Interger(i64),   // int
    Float(f64),      // float
    String(Vec<u8>), // string
    Name(String),    // name of variables or table keys
    Eos,             // end
}

#[derive(Debug)]
pub struct Lex<R: Read> {
    input: Peekable<Bytes<R>>,
    ahead: Token,
}

impl<R: Read> Lex<R> {
    pub fn new(input: R) -> Self {
        Lex {
            input: input.bytes().peekable(),
            ahead: Token::Eos,
        }
    }

    pub fn next(&mut self) -> Token {
        if self.ahead == Token::Eos {
            self.do_next()
        } else {
            mem::replace(&mut self.ahead, Token::Eos)
        }
    }

    pub fn peek(&mut self) -> &Token {
        if self.ahead == Token::Eos {
            self.ahead = self.do_next();
        }
        &self.ahead
    }

    pub fn expect(&mut self, t: Token) {
        assert_eq!(self.next(), t);
    }

    pub fn do_next(&mut self) -> Token {
        if let Some(b) = self.next_byte() {
            match b {
                b' ' | b'\r' | b'\n' | b'\t' => self.do_next(),
                b'+' => Token::Add,
                b'*' => Token::Mul,
                b'%' => Token::Mod,
                b'^' => Token::Pow,
                b'#' => Token::Len,
                b'&' => Token::BitAnd,
                b'|' => Token::BitOr,
                b'(' => Token::ParL,
                b')' => Token::ParR,
                b'{' => Token::CurlyL,
                b'}' => Token::CurlyR,
                b'[' => Token::SqurL,
                b']' => Token::SquarR,
                b';' => Token::SemiColon,
                b',' => Token::Comma,
                b'/' => self.check_ahead(b'/', Token::Idiv, Token::Div),
                b'=' => self.check_ahead(b'=', Token::Equal, Token::Assign),
                b'~' => self.check_ahead(b'=', Token::NotEq, Token::BitNot),
                b':' => self.check_ahead(b':', Token::DoubColon, Token::Colon),
                b'<' => self.check_ahead2(b'=', Token::LesEq, b'<', Token::ShiftL, Token::Less),
                b'>' => self.check_ahead2(b'=', Token::GreEq, b'>', Token::ShiftR, Token::Greater),
                b'\'' | b'"' => self.read_string(b),
                b'.' => match self.peek_byte() {
                    b'.' => {
                        self.next_byte();
                        if self.peek_byte() == b'.' {
                            self.next_byte();
                            Token::Dots
                        } else {
                            Token::Concat
                        }
                    }
                    b'0'..=b'9' => self.read_number_fraction(0),
                    _ => Token::Dot,
                },
                b'-' => {
                    if self.peek_byte() == b'-' {
                        self.next_byte();
                        self.read_comment();
                        self.do_next()
                    } else {
                        Token::Sub
                    }
                }
                b'0'..=b'9' => self.read_number(b),
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.read_name(b),
                _ => panic!("unexpected char: {b}"),
            }
        } else {
            Token::Eos
        }
    }

    fn peek_byte(&mut self) -> u8 {
        match self.input.peek() {
            Some(Ok(b)) => *b,
            Some(_) => panic!("lex peek error"),
            None => b'\0',
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.input.next().map(|r| r.unwrap())
    }

    fn check_ahead(&mut self, ahead: u8, long: Token, short: Token) -> Token {
        if self.peek_byte() == ahead {
            self.next_byte();
            long
        } else {
            short
        }
    }

    fn check_ahead2(&mut self, ahead1: u8, long1: Token, ahead2: u8, long2: Token, short: Token) -> Token {
        let b = self.peek_byte();
        if b == ahead1 {
            self.next_byte();
            long1
        } else if b == ahead2 {
            self.next_byte();
            long2
        } else {
            short
        }
    }

    fn read_number(&mut self, first: u8) -> Token {
        // heximal
        if first == b'0' {
            let second = self.peek_byte();
            if second == b'x' || second == b'X' {
                return self.read_heximal();
            }
        }

        // decimal
        let mut n = (first - b'0') as i64;
        loop {
            let b = self.peek_byte();
            if let Some(d) = char::to_digit(b as char, 10) {
                self.next_byte();
                n = n * 10 + d as i64;
            } else if b == b'.' {
                return self.read_number_fraction(n);
            } else if b == b'e' || b == b'E' {
                return self.read_number_exp(n as f64);
            } else {
                break;
            }
        }

        // check following
        let fb = self.peek_byte();
        if (fb as char).is_alphabetic() || fb == b'.' {
            panic!("malformat number");
        }

        Token::Interger(n)
    }

    fn read_number_fraction(&mut self, i: i64) -> Token {
        self.next_byte();

        let mut n: i64 = 0;
        let mut x: f64 = 1.0;

        loop {
            let b = self.peek_byte();
            if let Some(d) = char::to_digit(b as char, 10) {
                self.next_byte();
                n = n * 10 + d as i64;
                x *= 10.0;
            } else {
                break;
            }
        }

        Token::Float(i as f64 + n as f64 / x)
    }

    fn read_number_exp(&mut self, _f: f64) -> Token {
        self.next_byte();
        todo!("lex number exp")
    }

    fn read_heximal(&mut self) -> Token {
        self.next_byte();
        todo!("lex heximal")
    }

    fn read_string(&mut self, quote: u8) -> Token {
        let mut s = Vec::new();
        loop {
            match self.next_byte().expect("unfinished string") {
                b'\n' => panic!("unfinished string"),
                b'\\' => s.push(self.read_escape()),
                b if b == quote => break,
                b => s.push(b),
            }
        }

        Token::String(s)
    }

    fn read_escape(&mut self) -> u8 {
        match self.next_byte().expect("string escape") {
            b'a' => 0x07,
            b'b' => 0x08,
            b'f' => 0x0c,
            b'v' => 0x0b,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'\\' => b'\\',
            b'"' => b'"',
            b'\'' => b'\'',
            b'x' => {
                let n1 = char::to_digit(self.next_byte().unwrap() as char, 16).unwrap();
                let n2 = char::to_digit(self.next_byte().unwrap() as char, 16).unwrap();
                (n1 * 16 + n2) as u8
            }
            ch @ b'0'..=b'9' => {
                let mut n = char::to_digit(ch as char, 10).unwrap();
                if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                    self.next_byte();
                    n = n * 10 + d;
                    if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                        self.next_byte();
                        n = n * 10 + d;
                    }
                }
                u8::try_from(n).expect("decimal escape to large")
            }
            _ => panic!("invalid string escape"),
        }
    }

    fn read_name(&mut self, first: u8) -> Token {
        let mut s = String::new();
        s.push(first as char);

        loop {
            let ch = self.peek_byte() as char;
            if ch.is_alphanumeric() || ch == '_' {
                self.next_byte();
                s.push(ch);
            } else {
                break;
            }
        }

        match &s as &str {
            "and" => Token::And,
            "break" => Token::Break,
            "do" => Token::Do,
            "else" => Token::Else,
            "elseif" => Token::Elseif,
            "end" => Token::End,
            "false" => Token::False,
            "for" => Token::For,
            "function" => Token::Function,
            "goto" => Token::Goto,
            "if" => Token::If,
            "in" => Token::In,
            "local" => Token::Local,
            "nil" => Token::Nil,
            "not" => Token::Not,
            "or" => Token::Or,
            "repeat" => Token::Repeat,
            "return" => Token::Return,
            "then" => Token::Then,
            "true" => Token::True,
            "until" => Token::Until,
            "while" => Token::While,
            _ => Token::Name(s),
        }
    }

    // '--' has been read
    fn read_comment(&mut self) {
        match self.next_byte() {
            None => (),
            Some(b'[') => todo!("long comment"),
            Some(_) => {
                while let Some(b) = self.next_byte() {
                    if b == b'\n' {
                        break;
                    }
                }
            }
        }
    }
}
