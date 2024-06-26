use crate::token::Token;
use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
  "fn" => Token::Function,
  "let" => Token::Let,
  "true" => Token::True,
  "false" => Token::False,
  "if" => Token::If,
  "else" => Token::Else,
  "return" => Token::Return,
};

pub struct Lexer {
  input: Vec<char>,
  position: usize,
  read_position: usize,
  ch: char,
}

impl Lexer {
  pub fn new(input: &str) -> Self {
    let mut lexer = Self {
      input: input.chars().collect(),
      position: 0,
      read_position: 0,
      ch: '\0',
    };
    lexer.read_char();
    lexer
  }

  fn read_char(&mut self) {
    if self.read_position >= self.input.len() {
      self.ch = '\0';
    } else {
      self.ch = self.input[self.read_position];
    }
    self.position = self.read_position;
    self.read_position += 1;
  }

  fn peek_char(&self) -> char {
    if self.read_position >= self.input.len() {
      '\0'
    } else {
      self.input[self.read_position]
    }
  }

  fn read_identifier(&mut self) -> Token {
    let initial = self.position;
    while is_letter(self.ch) {
      self.read_char();
    }
    let ident: String = self.input[initial..self.position].iter().collect();
    KEYWORDS
      .get(&ident)
      .cloned()
      .map_or(Token::Ident(ident), |keyword| keyword)
  }

  fn read_number(&mut self) -> Token {
    let initial = self.position;
    while self.ch.is_ascii_digit() {
      self.read_char();
    }
    let number: String = self.input[initial..self.position].iter().collect();
    Token::Int(number)
  }

  fn skip_whitespace(&mut self) {
    while self.ch.is_ascii_whitespace() {
      self.read_char();
    }
  }

  pub fn next_token(&mut self) -> Token {
    self.next().unwrap()
  }

  // #[cfg(test)]
  // pub fn from(input: &str) -> Self {
  //   Self::new(input)
  // }

  fn read_string(&mut self) -> String {
    let start_pos = self.position + 1;
    self.read_char();
    while self.ch != '"' && self.ch != '\0' {
      self.read_char();
    }
    self.input[start_pos..self.position].iter().collect()
  }
}

impl Iterator for Lexer {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();
    let token: Token;
    match self.ch {
      ';' => token = Token::Semicolon,
      ':' => token = Token::Colon,
      '(' => token = Token::LParen,
      ')' => token = Token::RParen,
      ',' => token = Token::Comma,
      '+' => token = Token::Plus,
      '-' => token = Token::Minus,
      '{' => token = Token::LBrace,
      '}' => token = Token::RBrace,
      '[' => token = Token::LBracket,
      ']' => token = Token::RBracket,
      '/' => token = Token::Slash,
      '*' => token = Token::Asterisk,
      '<' => token = Token::Lt,
      '>' => token = Token::Gt,
      '"' => token = Token::String(self.read_string()),
      '!' => {
        if self.peek_char() == '=' {
          self.read_char();
          token = Token::NotEq;
        } else {
          token = Token::Bang;
        }
      }
      '=' => {
        if self.peek_char() == '=' {
          self.read_char();
          token = Token::Eq;
        } else {
          token = Token::Assign;
        }
      }
      '\0' => token = Token::Eof,
      _ => {
        if is_letter(self.ch) {
          return Some(self.read_identifier());
        } else if self.ch.is_ascii_digit() {
          return Some(self.read_number());
        }
        token = Token::Illegal;
      }
    };
    self.read_char();
    Some(token)
  }
}

const fn is_letter(ch: char) -> bool {
  ch.is_ascii_alphabetic() || ch == '_'
}

// tests

#[cfg(test)]
mod tests {
  use crate::lexer::*;
  use crate::token::*;

  #[test]
  fn token_variant() {
    assert!(Token::Eof.same_variant(&Token::Eof));
    assert!(!Token::Eof.same_variant(&Token::Comma));
    assert!(Token::Int("33".to_string()).same_variant(&Token::Int("44".to_string())));
    assert!(!Token::Int("33".to_string()).same_variant(&Token::Ident("33".to_string())));
  }

  #[test]
  fn next_token() {
    let input = r#"
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
    "foobar"
    "foo bar"
    [1, 2];
    {"foo": "bar"}
    "#;
    let cases = vec![
      Token::Let,
      Token::Ident("five".to_string()),
      Token::Assign,
      Token::Int("5".to_string()),
      Token::Semicolon,
      Token::Let,
      Token::Ident("ten".to_string()),
      Token::Assign,
      Token::Int("10".to_string()),
      Token::Semicolon,
      Token::Let,
      Token::Ident("add".to_string()),
      Token::Assign,
      Token::Function,
      Token::LParen,
      Token::Ident("x".to_string()),
      Token::Comma,
      Token::Ident("y".to_string()),
      Token::RParen,
      Token::LBrace,
      Token::Ident("x".to_string()),
      Token::Plus,
      Token::Ident("y".to_string()),
      Token::Semicolon,
      Token::RBrace,
      Token::Semicolon,
      Token::Let,
      Token::Ident("result".to_string()),
      Token::Assign,
      Token::Ident("add".to_string()),
      Token::LParen,
      Token::Ident("five".to_string()),
      Token::Comma,
      Token::Ident("ten".to_string()),
      Token::RParen,
      Token::Semicolon,
      Token::Bang,
      Token::Minus,
      Token::Slash,
      Token::Asterisk,
      Token::Int("5".to_string()),
      Token::Semicolon,
      Token::Int("5".to_string()),
      Token::Lt,
      Token::Int("10".to_string()),
      Token::Gt,
      Token::Int("5".to_string()),
      Token::Semicolon,
      Token::If,
      Token::LParen,
      Token::Int("5".to_string()),
      Token::Lt,
      Token::Int("10".to_string()),
      Token::RParen,
      Token::LBrace,
      Token::Return,
      Token::True,
      Token::Semicolon,
      Token::RBrace,
      Token::Else,
      Token::LBrace,
      Token::Return,
      Token::False,
      Token::Semicolon,
      Token::RBrace,
      Token::Int("10".to_string()),
      Token::Eq,
      Token::Int("10".to_string()),
      Token::Semicolon,
      Token::Int("10".to_string()),
      Token::NotEq,
      Token::Int("9".to_string()),
      Token::Semicolon,
      Token::String("foobar".to_string()),
      Token::String("foo bar".to_string()),
      Token::LBracket,
      Token::Int("1".to_string()),
      Token::Comma,
      Token::Int("2".to_string()),
      Token::RBracket,
      Token::Semicolon,
      Token::LBrace,
      Token::String("foo".to_string()),
      Token::Colon,
      Token::String("bar".to_string()),
      Token::RBrace,
      Token::Eof,
    ];

    let lexer = Lexer::new(input);

    for (token, expected) in lexer.zip(cases.iter()) {
      assert_eq!(token, *expected);
    }
  }
}
