use crate::token::Token;
use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
  "fn" => Token::Function,
  "let" => Token::Let,
};

struct Lexer {
  input: Vec<char>,
  position: usize,
  read_position: usize,
  ch: char,
}

impl Lexer {
  fn new(input: String) -> Lexer {
    let mut lexer = Lexer {
      input: input.chars().collect(),
      position: 0,
      read_position: 0,
      ch: '\0',
    };
    lexer.read_char();
    return lexer;
  }

  fn from(input: &str) -> Lexer {
    Lexer::new(String::from(input))
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

  fn read_identifier(&mut self) -> Token {
    let initial = self.position;
    while is_letter(&self.ch) {
      self.read_char();
    }
    let ident: String = self.input[initial..self.position].iter().collect();
    return match KEYWORDS.get(&ident).cloned() {
      Some(keyword) => keyword,
      None => Token::Ident(ident),
    };
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
}

impl Iterator for Lexer {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();
    let token: Token;
    match self.ch {
      '=' => token = Token::Assign,
      ';' => token = Token::Semicolon,
      '(' => token = Token::LParen,
      ')' => token = Token::RParen,
      ',' => token = Token::Comma,
      '+' => token = Token::Plus,
      '{' => token = Token::LBrace,
      '}' => token = Token::RBrace,
      '\0' => token = Token::EOF,
      _ => {
        if is_letter(&self.ch) {
          return Some(self.read_identifier());
        } else if self.ch.is_ascii_digit() {
          return Some(self.read_number());
        }
        token = Token::Illegal
      }
    };
    self.read_char();
    return Some(token);
  }
}

fn is_letter(ch: &char) -> bool {
  ch.is_ascii_alphabetic() || *ch == '_'
}

// tests

#[cfg(test)]
mod tests {
  use crate::lexer::*;
  use crate::token::*;

  #[test]
  fn next_token() {
    let input = r#"
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
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
      Token::EOF,
    ];

    let lexer = Lexer::from(input);

    for (token, expected) in lexer.zip(cases.iter()) {
      assert_eq!(token, *expected);
    }
  }
}
