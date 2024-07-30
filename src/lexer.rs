use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    And,
    AndAnd,
    Caret,
    Colon,
    Dot,
    DotDotDot,
    Eq,
    EqEq,
    EqEqEq,
    EqGt,
    Gt,
    GtEq,
    GtGt,
    GtGtGt,
    Lt,
    LtEq,
    LtLt,
    Minus,
    MinusEq,
    MinusMinus,
    Not,
    NotEq,
    NotEqEq,
    Percent,
    PercentEq,
    Pipe,
    PipePipe,
    Plus,
    PlusEq,
    PlusPlus,
    Question,
    QuestionDot,
    QuestionQuestion,
    Slash,
    SlashEq,
    Star,
    StarEq,
    StarStar,
    StarStarEq,
    Tilde,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Backtick,
    DollarBrace,
    Await,
    Delete,
    False,
    In,
    InstanceOf,
    New,
    Null,
    Super,
    This,
    True,
    Typeof,
    Undefined,
    Void,
    Identifier(String),
    Number(f64),
    BigInt(num_bigint::BigInt),
    String(String),
    Regex(String, Option<String>),
    Eof,
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedCharacter { position: usize, character: char },
    UnterminatedString { position: usize },
    UnterminatedRegex { position: usize },
    UnexpectedEndOfInput,
    UnicodeOutOfRange,
}

fn ws(ch: char) -> bool {
    matches!(
        ch,
        ' ' | '\t'
            | '\u{000b}'
            | '\u{000c}'
            | '\u{00a0}'
            | '\u{1680}'
            | '\u{2000}'
            | '\u{2001}'
            | '\u{2002}'
            | '\u{2003}'
            | '\u{2004}'
            | '\u{2005}'
            | '\u{2006}'
            | '\u{2007}'
            | '\u{2008}'
            | '\u{2009}'
            | '\u{200a}'
            | '\u{202f}'
            | '\u{205f}'
            | '\u{3000}'
            | '\u{feff}'
    )
}

fn lt(ch: char) -> bool {
    matches!(ch, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

fn id_start(ch: char) -> bool {
    match ch {
        '$' | '_' | ('a'..='z') | ('A'..='Z') | ('\u{80}'..='\u{10FFFF}') => true,
        _ => ch > '\u{80}' && unicode_id_start::is_id_start_unicode(ch),
    }
}

fn id_continue(ch: char) -> bool {
    match ch {
        '$' | '_' | 'A'..='Z' | 'a'..='z' | '0'..='9' => true,
        _ => ch > '\u{80}' && unicode_id_start::is_id_continue_unicode(ch),
    }
}

fn keyword(id: &str) -> Option<Token> {
    match id.len() {
        2 if id == "in" => Some(Token::In),
        3 if id == "new" => Some(Token::New),
        4 if id == "true" => Some(Token::True),
        4 if id == "null" => Some(Token::Null),
        4 if id == "this" => Some(Token::This),
        4 if id == "void" => Some(Token::Void),
        5 if id == "false" => Some(Token::False),
        5 if id == "super" => Some(Token::Super),
        5 if id == "await" => Some(Token::Await),
        6 if id == "typeof" => Some(Token::Typeof),
        6 if id == "delete" => Some(Token::Delete),
        9 if id == "undefined" => Some(Token::Undefined),
        10 if id == "instanceof" => Some(Token::InstanceOf),
        _ => None,
    }
}

fn regex_flag(ch: char) -> bool {
    matches!(ch, 'g' | 'i' | 'm' | 's' | 'u' | 'y')
}

macro_rules! greedy_match {
    ($chars:expr, $token:expr, $default:expr, $($char:expr, $next:expr),+) => {
        $token = $default;
        $(
            if let Some((_, la)) = $chars.peek() {
                if *la == $char {
                    $chars.next();
                    $token = $next;
                }
            }
        )+
    }
}

macro_rules! template_expr {
    (
$chars:expr,
$token:expr,
$tokens:expr,
$in_template:expr,
$template_expr_stack:expr,
$end_token:expr
) => {
        if $in_template[$in_template.len() - 1] {
            $tokens.push($end_token);

            let (str, dollar_brace) = lex_string(&mut $chars, '`')?;

            if dollar_brace {
                $template_expr_stack.push(true);
                $in_template.push(false);
                $tokens.push(str);
                $token = Token::DollarBrace;
            } else {
                $token = str;
            }
        } else {
            $token = $end_token;
        }
    };
}

pub fn lex(expr: &str) -> Result<Vec<Token>, LexerError> {
    if expr.trim().is_empty() {
        return Err(LexerError::UnexpectedEndOfInput);
    }

    let mut tokens = Vec::new();
    let mut chars = expr.chars().enumerate().peekable();
    let mut token: Token;
    let mut in_template = vec![false];
    let mut template_expr_stack: Vec<bool> = Vec::new();

    while let Some((i, ch)) = chars.next() {
        if ws(ch) || lt(ch) {
            continue;
        }

        if id_start(ch) {
            let mut j = i;

            while let Some((_, ch)) = chars.peek() {
                if id_continue(*ch) {
                    chars.next();
                    j += 1;
                } else {
                    break;
                }
            }

            let value = &expr[i..=j];

            token = keyword(value).unwrap_or_else(|| Token::Identifier(value.to_string()))
        } else if ch == '\'' || ch == '"' {
            token = lex_string(&mut chars, ch)?.0;
        } else if ch.is_ascii_digit() {
            token = lex_number(&mut chars, ch);
        } else {
            match ch {
                '(' => {
                    token = Token::LParen;
                }

                ')' => {
                    token = Token::RParen;
                }

                ';' => {
                    token = Token::Semicolon;
                }

                ',' => {
                    token = Token::Comma;
                }

                '.' => {
                    token = Token::Dot;

                    if let Some((_, la)) = chars.peek() {
                        if *la == '.' {
                            chars.next();
                            if let Some((_, la)) = chars.peek() {
                                if *la == '.' {
                                    chars.next();
                                    token = Token::DotDotDot;
                                }
                            }
                        } else if (*la).is_ascii_digit() {
                            token = lex_number(&mut chars, ch);
                        }
                    }
                }

                '`' => {
                    let last = in_template.len() - 1;
                    in_template[last] = !in_template[last];

                    template_expr!(
                        chars,
                        token,
                        tokens,
                        in_template,
                        template_expr_stack,
                        Token::Backtick
                    );
                }

                '[' => {
                    token = Token::LBracket;
                }

                ']' => {
                    token = Token::RBracket;
                }

                '{' => {
                    template_expr_stack.push(false);
                    token = Token::LBrace
                }

                '}' => {
                    let end_expr = template_expr_stack.pop().unwrap_or(false);

                    if end_expr {
                        in_template.pop();
                    }

                    template_expr!(
                        chars,
                        token,
                        tokens,
                        in_template,
                        template_expr_stack,
                        Token::RBrace
                    );
                }

                '?' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Question,
                        '?',
                        Token::QuestionQuestion,
                        '.',
                        Token::QuestionDot
                    );
                }

                ':' => {
                    token = Token::Colon;
                }

                '=' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Eq,
                        '=',
                        Token::EqEq,
                        '=',
                        Token::EqEqEq,
                        '>',
                        Token::EqGt
                    );
                }

                '!' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Not,
                        '=',
                        Token::NotEq,
                        '=',
                        Token::NotEqEq
                    );
                }

                '&' => {
                    greedy_match!(chars, token, Token::And, '&', Token::AndAnd);
                }

                '|' => {
                    greedy_match!(chars, token, Token::Pipe, '|', Token::PipePipe);
                }

                '<' => {
                    greedy_match!(chars, token, Token::Lt, '=', Token::LtEq, '<', Token::LtLt);
                }

                '>' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Gt,
                        '=',
                        Token::GtEq,
                        '>',
                        Token::GtGt,
                        '>',
                        Token::GtGtGt
                    );
                }

                '+' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Plus,
                        '=',
                        Token::PlusEq,
                        '+',
                        Token::PlusPlus
                    );
                }

                '-' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Minus,
                        '=',
                        Token::MinusEq,
                        '-',
                        Token::MinusMinus
                    );
                }

                '*' => {
                    greedy_match!(
                        chars,
                        token,
                        Token::Star,
                        '=',
                        Token::StarEq,
                        '*',
                        Token::StarStar,
                        '=',
                        Token::StarStarEq
                    );
                }

                '/' => {
                    token = Token::Slash;

                    if let Some((_, la)) = chars.peek() {
                        if *la == '/' {
                            // Line comments
                            chars.next();
                            for (_, ch) in chars.by_ref() {
                                if lt(ch) {
                                    break;
                                }
                            }
                            continue;
                        } else if *la == '*' {
                            // Block comments
                            chars.next();
                            while let Some((_, ch)) = chars.next() {
                                if ch == '*' {
                                    if let Some((_, ch)) = chars.peek() {
                                        if *ch == '/' {
                                            chars.next();
                                            break;
                                        }
                                    }
                                }
                            }
                            continue;
                        } else if *la == '=' {
                            chars.next();
                            token = Token::SlashEq;
                        } else if tokens.is_empty()
                            || tokens.last().map_or(false, |t| {
                                matches!(
                                    *t,
                                    Token::LParen
                                        | Token::Comma
                                        | Token::Eq
                                        | Token::Colon
                                        | Token::LBracket
                                        | Token::Not
                                        | Token::And
                                        | Token::Pipe
                                        | Token::Question
                                        | Token::LBrace
                                        | Token::RBrace
                                        | Token::Semicolon
                                )
                            })
                        {
                            let mut value = String::new();
                            let mut in_set = false;

                            loop {
                                if let Some((i, ch)) = chars.next() {
                                    if ch == '\n' || ch == '\r' {
                                        return Err(LexerError::UnterminatedRegex { position: i });
                                    }

                                    match ch {
                                        '/' if !in_set => break,
                                        '\\' => {
                                            value.push(ch);
                                            if let Some((_, ch)) = chars.next() {
                                                value.push(ch);
                                            } else {
                                                return Err(LexerError::UnexpectedEndOfInput);
                                            }
                                            continue;
                                        }
                                        '[' => {
                                            in_set = true;
                                        }
                                        ']' => {
                                            in_set = false;
                                        }
                                        _ => {}
                                    }

                                    value.push(ch);
                                } else {
                                    return Err(LexerError::UnexpectedEndOfInput);
                                }
                            }

                            let mut flags = String::new();

                            while let Some((_, ch)) = chars.peek() {
                                if regex_flag(*ch) {
                                    flags.push(*ch);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }

                            token =
                                Token::Regex(value, Option::from(flags).filter(|s| !s.is_empty()))
                        }
                    }
                }

                '%' => {
                    greedy_match!(chars, token, Token::Percent, '=', Token::PercentEq);
                }

                '~' => {
                    token = Token::Tilde;
                }

                '^' => {
                    token = Token::Caret;
                }

                _ => {
                    return Err(LexerError::UnexpectedCharacter {
                        position: i,
                        character: ch,
                    });
                }
            }
        }

        tokens.push(token);
    }

    tokens.push(Token::Eof);

    Ok(tokens)
}

macro_rules! hex_to_char {
    ($s:expr, $hex:expr) => {
        let point = u32::from_str_radix($hex, 16).unwrap();

        if (point > 0x10ffff) {
            return Err(LexerError::UnicodeOutOfRange);
        }

        $s.push(char::from_u32(u32::from_str_radix($hex, 16).unwrap()).unwrap());
    };
}

macro_rules! invalid_hex {
    ($position:expr, $char:expr) => {
        return Err(LexerError::UnexpectedCharacter {
            position: $position,
            character: $char,
        });
    };
}

macro_rules! match_hex {
    ($chars:expr, $count:expr, $hex:expr) => {
        for _ in 0..$count {
            if let Some((i, ch)) = $chars.peek() {
                if ch.is_digit(16) {
                    $hex.push(*ch);
                    $chars.next();
                } else {
                    invalid_hex!(*i, *ch);
                }
            } else {
                return Err(LexerError::UnexpectedEndOfInput);
            }
        }
    };
}

fn lex_string(
    chars: &mut Peekable<Enumerate<Chars>>,
    delim: char,
) -> Result<(Token, bool), LexerError> {
    let mut s = String::new();
    let template = delim == '`';
    let mut dollar_brace = false;

    loop {
        if let Some((i, ch)) = chars.peek() {
            if *ch == '\n' || *ch == '\r' {
                return Err(LexerError::UnterminatedString { position: *i });
            }

            if *ch == delim {
                if !template {
                    chars.next();
                }
                break;
            }

            if *ch == '\\' {
                chars.next();

                if let Some((_, ch)) = chars.peek() {
                    match *ch {
                        'n' => s.push('\n'),
                        't' => s.push('\t'),
                        'r' => s.push('\r'),
                        '0' => s.push('\0'),
                        'b' => s.push('\u{0008}'),
                        'f' => s.push('\u{000C}'),
                        'v' => s.push('\u{000B}'),

                        // Line continuation
                        '\n' | '\r' | '\u{2028}' | '\u{2029}' => {}

                        'x' => {
                            chars.next();
                            let mut hex = String::new();
                            match_hex!(chars, 2, hex);
                            hex_to_char!(s, &hex);
                            continue;
                        }

                        'u' => {
                            chars.next();
                            let mut hex = String::new();

                            if let Some((i, ch)) = chars.peek() {
                                if ch.is_ascii_hexdigit() {
                                    hex.push(*ch);
                                    chars.next();
                                    match_hex!(chars, 3, hex);
                                } else if *ch == '{' {
                                    chars.next();

                                    while let Some((i, ch)) = chars.peek() {
                                        if ch.is_ascii_hexdigit() {
                                            hex.push(*ch);
                                            chars.next();
                                        } else if *ch == '}' {
                                            chars.next();
                                            break;
                                        } else {
                                            invalid_hex!(*i, *ch);
                                        }
                                    }
                                } else {
                                    invalid_hex!(*i, *ch);
                                }
                            }

                            hex_to_char!(s, &hex);
                            continue;
                        }

                        _ => s.push(*ch),
                    }

                    chars.next();
                }
            } else if template && *ch == '$' {
                chars.next();

                if let Some((_, '{')) = chars.peek() {
                    chars.next();
                    dollar_brace = true;
                    break;
                }

                s.push('$');
            } else {
                s.push(*ch);
                chars.next();
            }
        } else {
            return Err(LexerError::UnexpectedEndOfInput);
        }
    }

    Ok((Token::String(s), dollar_brace))
}

fn lex_number(chars: &mut Peekable<Enumerate<Chars>>, ch: char) -> Token {
    if ch == '0' {
        if let Some((_, la)) = chars.peek() {
            if *la == 'b' || *la == 'B' {
                return based(chars, 2);
            }

            if *la == 'o' || *la == 'O' {
                return based(chars, 8);
            }

            if *la == 'x' || *la == 'X' {
                return based(chars, 16);
            }
        }
    }

    let mut value = String::from(ch);
    let mut seen_dot = false;
    let mut seen_e = false;
    let mut sep = ch != '.';

    while let Some((_, ch)) = chars.peek() {
        let res = {
            let ch = *ch;
            ch.is_ascii_digit()
        };
        if res {
            value.push(*ch);
            chars.next();
            sep = true;
        } else if !seen_dot && !seen_e && *ch == '.' {
            seen_dot = true;
            value.push(*ch);
            chars.next();
        } else if *ch == '+' || *ch == '-' {
            value.push(*ch);
            chars.next();
        } else if !seen_e && (*ch == 'e' || *ch == 'E') {
            seen_e = true;
            value.push(*ch);
            chars.next();
        } else if *ch == 'n' {
            chars.next();
            return Token::BigInt(value.parse().unwrap());
        } else if sep && *ch == '_' {
            chars.next();
            sep = false;
        } else {
            break;
        }
    }

    Token::Number(value.parse().unwrap())
}

fn based(chars: &mut Peekable<Enumerate<Chars>>, radix: u32) -> Token {
    chars.next();

    let mut value = String::new();
    let mut sep = false;

    while let Some((_, ch)) = chars.peek() {
        if char::is_digit(*ch, radix) {
            value.push(*ch);
            chars.next();
            sep = true;
        } else if sep && *ch == '_' {
            chars.next();
            sep = false;
        } else if *ch == 'n' {
            chars.next();
            return Token::BigInt(u64::from_str_radix(&value, radix).unwrap().into());
        } else {
            break;
        }
    }

    Token::Number(u64::from_str_radix(&value, radix).unwrap() as f64)
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::*;

    #[test]
    fn arithmetic_operators() {
        assert_debug_snapshot!(lex("+ - * / ** %"));
    }

    #[test]
    fn assignment_operators() {
        assert_debug_snapshot!(lex("= += -= *= /= %= **="));
    }

    #[test]
    fn comparison_operators() {
        assert_debug_snapshot!(lex("== === != !== < > <= >="));
    }

    #[test]
    fn ternary_operator() {
        assert_debug_snapshot!(lex("? :"));
    }

    #[test]
    fn logical_operators() {
        assert_debug_snapshot!(lex("&& ||"));
    }

    #[test]
    fn bitwise_operators() {
        assert_debug_snapshot!(lex("~ & | ^ << >> >>>"));
    }
    #[test]
    fn unary_operators() {
        assert_debug_snapshot!(lex("++ -- !"));
    }

    #[test]
    fn nullish_coalescing_operator() {
        assert_debug_snapshot!(lex("??"));
    }

    #[test]
    fn other_punctuation() {
        assert_debug_snapshot!(lex(", ; ( ) [ ] { } . ?. => ..."));
    }

    #[test]
    fn keywords() {
        assert_debug_snapshot!(lex(
            "true false null undefined typeof void in instanceof new super await this"
        ));
    }

    #[test]
    fn identifiers() {
        assert_debug_snapshot!(lex("foo $bar _baz $123 _456 $ _ abc123"));
    }

    #[test]
    fn whitespace() {
        assert_debug_snapshot!(lex("a \t\n\rb"));
    }

    #[test]
    fn strings() {
        assert_debug_snapshot!(lex(
            r#" "" '' "foo" 'bar' "foo\"bar" 'bar\'foo' "foo\nbar" 'bar\tfoo' "\x00" '\x58' "😀"
    '\u{1F600}' "\u1234" "\u{0}" "\u{0}" "#
        ));
    }

    #[test]
    fn templates() {
        assert_debug_snapshot!(lex(r#" ${ `` `$` `\${` `}` `foo` `${bar}` `abc${bar}def`
        `${ `a${b}c` }` `\u{1F600}` sql`select` `${{ $id }}`
        `${ { id: { } } }` sql``.f { a: `${a.title}!` } "#));
    }

    #[test]
    fn long_strings() {
        assert_debug_snapshot!(lex("'abc \\\n def'"));
    }

    #[test]
    fn numbers() {
        assert_debug_snapshot!(lex(
            "123 -123.456 .456 0.456 123e45 123e+45 123e-45 1. +42.4 -69e12
    .34e-5 123456789123456789 123456789123456789n -123n"
        ));
    }

    #[test]
    fn binary_numbers() {
        assert_debug_snapshot!(lex("0b0 0b1 0B1111 -0b01111 +0b10101010 0b0101n"));
    }

    #[test]
    fn octal_numbers() {
        assert_debug_snapshot!(lex("0o0 0o1 0O7777 -0o7777 +0o7777 0o123n"));
    }

    #[test]
    fn hexadecimal_numbers() {
        assert_debug_snapshot!(lex("0x0 0x1 0XFfFF -0xFFFF +0xAaAA 0x123n"));
    }

    #[test]
    fn separators() {
        assert_debug_snapshot!(lex("0b1_0_0 1_000_000 1.0e1_1 1_2.3_4 .55_5 0o7_7 0xf_f"));
    }

    #[test]
    fn line_comments() {
        assert_debug_snapshot!(lex("// foo\n42 // bar\r// baz qux"));
    }

    #[test]
    fn block_comments() {
        assert_debug_snapshot!(lex("1 /* foo */ 2 /* bar\n\nabc */ 3 /*** baz qux **/
                            4 /*\\*/ 5 /*/*/ 6 /**/ 7 /* /*  \\*\\/ */ 8"));
    }

    #[test]
    fn regexes() {
        assert_debug_snapshot!(lex(r#" /foo/, /o\/o/, /fo\[[/a\]]a\/]o\\/, /abc/gimsuy "#));
    }

    #[test]
    fn lexer_errors() {
        assert!(matches!(lex(""), Err(LexerError::UnexpectedEndOfInput)));
        assert!(matches!(lex(" "), Err(LexerError::UnexpectedEndOfInput)));
        assert!(matches!(lex("\""), Err(LexerError::UnexpectedEndOfInput)));
        assert!(matches!(lex("'"), Err(LexerError::UnexpectedEndOfInput)));
        assert!(matches!(
            lex("'\n'"),
            Err(LexerError::UnterminatedString { position: 1 })
        ));
        assert!(matches!(
            lex("'\r'"),
            Err(LexerError::UnterminatedString { position: 1 })
        ));
        assert!(matches!(
            lex("'\\' 12"),
            Err(LexerError::UnexpectedEndOfInput)
        ));
        assert!(matches!(
            lex("`${` 12"),
            Err(LexerError::UnexpectedEndOfInput)
        ));
        assert!(matches!(
            lex("'\\x'"),
            Err(LexerError::UnexpectedCharacter {
                position: 3,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\x0'"),
            Err(LexerError::UnexpectedCharacter {
                position: 4,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u0'"),
            Err(LexerError::UnexpectedCharacter {
                position: 4,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u01'"),
            Err(LexerError::UnexpectedCharacter {
                position: 5,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u012'"),
            Err(LexerError::UnexpectedCharacter {
                position: 6,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u012z'"),
            Err(LexerError::UnexpectedCharacter {
                position: 6,
                character: 'z'
            })
        ));
        assert!(matches!(
            lex("'\\u{1F600A}'"),
            Err(LexerError::UnicodeOutOfRange)
        ));
        assert!(matches!(
            lex("'\\u{1F600A'"),
            Err(LexerError::UnexpectedCharacter {
                position: 10,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u{1F600AAAAA'"),
            Err(LexerError::UnexpectedCharacter {
                position: 14,
                character: '\''
            })
        ));
        assert!(matches!(
            lex("'\\u{1FW}'"),
            Err(LexerError::UnexpectedCharacter {
                position: 6,
                character: 'W'
            })
        ));
        assert!(matches!(lex("/abc"), Err(LexerError::UnexpectedEndOfInput)));
    }
}
