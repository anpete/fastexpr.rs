use crate::lexer::Token::*;
use crate::lexer::{lex, LexerError, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal {
        token: Token,
    },
    Identifier {
        token: Token,
    },
    Array {
        elements: Vec<Expr>,
    },
    ArrowFunction {
        params: Vec<Expr>,
        body: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Comma {
        expressions: Vec<Expr>,
    },
    Conditional {
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>,
    },
    Object {
        properties: Vec<PropertyExpr>,
    },
    Spread {
        expr: Box<Expr>,
    },
    Member {
        computed: bool,
        object: Box<Expr>,
        property: Box<Expr>,
        optional: bool,
    },
    New {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    TemplateLiteral {
        quasis: Vec<TemplateString>,
        expressions: Vec<Expr>,
    },
    TaggedTemplate {
        tag: Box<Expr>,
        quasi: Box<Expr>,
    },
    Unary {
        operator: Token,
        argument: Box<Expr>,
        prefix: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateString {
    token: Token,
    tail: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyExpr {
    Property {
        computed: bool,
        key: Expr,
        shorthand: bool,
        value: Option<Expr>,
    },
    Spread {
        expr: Expr,
    },
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    ExpectedToken(Token),
    LexerError(LexerError),
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

macro_rules! cur {
    ($tokens:expr, $pos:expr) => {
        $tokens.get(*$pos).unwrap()
    };
}

macro_rules! la {
    ($tokens:expr, $pos:expr) => {
        $tokens.get(*$pos + 1).unwrap()
    };
}

macro_rules! next {
    ($tokens:expr, $pos:expr) => {{
        let token = cur!($tokens, $pos);
        *$pos += 1;
        token
    }};
}

macro_rules! check {
    ($tokens:expr, $pos:expr, $token:expr) => {
        cur!($tokens, $pos) == &$token
    };
}

macro_rules! consume {
    ($tokens:expr, $pos:expr, $expected:expr) => {{
        let token = next!($tokens, $pos);
        if token != &$expected {
            return Err(ParserError::ExpectedToken($expected));
        }
        token
    }};
}

macro_rules! match_token {
    ($tokens:expr, $pos:expr, $expected:expr) => {
        if check!($tokens, $pos, $expected) {
            next!($tokens, $pos);
            true
        } else {
            false
        }
    };
}

pub fn parse(expression: &str) -> Result<Expr, ParserError> {
    let tokens = lex(expression)?;
    let mut pos: usize = 0;
    let expr = parse_expr(&tokens, &mut pos, 0)?;
    let token = cur!(tokens, &pos);

    if token != &Eof {
        return Err(ParserError::UnexpectedToken(token.clone()));
    }

    Ok(expr)
}

fn parse_expr(tokens: &[Token], pos: &mut usize, prec: i32) -> Result<Expr, ParserError> {
    let mut token = next!(tokens, pos);
    let mut expr = parse_prefix(tokens, pos, token)?;

    while prec < precedence(cur!(tokens, pos)) {
        token = next!(tokens, pos);
        expr = parse_infix(tokens, pos, token, expr)?;
    }

    Ok(expr)
}

fn parse_prefix(tokens: &[Token], pos: &mut usize, token: &Token) -> Result<Expr, ParserError> {
    match token {
        Identifier(_) => Ok(Expr::Identifier {
            token: token.clone(),
        }),

        True | False | Null | Undefined | This | Super | Number(_) | String(_) | Regex(_, _) => {
            Ok(Expr::Literal {
                token: token.clone(),
            })
        }

        LParen => {
            if cur!(tokens, pos) == &RParen {
                next!(tokens, pos);
                return Ok(Expr::Comma {
                    expressions: Vec::new(),
                });
            }

            let expr = parse_expr(tokens, pos, 0)?;
            consume!(tokens, pos, RParen);
            Ok(expr)
        }

        Backtick => {
            let mut quasis: Vec<TemplateString> = Vec::new();
            let mut expressions: Vec<Expr> = Vec::new();

            while !check!(tokens, pos, Backtick) {
                if match_token!(tokens, pos, DollarBrace) {
                    expressions.push(parse_expr(tokens, pos, 1)?);
                    consume!(tokens, pos, RBrace);
                } else {
                    let token = next!(tokens, pos);
                    match token {
                        String(_) => quasis.push(TemplateString {
                            token: (*token).clone(),
                            tail: false,
                        }),
                        _ => return Err(ParserError::ExpectedToken(String("".to_string()))),
                    }
                }
            }

            quasis.last_mut().unwrap().tail = true;

            consume!(tokens, pos, Backtick);

            Ok(Expr::TemplateLiteral {
                quasis,
                expressions,
            })
        }

        LBrace => {
            let mut properties: Vec<PropertyExpr> = Vec::new();

            while !check!(tokens, pos, RBrace) {
                let expr = parse_expr(tokens, pos, 1)?;

                if check!(tokens, pos, Colon) {
                    let key = expr;
                    next!(tokens, pos);
                    let value = parse_expr(tokens, pos, 1)?;

                    properties.push(PropertyExpr::Property {
                        computed: matches!(&key, Expr::Array { .. }),
                        key,
                        shorthand: false,
                        value: Some(value),
                    });
                } else {
                    match expr {
                        Expr::Identifier { .. } => {
                            properties.push(PropertyExpr::Property {
                                computed: false,
                                key: expr,
                                shorthand: true,
                                value: None,
                            });
                        }
                        Expr::Spread { expr } => {
                            properties.push(PropertyExpr::Spread { expr: *expr });
                        }
                        _ => {
                            return Err(ParserError::ExpectedToken(Colon));
                        }
                    }
                }

                if check!(tokens, pos, Comma) {
                    next!(tokens, pos);
                }
            }

            consume!(tokens, pos, RBrace);

            Ok(Expr::Object { properties })
        }

        LBracket => {
            let mut elements: Vec<Expr> = Vec::new();

            while !check!(tokens, pos, RBracket) {
                elements.push(parse_expr(tokens, pos, 1)?);

                if check!(tokens, pos, Comma) {
                    next!(tokens, pos);
                }
            }

            consume!(tokens, pos, RBracket);

            Ok(Expr::Array { elements })
        }

        New => match parse_expr(tokens, pos, 16)? {
            Expr::Call { callee, arguments } => Ok(Expr::New { callee, arguments }),
            _ => Err(ParserError::ExpectedToken(LParen)),
        },

        DotDotDot => Ok(Expr::Spread {
            expr: parse_expr(tokens, pos, 1)?.into(),
        }),

        PlusPlus | MinusMinus | Tilde | Not | Minus | Await | Plus | Typeof | Delete | Void => {
            Ok(Expr::Unary {
                operator: token.clone(),
                argument: parse_expr(tokens, pos, 14)?.into(),
                prefix: true,
            })
        }

        _ => Err(ParserError::UnexpectedToken(token.clone())),
    }
}

fn parse_infix(
    tokens: &[Token],
    pos: &mut usize,
    token: &Token,
    expr: Expr,
) -> Result<Expr, ParserError> {
    match token {
        LParen => {
            let mut arguments: Vec<Expr> = Vec::new();

            while !check!(tokens, pos, RParen) {
                arguments.push(parse_expr(tokens, pos, 1)?);

                if check!(tokens, pos, Comma) {
                    next!(tokens, pos);
                }
            }

            consume!(tokens, pos, RParen);

            Ok(Expr::Call {
                callee: expr.into(),
                arguments,
            })
        }

        Dot => Ok(Expr::Member {
            computed: false,
            object: expr.into(),
            property: parse_expr(tokens, pos, 17)?.into(),
            optional: false,
        }),

        Backtick => Ok(Expr::TaggedTemplate {
            tag: expr.into(),
            quasi: parse_prefix(tokens, pos, token)?.into(),
        }),

        Comma => {
            if check!(tokens, pos, RParen) && la!(tokens, pos) == &EqGt {
                return Ok(expr);
            }

            let mut expressions = vec![expr];

            loop {
                expressions.push(parse_expr(tokens, pos, 1)?);

                if !match_token!(tokens, pos, Comma) {
                    break;
                }
            }

            Ok(Expr::Comma { expressions })
        }

        Question => {
            let consequent = parse_expr(tokens, pos, 1)?;
            consume!(tokens, pos, Colon);
            let alternate = parse_expr(tokens, pos, 1)?;

            Ok(Expr::Conditional {
                test: expr.into(),
                consequent: consequent.into(),
                alternate: alternate.into(),
            })
        }

        EqGt => Ok(Expr::ArrowFunction {
            params: match expr {
                Expr::Comma { expressions } => expressions,
                _ => vec![expr],
            },
            body: parse_expr(tokens, pos, 1)?.into(),
        }),

        StarStar => Ok(Expr::Binary {
            left: expr.into(),
            operator: token.clone(),
            right: parse_expr(tokens, pos, 12)?.into(),
        }),

        PlusPlus | MinusMinus => Ok(Expr::Unary {
            operator: token.clone(),
            argument: expr.into(),
            prefix: false,
        }),

        Plus | Minus | Star | Slash | Percent | EqEq | EqEqEq | NotEq | NotEqEq | Lt | LtEq
        | Gt | GtEq | In | InstanceOf | LtLt | GtGt | GtGtGt | And | AndAnd | Pipe | PipePipe
        | Caret | QuestionQuestion => Ok(Expr::Binary {
            left: expr.into(),
            operator: token.clone(),
            right: parse_expr(tokens, pos, precedence(token))?.into(),
        }),

        QuestionDot => Ok(Expr::Member {
            computed: false,
            object: expr.into(),
            property: parse_expr(tokens, pos, 17)?.into(),
            optional: true,
        }),

        LBracket => {
            let property = parse_expr(tokens, pos, 0)?;

            consume!(tokens, pos, RBracket);

            Ok(Expr::Member {
                computed: true,
                object: expr.into(),
                property: property.into(),
                optional: false,
            })
        }

        _ => Err(ParserError::UnexpectedToken(token.clone())),
    }
}

fn precedence(token: &Token) -> i32 {
    match token {
        Comma => 1,
        DotDotDot | EqGt | Question => 2,
        PipePipe | QuestionQuestion => 3,
        AndAnd => 4,
        Pipe => 5,
        Caret => 6,
        And => 7,
        EqEq | EqEqEq | NotEq | NotEqEq => 8,
        Lt | LtEq | Gt | GtEq | In | InstanceOf => 9,
        LtLt | GtGt | GtGtGt => 10,
        Plus | Minus => 11,
        Star | Slash | Percent => 12,
        StarStar => 13,
        PlusPlus | MinusMinus => 15,
        Dot | QuestionDot | LBracket | Backtick | LParen => 17,
        _ => -1,
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::*;

    macro_rules! parse {
        ($($expr:expr),*) => {
            assert_debug_snapshot!(
                vec![$(parse($expr).unwrap()),*]
            )
        };
    }

    macro_rules! precedence {
        ($expr:expr, $expected:expr) => {
            assert_eq!(parse($expr).unwrap(), parse($expected).unwrap())
        };
    }

    #[test]
    fn primaries() {
        parse!(
            "false",
            "true",
            "null",
            "undefined",
            "this",
            "super",
            "123",
            "'abc'",
            "/foo/",
            "(123)",
            "[123]",
            "[123,]",
            "[123, 456]",
            "[]",
            "[_, al]",
            "{}",
            "{foo}",
            "{foo, bar}",
            "{foo: 123}",
            "{1: \"foo\"}",
            "{\"foo:bar\": 42}",
            "{...foo}",
            "{foo: {bar} }",
            "{ album: a.title, artist: ar.name, track: t.name }",
            "``",
            "`abc`",
            "`${foo}`",
            "`$`",
            "`\\${`",
            "`${ `a${b}c` }`",
            "tag`foo`",
            "sql`select * from table`",
            "`${{ $id }}`",
            "`${ { id: { } } }`",
            "sql``.f",
            "{ a: `${a.title}!` }"
        );
    }

    #[test]
    fn computed_key() {
        parse!("{[abc]: 123}");
    }

    #[test]
    fn members() {
        parse!(
            "a[10]",
            "this.foo",
            "foo.bar.baz",
            "foo?.bar?.baz",
            "foo[bar]",
            "foo[bar]?.baz",
            "foo[(a, b)]"
        );
    }

    #[test]
    fn comma() {
        parse!("a, 12, b", "(a, 12)", "foo[a, b]");
    }

    #[test]
    fn call() {
        parse!(
            "foo()",
            "foo.bar()",
            "foo.bar()[baz?.qux()]",
            "foo(12, abc)"
        );
    }

    #[test]
    fn spread() {
        parse!("...foo", "...foo.bar", "foo(...bar)", "foo(...bar, ...baz)");
    }

    #[test]
    fn arrow() {
        parse!(
            "(foo,baz) => bar",
            "foo => bar",
            "() => a+b",
            "(a, [b, _]) => a+b",
            "() => (a, b) => a+b",
            "([_, al]) => al.title",
            "(al,) => al.title"
        );
    }

    #[test]
    fn ternary() {
        parse!("foo ? bar : baz", "foo ? bar : baz ? qux : quux");
        precedence!(
            "foo ? bar : baz ? qux : quux",
            "foo ? bar : (baz ? qux : quux)"
        );
    }

    #[test]
    fn nullish() {
        parse!("foo ?? bar", "foo ?? bar ?? baz");
        precedence!("foo ?? bar ?? baz", "(foo ?? bar) ?? baz");
    }

    #[test]
    fn logical_or() {
        parse!("123 || 456");
        precedence!("123 || 456 || 789", "(123 || 456) || 789");
        precedence!("123 || 456 && 789", "123 || (456 && 789)");
    }

    #[test]
    fn logical_and() {
        parse!("123 && 456", "foo || bar && baz");
        precedence!("foo && bar && baz", "(foo && bar) && baz");
        precedence!("foo && bar | baz", "foo && (bar | baz)");
    }

    #[test]
    fn bitwise_or() {
        parse!("123 | 456");
        precedence!("123 | 456 | 789", "(123 | 456) | 789");
        precedence!("123 | 456 ^ 789", "123 | (456 ^ 789)");
    }

    #[test]
    fn bitwise_xor() {
        parse!("123 ^ 456");
        precedence!("123 ^ 456 ^ 789", "(123 ^ 456) ^ 789");
        precedence!("123 ^ 456 & 789", "123 ^ (456 & 789)");
    }

    #[test]
    fn bitwise_and() {
        parse!("123 & 456");
        precedence!("123 & 456 & 789", "(123 & 456) & 789");
        precedence!("123 & 456 === 789", "123 & (456 === 789)");
    }

    #[test]
    fn equality() {
        parse!("123 == bar[12]", "123 === 456", "123 != 456", "123 !== 456");
        precedence!("123 == 456 === 789", "(123 == 456) === 789");
        precedence!("123 == 456 < 789", "123 == (456 < 789)");
    }

    #[test]
    fn relational() {
        parse!("123 < foo.bar", "123 > 456", "123 <= \"abc\"", "bar >= 456");
        precedence!("123 < 456 > 789", "(123 < 456) > 789");
        precedence!("123 < 456 << foo", "123 < (456 << foo)");
    }

    #[test]
    fn bitwise_shift() {
        parse!("123 << 456", "123 >> 456", "123 >>> 456");
    }

    #[test]
    fn additive() {
        parse!("123 + 456", "123 - 456 * 789");
        precedence!("123 + 456 + 789", "(123 + 456) + 789");
        precedence!("123 + 456 * 789", "123 + (456 * 789)");
    }

    #[test]
    fn multiplicative() {
        parse!("123 / 456 * 789", "123 % (456 / abc.def)");
        precedence!("123 * 456 / 789", "(123 * 456) / 789");
        precedence!("123 * 456 ** 789", "123 * (456 ** 789)");
    }

    #[test]
    fn exponentiation() {
        parse!("123 ** 456 ** 789");
        precedence!("123 ** 456 ** 789", "123 ** (456 ** 789)");
        precedence!("123 ** -456", "123 ** (-456)");
    }

    #[test]
    fn prefix() {
        parse!("+123", "+abc", "-abc", "!abc", "-(foo)", "~foo");
    }

    #[test]
    fn prefix_increment() {
        parse!(
            "++foo",
            "--foo",
            "void 0",
            "typeof foo",
            "await obj[key]",
            "delete obj[key]"
        );
    }

    #[test]
    fn postfix() {
        parse!("foo++", "abc--");
    }

    #[test]
    fn new() {
        parse!("new Foo()", "new Foo(123)", "new x(y)");
    }

    #[test]
    fn trailing_commas() {
        parse!("foo(12,)", "{foo,}", "(foo,)=>123");
    }

    #[test]
    fn parser_errors() {
        assert!(matches!(parse("("), Err(ParserError::UnexpectedToken(Eof))));
        assert!(matches!(
            parse("new Foo"),
            Err(ParserError::ExpectedToken(LParen))
        ));
        assert!(matches!(
            parse("new Foo(12"),
            Err(ParserError::UnexpectedToken(Eof))
        ));
        assert!(matches!(
            parse("foo."),
            Err(ParserError::UnexpectedToken(Eof))
        ));
        assert!(matches!(
            parse("foo[12"),
            Err(ParserError::ExpectedToken(RBracket))
        ));
        assert!(matches!(
            parse("(12"),
            Err(ParserError::ExpectedToken(RParen))
        ));
        assert!(matches!(
            parse("12,"),
            Err(ParserError::UnexpectedToken(Eof))
        ));
        assert!(matches!(
            parse("(12,"),
            Err(ParserError::UnexpectedToken(Eof))
        ));
        assert!(matches!(
            parse("foo(12"),
            Err(ParserError::UnexpectedToken(Eof))
        ));
        assert!(matches!(
            parse("[,]"),
            Err(ParserError::UnexpectedToken(Comma))
        ));
        assert!(matches!(
            parse("foo(,)"),
            Err(ParserError::UnexpectedToken(Comma))
        ));
        assert!(matches!(
            parse("{12}"),
            Err(ParserError::ExpectedToken(Colon))
        ));
    }
}
