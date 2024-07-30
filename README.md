# fastexpr.rs

A fast, tiny, minimal dependency JavaScript expression parser, written in Rust.

## Features

- Fast. Hand-coded lexer and top-down operator precedence parser.
- Small. Around ~1000 LOC.
- Compatible. Parses all valid JavaScript expressions. Produces an `esprima` style AST.

Usage:

```rust
let result = parse("(s) => `hello from ${s}!`");

match result {
    Ok(expr) => {
        println!("{:#?}", expr);
    }
    Err(err) => {
        println!("{:#?}", err);
    }
}
```

which produces:

```
ArrowFunction {
    params: [
        Identifier {
            token: Identifier(
                "s",
            ),
        },
    ],
    body: TemplateLiteral {
        quasis: [
            TemplateString {
                token: String(
                    "hello from ",
                ),
                tail: false,
            },
            TemplateString {
                token: String(
                    "!",
                ),
                tail: true,
            },
        ],
        expressions: [
            Identifier {
                token: Identifier(
                    "s",
                ),
            },
        ],
    },
}
```
