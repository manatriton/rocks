use rocks::{
    ast::{debug::AstFormatter, Expr},
    token::{Literal, Token, TokenKind},
};

fn main() {
    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            Token {
                kind: TokenKind::Minus,
                lexeme: "-",
                literal: Literal::None,
                line: 1,
            },
            Box::new(Expr::Literal(Literal::Number(123.0))),
        )),
        Token {
            kind: TokenKind::Star,
            lexeme: "*",
            literal: Literal::None,
            line: 1,
        },
        Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
            45.67,
        ))))),
    );

    let mut f = AstFormatter;
    println!("{}", f.format(&expr));
}
