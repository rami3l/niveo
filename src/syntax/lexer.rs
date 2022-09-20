use chumsky::prelude::*;
use derive_more::Display;

use super::Span;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default, Display)]
enum Token {
    #[default]
    #[display(fmt = "EOF")]
    Eof,

    #[display(fmt = "true")]
    True,

    #[display(fmt = "false")]
    False,

    #[display(fmt = "null")]
    Null,

    #[display(fmt = "struct")]
    Struct,

    #[display(fmt = "if")]
    If,

    #[display(fmt = "else")]
    Else,

    #[display(fmt = "let")]
    Let,

    #[display(fmt = "fun")]
    Fun,

    #[display(fmt = "(")]
    LParen,

    #[display(fmt = ")")]
    RParen,

    #[display(fmt = "[")]
    LBrack,

    #[display(fmt = "]")]
    RBrack,

    #[display(fmt = "{{")]
    LBrace,

    #[display(fmt = "}}")]
    RBrace,

    #[display(fmt = ",")]
    Comma,

    #[display(fmt = ".")]
    Dot,

    #[display(fmt = ";")]
    Semi,

    #[display(fmt = ":")]
    Colon,

    #[display(fmt = "+")]
    Plus,

    #[display(fmt = "-")]
    Minus,

    #[display(fmt = "*")]
    Star,

    #[display(fmt = "**")]
    Star2,

    #[display(fmt = "/")]
    Slash,

    #[display(fmt = "<")]
    Le,

    #[display(fmt = "<=")]
    LeEq,

    #[display(fmt = "=")]
    Eq,

    #[display(fmt = "==")]
    Eq2,

    #[display(fmt = ">")]
    Gt,

    #[display(fmt = ">=")]
    GtEq,

    #[display(fmt = "!")]
    Bang,

    #[display(fmt = "!=")]
    BangEq,

    #[display(fmt = "&&")]
    Amp2,

    #[display(fmt = "||")]
    Pipe2,

    Num(String),

    #[display(fmt = r#""{_0}""#)]
    Str(String),

    Ident(String),

    #[display(fmt = ":{_0}")]
    Atom(String),
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // Based on <https://github.com/zesterer/chumsky/blob/a0a67e47fe6341c2f6637dac3c09c38080803070/examples/nano_rust.rs>.

    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect()
        .map(Token::Num);

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect()
        .map(Token::Str);

    let op = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('[').to(Token::LBrack),
        just(']').to(Token::RBrack),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(',').to(Token::Comma),
        just('.').to(Token::Dot),
        just(';').to(Token::Semi),
        just(':').to(Token::Colon),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just("**").to(Token::Star2),
        just('/').to(Token::Slash),
        just("<=").to(Token::LeEq),
        just('<').to(Token::Le),
        just("==").to(Token::Eq2),
        just('=').to(Token::Eq),
        just(">=").to(Token::GtEq),
        just('>').to(Token::Gt),
        just("!=").to(Token::BangEq),
        just('!').to(Token::Bang),
        just("&&").to(Token::Amp2),
        just("||").to(Token::Pipe2),
    ));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "true" => Token::True,
        "false" => Token::False,
        "null" => Token::Null,
        "struct" => Token::Struct,
        "if" => Token::If,
        "else" => Token::Else,
        "let" => Token::Let,
        "fun" => Token::Fun,
        _ => Token::Ident(ident),
    });

    let comment = choice((
        just("//").then(take_until(text::newline())).ignored(),
        just("/*").then(take_until(just("*/"))).ignored(),
    ))
    .padded();

    choice((num, str_, op, ident))
        .recover_with(skip_then_retry_until([]))
        .map_with_span(|tk, span| (tk, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
