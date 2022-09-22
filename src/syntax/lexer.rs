use chumsky::prelude::*;
use derive_more::Display;

use super::Spanned;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Token {
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

    #[display(fmt = "'{_0}")]
    Atom(String),
}

impl Token {
    pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
        // Based on <https://github.com/zesterer/chumsky/blob/a0a67e47fe6341c2f6637dac3c09c38080803070/examples/nano_rust.rs>.

        let num = text::int(10)
            .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
            .collect()
            .map(Token::Num);

        let str_ = just('"')
            .ignore_then(filter(|&c| c != '"').repeated())
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
            just("**").to(Token::Star2),
            just('*').to(Token::Star),
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

        let ident = text::ident().map(|s: String| match s.as_str() {
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,
            "struct" => Token::Struct,
            "if" => Token::If,
            "else" => Token::Else,
            "let" => Token::Let,
            "fun" => Token::Fun,
            _ => Token::Ident(s),
        });

        let atom = just('\'').ignore_then(text::ident().map(Token::Atom));

        let comment = choice((
            just("//").then(take_until(text::newline())).ignored(),
            just("/*").then(take_until(just("*/"))).ignored(),
        ))
        .padded();

        choice((num, str_, atom, op, ident))
            .recover_with(skip_then_retry_until([]))
            .map_with_span(Spanned)
            .padded_by(comment.repeated())
            .padded()
            .repeated()
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use super::*;

    fn assert_lex(expected: &str, src: &str) {
        assert_eq!(
            Ok(expected),
            Token::lexer()
                .parse(src)
                .map(|tks| tks.iter().map(|Spanned(tk, _)| tk).join(" "))
                .as_deref(),
        );
    }

    fn assert_lex_refl(expected: &str) {
        assert_lex(expected, expected);
    }

    fn assert_lex_join(expected: &str) {
        assert_lex(expected, &expected.split_whitespace().join(""));
    }

    #[test]
    fn basic_refl() {
        [
            "42",
            "BladeRunner2049",
            "'__struct__",
            r#""You're gonna carry that weight.""#,
        ]
        .into_iter()
        .for_each(assert_lex_refl);
    }

    #[test]
    fn basic_join() {
        [
            "( 1 + 2 ) - - 3 >= 4 * 5 ** 6 / 7 || ! true && false == null",
            r#"struct { 'foo : 2 , bar : struct { baz : 3 } } [ "bar" ] . baz == 3"#,
        ]
        .into_iter()
        .for_each(assert_lex_join);
    }

    #[test]
    fn struct_() {
        assert_lex("struct { }", "struct{}");
        assert_lex(
            r#"struct { baz : 3 } [ "baz" ] == struct { baz : 3 } . baz == 3"#,
            indoc! {r#"
                struct{ baz: 3 }["baz"]
                    == struct{ baz: 3 }.baz // String literal field name sugar
                    == 3
            "#},
        );
    }

    #[test]
    fn let_in() {
        assert_lex(
            r#"let foo = "bar" ; struct { foo , baz : 3 , }"#,
            indoc! {r#"
                let foo = "bar"; // `in` keyword à la ReasonML
                struct{
                    foo, // Named field punning
                    baz: 3, // Optional trailing comma
                }
            "#},
        );
    }

    #[test]
    fn fun() {
        assert_lex(
            r#"let abs = fun ( a ) { if ( a >= 0 ) a else - a }"#,
            indoc! {r#"
                let abs = fun(a) { // Closure lambda
                    if (a >= 0) a else -a // Expression-based return
                }
            "#},
        );
        assert_lex_refl("fun abs1 ( a ) { if ( a >= 0 ) a else - a }");
    }
}
