use chumsky::prelude::*;
use derive_more::Display;
use im::{HashMap, Vector};
use itertools::Itertools;
use tap::prelude::*;

use super::{Spanned, Token};

pub fn parser() -> impl Parser<Token, Spanned<Prog>, Error = Simple<Token>> {
    let mut block = Recursive::declare();
    let mut decl = Recursive::declare();

    let lit = select! {
        Token::Null => Lit::Null,
        Token::True => Lit::Bool(true),
        Token::False => Lit::Bool(false),
        Token::Num(s) => Lit::Num(s),
        Token::Str(s) => Lit::Str(s),
        Token::Atom(s) => Lit::Atom(s),
    }
    .map_with_span(Spanned);

    let mut paren = Recursive::declare();
    let mut expr = Recursive::declare();

    paren.define(
        just(Token::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::RParen)),
    );
    expr.define(recursive(|expr| {
        #[allow(clippy::similar_names)]
        let list = just(Token::LBrack)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::RBrack))
            .map_with_span(|exprs, span| Expr::List(exprs.into()).pipe(|it| Spanned(it, span)));
        let struct_kv = choice((
            select! { Token::Str(s) => s, Token::Ident(s) => s }
                .then_ignore(just(Token::Colon))
                .then(expr),
            select! { i@Token::Ident(s) => (s.clone(), Expr::Var(i)) }
                .map_with_span(|(k, v), span| (k, Spanned(v, span))),
        ));
        let struct_ = just([Token::Struct, Token::LBrace])
            .ignore_then(struct_kv.separated_by(just(Token::Comma)).allow_trailing())
            .then_ignore(just(Token::RBrace))
            .map_with_span(|kvs, span| Expr::Struct(kvs.into()).pipe(|it| Spanned(it, span)));
        let if_else = todo!();
        let lambda = just([Token::Fun, Token::LParen])
            .ignore_then(
                // TODO: Make the following `select!` call a new macro.
                select! {Token::Ident(s) => s}
                    .separated_by(just(Token::Comma))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::RParen))
            .then(block.clone())
            .map_with_span(|(params, body), span| {
                Expr::Lambda {
                    params: params.into(),
                    body,
                }
                .pipe(|it| Spanned(it, span))
            });
        let prim = choice((list, struct_, if_else, lambda, lit));
        let call = todo!();
        let unary = todo!();
        let expon = todo!();
        let factor = todo!();
        let term = todo!();
        let comp = todo!();
        let equal = todo!();
        let logic_and = todo!();
        let logic_or = todo!();
        logic_or
    }));

    decl.define({
        let let_ = just(Token::Let)
            .ignore_then(select! {Token::Ident(s) => s})
            .then_ignore(just(Token::Eq))
            .then(expr)
            .then_ignore(just(Token::Semi))
            .map_with_span(|(ident, expr), span| {
                Decl::Let { ident, expr }.pipe(|it| Spanned(it, span))
            });

        let fun = just(Token::Fun)
            .ignore_then(select! {Token::Ident(s) => s})
            .then_ignore(just(Token::LParen))
            .then(
                // TODO: Make the following `select!` call a new macro.
                select! {Token::Ident(s) => s}
                    .separated_by(just(Token::Comma))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::RParen))
            .then(block.clone())
            .map_with_span(|((ident, params), body): ((_, Vec<_>), _), span| {
                Decl::Fun {
                    ident,
                    params: params.into(),
                    body,
                }
                .pipe(|it| Spanned(it, span))
            });

        choice((let_, fun))
    });

    block.define(decl.clone().repeated().then(expr.clone()).map_with_span(
        |(decls, val): (Vec<_>, _), span| {
            Block {
                decls: decls.into(),
                val,
            }
            .pipe(|it| Spanned(it, span))
            .pipe(Box::new)
        },
    ));

    decl.repeated()
        .then(expr)
        .map_with_span(|(decls, val): (Vec<_>, _), span| {
            Prog {
                decls: decls.into(),
                val,
            }
            .pipe(|it| Spanned(it, span))
        })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[display(fmt = "{}{}", r#"format_args!("{} ", decls.iter().format(""))"#, val)]
pub struct Prog {
    decls: Vector<Spanned<Decl>>,
    val: Spanned<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Decl {
    #[display(fmt = "(let {ident} {expr})")]
    Let { ident: String, expr: Spanned<Expr> },
    #[display(fmt = "(fun {} ({}) {})", ident, r#"params.iter().join(" ")"#, body)]
    Fun {
        ident: String,
        params: Vector<String>,
        body: Box<Spanned<Block>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[display(fmt = "(begin {} {})", r#"decls.iter().join(" ")"#, val)]
pub struct Block {
    decls: Vector<Spanned<Decl>>,
    val: Spanned<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Lit {
    #[display(fmt = "null")]
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Atom(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Expr {
    #[display(fmt = "({op} {rhs})")]
    Unary {
        op: Token,
        rhs: Box<Spanned<Expr>>,
    },
    #[display(fmt = "({lhs} {op} {rhs})")]
    Binary {
        lhs: Box<Spanned<Expr>>,
        op: Token,
        rhs: Box<Spanned<Expr>>,
    },
    #[display(
        fmt = "({}{})",
        callee,
        r#"format_args!(" {}", args.iter().format(""))"#
    )]
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vector<Spanned<Expr>>,
    },
    #[display(fmt = "(@ {this} {idx})")]
    Index {
        this: Box<Spanned<Expr>>,
        idx: Box<Spanned<Expr>>,
    },
    Paren(Box<Spanned<Expr>>),
    Block(Box<Spanned<Block>>),
    #[display(fmt = "(list{})", r#"format_args!(" {}", _0.iter().format(""))"#)]
    List(Vector<Spanned<Expr>>),
    #[display(
        fmt = "(struct{})",
        r#"_0.iter().map(|(k, v)| format!(" {k} {v}")).join("")"#
    )]
    Struct(HashMap<String, Spanned<Expr>>),
    #[display(fmt = "(if {cond} {then_} {else_})")]
    IfElse {
        cond: Box<Spanned<Expr>>,
        then_: Box<Spanned<Expr>>,
        else_: Box<Spanned<Expr>>,
    },
    #[display(fmt = "(lambda ({}) {})", r#"params.iter().join(" ")"#, body)]
    Lambda {
        params: Vector<String>,
        body: Box<Spanned<Block>>,
    },
    Lit(Lit),
    Var(Token),
}
