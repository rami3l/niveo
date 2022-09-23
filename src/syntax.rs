pub mod lexer;
pub mod parser;

use std::ops::Range;

use delegate::delegate;
use derive_more::Display;

pub use self::lexer::Token;

pub type Span = Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[display(bound = "T: std::fmt::Display")]
#[display(fmt = "{_0}")]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned(f(self.0), self.1)
    }
}

impl<T: Clone> chumsky::Span for Spanned<T> {
    type Context = T;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self(context, range)
    }

    delegate! {
        to (self.0, self.1) {
            fn context(&self) -> Self::Context;
            fn start(&self) -> Self::Offset;
            fn end(&self) -> Self::Offset;
        }
    }
}
