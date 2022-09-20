#![warn(
    clippy::pedantic,
    missing_copy_implementations,
    missing_debug_implementations,
    // missing_docs,
    rustdoc::broken_intra_doc_links,
    trivial_numeric_casts,
    unused_allocation
)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

mod syntax;

#[must_use]
pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
