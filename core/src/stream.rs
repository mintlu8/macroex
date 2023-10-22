use proc_macro2::Delimiter;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

use crate::Error;

use crate::Extractor;

/// An extractable stream, like [`TokenStream::IntoIter`](proc_macro2::token_stream::IntoIter).
pub trait StreamExtract {
    fn extract<T: Extractor>(&mut self) -> Result<T, Error>;
}

impl<T> StreamExtract for T where T: Iterator<Item=TokenTree> {
    fn extract<E: Extractor>(&mut self) -> Result<E, Error> {
        E::extract(self)
    }
}

/// Flattens all none delimited groups and returns the length.
pub fn sanitize(tokens: impl IntoIterator<Item = TokenTree>) -> (TokenStream, usize) {
    let mut count = 0;
    (tokens.into_iter().flat_map(|x| match x {
        proc_macro2::TokenTree::Group(g) if g.delimiter() == Delimiter::None => {
            let (stream, c) = sanitize(g.stream());
            count += c;
            stream
        },
        tt => {
            count += 1;
            tt.into()
        },
    }).collect(), count)
}