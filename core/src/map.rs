use std::{collections::{HashMap, BTreeMap}, hash::Hash, borrow::Borrow};

use proc_macro2::TokenTree;

use crate::{FromMacro, Error, CurlyBraced, CommaExtractor, Field, EndOfStream, StreamExtract, Iter, Parenthesized, Meta, EitherStream, IdentString};

/// A map used by [`FromMacro`](::macroex_derive::FromMacro).
/// 
/// Gracefully handles duplicate keys by preserving them and 
/// exposing the last inserted one through a HashMap like API.
#[doc(hidden)]
#[derive(Debug, Clone)]
pub struct FieldMap<TKey, TValue, const N: usize = 0>(pub HashMap<TKey, Vec<TValue>>);

/// A map used by [`FromAttrs`](::macroex_derive::FromAttrs).
/// 
/// Gracefully handles duplicate keys by preserving them and 
/// exposing the last inserted one through a HashMap like API.
#[doc(hidden)]
pub type MetaMap = FieldMap<IdentString, EitherStream, 1>;

impl<K, V, const N: usize> Default for FieldMap<K, V, N> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<K: Hash + Eq, V, const N: usize> FieldMap<K, V, N> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, key: K, value: V) {
        if let Some(item) = self.0.get_mut(&key) {
            item.push(value)
        } else {
            self.0.insert(key, vec![value]);
        }
    }

    pub fn remove<Q: Hash + Eq + ?Sized>(&mut self, key: &Q) -> Option<V> where K: Borrow<Q>{
        if let Some(item) = self.0.get_mut(key) {
            item.pop()
        } else {
            None
        }
    }

    pub fn remove_all<Q: Hash + Eq + ?Sized>(&mut self, key: &Q) -> impl IntoIterator<Item = V> where K: Borrow<Q>{
        self.0.remove(key).unwrap_or(Vec::new())
    }

    pub fn get<Q: Hash + Eq + ?Sized>(&self, key: &Q) -> Option<&V> where K: Borrow<Q>{
        if let Some(item) = self.0.get(key) {
            item.last()
        } else {
            None
        }
    }

    pub fn get_all<Q: Hash + Eq + ?Sized>(&mut self, key: &Q) -> impl IntoIterator<Item = &V> where K: Borrow<Q>{
        if let Some(item) = self.0.get(key) {
            item.iter()
        } else {
            [].iter()
        }
    }
}



impl<A, B> FromMacro for BTreeMap<A, B> where A: FromMacro + Ord, B: FromMacro{
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let CurlyBraced(Iter(mut iter)) = CurlyBraced::from_one(tt)?;
        let mut result = BTreeMap::new();
        while let Ok(CommaExtractor(Field(name, field))) = iter.extract(){
            result.insert(name, field);
        }
        iter.extract::<EndOfStream>()?;
        Ok(result)
    }
}

impl<A, B> FromMacro for HashMap<A, B> where A: FromMacro + Hash + Eq, B: FromMacro{
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let CurlyBraced(Iter(mut iter)) = CurlyBraced::from_one(tt)?;
        let mut result = HashMap::new();
        while let Ok(CommaExtractor(Field(name, field))) = iter.extract(){
            result.insert(name, field);
        }
        iter.extract::<EndOfStream>()?;
        Ok(result)
    }
}


impl<A, B> FromMacro for FieldMap<A, B> where A: FromMacro + Hash + Eq, B: FromMacro {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let CurlyBraced(Iter(mut iter)) = CurlyBraced::from_one(tt)?;
        let mut result = FieldMap::new();
        while let Ok(CommaExtractor(Field(name, field))) = iter.extract(){
            result.insert(name, field);
        }
        iter.extract::<EndOfStream>()?;
        Ok(result)
    }
}


impl FromMacro for MetaMap {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let Parenthesized(tokens) = Parenthesized::from_one(tt)?;
        Self::from_many(tokens)
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let mut result = Self::new();
        while let Ok(CommaExtractor(Meta(name, field))) = iter.extract(){
            result.insert(name.into(), field);
        }
        iter.extract::<EndOfStream>()?;
        Ok(result)
    }
}
