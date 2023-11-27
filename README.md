# macroex

[![Crates.io](https://img.shields.io/crates/v/macroex.svg)](https://crates.io/crates/macroex)
[![Docs](https://docs.rs/macroex/badge.svg)](https://docs.rs/macroex/latest/macroex/)

An extractor based low level macro parsing crate
that provides high level parsing support through derive macros.

## `FromMacro` and `Extractor`

`FromMacro` is the bread and butter of this crate.
`FromMacro` provides a `from_one` and a `from_many` function
that parses a `TokenTree` and a `TokenStream` respectively.

We can mostly assume `from_many` will contain two or more `TokenTrees`. If not
that is considered a bug in an extractor.

All implementors of `FromMacro` are `Extractors` of one `TokenTree`. When used on a
`TokenStream` iterator directly, they will consume a single `TokenTree`
and try to parse it using `from_one`.

```rust
let mut iter = quote!{45, Hello; true false}.into_iter();
let a: i32 = iter.extract()?;
let b: Punct = iter.extract()?;
// Extracts a string value from an ident
let IdentString(c) = iter.extract()?;
// Validates a semicolon
let d: PunctOf<';'> = iter.extract()?;
let e: bool = iter.extract()?;
// Validates a false literal
let f: LitFalse = iter.extract()?;
```

This is pretty great! As most things can be represented as a single `TokenTree`.

```rust
// This is a single TokenTree::Group
{
    name: "Tom".
    age: 45,
    children: [
        "Tim", "Tam"
    ],
}
```

However there are other things one `TokenTree` cannot account for.

```rust
// This fails because -45 is two tokens
let a: i32 = quote!{-45}.into_iter().extract().unwrap();
```

Wrapping `FromMacro` implementers in other `Extractors`
allow `FromMacro` implementors to parse additional `TokenTrees` and
potentially utilize the `from_many` method
if more than one `TokenTree` is matched.

```rust
// Note -45 is two tokens
let mut iter = quote!{-45}.into_iter();
// All extracts everything from a stream
let All(a) = iter.extract()?;
assert_eq!(a, -45i32);

let mut iter = quote!{-45, 21, 9.5,}.into_iter();
// CommaExtractor extracts until a comma or end of stream is found.
let CommaExtractor(a) = iter.extract()?;
let CommaExtractor(b) = iter.extract()?;
let CommaExtractor(c) = iter.extract()?;
// EndOfStream is a unit struct extractor and this asserts iter is empty
let EndOfStream = iter.extract()?;
assert_eq!(a, -45);
assert_eq!(b, 21);
assert_eq!(c, 9.5);
```

## Derive

We provide derive macro `FromMacro`
and `FromAttrs` that functions similarly to
to `serde::Deserialize`. This enables ergonomic
parsing for structs and enums following a specific data format.

`FromMacro` parses syntax similar to native rust,
while `FromAttrs` parses syntax commonly used in macro attributes.

### Why not `serde_tokenstream`?

Since we do not use the `serde` data model. Our data model is much more powerful in the macro context.
We are allowed to extract all `FromMacro` implementors,
including `TokenStream`, `Ident`, `Group`, etc.

## FromMacro

`FromMacro` parses syntax similar to native rust.

| Type | `from_one` | `from_many` |
| --- | --- | --- |
| Unit Struct | `StructName` | -- |
| Tuple Struct | `(tuple, ..)` | `StructName (tuple, ..)` |
| Named Struct | `{field: value, ..}` | `StructName {field: value, ..}` |
| Unit Enum Variant | `VariantName` | -- |
| Tuple Enum Variant | -- | `VariantName (tuple, ..)` |
| Named Enum Variant | -- | `VariantName {field: value, ..}` |

### Examples

| Type | Rust Type | `from_one` | `from_many` |
| --- | --- | --- | --- |
| Unit Struct | `struct Red;` | `Red` | -- |
| Tuple Struct | `struct Vec2 (i32, i32)` | `(4, 5)` | `Vec2 (4, 5)` |
| Named Struct | `struct Vec2 {x: i32, y: i32}` | `{x: 4, y: 5}` | `Vec2 {x: 4, y: 5}` |
| Unit Variant | `enum Color {Black, White}` | `Black` | -- |
| Tuple Variant | `enum Animals {Dog(String), Sheep(usize)}` | -- | `Dog ("Rex")` |
| Named Variant | `enum Shapes {Square {x: f32}, Rect {x: f32, y: f32}}` | -- | `Rect {x: 4, y: 5}` |

#### Use Case

Since we are likely to be parsing configurations in macros,
we supply a `Default::default()` value if a field is not found.

You are required to opt **out** of this with `#[macroex(required)]` if
your type does not implement `Default`.

```rust
#[derive(FromMacro)]
pub struct Person {
    pub name: String,
    pub age: i32,
    pub height: f32,
    // This works as long as Gender implements `FromMacro` and `Default`
    pub gender: Gender,
    // Using Option is idiomatic to handle the default case.
    pub hair_color: Option<NumberList<[f32;4]>>,
    // We can extract macro based things
    pub do_something: Option<TokenStream>,
}
```

Example macro input:

```rust
person! {
    name: "Lina",
    age: 23,
    gender: Female,
    hair_color: [0.7, 0.4, 0],
    do_something: {
        let w = "world";
        println!("Hello, {}!", w)
    },
}
```

#### Attributes

The `FromMacro` macro supports the following attributes:

```rust
#[derive(FromMacro)]
// We use the same casing names as serde.
#[macroex(rename_all="SCREAMING-KEBAB-CASE")]
pub struct Animal {
    // Errors if not specified.
    #[macroex(required)]
    pub name: String,
    // Evaluate an expression instead of `Default::default()`
    #[macroex(default="0.0")]
    pub height: f32,
    #[macroex(default=r#""dog".to_owned()"#)]
    pub species: String,
    // Take strings as inputs, and collects them into a vec.
    #[macroex(repeat)]
    // and rename "nicknames" to "nickname" during parsing.
    #[macroex(rename="nickname")]
    pub nicknames: Vec<String>,
}
```

## FromAttrs

`FromAttrs`
Generates a simple `FromMacro` implementation for syntax commonly associated with macro attributes.

This macro is only allowed on named structs and supports 3 basic syntax:

* `.., name, ..` parses to `name: true`, which matches a boolean value.
* `.., name = expr, ..` parses to `name: expr`
* `.., name(..), ..` parses to `name: T{ .. }`

Other types like fieldless enums can potentially use `FromMacro`
to generated compatible `FromMacro` implementations to use with this macro.

### Example

We use the same set of attributes as `FromMacro`

```rust
#[derive(FromAttrs)]
#[macroex(rename_all="snake_case")]
pub struct Animal {
    #[macroex(required)]
    pub name: String,
    #[macroex(default="0.0")]
    pub height: f32,
    #[macroex(default=r#"Ident::new("Dog", Span::call_site())"#)]
    pub species: Ident,
    #[macroex(repeat, rename="mascot")]
    pub mascot_infos: Vec<MascotInfo>,
}
```

Example attribute:

```rust
#[animal(name = "Ferris", species = Crab, mascot(language = "Rust"))]
```

We can parse either

```rust
(name = "Ferris", species = Crab, mascot(language = "Rust"))
```

with `from_one`, or

```rust
name = "Ferris", species = Crab, mascot(language = "Rust")
```

with `from_many`, commonly extracted with `syn`.

## Macro Chaining and Hygeine

We treat our input as string-like and we will try
to flatten all `None` delimited groups encountered during parsing.
