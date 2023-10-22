use macroex::{FromMacro, Extractor};
use quote::quote;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub enum Color {
    Transparent,
    Rgb{r: u8, g: u8, b: u8},
    Rgba(u8, u8, u8, u8),
    Named(String),
    Unit1(),
    Unit2{}
}


#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub enum Gender {
    Female, Male, NonBinary
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub enum Never{}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub struct Birthday(String, i32, i32);


const DEFAULT_HEIGHT: f32 = 100.0;
#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub struct Person {
    #[macroex(required)]
    pub name: String,
    pub age: i32,
    #[macroex(default="DEFAULT_HEIGHT")]
    pub height: f32,
    pub birthday: Option<Birthday>,
    pub hair_color: Option<Color>,
    #[macroex(required)]
    pub gender: Gender,
    #[macroex(rename="dead")]
    pub is_dead: bool,
    #[macroex(repeat, rename="child")]
    pub children: Vec<String>,
    pub never: Option<Never>,
}


#[test]
pub fn enums(){

    let tokens = quote! (
        Rgb { r: 4, g: 12, b: 16}
    );
    let parsed = Color::from_many(tokens).unwrap();
    let other = Color::Rgb { r: 4, g: 12, b: 16 };
    assert_eq!(parsed, other);

    let tokens = quote! (
        Rgba (1, 2, 3, 4)
    );
    let parsed = Color::from_many(tokens).unwrap();
    let other = Color::Rgba (1, 2, 3, 4);
    assert_eq!(parsed, other);

    let tokens = quote! (
        Named("Red")
    );
    let parsed = Color::from_many(tokens).unwrap();
    let other = Color::Named("Red".to_owned());
    assert_eq!(parsed, other);

    let tokens = quote! (
        Transparent
    );
    let parsed = Color::extract(&mut tokens.into_iter()).unwrap();
    let other = Color::Transparent;
    assert_eq!(parsed, other);

    let tokens = quote! (
        Unit1
    );
    let parsed = Color::extract(&mut tokens.into_iter()).unwrap();
    let other = Color::Unit1();
    assert_eq!(parsed, other);

    let tokens = quote! (
        Unit2
    );
    let parsed = Color::extract(&mut tokens.into_iter()).unwrap();
    let other = Color::Unit2{};
    assert_eq!(parsed, other);

    let tokens = quote! (
        Unit1 ()
    );
    let parsed = Color::from_many(tokens).unwrap();
    let other = Color::Unit1();
    assert_eq!(parsed, other);

    let tokens = quote! (
        Unit2 {}
    );
    let parsed = Color::from_many(tokens).unwrap();
    let other = Color::Unit2{};
    assert_eq!(parsed, other);
}

#[test]
pub fn main(){
    let tokens = quote!(
        Person {
            name: "Dave",
            age: 34,
            birthday: ( "Feb", 29, 1992 ),
            gender: Male,
            hair_color: Rgb { r: 1, g: 2, b: 3 },
            child: "Jane",
            child: "James",
        }
    );
    let parsed = Person::from_many(tokens).unwrap();
    let other = Person{
        name: "Dave".to_owned(),
        age: 34,
        height: 100.0,
        birthday: Some(Birthday("Feb".to_owned(), 29, 1992 )),
        gender: Gender::Male,
        hair_color: Some(Color::Rgb { r: 1, g: 2, b: 3 }),
        is_dead: false,
        children: vec![
            "Jane".to_owned(),
            "James".to_owned(),
        ],
        never: None,
    };
    assert_eq!(parsed, other);

}