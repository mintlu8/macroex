use macroex::*;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub enum Never {}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromMacro))]
pub enum SkillLevel {
    Novice, Expert, Master,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromAttrs))]
pub struct Skill {
    #[macroex(required)]
    pub name: String,
    #[macroex(required)]
    pub level: SkillLevel,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature="derive", derive(macroex::FromAttrs))]
pub struct Person {
    pub name: String,
    pub age: i32,
    pub student: bool,
    pub parent: bool,
    #[macroex(repeat, rename="skill")]
    pub skills: Vec<Skill>,
    pub never: Option<Never>,
}

#[test]
pub fn main() -> Result<(), Error>{
    let mut attr = quote::quote!(
        #[name="Timmy", age=12, student,
            skill(name = "gardening", level = Novice), 
            skill(name = "cooking", level = Expert)]
    ).into_iter();

    let PunctOf::<'#'> = attr.extract()?;
    let Bracketed(tokens) = attr.extract()?;


    let person = Person::from_many(tokens)?;
    dbg!(&person);
    let other = Person {
        name: "Timmy".to_owned(),
        age: 12,
        student: true,
        parent: false,
        skills: vec![
            Skill { name: "gardening".to_owned(), level: SkillLevel::Novice },
            Skill { name: "cooking".to_owned(), level: SkillLevel::Expert }
        ],
        never: None,
    };
    assert!(person == other);
    Ok(())
}