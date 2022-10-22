
//! This module contains the AST data structures that are the output
//! of the parsing state.

use std::path::{ Path, PathBuf };
use std::ops::Deref;

pub struct Module {
    pub name : String,
    pub file : Option<PathBuf>,
    pub trivia : Vec<Annotated<()>>,
    pub imports : Vec<Annotated<Import>>,
    pub exports : Option<Vec<Annotated<String>>>,
    pub declarations : Vec<Annotated<Declaration>>
}

impl Module {
    pub fn new() -> Module {
        Module { 
            name: String::new(),
            file: None,
            trivia: Vec::new(),
            imports: Vec::new(),
            exports: None
        }
    }
}

pub struct Import {
    pub name : String,
    pub qualified : bool,
    pub alias : Option<String>,
    pub specific : Option<Vec<String>>,
    pub hidden : Vec<String>
}

pub enum Annotation {
    OtherPragma(String),
    Doc(String),
    Comment(String)
}

pub struct Annotated<T> {
    pub annotations : Vec<Annotation>,
    pub location : (usize, usize),
    pub value : T
}

impl<T> Deref for Annotated<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub enum Declaration {

}

pub enum Expression {

}
