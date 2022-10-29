
//! This module contains the AST data structures that are the output
//! of the parsing state.

use std::path::{ Path, PathBuf };
use std::ops::Deref;

use num_bigint::BigInt;

#[derive(Debug)]
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
            exports: None,
            declarations: Vec::new()
        }
    }
}

#[derive(Debug)]
pub struct Import {
    pub name : String,
    pub qualified : bool,
    pub alias : Option<String>,
    pub specific : Option<Vec<String>>,
    pub hidden : Vec<String>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    OtherPragma(String),
    Doc(String),
    Comment(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotated<T> {
    pub annotations : Vec<Annotation>,
    pub location : Option<(usize, usize)>,
    pub value : T
}

impl<T> Deref for Annotated<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug)]
pub enum Declaration {

}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Integer(BigInt),
    Float(f64),
    Char(char),
    String(String)
}

// TODO: Implement
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Na
}

// TODO: Implement
#[derive(Debug, Clone, PartialEq)]
pub enum Context {
    Mmm
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operator {
    pub module : Option<String>,
    pub constructor : bool,
    pub ticked : bool,
    pub name : String
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub module : Option<String>,
    pub constructor : bool,
    pub operator : bool,
    pub name : String
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Var(String),
    Const(Const),
    App(Box<Expression>, Vec<Expression>),
    Infix(Operator, Box<Expression>, Box<Expression>),
    Typed(Box<Expression>, Context, Type),
    Lambda(Vec<Pattern>, Box<Expression>),
    // Apologia for Wrapped: Infix expressions are desugared after parsing because
    // infix declarations can come after an expression that is affected by them in the source,
    // until that happens we need to keep track of parenthesized expressions.
    Wrapped(Box<Expression>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String),
    As(String, Box<Pattern>),
    Literal(Const),
    Constructor(String, Vec<Pattern>),
    InfixConstructor(String, Box<Pattern>, Box<Pattern>),
    Labeled(String, Vec<(String, Pattern)>),
    Wildcard,
    Tuple(Vec<Pattern>),
    List(Vec<Pattern>),
    Irrefutable(Box<Pattern>)
}


pub fn app(f : Expression, args: Vec<Expression>) -> Expression {
    Expression::App(Box::new(f), args)
}

pub fn infix(op: Operator, left: Expression, right: Expression) -> Expression {
    Expression::Infix(op, Box::new(left), Box::new(right))
}

pub fn var(name : &str) -> Expression {
    Expression::Var(name.to_string())
}

pub fn typed(exp :Expression, c: Context, t:Type) -> Expression {
    Expression::Typed(Box::new(exp), c, t)
}

pub fn integer(n : BigInt) -> Expression {
    Expression::Const(Const::Integer(n))
}

pub fn float(x : &f64) -> Expression {
    Expression::Const(Const::Float(*x))
}

pub fn char_const(c : &char) -> Expression {
    Expression::Const(Const::Char(*c))
}

pub fn string_const(s : &str) -> Expression {
    Expression::Const(Const::String(s.to_string()))
}
