
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


#[derive(Debug, Clone, PartialEq)]
pub enum TypeCon {

}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDeclaration {

}

#[derive(Debug, Clone, PartialEq)]
pub struct DataDeclaration {

}

#[derive(Debug, Clone, PartialEq)]
pub enum Constructor {

}

#[derive(Debug, Clone, PartialEq)]
pub enum TopDeclaration {
    Type(TypeDeclaration),
    Data(DataDeclaration),
    Newtype,
    Class,
    Instance,
    Default,
    Foreign,
    Decl(Declaration)

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Association {
    Left,
    Right,
    NonAssociative
}

#[derive(Debug, Clone, PartialEq)]
pub enum Guard {
    Pattern(Pattern, Expression),
    Decls(Vec<Declaration>),
    Boolean(Expression)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Plain(Expression, Vec<Declaration>),
    Guarded(Expression, Vec<(Vec<Guard>, Expression)>, Vec<Declaration>)
}


#[derive(Debug, Clone, PartialEq)]
pub enum FunBind {
    Plain(Identifier, Vec<Pattern>),
    Op(Operator, Pattern, Pattern),
    Wrapped(Box<FunBind>, Vec<Pattern>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    TypeDecl(Vec<Identifier>, Context, Type),
    Fixity(Vec<Operator>, Association, u32),
    VarBind(Identifier, Binding),
    FunBind(FunLeft, Binding)
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
pub struct CaseAlt {
    matcher : Pattern,
    guards : Vec<Expression>,
    body : Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Var(String),
    Const(Const),
    App(Box<Expression>, Vec<Expression>),
    Let(Vec<Declaration>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Case(Box<Expression>, Vec<CaseAlt>),
    Infix(Operator, Box<Expression>, Box<Expression>),
    Typed(Box<Expression>, Context, Type),
    Lambda(Vec<Pattern>, Box<Expression>),
    // Apologia for Wrapped: Infix expressions are desugared after parsing because
    // infix declarations can come after an expression that is affected by them in the source,
    // until that happens we need to keep track of parenthesized expressions.
    Wrapped(Box<Expression>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    LabeledCon(Identifier, Vec<(Identifier, Expression)>),
    LabeledUpdate(Box<Expression>, Vec<(Identifier, Expression)>)
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

pub fn lambda(args : Vec<Pattern>, exp : Expression) -> Expression {
    Expression::Lambda(args, Box::new(exp))
}

pub fn let_expression(decls : Vec<Declaration>, exp : Expression) -> Expression {
    Expression::Let(decls, Box::new(exp))
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
