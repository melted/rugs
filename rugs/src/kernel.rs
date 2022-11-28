use num_bigint::BigInt;

type Name = String;
#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    name : Name,
    ty : Type
}


#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    NonRec(Name, Box<Expression>),
    Rec(Vec<(Name, Box<Expression>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Var(Name),
    Lit(Const),
    App {
        fun: Box<Expression>,
        arg: Box<Expression>,
    },
    Let {
        bind: Binding,
        body: Box<Expression>,
    },
    Case {
        scrutinee: Box<Expression>,
        sc_name : Name,
        ty: Type,
        alts: Vec<CaseAlt>,
    },
    Lambda {
        var: Name,
        body: Box<Expression>,
    },
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseAlt {
    pub con: AltCon,
    pub args: Vec<Name>,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AltCon {
    Constructor(Name),
    Literal(Const),
    Default,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Integer(BigInt),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {}
