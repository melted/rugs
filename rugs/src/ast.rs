
//! This module contains the AST data structures that are the output
//! of the parsing state.
use std::collections::HashMap;

use num_bigint::BigInt;

use crate::location::Location;

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub file : Option<String>,
    pub trivia : Vec<Annotation>,
    pub annotations : HashMap<NodeId, Annotation>,
    pub locations : HashMap<NodeId, Location>,
    pub newlines : Vec<usize>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name : Identifier,
    pub metadata : Metadata,
    pub imports : Vec<ImportDecl>,
    pub exports : Option<Vec<Export>>,
    pub declarations : Vec<TopDeclaration>
}

impl Module {
    pub fn new() -> Module {
        Module { 
            name: module(""),
            metadata: Metadata::new(),
            imports: Vec::new(),
            exports: None,
            declarations: Vec::new()
        }
    }
}

impl Metadata {
    pub fn new() -> Metadata {
        Metadata {
            file: None,
            trivia: Vec::new(),
            annotations: HashMap::new(),
            locations: HashMap::new(),
            newlines: Vec::new()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopExpression {
    pub metadata : Metadata,
    pub expression : Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub id : NodeId,
    pub name : Identifier,
    pub qualified : bool,
    pub alias : Option<Identifier>,
    pub specific : Option<Vec<Import>>,
    pub hidden : Option<Vec<Import>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExposedSpec {
    None,
    All,
    List(Vec<Identifier>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    Var(Identifier),
    TypeOrClass(Identifier, ExposedSpec)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Export {
    Var(Identifier),
    TypeOrClass(Identifier, ExposedSpec),
    Module(Identifier)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    OtherPragma(String),
    Doc(String),
    Comment(String)
}


#[derive(Debug, Clone, PartialEq)]
pub enum TopDeclaration {
    Type(TypeDecl),
    Data(Data),
    Newtype(Newtype),
    Class(Class),
    Instance(Instance),
    Default(Vec<Type>),
    Foreign(Foreign),
    Declaration(Declaration)
}


#[derive(Debug, Clone, PartialEq)]
pub enum TypeCon {

}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    id : NodeId,
    alias : Type,
    the_type : Type
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub id : NodeId,
    pub this_type : Type,
    pub constructors : Vec<Constructor>,
    pub deriving : Vec<Identifier>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Newtype {
    pub id : NodeId,
    pub this_type : Type,
    pub constructor : Constructor,
    pub deriving : Vec<Identifier>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub id : NodeId,
    pub context: Context,
    pub tycls : Identifier,
    pub tyvars : Vec<Identifier>,
    pub decls : Vec<Declaration>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub id : NodeId,
    pub context : Context,
    pub tycon : Identifier,
    pub tyvars : Vec<Identifier>,
    pub decls : Vec<Declaration>
}


#[derive(Debug, Clone, PartialEq)]
pub enum Constructor {
    Plain { con: Identifier, the_types: Vec<Type> },
    Labelled { con: Identifier, fields: Vec<TypeField> }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeField {
    pub label: Identifier,
    pub the_type : Type
}
#[derive(Debug, Clone, PartialEq)]
pub struct Foreign {
    pub id : NodeId,
    pub decl : ForeignDeclaration
}

#[derive(Debug, Clone, PartialEq)]
pub enum Safety {
    Unsafe,
    Safe
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForeignDeclaration {
    Import {
        callconv : String,
        impent : String,
        safety : Safety,
        var : Identifier,
        ftype : Type
    },
    Export {
        callconv : String,
        expent : String,
        var : Identifier,
        ftype : Type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub id : NodeId,
    pub value : DeclarationValue
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationValue {
    TypeDecl(Vec<Identifier>, Context, Type),
    Fixity(Vec<Identifier>, Association, u32),
    VarBind(Identifier, Binding),
    FunBind(FunBind, Binding)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Association {
    Left,
    Right,
    NonAssociative
}

#[derive(Debug, Clone, PartialEq)]
pub enum SeqSyntax {
    Pattern(Pattern, Expression),
    Decls(Vec<Declaration>),
    Expr(Expression)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Plain(Expression, Vec<Declaration>),
    Guarded(Expression, Vec<GuardedExpression>, Vec<Declaration>)
}


#[derive(Debug, Clone, PartialEq)]
pub enum FunBind {
    Plain(Identifier, Vec<Pattern>),
    Op(Identifier, Pattern, Pattern),
    Wrapped(Box<FunBind>, Vec<Pattern>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Integer(BigInt),
    Float(f64),
    Char(char),
    String(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Base(Identifier),
    Var(Identifier),
    App(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    List(Box<Type>),
    Fun(Box<Type>, Box<Type>)
}


impl Type {
    pub (super) fn base(name : Identifier) -> Type {
        Type::Base(name)
    }

    pub (super) fn var(name : Identifier) -> Type {
        Type::Var(name)
    }

    pub (super) fn app(self, rhs : Type) -> Type {
        Type::App(Box::new(self), Box::new(rhs))
    }

    pub (super) fn list(self) -> Type {
        Type::List(Box::new(self))
    }

    pub (super) fn tuple(types : Vec<Type>) -> Type {
        Type::Tuple(types)
    }

    pub (super) fn fun(self, rhs : Type) -> Type {
        Type::Fun(Box::new(self), Box::new(rhs))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub classes : Vec<(Identifier, Vec<Type>)>
}

pub type NodeId = u32;

#[derive(Debug, Clone)]
pub struct Expression {
    pub id : NodeId,
    pub value : Box<ExpressionValue>
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue {
    Var(Identifier),
    Const(Const),
    App(Expression, Expression),
    Let(Vec<Declaration>, Expression),
    If(Expression, Expression, Expression),
    Case(Expression, Vec<CaseAlt>),
    Infix(Identifier, Expression, Expression),
    Typed(Expression, Context, Type),
    Lambda(Vec<Pattern>, Expression),
    // Apologia for Wrapped: Infix expressions are desugared after parsing because
    // infix declarations can come after an expression that is affected by them in the source,
    // until that happens we need to keep track of parenthesized expressions.
    Wrapped(Expression),
    List(Vec<Expression>),
    Comprehension(Expression, Vec<SeqSyntax>),
    Do(Vec<SeqSyntax>),
    Tuple(Vec<Expression>),
    LabeledCon(Identifier, Vec<(Identifier, Expression)>),
    LabeledUpdate(Expression, Vec<(Identifier, Expression)>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct GuardedExpression {
    guards : Vec<SeqSyntax>,
    body : Expression
}

#[derive(Debug, Clone, PartialEq)]
pub enum CaseAlt {
    Simple(Pattern, Expression),
    Guarded(Pattern, GuardedExpression)
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub id : NodeId,
    pub value : Box<PatternValue>
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternValue {
    Var(String),
    As(String, Pattern),
    Literal(Const),
    Constructor(String, Vec<Pattern>),
    InfixConstructor(String, Pattern, Pattern),
    Labeled(String, Vec<(String, Pattern)>),
    Wildcard,
    Tuple(Vec<Pattern>),
    List(Vec<Pattern>),
    Irrefutable(Pattern)
}


#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    Var { 
        module: Option<String>,
        name: String
    }, 
    Con { 
        module: Option<String>,
        name: String
    },
    Module(String),
    Sym { 
        module: Option<String>,
        name: String
    }, 
    ConSym { 
        module: Option<String>,
        name: String
    },
}

pub trait AstMaker {
    fn next_id(&mut self) -> NodeId;

    fn expr(&mut self, value : ExpressionValue) -> Expression {
        Expression { id: self.next_id(), value: Box::new(value) }
    }

    fn new_import_decl(&mut self, name : Identifier) -> ImportDecl {
        ImportDecl { id: self.next_id(), name: name, qualified: false,
                     alias: None, specific: None, hidden: None }
    }

    fn new_context(&mut self) -> Context {
        Context { classes: Vec::new() }
    }

    fn new_class(&mut self, name : Identifier) -> Class {
        Class { id: self.next_id(), context: self.new_context(), tycls: name,
                tyvars: Vec::new(), decls: Vec::new() }
    }

    fn new_data(&mut self, the_type : Type) -> Data {
        Data { id: self.next_id(), this_type: the_type,
               constructors: Vec::new(), deriving: Vec::new() }
    }

    fn new_newtype(&mut self, the_type : Type, constructor : Constructor) -> Newtype {
        Newtype { id: self.next_id(), this_type: the_type, constructor: constructor, deriving: Vec::new() }
    }

    fn new_typedecl(&mut self, this_type : Type, that_type : Type) -> TypeDecl {
        TypeDecl { id: self.next_id(), alias: this_type, the_type: that_type }
    }

    fn app(&mut self, f : Expression, arg: Expression) -> Expression {
        self.expr(ExpressionValue::App(f, arg))
    }

    fn infix(&mut self, op: Identifier, left: Expression, right: Expression) -> Expression {
        self.expr(ExpressionValue::Infix(op, left, right))
    }

    fn var(&mut self, name : Identifier) -> Expression {
        self.expr(ExpressionValue::Var(name))
    }

    fn lambda(&mut self, args : Vec<Pattern>, exp : Expression) -> Expression {
        self.expr(ExpressionValue::Lambda(args, exp))
    }

    fn let_expression(&mut self, decls : Vec<Declaration>, exp : Expression) -> Expression {
        self.expr(ExpressionValue::Let(decls, exp))
    }

    fn case_expression(&mut self, scrutinee:Expression, alts:Vec<CaseAlt>) -> Expression {
        self.expr(ExpressionValue::Case(scrutinee, alts))
    }

    fn tuple(&mut self, fields : Vec<Expression>) -> Expression {
        self.expr(ExpressionValue::Tuple(fields))
    }

    fn wrapped(&mut self, e : Expression) -> Expression {
        self.expr(ExpressionValue::Wrapped(e))
    }

    fn list(&mut self, fields : Vec<Expression>) -> Expression {
        self.expr(ExpressionValue::Tuple(fields))
    }

    fn if_expression(&mut self, predicate : Expression, then_exp : Expression, else_exp : Expression) -> Expression {
        self.expr(ExpressionValue::If(predicate, then_exp, else_exp))
    }

    fn typed(&mut self, exp :Expression, c: Context, t:Type) -> Expression {
        self.expr(ExpressionValue::Typed(exp, c, t))
    }

    fn integer(&mut self, n : BigInt) -> Expression {
        self.expr(ExpressionValue::Const(Const::Integer(n)))
    }

    fn float(&mut self, x : &f64) -> Expression {
        self.expr(ExpressionValue::Const(Const::Float(*x)))
    }

    fn char_const(&mut self, c : &char) -> Expression {
        self.expr(ExpressionValue::Const(Const::Char(*c)))
    }

    fn string_const(&mut self, s : &str) -> Expression {
        self.expr(ExpressionValue::Const(Const::String(s.to_string())))
    }
}

pub fn varid(name : &str) -> Identifier {
    Identifier::Var { module: None, name: name.to_string() }
}

pub fn conid(name : &str) -> Identifier {
    Identifier::Con { module: None,  name: name.to_string() }
}

pub fn varsym(name : &str) -> Identifier {
    Identifier::Sym { module: None, name: name.to_string() }
}

pub fn consym(name : &str) -> Identifier {
    Identifier::ConSym { module: None, name: name.to_string() }
}

pub fn qvarid(module :&str, name : &str) -> Identifier {
    Identifier::Var { module: Some(module.to_string()), name: name.to_string() }
}

pub fn qconid(module :&str, name : &str) -> Identifier {
    Identifier::Con { module: Some(module.to_string()), name: name.to_string() }
}

pub fn qvarsym(module :&str, name : &str) -> Identifier {
    Identifier::Sym { module: Some(module.to_string()), name: name.to_string() }
}

pub fn qconsym(module :&str, name : &str) -> Identifier {
    Identifier::ConSym { module: Some(module.to_string()), name: name.to_string() }
}

pub fn module(module: &str) -> Identifier {
    Identifier::Module(module.to_string())
}