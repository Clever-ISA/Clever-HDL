
use std::cmp::Ordering;

use peekmore::{PeekMoreIterator, PeekMore};

use crate::lex::*;


#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SimplePrefix {
    SelfPath,
    Super,
    Crate,
    Root,
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SimplePath {
    pub root: Option<SimplePrefix>,
    pub idents: Vec<String>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Meta {
    Ident(SimplePath),
    String(String),
    IntLit(i128),
    Group(SimplePath, Vec<Self>),
    KeyValue(SimplePath, Box<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArg {
    Name(Path),
    Type(Type),
    Expr(Expr),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    Id(String),
    Generics(Vec<GenericArg>),
}

#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum SignalDirection{
    In,
    Out,
    InOut
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path {
    pub root: Option<SimplePrefix>,
    pub components: Vec<PathComponent>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type{
    Named(Path),
    Array{
        element: Box<Type>,
        len: Expr
    },
    Reference(Mutability,Box<Type>), // &T or &mut t
    Signal(SignalDirection,Box<Type>), // &in T or &out T or &inout T
    SelfTy,
}

#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum TriggerType{
    RisingEdge,
    HighSignal,
    FallingEdge,
    LowSignal,
    Biedge
}


#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp{
    AddrOf(Mutability),
    SignalOf(SignalDirection),
    Deref,
    Try,
    Not,
    Neg,
    Await,
    AwaitSignal(TriggerType),
}

#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp{
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    LogicAnd,
    LogicOr,
    LeftShift,
    RightShift,
    LeftRotate, // ~<<
    RightRotate, // ~>>
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LogicAndAssign,
    LogicOrAssign,
    LeftShiftAssign,
    RightShiftAssign,
    LeftRotateAssign,
    RightRotateAssign,
    CompareEq,
    CompareNe,
    CompareLt,
    CompareGt,
    CompareLe,
    CompareGe,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldName{
    Named(String),
    Tuple(u32)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr{
    IntLit(u128),
    Name(Path),
    Await(Box<Expr>,Option<TriggerType>), // `x.await` or `x.await rising_edge`
    Unary(UnaryOp, Box<Expr>), // `@x`'
    Empty, // `_`
    Paren(Box<Expr>), // `(x)`
    Binary(Box<Expr>,BinaryOp,Box<Expr>), // `(x @ y)`
    Call{base: Box<Expr>, args: Vec<Expr>}, // `x(y,...,z)`
    MemberAccess(Box<Expr>,FieldName), // x.y
    MethodCall{base: Box<Expr>,method: String,args: Vec<Expr>}, // `w.x(y,...,z)`
    Subscript{base: Box<Expr>, index: Box<Expr>} // x[y]
}


#[derive(Clone, Debug, Hash, PartialEq,Eq)]
pub enum AsyncType{
    Async,
    Procedure,
}

#[derive(Clone, Debug, Hash, PartialEq,Eq)]
pub enum Visibilty{
    None,
    Pub
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Item{
    pub vis: Visibilty,
    pub attrs: Vec<Meta>,
    pub inner: ItemInner,   
}

#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValueKind{
    Signal(SignalDirection),
    Static(Mutability),
    Const,
}

#[derive(Copy,Clone, Debug, Hash, PartialEq, Eq)]
pub enum AggregateKind{
    Struct,
    Union,
    Application
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemInner{
    TypeAlias{
        name: String,
        def: Type
    },
    Function{
        constness: bool,
        async_type: Option<AsyncType>,
        params: Vec<Param>,
    },
    ValueDef{
        kind: ValueKind,
        name: String,
        init: Expr,
    },
    Aggregate{
        aggregate_kind: AggregateKind,
        name: String,
        body: Option<StructBody>,
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum StructBody{
    Tuple(Vec<Type>),
    Struct(Vec<StructField>)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField{
    pub vis: Visibilty,
    pub field: String,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Param{
    pub pattern: Pattern,
    pub ty: Option<Type>
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Pattern{
    SelfPatern,
    Name(Mutability,Path),
    Ref(Mutability,Box<Pattern>),
}

pub fn parse_expression<I: Iterator<Item=Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr{
    parse_binary_expr(it,0)
}

fn binary_op_symbol(c: &str) -> Option<(u32,u32,BinaryOp)>{
    match c{
        "=" => Some((2,1,BinaryOp::Assign)),
        "+=" => Some((2,1,BinaryOp::AddAssign)),
        "-=" => Some((2,1,BinaryOp::SubAssign)),
        "*=" => Some((2,1,BinaryOp::MulAssign)),
        "/=" => Some((2,1,BinaryOp::DivAssign)),
        "%=" => Some((2,1,BinaryOp::ModAssign)),
        "^=" => Some((2,1,BinaryOp::XorAssign)),
        "&=" => Some((2,1,BinaryOp::AndAssign)),
        "|=" => Some((2,1,BinaryOp::OrAssign)),
        "&&=" => Some((2,1,BinaryOp::LogicAndAssign)),
        "||=" => Some((2,1,BinaryOp::LogicOrAssign)),
        "<<=" => Some((2,1,BinaryOp::LeftShiftAssign)),
        ">>=" => Some((2,1,BinaryOp::RightShiftAssign)),
        "||" => Some((3,4,BinaryOp::LogicOr)),
        "&&" => Some((5,6,BinaryOp::LogicAnd)),
        "==" => Some((7,7,BinaryOp::CompareEq)),
        "<" => Some((7,7,BinaryOp::CompareLt)),
        ">" => Some((7,7,BinaryOp::CompareGt)),
        "!=" => Some((7,7,BinaryOp::CompareNe)),
        "<=" => Some((7,7,BinaryOp::CompareLe)),
        ">=" => Some((7,7,BinaryOp::CompareGe)),
        "|" => Some((9,10,BinaryOp::Or)),
        "^" => Some((11,12,BinaryOp::Xor)),
        "&" => Some((13,14,BinaryOp::And)),
        "+" => Some((15,16,BinaryOp::Add)),
        "-" => Some((15,16,BinaryOp::Sub)),
        "*" => Some((17,18,BinaryOp::Mul)),
        "/" => Some((17,18,BinaryOp::Div)),
        "%" => Some((17,18,BinaryOp::Mod)),
        _ => None,
    }
}

pub fn parse_binary_expr<I: Iterator<Item=Lexeme>>(it: &mut PeekMoreIterator<I>, precedence: u32) -> Expr{
    let mut lhs = parse_unary_expr(it);
    loop{
        match it.peek(){
            Some(Lexeme::Token { ty: TokenType::Symbol, tok, span }) => {
                if let Some((lbp, rbp, op)) = binary_op_symbol(tok){
                    match lbp.cmp(&precedence){
                        Ordering::Equal => panic!("Cannot chain comparison operators (at {:?})",span),
                        Ordering::Less => break,
                        Ordering::Greater => {},
                    }
                    it.next();
                    let rhs = parse_binary_expr(it, rbp);

                    lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
                }else{
                    break
                }
            }
            _ => break,
        }
    }
    lhs
}

pub fn parse_unary_expr<I: Iterator<Item=Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr{
    match it.peek().unwrap(){
        Lexeme::Token { ty: TokenType::Symbol, tok, .. } if tok=="*" => {
            it.next();
            Expr::Unary(UnaryOp::Deref, Box::new(parse_unary_expr(it)))
        }
        Lexeme::Token { ty: TokenType::Symbol, tok, .. } if tok=="!" => {
            it.next();
            Expr::Unary(UnaryOp::Not, Box::new(parse_unary_expr(it)))
        }
        Lexeme::Token { ty: TokenType::Symbol, tok, .. } if tok=="-" => {
            it.next();
            Expr::Unary(UnaryOp::Neg, Box::new(parse_unary_expr(it)))
        }
        Lexeme::Token { ty: TokenType::Symbol, tok, .. } if tok=="&" => {
            it.next();
            match it.peek().unwrap(){
                Lexeme::Token { ty: TokenType::Identifier(IdentifierKind::Keyword), tok, .. } if tok=="mut" => {
                    it.next();
                    Expr::Unary(UnaryOp::AddrOf(Mutability::Mut),Box::new(parse_unary_expr(it)))
                }
                Lexeme::Token { ty: TokenType::Identifier(IdentifierKind::Keyword), tok, .. } if tok=="in" => {
                    it.next();
                    Expr::Unary(UnaryOp::SignalOf(SignalDirection::In),Box::new(parse_unary_expr(it)))
                }
                Lexeme::Token { ty: TokenType::Identifier(IdentifierKind::Keyword), tok, .. } if tok=="inout" => {
                    it.next();
                    Expr::Unary(UnaryOp::SignalOf(SignalDirection::InOut),Box::new(parse_unary_expr(it)))
                }
                Lexeme::Token { ty: TokenType::Identifier(IdentifierKind::Keyword), tok, .. } if tok=="out" => {
                    it.next();
                    Expr::Unary(UnaryOp::SignalOf(SignalDirection::Out),Box::new(parse_unary_expr(it)))
                }
                _ => Expr::Unary(UnaryOp::AddrOf(Mutability::Const),Box::new(parse_unary_expr(it)))
            }
        }
        _ => {
            let mut val = parse_simple_expr(it);
            loop{
                match it.peek(){
                    Some(Lexeme::Token { ty: TokenType::Symbol, tok, .. }) if tok=="?" => {
                        it.next();
                        val = Expr::Unary(UnaryOp::Try, Box::new(val));
                    }
                    Some(Lexeme::Token{ty: TokenType::Symbol, tok, ..}) if tok=="." => {
                        it.next();
                        match it.next().unwrap(){
                            Lexeme::Token { ty: TokenType::Identifier(IdentifierKind::Keyword), tok, .. } if tok=="await" => {
                                match it.peek(){
                                    Some(Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Keyword),tok,..}) if tok=="rising_edge" => {
                                        it.next();
                                        val = Expr::Unary(UnaryOp::AwaitSignal(TriggerType::RisingEdge), Box::new(val))
                                    }
                                    Some(Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Keyword),tok,..}) if tok=="falling_edge" => {
                                        it.next();
                                        val = Expr::Unary(UnaryOp::AwaitSignal(TriggerType::RisingEdge), Box::new(val))
                                    }
                                    Some(Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Keyword),tok,..}) if tok=="low_signal" => {
                                        it.next();
                                        val = Expr::Unary(UnaryOp::AwaitSignal(TriggerType::LowSignal), Box::new(val))
                                    }
                                    Some(Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Keyword),tok,..}) if tok=="hi_signal" => {
                                        it.next();
                                        val = Expr::Unary(UnaryOp::AwaitSignal(TriggerType::HighSignal), Box::new(val))
                                    }
                                    Some(Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Keyword),tok,..}) if tok=="biedge" => {
                                        it.next();
                                        val = Expr::Unary(UnaryOp::AwaitSignal(TriggerType::Biedge), Box::new(val))
                                    }
                                    _ => val = Expr::Unary(UnaryOp::Await,Box::new(val))
                                }
                            }
                            Lexeme::Token{ty: TokenType::Identifier(IdentifierKind::Normal | IdentifierKind::Raw), tok, ..} => {
                                match it.peek(){
                                    Some(Lexeme::Group { ty: GroupType::Parentheses, .. }) => {
                                        match it.next().unwrap(){
                                            Lexeme::Group { inner, .. } => {
                                                let mut it = inner.into_iter().peekmore();
                                                let mut args = Vec::new();
                                                loop{
                                                    if it.peek().is_none(){
                                                        break
                                                    }
                                                    args.push(parse_expression(&mut it));

                                                    match it.next(){
                                                        Some(Lexeme::Token { ty: TokenType::Symbol, tok, .. }) if tok=="," => continue,
                                                        None => break,
                                                        Some(tok) => panic!("Expected a comma or end of argument list, got {:?}",tok)
                                                    }
                                                }
                                                val = Expr::MethodCall { base: Box::new(val), method: tok, args };
                                            }
                                            _ => unreachable!()
                                        }
                                    }
                                    _ => val = Expr::MemberAccess(Box::new(val), FieldName::Named(tok))
                                }
                            }
                            Lexeme::Token{ty: TokenType::Number, tok, ..} => {
                                val = Expr::MemberAccess(Box::new(val), FieldName::Tuple(tok.parse().unwrap()))
                            }
                            tok => panic!("Expected await or a field or method name, got {:?}",tok)
                        }
                    }
                    Some(Lexeme::Group { ty: GroupType::Brackets, .. }) => {
                        match it.next().unwrap(){
                            Lexeme::Group{inner, ..} => {
                                let mut it = inner.into_iter().peekmore();
                                val = Expr::Subscript { base: Box::new(val), index: Box::new(parse_expression(&mut it)) };
                            }
                            _ => unreachable!()
                        }
                    }
                    Some(Lexeme::Group { ty: GroupType::Parentheses, .. }) => {
                        match it.next().unwrap(){
                            Lexeme::Group { inner, .. } => {
                                let mut it = inner.into_iter().peekmore();
                                let mut args = Vec::new();
                                loop{
                                    if it.peek().is_none(){
                                        break
                                    }
                                    args.push(parse_expression(&mut it));

                                    match it.next(){
                                        Some(Lexeme::Token { ty: TokenType::Symbol, tok, .. }) if tok=="," => continue,
                                        None => break,
                                        Some(tok) => panic!("Expected a comma or end of argument list, got {:?}",tok)
                                    }
                                }
                                val = Expr::Call { base: Box::new(val), args }
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => break val
                }
            }
        }
    }
}

pub fn parse_simple_expr<I: Iterator<Item=Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr{
    todo!()
}