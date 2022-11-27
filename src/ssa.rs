use crate::parse::Pattern;
pub use crate::parse::{BinaryOp, TriggerType, UnaryOp};
pub use crate::sema::{ConstVal, IntType, LogicType, Type};
use crate::sema::{Definitions, FunctionType};

use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaExpr {
    Local(u32),
    Const(ConstVal),
    BinaryOp(BinaryOp, Box<SsaExpr>, Box<SsaExpr>),
    UnaryOp(UnaryOp, Box<SsaExpr>),
    Cast(Box<SsaExpr>, Box<Type>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaJump {
    pub target_bb: u32,
    pub remaps: Vec<(u32, SsaExpr)>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaBranch {
    pub cond: SsaExpr,
    pub if_true_target: SsaJump,
    pub else_target: SsaJump,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaFunctionCall {
    pub func: SsaExpr,
    pub args: Vec<SsaExpr>,
    pub ret_place: Option<u32>,
    pub target: SsaJump,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaAwait {
    pub future: SsaExpr,
    pub cur_future: u32,
    pub ret_place: Option<u32>,
    pub resume_target: SsaJump,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaAwaitSignal {
    pub signal: SsaExpr,
    pub trigger: TriggerType,
    pub resume_target: SsaJump,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaSwitch {
    pub cond: SsaExpr,
    pub default_target: SsaJump,
    pub case_targets: Vec<(ConstVal, SsaJump)>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaTerminator {
    Jump(SsaJump),
    Branch(SsaBranch),
    FunctionCall(SsaFunctionCall),
    TailCall(SsaFunctionCall),
    Await(SsaAwait),
    AwaitSignal(SsaAwaitSignal),
    Unreachable,
    Return(SsaExpr),
    Switch(SsaSwitch),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaStatement {
    Declare { num: u32, ty: Type, init: SsaExpr },
    StoreDead(u32),
    Discard(SsaExpr),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: u32,
    pub stats: Vec<SsaStatement>,
    pub term: SsaTerminator,
    pub local_tys: Vec<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnbuiltBasicBlock {
    pub id: u32,
    pub stats: Vec<SsaStatement>,
    pub term: Option<SsaTerminator>,
}

pub struct FunctionConvert<'a> {
    defs: &'a mut Definitions,
    local_tys: Vec<Type>,
    nextbb: u32,
    nextlocal: u32,
    fnty: &'a FunctionType,
    basic_blocks: Vec<BasicBlock>,
    cur_bb: UnbuiltBasicBlock,
    local_names: HashMap<String, u32>,
}

impl<'a> FunctionConvert<'a> {
    pub fn new(fnty: &'a FunctionType, defs: &'a mut Definitions) -> Self {
        let local_tys = fnty.params.iter().cloned().collect::<Vec<_>>();
        let nextlocal = local_tys.len() as u32;
        Self {
            defs,
            local_tys,
            nextbb: 1,
            nextlocal,
            fnty,
            basic_blocks: Vec::new(),
            cur_bb: UnbuiltBasicBlock {
                id: 0,
                stats: Vec::new(),
                term: None,
            },
            local_names: HashMap::new(),
        }
    }

    pub fn convert_binding(&mut self, local: u32, pat: &crate::parse::Pattern){
        match pat{
            Pattern::Ident(_, name) => {
                self.local_names.insert(name.clone(),local);
            },
            pat => todo!("{pat:?}")
        }
    }

    pub fn convert_expr(&mut self, expr: &crate::parse::Expr){
        match expr{
            crate::parse::Expr::Block(b) => todo!(),
            crate::parse::Expr::LetExpr(_, _) => todo!(),
            crate::parse::Expr::Id(_) => todo!(),
            crate::parse::Expr::FunctionCall { func, args } => todo!(),
            crate::parse::Expr::Cast(_, _) => todo!(),
            crate::parse::Expr::StringLiteral(_, _) => todo!(),
            crate::parse::Expr::CharLiteral(_, _) => todo!(),
            crate::parse::Expr::Parentheses(_) => todo!(),
            crate::parse::Expr::MacroExpansion { target, args } => todo!(),
            crate::parse::Expr::IntLiteral(_) => todo!(),
            crate::parse::Expr::StructConstructor(_, _) => todo!(),
            crate::parse::Expr::Field(_, _) => todo!(),
            crate::parse::Expr::Await(_) => todo!(),
            crate::parse::Expr::AwaitSignal(_, _) => todo!(),
            crate::parse::Expr::Return(_) => todo!(),
            crate::parse::Expr::Break(_, _) => todo!(),
            crate::parse::Expr::Continue(_) => todo!(),
            crate::parse::Expr::Yield(_) => todo!(),
            crate::parse::Expr::Yeet(_) => todo!(),
            crate::parse::Expr::Try(_) => todo!(),
            crate::parse::Expr::BinaryOp(_, _, _) => todo!(),
            crate::parse::Expr::UnaryOp(_, _) => todo!(),
            crate::parse::Expr::ArrayIndex { base, index } => todo!(),
            crate::parse::Expr::ArrayCtor(_) => todo!(),
            crate::parse::Expr::TypeAscription(_, _) => todo!(),
            crate::parse::Expr::RangeFull => todo!(),
            crate::parse::Expr::TupleCtor(_) => todo!(),
        }
    }
}
