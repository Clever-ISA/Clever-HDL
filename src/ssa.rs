
pub use crate::parse::{BinaryOp,UnaryOp,TriggerType};
pub use crate::sema::{Type,IntType,LogicType,ConstVal};


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaExpr{
    Local(u32),
    Const(ConstVal),
    BinaryOp(BinaryOp,Box<SsaExpr>,Box<SsaExpr>),
    UnaryOp(BinaryOp,Box<SsaExpr>),
}


#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaJump{
    pub target_bb: u32,
    pub remaps: Vec<(u32,SsaExpr)>
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaBranch{
    pub cond: SsaExpr,
    pub if_true_target: SsaJump,
    pub else_target: SsaJump,
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaFunctionCall{
    pub func: SsaExpr,
    pub args: Vec<SsaExpr>,
    pub ret_place: Option<u32>,
    pub target: SsaJump,
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaAwait{
    pub future: SsaExpr,
    pub cur_future: u32,
    pub ret_place: Option<u32>,
    pub resume_target: SsaJump,
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaAwaitSignal{
    pub signal: SsaExpr,
    pub trigger: TriggerType,
    pub resume_target: SsaJump,
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct SsaSwitch{
    pub cond: SsaExpr,
    pub default_target: SsaJump,
    pub case_targets: Vec<(ConstVal,SsaJump)>
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub enum SsaTerminator{
    Jump(SsaJump),
    Branch(SsaBranch),
    FunctionCall(SsaFunctionCall),
    Await(SsaAwait),
    AwaitSignal(SsaAwaitSignal),
    Unreachable,
    Return(SsaExpr),
    Switch(SsaSwitch)
}


#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub enum SsaStatement{
    Declare{
        num: u32,
        ty: Type,
        init: SsaExpr,
    },
    StoreDead(u32),
    Discard(SsaExpr),
    
}