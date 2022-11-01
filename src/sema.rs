
use std::collections::HashMap;

use crate::lang::LangItem;
pub use crate::parse::{Mutability,SignalDirection,Safety};

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub struct DefId(u64);

#[derive(Clone,Debug)]
pub enum Definition{
    IncompleteType,
    IncompleteAlias,
    IncompleteFunction(FunctionType),
    IncompleteStatic(Type),
    Module(Module),
}

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub enum LogicType{
    Binary,
    Tristate,
    Ieee1364,
    Ieee1164,
}

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub struct IntType{
    pub logic: LogicType,
    pub bits: u16,
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub enum Type{
    UserDef(DefId),
    IntegerType(IntType),
    Logic(LogicType),
    Array(Box<Type>,Box<ConstVal>),
    GenericParam(u32),
    FnPtr(FunctionType),
    FnItem(DefId),
}

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub enum AsyncFnTy{
    Normal,
    Async,
    Proc,
    Entity,
}

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub enum LogicVal{
    StrongOne,
    WeakOne,
    StrongUnknown,
    DontCare,
    Uninitialized,
    WeakUnknown,
    HighImpedence,
    WeakZero,
    StrongZero,
}


#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub struct FunctionType{
    async_ty: AsyncFnTy,
    safety: Safety,
    constness: Mutability,
    params: Vec<Type>,
    retty: Box<Type>
}

#[derive(Clone,Debug,Hash,PartialEq,Eq)]
pub enum ConstVal{
    ScalarConst(u128),
    LargeScalar(Vec<u8>),
    LogicVal(LogicVal),
    GenericParam(u32),
    ArrayLit(Vec<ConstVal>),
    ArrayRepeat{base: Box<ConstVal>, repeat: Box<ConstVal>},
}



#[derive(Clone, Debug)]
pub struct Module{
    parent: DefId,
    types: HashMap<String,DefId>,
    values: HashMap<String,DefId>,
}

#[derive(Clone,Debug)]
pub struct Definitions{
    nextdefid: DefId,
    defs: HashMap<DefId,Definition>,
    crates: HashMap<String,Module>,
    lang_items: HashMap<LangItem,DefId>
}

impl Definitions{
    pub fn new() -> Self{
        Self { nextdefid: DefId(1), defs: HashMap::new(), crates: HashMap::new(), lang_items: HashMap::new() }
    }
    pub fn next_defid(&mut self) -> DefId{
        let ret = self.nextdefid;
        self.nextdefid.0 += 1;
        ret
    }

}