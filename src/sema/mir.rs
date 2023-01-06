use std::hash::BuildHasherDefault;

use crate::utils::*;

use crate::parse::Pattern;
pub use crate::parse::{BinaryOp, TriggerType, UnaryOp, Mutability};
pub use crate::sema::{ConstVal, IntType, LogicType, Type,FieldName};
pub use crate::sema::tycheck::ValueCategory;
use crate::sema::{Definitions, FunctionType,DefId};

use crate::sema::hir;

use fxhash::{FxHashMap,FxHashSet};

use super::hir::HirVarId;
use super::tycheck::{ThirExpr, ThirStatement, ThirExprInner};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SsaVarId(u32);

impl core::fmt::Debug for SsaVarId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_fmt(format_args!("_{}",self.0))
    }
}

impl core::fmt::Display for SsaVarId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_fmt(format_args!("_{}",self.0))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaExpr{
    pub ty: Type,
    pub cat: ValueCategory,
    pub inner: SsaExprInner,
}

impl SsaExpr{
    pub fn has_side_effects(&self) -> bool{
        self.inner.has_side_effects()
    }
}

impl core::fmt::Display for SsaExpr{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        self.inner.fmt(f)?;
        f.write_str(": ")?;
        self.ty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaFieldInit{
    pub name: FieldName,
    pub init: SsaExpr,
}

impl core::fmt::Display for SsaFieldInit{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        self.name.fmt(f)?;
        f.write_str(": ")?;
        self.init.fmt(f)
    }
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaExprInner {
    Local(SsaVarId),
    Const(ConstVal),
    BinaryOp(BinaryOp, Box<SsaExpr>, Box<SsaExpr>),
    UnaryOp(UnaryOp, Box<SsaExpr>),
    Cast(Box<SsaExpr>, Box<Type>),
    Move(Box<SsaExpr>),
    Copy(Box<SsaExpr>),
    Read(Box<SsaExpr>),
    Unreachable,
    Ctor(DefId, Vec<SsaFieldInit>, Option<Box<SsaExpr>>)
}

impl SsaExprInner{
    pub fn has_side_effects(&self) -> bool{
        match self{
            Self::Local(_) => false,
            Self::Const(_) => false,
            Self::Unreachable => false,
            Self::BinaryOp(_, left, right) => left.has_side_effects()||right.has_side_effects(),
            Self::UnaryOp(_, inner) => inner.has_side_effects(),
            Self::Cast(inner,_) => inner.has_side_effects(),
            Self::Move(inner) => inner.has_side_effects(),
            Self::Copy(inner) => inner.has_side_effects(),
            Self::Read(inner) => inner.has_side_effects(),
            Self::Ctor(_, fields, rest) => {
                fields.iter().any(|field| field.init.has_side_effects())||rest.as_ref().map(|stat| stat.has_side_effects()).unwrap_or(false)
            }
        }
    }
}

impl core::fmt::Display for SsaExprInner{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            SsaExprInner::Local(var) => var.fmt(f),
            SsaExprInner::Const(val) => val.fmt(f),
            SsaExprInner::Move(val) => {
                f.write_str("move ")?;
                val.fmt(f)
            }
            SsaExprInner::Copy(val) => {
                f.write_str("copy ")?;
                val.fmt(f)
            }
            SsaExprInner::Read(val) => {
                f.write_str("read ")?;
                val.fmt(f)
            }
            SsaExprInner::Cast(val, ty) => {
                val.fmt(f)?;
                f.write_str(" as ")?;
                ty.fmt(f)
            }
            SsaExprInner::UnaryOp(op, inner) => {
                f.write_str("(")?;
                op.fmt(f)?;
                inner.fmt(f)?;
                f.write_str(")")
            }
            SsaExprInner::BinaryOp(op, left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
                op.fmt(f)?;
                right.fmt(f)?;
                f.write_str(")")
            }
            SsaExprInner::Unreachable => f.write_str("unreachable"),
            SsaExprInner::Ctor(defid, fields, rest) => {
                defid.fmt(f)?;
                f.write_str("{")?;
                let mut sep = "";
                for field in fields{
                    f.write_str(sep)?;
                    sep = ", ";
                    field.fmt(f)?;
                }

                if let Some(rest) = rest{
                    f.write_str(sep)?;
                    rest.fmt(f)?;
                }
                f.write_str("}")
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaJump {
    pub target_bb: u32,
    pub remaps: Vec<(SsaVarId, SsaExpr)>,
}

impl core::fmt::Display for SsaJump{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("@")?;
        self.target_bb.fmt(f)?;
        f.write_str(" [")?;

        let mut sep = "";
        for (var,expr) in &self.remaps{
            f.write_str(sep)?;
            sep = ", ";
            var.fmt(f)?;
            f.write_str(" => ")?;
            expr.fmt(f)?;
        }
        f.write_str("]")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaBranch {
    pub cond: SsaExpr,
    pub if_true_target: SsaJump,
    pub else_target: SsaJump,
}

impl core::fmt::Display for SsaBranch{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("branch if ")?;
        self.cond.fmt(f)?;
        f.write_str(" ")?;
        self.if_true_target.fmt(f)?;
        f.write_str(" else ")?;
        self.else_target.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaFunctionCall {
    pub func: SsaExpr,
    pub args: Vec<SsaExpr>,
    pub ret_place: Option<SsaVarId>,
    pub target: SsaJump,
}

impl core::fmt::Display for SsaFunctionCall{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("call ")?;
        self.func.fmt(f)?;
        f.write_str("(")?;
        let mut sep = "";
        for arg in &self.args{
            f.write_str(sep)?;
            sep = ", ";
            arg.fmt(f)?;
        }
        f.write_str(")")?;

        if let Some(place) = self.ret_place{
            f.write_str(" => ")?;
            place.fmt(f)?;
        }

        f.write_str(" next ")?;
        self.target.fmt(f)

    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaAwait {
    pub future: SsaExpr,
    pub cur_future: SsaVarId,
    pub ret_place: Option<SsaVarId>,
    pub resume_target: SsaJump,
}

impl core::fmt::Display for SsaAwait{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("await(in ")?;
        self.cur_future.fmt(f)?;
        f.write_str(") ")?;
        self.future.fmt(f)?;

        if let Some(place) = self.ret_place{
            f.write_str(" => ")?;
            place.fmt(f)?;
        }
        f.write_str(" resume ")?;
        self.resume_target.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaAwaitSignal {
    pub signal: SsaExpr,
    pub trigger: TriggerType,
    pub resume_target: SsaJump,
}

impl core::fmt::Display for SsaAwaitSignal{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("await(")?;
        self.trigger.fmt(f)?;
        f.write_str(") ")?;
        self.signal.fmt(f)?;
        f.write_str(" resume ")?;
        self.resume_target.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaSwitch {
    pub cond: SsaExpr,
    pub default_target: SsaJump,
    pub case_targets: Vec<(ConstVal, SsaJump)>,
}

impl core::fmt::Display for SsaSwitch{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("switch ")?;
        self.cond.fmt(f)?;
        for (val,target) in &self.case_targets{
            f.write_str("case ")?;
            val.fmt(f)?;
            f.write_str(" ")?;
            target.fmt(f)?;
        }

        f.write_str("default ")?;
        self.default_target.fmt(f)
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SsaTailcall{
    pub func: SsaExpr,
    pub args: Vec<SsaExpr>
}

impl core::fmt::Display for SsaTailcall{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("tailcall ")?;
        self.func.fmt(f)?;
        f.write_str("(")?;
        let mut sep = "";
        for arg in &self.args{
            f.write_str(sep)?;
            sep = ", ";
            arg.fmt(f)?;
        }
        f.write_str(")")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaTerminator {
    Jump(SsaJump),
    Branch(SsaBranch),
    FunctionCall(SsaFunctionCall),
    TailCall(SsaTailcall),
    Await(SsaAwait),
    AwaitSignal(SsaAwaitSignal),
    Unreachable,
    Return(SsaExpr),
    Switch(SsaSwitch),
}



impl core::fmt::Display for SsaTerminator{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            Self::Jump(jump) => {
                f.write_str("jump ")?;
                jump.fmt(f)
            }
            Self::Branch(branch) => branch.fmt(f),
            Self::FunctionCall(fncall) => fncall.fmt(f),
            Self::TailCall(tailcall) => tailcall.fmt(f),
            Self::Await(aw) => aw.fmt(f),
            Self::AwaitSignal(sig) => sig.fmt(f),
            Self::Unreachable => f.write_str("unreachable"),
            Self::Return(expr) => {
                f.write_str("return ")?;
                expr.fmt(f)
            }
            Self::Switch(switch) => switch.fmt(f),
        }
    }
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaStatement {
    Declare { var: SsaVarId, ty: Type, init: SsaExpr },
    StoreDead(SsaVarId),
    Pin(SsaVarId),
}

impl core::fmt::Display for SsaStatement{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            Self::Declare { var, ty, init } => {
                f.write_str("let ")?;
                var.fmt(f)?;
                f.write_str(": ")?;
                ty.fmt(f)?;
                f.write_str(" = ")?;
                init.fmt(f)
            }
            Self::StoreDead(var) => {
                f.write_str("StoreDead(")?;
                var.fmt(f)?;
                f.write_str(");")
            }
            Self::Pin(id) => {
                f.write_str("Pin(")?;
                id.fmt(f)?;
                f.write_str(");")
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: u32,
    pub stats: Vec<SsaStatement>,
    pub term: SsaTerminator,
}

impl core::fmt::Display for BasicBlock{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_str("'")?;
        self.id.fmt(f)?;
        f.write_str(": {\n")?;
        for stat in &self.stats{
            f.write_str("\t")?;
            stat.fmt(f)?;
            f.write_str("\n")?;
        }
        f.write_str("\t")?;
        self.term.fmt(f)?;
        f.write_str("\n")?;
        f.write_str("}")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct UnbuiltBasicBlock {
    pub id: u32,
    pub stats: Vec<SsaStatement>,
}

impl UnbuiltBasicBlock{
    pub fn build(self, term: SsaTerminator) -> BasicBlock{
        let Self{id,stats} = self;
        BasicBlock{id,stats,term}
    }

    pub fn build_and_reset(&mut self,term: SsaTerminator) -> BasicBlock{
        core::mem::take(self).build(term)
    }
}

pub struct FunctionConvert<'a> {
    defs: &'a mut Definitions,
    nextbb: u32,
    nextlocal: u32,
    fnty: &'a FunctionType,
    basic_blocks: Vec<BasicBlock>,
    cur_bb: UnbuiltBasicBlock,
    local_names: FxHashMap<HirVarId, SsaVarId>,
    pinned_locals: FxHashSet<SsaVarId>,
    known_locals: Vec<HirVarId>,
    var_tys: FxHashMap<HirVarId,Type>
}

impl<'a> FunctionConvert<'a> {
    fn get_local_read_expr(&self, varid: HirVarId) -> SsaExpr{
        let oldlocal = self.local_names[&varid];
        let ty = self.var_tys[&varid].clone();
        SsaExpr { ty: ty.clone(), cat: ValueCategory::Value, inner: SsaExprInner::Move(Box::new(SsaExpr{ty, cat: ValueCategory::Place(Mutability::Const, super::tycheck::Movability::Movable), inner: SsaExprInner::Local(oldlocal)})) }
    }
    pub fn new(fnty: &'a FunctionType, defs: &'a mut Definitions) -> Self {
        let nextlocal = fnty.params.len() as u32;
        Self {
            defs,
            nextbb: 1,
            nextlocal,
            fnty,
            basic_blocks: Vec::new(),
            cur_bb: UnbuiltBasicBlock {
                id: 0,
                stats: Vec::new(),
            },
            local_names: FxHashMap::with_hasher(BuildHasherDefault::default()),
            pinned_locals: FxHashSet::with_hasher(Default::default()),
            known_locals: Vec::new(),
            var_tys: FxHashMap::with_hasher(Default::default())
        }
    }

    pub fn convert_thir_expr(&mut self, expr: ThirExpr) -> SsaExpr{
        let ty = expr.ty;
        let cat = expr.cat;

        let inner = match expr.expr{
            ThirExprInner::Unreachable => SsaExprInner::Unreachable, // Optizations will reduce a basic-block that contains an unreachable expression to an unreachable terminator
            ThirExprInner::Place2Val(val) => {
                let inner = self.convert_thir_expr(*val);
                if ty.is_type_copy(self.defs){
                    SsaExprInner::Copy(Box::new(inner))
                }else{
                    SsaExprInner::Move(Box::new(inner))
                }
            }
            ThirExprInner::ReadSignal(val) => {
                let inner = self.convert_thir_expr(*val);
                if ty.is_type_copy(self.defs){
                    SsaExprInner::Read(Box::new(inner))
                }else{
                    panic!("Signal Read expression {} cannot read non-copy type {}",inner,ty);
                }
            }
            ThirExprInner::Local(loc) => {
                let ssaloc = self.local_names[&loc];
                SsaExprInner::Local(ssaloc)
            }
            ThirExprInner::Const(val) => SsaExprInner::Const(val),
            ThirExprInner::Constructor(defid, fields,rest ) => {
                let fields = fields.into_iter().map(|expr|{
                    let init = self.convert_thir_expr(expr.init);
                    SsaFieldInit{name: expr.name, init}
                }).collect::<Vec<_>>();

                let rest = rest.map(|rest|{
                    Box::new(self.convert_thir_expr(*rest))
                });

                SsaExprInner::Ctor(defid, fields, rest)
            }
            expr => todo!("{}",expr)
        };
        SsaExpr { ty, cat, inner}
    }

    pub fn convert_thir_stat(&mut self, stat: ThirStatement){
        match stat{ 
            ThirStatement::Loop(stats) => {
                let loopbb = self.nextbb.fetch_and_increment();

                let known_locals = self.known_locals.clone();

                let mut remaps = Vec::new();

                for local in &known_locals{
                    if self.local_names.contains_key(local){
                        let newlocal = SsaVarId(self.nextlocal.fetch_and_increment());
                        remaps.push((newlocal,self.get_local_read_expr(*local)));
                        self.local_names.insert(*local,newlocal);
                    }
                }

                let jmp = SsaJump{target_bb: loopbb,remaps};

                let nextbb = self.cur_bb.build_and_reset(SsaTerminator::Jump(jmp));

                self.basic_blocks.push(nextbb);

                self.cur_bb.id = loopbb;

                for stat in stats{
                    self.convert_thir_stat(stat)
                }

                let mut remaps = Vec::new();


                for local in &known_locals{
                    if self.local_names.contains_key(local){
                        let newlocal = SsaVarId(self.nextlocal.fetch_and_increment());
                        remaps.push((newlocal,self.get_local_read_expr(*local)));
                        self.local_names.insert(*local,newlocal);
                    }
                }

                let jmp = SsaJump{target_bb: loopbb,remaps};

                let nextbb = self.cur_bb.build_and_reset(SsaTerminator::Jump(jmp));
                self.cur_bb.id = self.nextbb.fetch_and_increment();
                self.basic_blocks.push(nextbb);

                self.known_locals = known_locals;
            }
            ThirStatement::AwaitSignal(signal, trigger) => {
                let resumebb = self.nextbb.fetch_and_increment();

                let mut remaps = Vec::new();


                for local in &self.known_locals{
                    if self.local_names.contains_key(local){
                        let newlocal = SsaVarId(self.nextlocal.fetch_and_increment());
                        remaps.push((newlocal,self.get_local_read_expr(*local)));
                        self.local_names.insert(*local,newlocal);
                    }
                }
                let jmp = SsaJump{target_bb: resumebb,remaps};

                let signal = self.convert_thir_expr(signal);

                let nextbb = self.cur_bb.build_and_reset(SsaTerminator::AwaitSignal(SsaAwaitSignal{signal,trigger,resume_target: jmp}));
                self.cur_bb.id = resumebb;
                self.basic_blocks.push(nextbb);
            }
            ThirStatement::Discard(_) => {}
            ThirStatement::Return(expr) => {
                let expr = self.convert_thir_expr(expr);
                let nextbb = self.cur_bb.build_and_reset(SsaTerminator::Return(expr));
                self.cur_bb.id = self.nextbb.fetch_and_increment();
                self.basic_blocks.push(nextbb);
            }
            ThirStatement::Let { id, mutability, ty, init } => {
                if let Some(init) = init{
                    let var = SsaVarId(self.nextlocal.fetch_and_increment());
                    let init = self.convert_thir_expr(init);
                    if ty.has_destructor(self.defs){
                        self.pinned_locals.insert(var);
                    }
                    self.cur_bb.stats.push(SsaStatement::Declare { var, ty, init});
                    self.local_names.insert(id,var);

                    
                }
            }
            stat => todo!("{}",stat)
        }
    }

    pub fn build(self) -> Vec<BasicBlock>{
        self.basic_blocks
    }
}
