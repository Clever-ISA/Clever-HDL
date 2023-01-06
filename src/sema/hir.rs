use std::{collections::{BTreeMap, HashMap, VecDeque}, hash::BuildHasherDefault};

use parse::TriggerType;

pub use crate::parse::{BinaryOp,UnaryOp,Mutability};

use crate::parse::{self, PathComponent};

pub use crate::sema::{ConstVal,Type, FunctionType, DefId,FieldName};


use fxhash::FxHashMap;

use super::{Definition, Definitions};


#[derive(Copy,Clone, Hash,PartialEq,Eq)]
pub struct HirVarId(u32);

impl core::fmt::Debug for HirVarId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_fmt(format_args!("_{}",self.0))
    }
}

impl core::fmt::Display for HirVarId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_fmt(format_args!("_{}",self.0))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirFieldInit{
    pub name: FieldName,
    pub value: HirExpr,
}

impl core::fmt::Display for HirFieldInit{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        self.name.fmt(f)?;
        f.write_str(": ")?;
        self.value.fmt(f)
    }
}

#[derive(Clone, Debug, Hash,PartialEq,Eq)]
pub enum HirExpr{
    Local(HirVarId),
    BinaryOp(BinaryOp,Box<HirExpr>,Box<HirExpr>),
    UnaryOp(UnaryOp,Box<HirExpr>),
    Const(ConstVal),
    Cast(Box<HirExpr>,Type),
    Unreachable,
    Constructor(DefId,Vec<HirFieldInit>,Option<Box<HirExpr>>)
}

#[derive(Copy, Clone, Debug, Hash,PartialEq,Eq)]
pub enum ExprCategory{
    Lvalue,
    Rvalue,
    Signal
}


impl core::fmt::Display for HirExpr{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            HirExpr::Local(var) => var.fmt(f),
            HirExpr::BinaryOp(op, left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
                op.fmt(f)?;
                right.fmt(f)?;
                f.write_str(")")
            },
            HirExpr::UnaryOp(op, inner) => {
                op.fmt(f)?;
                inner.fmt(f)
            },
            HirExpr::Const(val) => val.fmt(f),
            HirExpr::Cast(expr, ty) => {
                f.write_str("(")?;
                expr.fmt(f)?;
                f.write_str(" as ")?;
                ty.fmt(f)?;
                f.write_str(")")
            },
            HirExpr::Unreachable => f.write_str("unreachable"),
            HirExpr::Constructor(defid, fields,tailinit) => {
                defid.fmt(f)?;
                f.write_str("{")?;
                let mut sep = "";

                for field in fields{
                    f.write_str(sep)?;
                    sep = ", ";
                    field.fmt(f)?;
                }

                if let Some(tailinit) = tailinit{
                    f.write_str(sep)?;
                    f.write_str("..")?;
                    tailinit.fmt(f)?;
                }

                f.write_str("}")
            }
        }
    }
}

#[derive(Clone, Debug, Hash,PartialEq,Eq)]
pub enum HirStatement{
    Let{
        id: HirVarId,
        mutability: Mutability,
        ty: Type,
        init: Option<HirExpr>,
    },
    Call{
        retplace: HirVarId,
        fnty: FunctionType,
        func: HirExpr,
        args: Vec<HirExpr>,
    },
    Return(HirExpr),
    Loop(Vec<HirStatement>),
    AwaitSignal(HirExpr, TriggerType),
    Discard(HirExpr,bool),
}


impl core::fmt::Display for HirStatement{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            HirStatement::Let { id, mutability, ty, init } => {
                f.write_str("let ")?;
                if let Mutability::Mut =  mutability{
                    f.write_str("mut ")?;
                }
                id.fmt(f)?;
                f.write_str(": ")?;
                ty.fmt(f)?;
                if let Some(init) = init{
                    f.write_str(" = ")?;
                    init.fmt(f)?;
                }
                f.write_str(";")
            },
            HirStatement::Call { retplace, func, args, .. } => {
                retplace.fmt(f)?;
                f.write_str(" = (")?;
                func.fmt(f)?;
                f.write_str(")(")?;
                let mut sep = "";
                for arg in args{
                    f.write_str(sep)?;
                    sep = ", ";
                    arg.fmt(f)?;
                }
                f.write_str(");")
            },  
            HirStatement::Return(expr) => {
                f.write_str("return ")?;
                expr.fmt(f)?;
                f.write_str(";")
            },
            HirStatement::Loop(stats) => {
                f.write_str("loop{\n")?;
                for stat in stats{
                    stat.fmt(f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            },
            HirStatement::AwaitSignal(signal, tt) => {
                f.write_str("(")?;
                signal.fmt(f)?;
                f.write_str(").await ")?;
                match tt{
                    TriggerType::Biedge => f.write_str("biedge;"),
                    TriggerType::FallingEdge => f.write_str("falling_edge;"),
                    TriggerType::RisingEdge => f.write_str("rising_edge;"),
                    TriggerType::HighSignal => f.write_str("high_signal;"),
                    TriggerType::LowSignal => f.write_str("low_signal;")
                }
            },
            HirStatement::Discard(expr,_) => {
                expr.fmt(f)?;
                f.write_str(";")
            },
        }
    }
}

pub struct HirConverter<'a>{
    defs: &'a mut Definitions,
    local_tys: Vec<Type>,
    binding_names: FxHashMap<String,HirVarId>,
    stats: Vec<HirStatement>,
    storage_expr: Option<Box<dyn FnMut(&mut HirConverter<'a>,HirExpr)->HirStatement>>,
    storage_expr_stack: VecDeque<Box<dyn FnMut(&mut HirConverter<'a>,HirExpr)->HirStatement>>,
    outer_blocks: VecDeque<Vec<HirStatement>>,
    fnty: &'a FunctionType,
    cur_mod: DefId,
}

impl<'a> HirConverter<'a>{
    pub fn new(defs: &'a mut Definitions, fnty: &'a FunctionType, cur_mod: DefId) -> Self{
        Self{defs, local_tys: Vec::new(), binding_names: HashMap::with_hasher(BuildHasherDefault::default()), stats: Vec::new(), outer_blocks: VecDeque::new(), fnty, storage_expr: None, storage_expr_stack: VecDeque::new(), cur_mod}
    }

    pub fn allocate_local(&mut self, ty: Type) -> HirVarId{
        let local_num = self.local_tys.len() as u32;
        self.local_tys.push(ty);
        HirVarId(local_num)
    }

    pub fn bind_locals(&mut self, base: HirVarId, pat: &parse::Pattern){
        match pat{
            parse::Pattern::Discard => {},
            parse::Pattern::Ident(_, id) => {
                self.binding_names.insert(id.clone(),base);
            },
            parse::Pattern::Binding(_, _, _) => todo!(),
            parse::Pattern::Ref(_, _) => todo!(),
            parse::Pattern::Unref(_, _) => todo!(),
            parse::Pattern::Parentheses(_) => todo!(),
            parse::Pattern::Tuple(_) => todo!(),
            parse::Pattern::TupleStruct(_, _) => todo!(),
            parse::Pattern::Struct(_, _) => todo!(),
            parse::Pattern::Const(_) => todo!(),
            parse::Pattern::RangeInclusive(_, _) => todo!(),
            parse::Pattern::RangeExclusive(_, _) => todo!(),
            parse::Pattern::Slice(_) => todo!(),
            parse::Pattern::StringLiteral(_, _) => todo!(),
            parse::Pattern::CharLiteral(_, _) => todo!(),
            parse::Pattern::IntLiteral(_) => todo!(),
            parse::Pattern::DotDotDot => todo!(),
            parse::Pattern::Or(_, _) => todo!(),
            parse::Pattern::SelfPat => todo!(),
        }
    }


    pub fn convert_block(&mut self, block: &parse::Block){
        match block{
            parse::Block::Normal(_) => todo!(),
            parse::Block::Unsafe(_) => todo!(),
            parse::Block::Const(_) => todo!(),
            parse::Block::Loop(items) => {
                let current_block = core::mem::take(&mut self.stats);
                self.outer_blocks.push_back(current_block);
                if let Some(storage_expr) = self.storage_expr.replace(Box::new(|_,expr| HirStatement::Discard(expr,true))){
                    self.storage_expr_stack.push_back(storage_expr);
                }
                for item in items{
                    self.convert_statement(item);
                }
                let current_block = self.outer_blocks.pop_back().unwrap();
                self.storage_expr = self.storage_expr_stack.pop_back();
                let current_block = core::mem::replace(&mut self.stats, current_block);
                self.stats.push(HirStatement::Loop(current_block));
            },
            parse::Block::Async(_) => todo!(),
            parse::Block::While { control, block } => todo!(),
            parse::Block::If { control, block, elses } => todo!(),
            parse::Block::MacroExpansion { name, inner } => todo!(),
            parse::Block::Match { ctrl, arms } => todo!(),
            parse::Block::Try(_) => todo!(),
        }
    }

    pub fn convert_statement(&mut self, stat: &parse::BlockItem){
        match stat{
            parse::BlockItem::Item(_) => todo!(),
            parse::BlockItem::Expr(e) => {
                let ret = self.convert_expression(e);
                if let Some(mut f) = self.storage_expr.take(){
                    let stat = f(self,ret);
                    self.stats.push(stat);
                    self.storage_expr = Some(f)
                }else{
                    self.stats.push(HirStatement::Return(ret))
                }
            },
            parse::BlockItem::Discard(e) => {
                let expr = self.convert_expression(e);
                self.stats.push(HirStatement::Discard(expr,true))
            },
            parse::BlockItem::Let { pattern, ty, value } => {
                if let parse::Pattern::Discard = pattern{
                    if let Some(value) = value{
                        let expr = self.convert_expression(value);
                        self.stats.push(HirStatement::Discard(expr, ty.is_some()))
                    }
                }else{
                    let ty = ty.as_ref().map(|ty| super::convert_type(self.defs, self.cur_mod, ty)).unwrap_or(Type::Inferable);
                    let varid = self.allocate_local(ty.clone());


                    let value = value.as_ref().map(|expr|self.convert_expression(expr));


                    self.stats.push(HirStatement::Let { id: varid, mutability: Mutability::Mut, ty, init: value });

                    self.bind_locals(varid, pattern);

                }
            },
            parse::BlockItem::Block(b) => {
                self.convert_block(b);
            },
            parse::BlockItem::MacroExpansionStat { .. } => unreachable!(),
        }
    }

    pub fn convert_expression(&mut self, expr: &parse::Expr) -> HirExpr{
        match expr{
            parse::Expr::Block(b) => {
                self.convert_block(b);
                match b{
                    parse::Block::Loop(_) => HirExpr::Unreachable,
                    parse::Block::Normal(_) => todo!(),
                    parse::Block::Unsafe(_) => todo!(),
                    parse::Block::Const(_) => todo!(),
                    parse::Block::Async(_) => todo!(),
                    parse::Block::While { control, block } => todo!(),
                    parse::Block::If { control, block, elses } => todo!(),
                    parse::Block::MacroExpansion { name, inner } => todo!(),
                    parse::Block::Match { ctrl, arms } => todo!(),
                    parse::Block::Try(_) => todo!(),
                    
                }
            },
            parse::Expr::LetExpr(_, _) => todo!(),
            parse::Expr::Id(id) => {
                let mut res = None;
                if id.root.is_none()&&id.components.len()==1{
                    match &id.components[..]{
                        [PathComponent::Id(id)] => {
                            if let Some(local) = self.binding_names.get(id){
                                res = Some(HirExpr::Local(*local));
                            }
                        },
                        _ => {}
                    }
                }
                if let Some(res) = res{
                    res
                }else{
                    HirExpr::Const(crate::sema::convert_top_const_expr(&mut self.defs, self.cur_mod, expr))
                }
            },
            parse::Expr::FunctionCall { func, args } => todo!(),
            parse::Expr::Cast(_, _) => todo!(),
            parse::Expr::StringLiteral(_, _) => todo!(),
            parse::Expr::CharLiteral(_, _) => todo!(),
            parse::Expr::Parentheses(_) => todo!(),
            parse::Expr::MacroExpansion { .. } => unreachable!(),
            parse::Expr::IntLiteral(_) => todo!(),
            parse::Expr::StructConstructor(expr, fields) => {
                let def = self.defs.find_type_def(self.cur_mod, expr);

                if let Some(def) = def{
                    match &self.defs.get_definition(def).def{
                        super::DefinitionInner::UserType(super::UserType::Struct(_)) | super::DefinitionInner::IncompleteType => {
                            let init = fields.iter().map(|init|{
                                let name = init.name.clone();
                                let expr = self.convert_expression(&init.expr);
                                HirFieldInit{name,value: expr}
                            }).collect::<Vec<_>>();
                            HirExpr::Constructor(def, init, None)
                        }
                        _ => panic!("Not a type {}",expr)
                    }
                }else{
                    todo!()
                }
            },
            parse::Expr::Field(_, _) => todo!(),
            parse::Expr::Await(_) => todo!(),
            parse::Expr::AwaitSignal(tt, inner) => {
                let hir = self.convert_expression(inner);
                self.stats.push(HirStatement::AwaitSignal(hir, *tt));
                HirExpr::Const(ConstVal::UNIT)
            },
            parse::Expr::Return(_) => todo!(),
            parse::Expr::Break(_, _) => todo!(),
            parse::Expr::Continue(_) => todo!(),
            parse::Expr::Yield(_) => todo!(),
            parse::Expr::Yeet(_) => todo!(),
            parse::Expr::Try(_) => todo!(),
            parse::Expr::BinaryOp(_, _, _) => todo!(),
            parse::Expr::UnaryOp(_, _) => todo!(),
            parse::Expr::ArrayIndex { base, index } => todo!(),
            parse::Expr::ArrayCtor(_) => todo!(),
            parse::Expr::TypeAscription(_, _) => todo!(),
            parse::Expr::RangeFull => todo!(),
            parse::Expr::TupleCtor(_) => todo!(),
        }
    }

    pub fn into_function(self) -> Vec<HirStatement>{
        self.stats
    }
}