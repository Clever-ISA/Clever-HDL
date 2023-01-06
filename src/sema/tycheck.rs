use std::hash::BuildHasher;

use fxhash::FxHashMap;
use replace_with::replace_with_or_abort;

pub use super::{Type,DefId};
pub use crate::parse::{UnaryOp,BinaryOp};
use super::{Definitions,DefinitionInner, FunctionType, hir::{HirStatement, HirExpr, HirVarId}, Mutability, Safety, ConstVal, SignalDirection, FieldName};

use crate::parse::TriggerType;

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub enum Movability{
    Movable,
    Immovable,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValueCategory{
    Value,
    Place(Mutability,Movability),
    SignalPlace(SignalDirection)
}




#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypecheckError{
    FailedToUnify,
    InferenceError,
    NoCoercion,
    CannotMove,
    Private,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnifyResult{
    ExactMatch,
    InferenceChance,
    CoercionApplied,
    Unsize,
}

fn unify_type(defs: &mut Definitions, against_ty: &mut Type,expr_ty: &mut Type, is_unsafe: bool) -> Result<UnifyResult,TypecheckError>{
    match (against_ty,expr_ty){
        (Type::Inferable,Type::Inferable) => Ok(UnifyResult::ExactMatch),
        (against_ty @ Type::Inferable,ty) =>{
            *against_ty = ty.clone();
            Ok(UnifyResult::InferenceChance)
        }
        (ty,expr_ty @ Type::Inferable) => {
            *expr_ty = ty.clone();
            Ok(UnifyResult::InferenceChance)
        }
        (Type::InferableInteger,Type::InferableInteger) => Ok(UnifyResult::ExactMatch),
        (against_ty @ Type::InferableInteger,Type::IntegerType(intty)) => {
            *against_ty = Type::IntegerType(*intty);
            Ok(UnifyResult::InferenceChance)
        }
        (Type::IntegerType(intty),expr_ty @ Type::InferableInteger) => {
            *expr_ty = Type::IntegerType(*intty);
            Ok(UnifyResult::InferenceChance)
        }
        (Type::InferableInteger,_) | (_,Type::InferableInteger) => {
            Err(TypecheckError::InferenceError)
        },
        (against_ty @ Type::IncompleteAlias(_),other) => {
            let defid = if let Type::IncompleteAlias(defid) = against_ty{
                *defid
            }else{
                unreachable!()
            };
            let def = defs.get_definition(defid);

            match &mut def.def{
                DefinitionInner::Alias(ty) => {
                    let mut ty = ty.clone();
                    let res = unify_type(defs,&mut ty,other,is_unsafe);
                    if res.is_ok(){
                        *against_ty = ty.clone();
                    }
                    res
                }
                _ => unreachable!()
            }
        },
        (ty1,ty2) if ty1==ty2 => {
            Ok(UnifyResult::ExactMatch)
        }
        (against,expr_ty @ Type::IncompleteAlias(_)) => {
            let defid = if let Type::IncompleteAlias(defid) = expr_ty{
                *defid
            }else{
                unreachable!()
            };
            let def = defs.get_definition(defid);

            match &mut def.def{
                DefinitionInner::Alias(ty) => {
                    let mut ty = ty.clone();
                    let res = unify_type(defs,against,&mut ty,is_unsafe);
                    if res.is_ok(){
                        *expr_ty = ty.clone();
                    }
                    res
                }
                _ => unreachable!()
            }
        },
        (ty,Type::Never) => {
            Ok(UnifyResult::CoercionApplied)
        }
        (Type::Pointer(mut1, against_ty),Type::Pointer(mut2 @ Mutability::Mut, expr_ty)) => {
            match unify_type(defs, against_ty, expr_ty,is_unsafe)?{
                UnifyResult::CoercionApplied => Err(TypecheckError::NoCoercion),
                UnifyResult::ExactMatch | UnifyResult::InferenceChance if mut1!=mut2 => Ok(UnifyResult::CoercionApplied),
                UnifyResult::Unsize => Ok(UnifyResult::CoercionApplied),
                e => Ok(e)
            }
        }
        (Type::Pointer(Mutability::Const, against_ty),Type::Pointer(Mutability::Const, expr_ty)) => {
            match unify_type(defs, against_ty, expr_ty,is_unsafe)?{
                UnifyResult::CoercionApplied => Err(TypecheckError::NoCoercion),
                UnifyResult::Unsize => Ok(UnifyResult::CoercionApplied),
                r => Ok(r)
            }
        }
        (Type::FnPtr(against_fnty),Type::FnItem(_,expr_fnty)) => {
            if against_fnty.async_ty!=expr_fnty.async_ty{
                return Err(TypecheckError::FailedToUnify)
            }else if against_fnty.params.len()!=expr_fnty.params.len(){
                return Err(TypecheckError::FailedToUnify)
            }

            for (against_param,expr_param) in against_fnty.params.iter_mut().zip(expr_fnty.params.iter_mut()){
                match unify_type(defs, against_param, expr_param, is_unsafe)?{
                    UnifyResult::CoercionApplied => return Err(TypecheckError::NoCoercion),
                    _ => {}
                }
            }

            match unify_type(defs, &mut against_fnty.retty, &mut expr_fnty.retty, is_unsafe)?{
                UnifyResult::CoercionApplied => Err(TypecheckError::NoCoercion),
                _ => Ok(UnifyResult::CoercionApplied),
            }
        }
        (Type::FnPtr(against_fnty),Type::FnPtr(expr_fnty)) => {
            if against_fnty.async_ty!=expr_fnty.async_ty{
                return Err(TypecheckError::FailedToUnify)
            }else if against_fnty.params.len()!=expr_fnty.params.len(){
                return Err(TypecheckError::FailedToUnify)
            }

            for (against_param,expr_param) in against_fnty.params.iter_mut().zip(expr_fnty.params.iter_mut()){
                match unify_type(defs, against_param, expr_param, is_unsafe)?{
                    UnifyResult::CoercionApplied => return Err(TypecheckError::NoCoercion),
                    _ => {}
                }
            }

            match unify_type(defs, &mut against_fnty.retty, &mut expr_fnty.retty, is_unsafe)?{
                UnifyResult::CoercionApplied => Err(TypecheckError::NoCoercion),
                r if against_fnty.safety!=expr_fnty.safety => Ok(UnifyResult::CoercionApplied),
                r => Ok(r)
            }
        }
        (Type::Slice(against_ty),Type::Slice(expr_ty)) => unify_type(defs, against_ty, expr_ty, is_unsafe),
        (Type::Array(against_ty, against_len),Type::Array(expr_ty, expr_len)) => todo!("array"),
        (Type::Slice(against_ty),Type::Array(expr_ty, _)) => {
            match unify_type(defs, against_ty, expr_ty, is_unsafe)?{
                UnifyResult::CoercionApplied | UnifyResult::Unsize => Err(TypecheckError::NoCoercion),
                _ => Ok(UnifyResult::Unsize)
            }
        }
        (_,_) => Err(TypecheckError::FailedToUnify),
    }
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirExpr{
    pub ty: Type,
    pub cat: ValueCategory,
    pub expr: ThirExprInner,
}

impl core::fmt::Display for ThirExpr{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        self.expr.fmt(f)?;
        f.write_str(": ")?;
        self.ty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash,PartialEq,Eq)]
pub struct ThirFieldInit{
    pub name: FieldName,
    pub init: ThirExpr
}

impl core::fmt::Display for ThirFieldInit{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        self.name.fmt(f)?;
        f.write_str(": ")?;
        self.init.fmt(f)
    }
}

#[derive(Clone, Debug, Hash,PartialEq,Eq)]
pub enum ThirExprInner{
    Local(HirVarId),
    BinaryOp(BinaryOp,Box<ThirExpr>,Box<ThirExpr>),
    UnaryOp(UnaryOp,Box<ThirExpr>),
    Const(ConstVal),
    Cast(Box<ThirExpr>,Type),
    Unreachable,
    ReadSignal(Box<ThirExpr>),
    Place2Val(Box<ThirExpr>),
    MaterializePlace(Box<ThirExpr>),
    Constructor(DefId,Vec<ThirFieldInit>,Option<Box<ThirExpr>>)
}

impl core::fmt::Display for ThirExprInner{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            ThirExprInner::Local(var) => var.fmt(f),
            ThirExprInner::BinaryOp(op, left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
                op.fmt(f)?;
                right.fmt(f)?;
                f.write_str(")")
            },
            ThirExprInner::UnaryOp(op, inner) => {
                op.fmt(f)?;
                inner.fmt(f)
            },
            ThirExprInner::Const(val) => val.fmt(f),
            ThirExprInner::Cast(expr, ty) => {
                f.write_str("(")?;
                expr.fmt(f)?;
                f.write_str(" as ")?;
                ty.fmt(f)?;
                f.write_str(")")
            },
            ThirExprInner::Unreachable => f.write_str("unreachable"),
            ThirExprInner::Place2Val(expr) => {
                f.write_str("place2val ")?;
                expr.fmt(f)
            },
            ThirExprInner::ReadSignal(expr) => {
                f.write_str("read ")?;
                expr.fmt(f)
            },
            ThirExprInner::MaterializePlace(expr) => {
                f.write_str("materialize ")?;
                expr.fmt(f)
            },
            ThirExprInner::Constructor(defid, fields, rest) => {
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
                    f.write_str("..")?;
                    rest.fmt(f)?;
                }
                f.write_str("}")
            }
        }
    }
}


pub struct ScopeTypeInfo<'defs>{
    pub defs: &'defs mut Definitions,
    pub localmuts: FxHashMap<HirVarId,Mutability>,
    pub localtys: FxHashMap<HirVarId,Type>,
    pub field_cache: FxHashMap<DefId,FxHashMap<FieldName,Type>>,
    pub parent: DefId,
    pub cur_mod: DefId,
}

impl<'defs> ScopeTypeInfo<'defs>{
    pub fn get_ctor_field_type(&mut self, def: DefId, fname: &FieldName) -> Option<Type>{
        self.field_cache.entry(def).or_insert_with(||{
            match &self.defs.get_definition_immut(def).def{
                DefinitionInner::UserType(ty) => {
                    let mut field_tys = FxHashMap::with_hasher(Default::default());
                    match ty{
                        super::UserType::Enum(_) => todo!(),
                        super::UserType::Struct(ctor) => {
                            match ctor{
                                super::Constructor::Unit => {},
                                super::Constructor::Tuple(fields) => {
                                    for (i,field) in fields.iter().enumerate(){
                                        if self.defs.visible_from(self.cur_mod, field.visible_from){
                                            field_tys.insert(FieldName::Tuple(i.try_into().unwrap()), field.ty.clone());
                                        }
                                    }
                                },
                                super::Constructor::Struct(fields) => {
                                    for field in fields.iter(){
                                        if self.defs.visible_from(self.cur_mod, field.visible_from){
                                            field_tys.insert(FieldName::Id(field.name.clone()),field.ty.clone());
                                        }
                                    }
                                },
                            }
                        },
                        super::UserType::Union(_) => todo!(),
                    }
                    field_tys
                }
                _ => unreachable!()
            }
        }).get(fname).cloned()
    }
}

pub fn tycheck_expr(defs: &mut ScopeTypeInfo, mut expr: HirExpr, against_ty: Option<&mut Type>,is_unsafe: bool) -> Result<ThirExpr,TypecheckError>{
    match expr{
        HirExpr::Unreachable => {
            let mut ty = Type::Never;
            if let Some(against_ty) = against_ty{
                match unify_type(defs.defs, against_ty, &mut ty, is_unsafe)?{
                    UnifyResult::CoercionApplied => {
                        ty = against_ty.clone();
                    }
                    _ => {}
                }
                
            }
            Ok(ThirExpr{ty,cat: ValueCategory::Value,expr: ThirExprInner::Unreachable})
        }
        HirExpr::Local(loc) => {
            let mut ty = defs.localtys.get_mut(&loc).unwrap();
            if let Some(against_ty) = against_ty{
                unify_type(defs.defs, against_ty, &mut ty, is_unsafe)?;
            }
            // assume mut for now, fix it later
            Ok(ThirExpr { ty: ty.clone(), cat: ValueCategory::Place(defs.localmuts.get(&loc).copied().unwrap(),Movability::Movable), expr: ThirExprInner::Local(loc) })
        }
        HirExpr::Const(val) => {
            let (mut ty, cat) = match &val{
                ConstVal::ScalarConst(_) => {
                    (Type::InferableInteger,ValueCategory::Value)
                }
                ConstVal::LogicVal(_) => {
                    (Type::Inferable,ValueCategory::Value)
                }
                ConstVal::ConstDef(def) => {
                    match &defs.defs.get_definition(*def).def{
                        DefinitionInner::Const { ty, .. } => (ty.clone(),ValueCategory::Value),
                        DefinitionInner::Static { ty, mutability, .. } => (ty.clone(), ValueCategory::Place(*mutability, Movability::Immovable)),
                        DefinitionInner::HirFunction(fnty, _) | DefinitionInner::IncompleteFunction(fnty) | DefinitionInner::ThirFunction(fnty, _) => {
                            (Type::FnItem(*def, fnty.clone()),ValueCategory::Value)
                        }
                        DefinitionInner::Signal(ty, dir) => (ty.clone(),ValueCategory::SignalPlace(*dir)),
                        _ => unreachable!()
                    }
                }
                ConstVal::TupleConst(val) => {
                    if val.len()>0{
                        todo!()
                    }else{
                        (Type::UNIT,ValueCategory::Value)
                    }
                }
                _ => todo!()
            };

            if let Some(against_ty) = against_ty{
                unify_type(defs.defs, against_ty, &mut ty, is_unsafe)?;
            }

            Ok(ThirExpr { ty, cat, expr: ThirExprInner::Const(val.clone()) })
        }
        HirExpr::Constructor(defid, inits, rest)=> {
            let mut definite_type = Type::UserDef(defid);

            if defs.defs.has_invisible_fields(defs.cur_mod, defid){
                return Err(TypecheckError::Private);
            }

            let rest = rest.map(|b|*b).map(|expr|tycheck_expr(defs, expr, Some(&mut definite_type), is_unsafe))
                .transpose()?
                .map(|expr|into_value(defs, expr)).transpose()?
                .map(Box::new)
                ;

            
            let inits = inits.into_iter().map(|init|{
                let name = init.name;
                let value = init.value;

                let mut ty = defs.get_ctor_field_type(defid, &name).ok_or(TypecheckError::Private)?;

                let init = tycheck_expr(defs, value, Some(&mut ty), is_unsafe)?;

                Ok(ThirFieldInit{name,init})
            }).collect::<Result<Vec<_>,_>>()?;

            let expr = ThirExprInner::Constructor(defid, inits, rest);

            Ok(ThirExpr{ty: definite_type,cat: ValueCategory::Value,expr})
        }
        expr => todo!("{expr}")
    }
}


#[derive(Clone, Debug, Hash,PartialEq,Eq)]
pub enum ThirStatement{
    Let{
        id: HirVarId,
        mutability: Mutability,
        ty: Type,
        init: Option<ThirExpr>,
    },
    Call{
        retplace: HirVarId,
        fnty: FunctionType,
        func: ThirExpr,
        args: Vec<ThirExpr>,
    },
    Return(ThirExpr),
    Loop(Vec<ThirStatement>),
    AwaitSignal(ThirExpr, TriggerType),
    Discard(ThirExpr),
}

impl core::fmt::Display for ThirStatement{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        match self{
            ThirStatement::Let { id, mutability, ty, init } => {
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
            ThirStatement::Call { retplace, func, args, .. } => {
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
            ThirStatement::Return(expr) => {
                f.write_str("return ")?;
                expr.fmt(f)?;
                f.write_str(";")
            },
            ThirStatement::Loop(stats) => {
                f.write_str("loop{\n")?;
                for stat in stats{
                    stat.fmt(f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            },
            ThirStatement::AwaitSignal(signal, tt) => {
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
            ThirStatement::Discard(expr) => {
                expr.fmt(f)?;
                f.write_str(";")
            },
        }
    }
}

pub fn into_value(defs: &mut ScopeTypeInfo, expr: ThirExpr) -> Result<ThirExpr,TypecheckError>{
    match expr.cat{
        ValueCategory::Value => Ok(expr),
        ValueCategory::Place(_,_)  => {
            let ty = expr.ty.clone();

            let expr = ThirExprInner::Place2Val(Box::new(expr));

            Ok(ThirExpr{ty, cat: ValueCategory::Value, expr})
        }
        ValueCategory::SignalPlace(SignalDirection::In | SignalDirection::Inout) => {
            let ty = expr.ty.clone();

            let expr = ThirExprInner::ReadSignal(Box::new(expr));

            Ok(ThirExpr{ty, cat: ValueCategory::Value, expr})
        },
        ValueCategory::SignalPlace(_) => Err(TypecheckError::CannotMove)
    }
}

pub fn tycheck_stat(defs: &mut ScopeTypeInfo, stat: HirStatement, is_unsafe: bool) -> Result<ThirStatement,TypecheckError>{
    match stat{
        HirStatement::Loop(inner) => {
            let inner = inner.into_iter().map(|stat| tycheck_stat(defs,stat,is_unsafe)).collect::<Result<Vec<_>,_>>()?;
            Ok(ThirStatement::Loop(inner))
        }
        HirStatement::Discard(expr,force_rvalue) => {
            let thirexpr = tycheck_expr(defs,expr,None,is_unsafe)?;
            let thirexpr = if force_rvalue {
                into_value(defs,thirexpr)?
            }else{
                thirexpr
            };
            Ok(ThirStatement::Discard(thirexpr))
        }
        HirStatement::AwaitSignal(expr, trig) => {
            let mut expr = tycheck_expr(defs, expr, None, is_unsafe)?;
            match expr.cat{
                ValueCategory::SignalPlace(SignalDirection::In | SignalDirection::Inout) => {
                    if !expr.ty.is_type_pollable(&mut defs.defs){
                        Err(TypecheckError::FailedToUnify)
                    }else{
                        Ok(ThirStatement::AwaitSignal(expr, trig))
                    }
                }
                _ => todo!()
            }
        }
        HirStatement::Return(expr) => {
            let mut against_ty = match &mut defs.defs.get_definition(defs.parent).def{
                DefinitionInner::HirFunction(fnty, _) | DefinitionInner::ThirFunction(fnty, _) | DefinitionInner::IncompleteFunction(fnty) => {
                    (*fnty.retty).clone()
                }
                DefinitionInner::Const { ty, .. } | DefinitionInner::IncompleteConst(ty) | DefinitionInner::Static { ty, .. } | DefinitionInner::IncompleteStatic(ty) => ty.clone(),
                _ => unreachable!()
            };

            Ok(ThirStatement::Return(tycheck_expr(defs, expr, Some(&mut against_ty), is_unsafe)?))
        }
        HirStatement::Let { id, mutability, ty: mut ty, init } => {
            let init = init.map(|expr|{
                tycheck_expr(defs,expr,Some(&mut ty),is_unsafe)
            }).transpose()?;

            defs.localmuts.insert(id,mutability);
            defs.localtys.insert(id,ty.clone());

            Ok(ThirStatement::Let { id, mutability, ty, init })
        }
        stat => todo!("{}",stat)
    }
}