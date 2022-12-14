macro_rules! matches_simple_path{
    ($e:expr, self :: $($items:ident)::+) => {
        ($e .root==Some($crate::parse::SimplePrefix::SelfTy))&&($e.idents.len()==[$(stringify!($items)),*].len())&&($e.idents.iter().map(|s|&**s).zip([$(stringify!($items)),*]).all(|(a,b)|a==b))
    };
    ($e:expr, super :: $($items:ident)::+) => {
        ($e .root==Some($crate::parse::SimplePrefix::Super))&&($e.idents.len()==[$(stringify!($items)),*].len())&&($e.idents.iter().map(|s|&**s).zip([$(stringify!($items)),*]).all(|(a,b)|a==b))
    };
    ($e:expr, crate :: $($items:ident)::+) => {
        ($e .root==Some($crate::parse::SimplePrefix::Crate))&&($e.idents.len()==[$(stringify!($items)),*].len())&&($e.idents.iter().map(|s|&**s).zip([$(stringify!($items)),*]).all(|(a,b)|a==b))
    };
    ($e:expr, :: $($items:ident)::+) => {
        ($e .root==Some($crate::parse::SimplePrefix::Root))&&($e.idents.len()==[$(stringify!($items)),*].len())&&($e.idents.iter().map(|s|&**s).zip([$(stringify!($items)),*]).all(|(a,b)|a==b))
    };
    ($e:expr, $($items:ident)::+) => {
        ($e .root==None)&&($e.idents.len()==[$(stringify!($items)),*].len())&&($e.idents.iter().map(|s|&**s).zip([$(stringify!($items)),*]).all(|(a,b)|a==b))
    };
}

use std::{
    collections::{BTreeMap, HashMap},
    convert,
};

use fxhash::FxHashMap;

pub use crate::parse::{AsyncFnTy, Meta, Mutability, Safety, SignalDirection};

use crate::{
    lang::{LangItem, LangItemTarget},
    parse::{self, Mod, Path, Pattern, Visibility},

};

use self::hir::HirConverter;

pub mod hir;
pub mod mir;
pub mod tycheck;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefId(u64);

pub const ROOT: DefId = DefId(0);

impl core::fmt::Debug for DefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("#")?;
        self.0.fmt(f)
    }
}

impl core::fmt::Display for DefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("#")?;
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct Definition {
    pub visible_from: DefId,
    pub attrs: Vec<Meta>,
    pub def: DefinitionInner,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UserType {
    Enum(Vec<EnumVariant>),
    Struct(Constructor),
    Union(Vec<StructField>),
}

#[derive(Clone, Debug)]
pub enum DefinitionInner {
    Empty,
    IncompleteType,
    IncompleteAlias,
    IncompleteFunction(FunctionType),
    IncompleteStatic(Type),
    IncompleteConst(Type),
    Signal(Type, SignalDirection),
    Module(Module),
    UserType(UserType),
    Alias(Type),
    Static {
        ty: Type,
        mutability: Mutability,
        init: ConstVal,
    },
    Const {
        ty: Type,
        val: ConstVal,
    },
    HirFunction(FunctionType, Vec<hir::HirStatement>),
    ThirFunction(FunctionType, Vec<tycheck::ThirStatement>),
    MirFunction(FunctionType,Vec<mir::BasicBlock>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LogicType {
    Binary,
    Tristate,
    Ieee1364,
    Ieee1164,
}

impl core::fmt::Display for LogicType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            LogicType::Binary => f.write_str("bool"),
            LogicType::Tristate => f.write_str("logic3"),
            LogicType::Ieee1364 => f.write_str("logic1364"),
            LogicType::Ieee1164 => f.write_str("logic1164"),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct IntType {
    pub signed: bool,
    pub logic: LogicType,
    pub bits: u16,
}

impl core::fmt::Display for IntType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.signed {
            f.write_str("int")?;
        } else {
            f.write_str("uint")?;
        }

        match self.logic {
            LogicType::Binary => {}
            LogicType::Ieee1164 => f.write_str(" ieee1164")?,
            LogicType::Ieee1364 => f.write_str(" ieee1364")?,
            LogicType::Tristate => f.write_str(" tristate")?,
        }

        f.write_str("(")?;

        self.bits.fmt(f)?;

        f.write_str(")")
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Repr {
    Default,
    Vhdl,
    Int(IntType),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SemaLifetime {
    GenericParam(u32),
    Static,
    Erased,
    Region(u32),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    UserDef(DefId),
    IncompleteAlias(DefId),
    IntegerType(IntType),
    Logic(LogicType),
    Array(Box<Type>, Box<ConstVal>),
    GenericParam(u32),
    FnPtr(FunctionType),
    FnItem(DefId, FunctionType),
    Signal(SignalDirection, SemaLifetime, Box<Type>),
    Tuple(Vec<Type>),
    Never,
    Pointer(Mutability, Box<Type>),
    Char,
    Inferable,
    Str,
    Slice(Box<Type>),
    InferableInteger,
}

impl Type{
    pub const UNIT: Self = Self::Tuple(Vec::new());

    pub fn is_type_copy(&self, defs: &mut Definitions) -> bool{
        match self{
            Type::IncompleteAlias(def) => {
                match &defs.get_definition(*def).def{
                    DefinitionInner::Alias(ty) => ty.clone().is_type_copy(defs),
                    _ => unreachable!()
                }
            }
            Type::Array(inner, _) => inner.is_type_copy(defs),
            Type::Tuple(inner) => inner.iter().all(|ty|ty.is_type_copy(defs)),
            Type::IntegerType(_) | Type::Logic(_) | Type::FnPtr(_) | Type::FnItem(_, _) | Type::Char | Type::Signal(_, _, _) | Type::InferableInteger | Type::Never | Type::Pointer(_, _) => true,
            Type::Inferable => panic!("Type must be known by this point"), // `is_type_copy` should follow any replacement
            Type::UserDef(defid) => false, // for now
            Type::GenericParam(_) => todo!(),
            Type::Str => false,
            Type::Slice(_) => false,
        }
    }

    pub fn is_type_pollable(&self, defs: &mut Definitions) -> bool{
        match self{
            Type::IncompleteAlias(def) => {
                match &defs.get_definition(*def).def{
                    DefinitionInner::Alias(ty) => ty.clone().is_type_pollable(defs),
                    _ => unreachable!()
                }
            }
            Type::Logic(_) => true,
            Type::GenericParam(_) => todo!(),
            Type::Inferable => panic!("Type must be known by this point"),
            _ => false,
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Type::UserDef(defid) => defid.fmt(f),
            Type::IncompleteAlias(defid) => defid.fmt(f),
            Type::IntegerType(intty) => intty.fmt(f),
            Type::Logic(logicty) => logicty.fmt(f),
            Type::Array(arr, val) => {
                f.write_str("[")?;
                arr.fmt(f)?;
                f.write_str("; ]")?;
                val.fmt(f)?;
                f.write_str("]")
            }
            Type::GenericParam(n) => f.write_fmt(format_args!("%{}", n)),
            Type::FnPtr(fnty) => fnty.fmt(f),
            Type::FnItem(defid, fnty) => {
                fnty.fmt(f)?;
                f.write_str(" [")?;
                defid.fmt(f)?;
                f.write_str("]")
            }
            Type::Signal(dir, _, inner) => {
                f.write_str("&")?;
                match dir {
                    SignalDirection::In => f.write_str("in ")?,
                    SignalDirection::Inout => f.write_str("inout ")?,
                    SignalDirection::Out => f.write_str("out ")?,
                }

                inner.fmt(f)
            }
            Type::Tuple(tys) => {
                let mut sep = "";
                f.write_str("(")?;

                for ty in tys {
                    f.write_str(sep)?;
                    sep = ", ";
                    ty.fmt(f)?;
                }
                if tys.len() == 1 {
                    f.write_str(",")?;
                }

                f.write_str(")")
            }
            Type::Never => f.write_str("!"),
            Type::Pointer(mt, inner) => {
                f.write_str("*")?;
                match mt {
                    Mutability::Const => f.write_str("const ")?,
                    Mutability::Mut => f.write_str("mut ")?,
                }

                inner.fmt(f)
            }
            Type::Char => f.write_str("char"),
            Type::Inferable => f.write_str("_"),
            Type::Str => f.write_str("str"),
            Type::Slice(ty) => {
                f.write_str("[")?;
                ty.fmt(f)?;
                f.write_str("]")
            }
            Type::InferableInteger => f.write_str("{{integer}}"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleField {
    pub visible_from: DefId,
    pub attrs: Vec<Meta>,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField {
    pub visible_from: DefId,
    pub attrs: Vec<Meta>,
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constructor {
    Unit,
    Tuple(Vec<TupleField>),
    Struct(Vec<StructField>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumVariant {
    pub attrs: Vec<Meta>,
    pub name: String,
    pub ctor: Constructor,
    pub discrim: Option<ConstVal>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LogicVal {
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

impl core::fmt::Display for LogicVal {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            LogicVal::StrongOne => f.write_str("'1'"),
            LogicVal::WeakOne => f.write_str("'H'"),
            LogicVal::StrongUnknown => f.write_str("'X'"),
            LogicVal::DontCare => f.write_str("'-'"),
            LogicVal::Uninitialized => f.write_str("'U'"),
            LogicVal::WeakUnknown => f.write_str("'W'"),
            LogicVal::HighImpedence => f.write_str("'Z'"),
            LogicVal::WeakZero => f.write_str("'L'"),
            LogicVal::StrongZero => f.write_str("'0'"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionType {
    pub async_ty: AsyncFnTy,
    pub safety: Safety,
    pub constness: Mutability,
    pub params: Vec<Type>,
    pub retty: Box<Type>,
}

impl core::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.constness == Mutability::Const {
            f.write_str("const ")?;
        }

        if self.safety == Safety::Unsafe {
            f.write_str("unsafe ")?;
        }

        match self.async_ty {
            AsyncFnTy::Async => f.write_str("async fn")?,
            AsyncFnTy::Normal => f.write_str("fn")?,
            AsyncFnTy::Entity => f.write_str("entity")?,
            AsyncFnTy::Procedure => f.write_str("proc")?,
        }

        f.write_str("(")?;

        let mut sep = "";
        for param in &self.params {
            f.write_str(sep)?;
            sep = ", ";
            param.fmt(f)?;
        }
        f.write_str(") -> ")?;
        self.retty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CtorConst {
    Unit,
    Tuple(Vec<ConstVal>),
    Struct(Vec<(String, ConstVal)>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstVal {
    ScalarConst(i128),
    LargeScalar(Vec<u8>),
    LogicVal(LogicVal),
    GenericParam(u32),
    ArrayLit(Vec<ConstVal>),
    ArrayRepeat {
        base: Box<ConstVal>,
        repeat: Box<ConstVal>,
    },
    ConstDef(DefId),
    Constructor {
        ty: DefId,
        name: Option<String>,
        inner: CtorConst,
    },
    IncompleteExpr,
    TupleConst(Vec<ConstVal>),
}

impl core::fmt::Display for ConstVal {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ConstVal::ScalarConst(val) => val.fmt(f),
            ConstVal::LargeScalar(val) => {
                f.write_str("0x")?;
                for byte in val {
                    f.write_fmt(format_args!("{:02x}", byte))?;
                }
                Ok(())
            }
            ConstVal::LogicVal(val) => val.fmt(f),
            ConstVal::GenericParam(n) => f.write_fmt(format_args!("%{}", n)),
            ConstVal::ArrayLit(vals) => {
                f.write_str("[")?;

                let mut sep = "";

                for val in vals {
                    f.write_str(sep)?;
                    sep = ", ";
                    val.fmt(f)?;
                }

                f.write_str("]")
            }
            ConstVal::ArrayRepeat { base, repeat } => {
                f.write_str("[")?;
                base.fmt(f)?;
                f.write_str(";")?;
                repeat.fmt(f)?;
                f.write_str("]")
            }
            ConstVal::ConstDef(defid) => defid.fmt(f),
            ConstVal::Constructor { ty, name, inner } => {
                ty.fmt(f)?;

                if let Some(name) = name {
                    f.write_str("::")?;
                    f.write_str(name)?;
                }

                match inner {
                    CtorConst::Unit => Ok(()),
                    CtorConst::Tuple(tup) => {
                        f.write_str("(")?;
                        let mut sep = "";
                        for field in tup {
                            f.write_str(sep)?;
                            sep = ", ";
                            field.fmt(f)?;
                        }
                        f.write_str(")")
                    }
                    CtorConst::Struct(st) => {
                        f.write_str("{")?;
                        for (name, val) in st {
                            f.write_str(name)?;
                            f.write_str(": ")?;
                            val.fmt(f)?;
                            f.write_str(",")?;
                        }
                        f.write_str("}")
                    }
                }
            }
            ConstVal::IncompleteExpr => f.write_str("/* incomplete */"),
            ConstVal::TupleConst(tup) => {
                f.write_str("(")?;
                let mut sep = "";
                for field in tup {
                    f.write_str(sep)?;
                    sep = ", ";
                    field.fmt(f)?;
                }
                f.write_str(")")
            },
        }
    }
}

impl ConstVal{
    pub const UNIT: Self = Self::TupleConst(Vec::new());
}

#[derive(Clone, Debug)]
pub struct Module {
    parent: DefId,
    types: HashMap<String, DefId>,
    values: HashMap<String, DefId>,
}

#[derive(Clone, Debug)]
pub struct Definitions {
    nextdefid: DefId,
    curcrate: DefId,
    defs: BTreeMap<DefId, Definition>,
    crates: FxHashMap<String, DefId>,
    lang_items: FxHashMap<LangItem, DefId>,
}

impl Definitions {
    fn display_vis(
        &self,
        cur_mod: DefId,
        vis: DefId,
        f: &mut core::fmt::Formatter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        if vis == cur_mod {
            Ok(())
        } else if vis == ROOT {
            f.write_str("pub ")
        } else {
            f.write_str("pub(in ")?;
            vis.fmt(f)?;
            f.write_str(") ")
        }
    }
    fn display_item(
        &self,
        cur_mod: DefId,
        defid: DefId,
        lname: &str,
        f: &mut core::fmt::Formatter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        let def = self.defs.get(&defid).unwrap();
        for attr in &def.attrs {
            f.write_fmt(format_args!("#[{}]\n", attr))?;
        }

        self.display_vis(cur_mod, def.visible_from, f)?;

        match &def.def {
            DefinitionInner::Empty => unreachable!(),
            DefinitionInner::IncompleteType => {
                f.write_str("enum ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;
                f.write_str("{\n /* incomplete */\n}")
            }
            DefinitionInner::IncompleteAlias => {
                f.write_str("type ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;
                f.write_str(" = /*incomplete */;")
            }
            DefinitionInner::IncompleteFunction(fnty) => {
                if fnty.constness == Mutability::Const {
                    f.write_str("const ")?;
                }

                if fnty.safety == Safety::Unsafe {
                    f.write_str("unsafe ")?;
                }

                match fnty.async_ty {
                    AsyncFnTy::Normal => f.write_str("fn ")?,
                    AsyncFnTy::Async => f.write_str("async fn ")?,
                    AsyncFnTy::Entity => f.write_str("entity ")?,
                    AsyncFnTy::Procedure => f.write_str("proc ")?,
                }

                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;

                f.write_str("(")?;

                let mut sep = "";

                for (i, ty) in fnty.params.iter().enumerate() {
                    f.write_str(sep)?;
                    sep = ", ";
                    f.write_fmt(format_args!("_{}: {}", i, ty))?;
                }
                f.write_str(") -> ")?;
                fnty.retty.fmt(f)?;
                f.write_str("{\n/*incomplete*/\n}")
            }
            DefinitionInner::IncompleteStatic(ty) => {
                f.write_str("static ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */: ")?;
                ty.fmt(f)?;
                f.write_str(" = /*incomplete */;")
            }
            DefinitionInner::IncompleteConst(ty) => {
                f.write_str("const ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */: ")?;
                ty.fmt(f)?;
                f.write_str(" = /*incomplete */;")
            }
            DefinitionInner::Signal(ty, dir) => {
                f.write_str("signal ")?;
                match dir {
                    SignalDirection::In => f.write_str("in ")?,
                    SignalDirection::Inout => f.write_str("inout ")?,
                    SignalDirection::Out => f.write_str("out ")?,
                }
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */: ")?;
                ty.fmt(f)?;
                f.write_str(" = /*incomplete */;")
            }
            DefinitionInner::Module(md) => {
                f.write_str("mod ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */{\n")?;
                for (name, id) in &md.types {
                    self.display_item(defid, *id, name, f)?;
                    f.write_str("\n")?;
                }
                for (name, id) in &md.values {
                    self.display_item(defid, *id, name, f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            }
            DefinitionInner::UserType(UserType::Struct(ctor)) => {
                f.write_str("struct ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;

                match ctor {
                    Constructor::Unit => f.write_str(";"),
                    Constructor::Tuple(tup) => {
                        f.write_str("(")?;
                        let mut sep = "";
                        for field in tup {
                            f.write_str(sep)?;
                            self.display_vis(cur_mod, field.visible_from, f)?;
                            field.ty.fmt(f)?;
                        }
                        f.write_str(");")
                    }
                    Constructor::Struct(st) => {
                        f.write_str("{\n")?;
                        for field in st {
                            for attr in &field.attrs {
                                f.write_fmt(format_args!("#[{}]\n", attr))?;
                            }
                            self.display_vis(cur_mod, field.visible_from, f)?;
                            f.write_str(&field.name)?;
                            f.write_str(": ")?;
                            field.ty.fmt(f)?;
                            f.write_str(",\n")?;
                        }
                        f.write_str("}\n")
                    }
                }
            }
            DefinitionInner::UserType(UserType::Enum(en)) => {
                f.write_str("enum ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */{\n")?;

                for var in en {
                    for attr in &var.attrs {
                        f.write_fmt(format_args!("#[{}]\n", attr))?;
                    }

                    f.write_str(&var.name)?;
                    match &var.ctor {
                        Constructor::Unit => {}
                        Constructor::Tuple(tup) => {
                            f.write_str("(")?;
                            let mut sep = "";
                            for field in tup {
                                f.write_str(sep)?;
                                field.ty.fmt(f)?;
                            }
                            f.write_str(")")?;
                        }
                        Constructor::Struct(st) => {
                            f.write_str("{\n")?;
                            for field in st {
                                for attr in &field.attrs {
                                    f.write_fmt(format_args!("#[{}]\n", attr))?;
                                }
                                f.write_str(&field.name)?;
                                f.write_str(": ")?;
                                field.ty.fmt(f)?;
                                f.write_str(",\n")?;
                            }
                            f.write_str("}")?;
                        }
                    }

                    f.write_str(",\n")?;
                }
                f.write_str("}")
            }
            DefinitionInner::UserType(UserType::Union(fields)) => {
                f.write_str("enum ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */{\n")?;

                for field in fields {
                    for attr in &field.attrs {
                        f.write_fmt(format_args!("#[{}]\n", attr))?;
                    }
                    self.display_vis(cur_mod, field.visible_from, f)?;
                    f.write_str(&field.name)?;
                    f.write_str(": ")?;
                    field.ty.fmt(f)?;
                    f.write_str(",\n")?;
                }
                f.write_str("}")
            }
            DefinitionInner::Static {
                ty,
                mutability,
                init,
            } => {
                f.write_str("static ")?;
                if *mutability == Mutability::Mut {
                    f.write_str("mut ")?;
                }
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */: ")?;
                ty.fmt(f)?;
                f.write_str(" = ")?;
                init.fmt(f)?;
                f.write_str(";")
            }
            DefinitionInner::Const { ty, val } => {
                f.write_str("const ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */: ")?;
                ty.fmt(f)?;
                f.write_str(" = ")?;
                val.fmt(f)?;
                f.write_str(";")
            }
            DefinitionInner::HirFunction(fnty, stats) => {
                if fnty.constness == Mutability::Const {
                    f.write_str("const ")?;
                }

                if fnty.safety == Safety::Unsafe {
                    f.write_str("unsafe ")?;
                }

                match fnty.async_ty {
                    AsyncFnTy::Normal => f.write_str("fn ")?,
                    AsyncFnTy::Async => f.write_str("async fn ")?,
                    AsyncFnTy::Entity => f.write_str("entity ")?,
                    AsyncFnTy::Procedure => f.write_str("proc ")?,
                }

                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;

                f.write_str("(")?;

                let mut sep = "";

                for (i, ty) in fnty.params.iter().enumerate() {
                    f.write_str(sep)?;
                    sep = ", ";
                    f.write_fmt(format_args!("_{}: {}", i, ty))?;
                }
                f.write_str(") -> ")?;
                fnty.retty.fmt(f)?;
                f.write_str("{\n")?;
                for stat in stats{
                    stat.fmt(f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            },
            DefinitionInner::MirFunction(fnty, blocks) => {
                if fnty.constness == Mutability::Const {
                    f.write_str("const ")?;
                }

                if fnty.safety == Safety::Unsafe {
                    f.write_str("unsafe ")?;
                }

                match fnty.async_ty {
                    AsyncFnTy::Normal => f.write_str("fn ")?,
                    AsyncFnTy::Async => f.write_str("async fn ")?,
                    AsyncFnTy::Entity => f.write_str("entity ")?,
                    AsyncFnTy::Procedure => f.write_str("proc ")?,
                }

                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;

                f.write_str("(")?;

                let mut sep = "";

                for (i, ty) in fnty.params.iter().enumerate() {
                    f.write_str(sep)?;
                    sep = ", ";
                    f.write_fmt(format_args!("_{}: {}", i, ty))?;
                }
                f.write_str(") -> ")?;
                fnty.retty.fmt(f)?;
                f.write_str("{\n")?;
                for bb in blocks{
                    bb.fmt(f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            },
            DefinitionInner::ThirFunction(fnty, stats) => {
                if fnty.constness == Mutability::Const {
                    f.write_str("const ")?;
                }

                if fnty.safety == Safety::Unsafe {
                    f.write_str("unsafe ")?;
                }

                match fnty.async_ty {
                    AsyncFnTy::Normal => f.write_str("fn ")?,
                    AsyncFnTy::Async => f.write_str("async fn ")?,
                    AsyncFnTy::Entity => f.write_str("entity ")?,
                    AsyncFnTy::Procedure => f.write_str("proc ")?,
                }

                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;

                f.write_str("(")?;

                let mut sep = "";

                for (i, ty) in fnty.params.iter().enumerate() {
                    f.write_str(sep)?;
                    sep = ", ";
                    f.write_fmt(format_args!("_{}: {}", i, ty))?;
                }
                f.write_str(") -> ")?;
                fnty.retty.fmt(f)?;
                f.write_str("{\n")?;
                for stat in stats{
                    stat.fmt(f)?;
                    f.write_str("\n")?;
                }
                f.write_str("}")
            },
            DefinitionInner::Alias(ty) => {
                f.write_str("type ")?;
                f.write_str(lname)?;
                f.write_str("/* ")?;
                defid.fmt(f)?;
                f.write_str(" */")?;
                f.write_str(" = ")?;
                ty.fmt(f)?;
                f.write_str(";")
            },
        }
    }
}

impl core::fmt::Display for Definitions {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for (name, defid) in self.crates.iter() {
            f.write_str("extern crate ")?;
            f.write_str(name)?;
            f.write_str(" /* ")?;
            defid.fmt(f)?;
            f.write_str("*/\n")?;
        }

        f.write_str("\n")?;

        let rootmd = self.defs.get(&self.curcrate).unwrap();

        match &rootmd.def {
            DefinitionInner::Module(md) => {
                for attr in &rootmd.attrs {
                    f.write_fmt(format_args!("#![{}]\n", attr))?;
                }
                f.write_str("\n")?;
                for (name, defid) in &md.types {
                    self.display_item(self.curcrate, *defid, name, f)?;
                    f.write_str("\n")?;
                }
                for (name, defid) in &md.values {
                    self.display_item(self.curcrate, *defid, name, f)?;
                    f.write_str("\n")?;
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            nextdefid: DefId(1),
            curcrate: DefId(0),
            defs: BTreeMap::new(),
            crates: FxHashMap::with_hasher(Default::default()),
            lang_items: FxHashMap::with_hasher(Default::default()),
        }
    }
    pub fn next_defid(&mut self) -> DefId {
        let ret = self.nextdefid;
        self.nextdefid.0 = self
            .nextdefid
            .0
            .checked_add(1)
            .expect("More than 2^64 definitions in a program.");
        ret
    }

    pub fn get_definition(&mut self, defid: DefId) -> &mut Definition {
        self.defs.get_mut(&defid).expect("Expected a definition")
    }

    pub fn insert_type(&mut self, targ_mod: DefId, name: &str, def: Definition) -> DefId {
        let defid = self.next_defid();
        if let Some(Definition {
            def: DefinitionInner::Module(md),
            ..
        }) = self.defs.get_mut(&targ_mod)
        {
            md.types.insert(name.to_string(), defid);
        } else {
            panic!("Expected a module for current module {:?}", targ_mod);
        }
        self.defs.insert(defid, def);

        defid
    }

    pub fn insert_value(&mut self, targ_mod: DefId, name: &str, def: Definition) -> DefId {
        let defid = self.next_defid();
        if let Some(Definition {
            def: DefinitionInner::Module(md),
            ..
        }) = self.defs.get_mut(&targ_mod)
        {
            md.values.insert(name.to_string(), defid);
        } else {
            panic!("Expected a module for current module {:?}", targ_mod);
        }
        self.defs.insert(defid, def);

        defid
    }

    pub fn check_visible(&self, cur_mod: DefId, def: DefId) -> bool {
        if def == DefId(0) {
            true
        } else if let Some(Definition { visible_from, .. }) = self.defs.get(&def) {
            self.visible_from(cur_mod, *visible_from)
        } else {
            panic!("Expected a definition for {:?}", def)
        }
    }

    pub fn find_type_in_mod(&mut self, cur_mod: DefId, name: &str) -> Option<DefId> {
        if let Some(Definition {
            def: DefinitionInner::Module(md),
            ..
        }) = self.defs.get(&cur_mod)
        {
            md.types.get(name).copied()
        } else {
            panic!("Expected a module for current module {:?}", cur_mod);
        }
    }

    pub fn find_val_in_mod(&mut self, cur_mod: DefId, name: &str) -> Option<DefId> {
        if let Some(Definition {
            def: DefinitionInner::Module(md),
            ..
        }) = self.defs.get(&cur_mod)
        {
            md.values.get(name).copied()
        } else {
            panic!("Expected a module for current module {:?}", cur_mod)
        }
    }

    pub fn visible_from(&self, cur_mod: DefId, vis_from: DefId) -> bool {
        if vis_from == DefId(0) || vis_from == cur_mod {
            true
        } else {
            if let Some(Definition {
                def: DefinitionInner::Module(md),
                ..
            }) = self.defs.get(&cur_mod)
            {
                self.visible_from(md.parent, vis_from)
            } else {
                panic!("Expected a module for current module {:?}", cur_mod)
            }
        }
    }

    pub fn find_mod(&mut self, cur_mod: DefId, path: &parse::SimplePath) -> Option<DefId> {
        let mut base = match &path.root {
            Some(parse::SimplePrefix::Crate) => self.curcrate,
            Some(parse::SimplePrefix::Root) => DefId(0),
            Some(parse::SimplePrefix::SelfPath) => cur_mod,
            Some(parse::SimplePrefix::Super) => {
                if let Some(Definition {
                    def: DefinitionInner::Module(md),
                    ..
                }) = self.defs.get_mut(&cur_mod)
                {
                    md.parent
                } else {
                    panic!("Expected a module for current module {:?}", cur_mod)
                }
            }
            None => {
                let first_id = path
                    .idents
                    .first()
                    .expect("No-prefix path contains no components");
                if let Some(Definition {
                    def: DefinitionInner::Module(md),
                    ..
                }) = self.defs.get_mut(&cur_mod)
                {
                    if md.types.contains_key(first_id) {
                        cur_mod
                    } else {
                        DefId(0)
                    }
                } else {
                    panic!("Expected a module for current module {:?}", cur_mod)
                }
            }
        };
        for id in &path.idents {
            if let Some(Definition {
                def: DefinitionInner::Module(md),
                ..
            }) = self.defs.get(&base)
            {
                base = *md.types.get(id)?
            } else if base == DefId(0) {
                base = *self.crates.get(id)?
            } else {
                panic!(
                    "Expected a module for current module {:?}, got {:?}",
                    base,
                    self.defs.get(&base)
                )
            }

            if let Some(def) = self.defs.get(&base) {
                if !self.visible_from(cur_mod, def.visible_from) {
                    return None;
                }
            }
        }
        Some(base)
    }

    pub fn find_type_def(&mut self, cur_mod: DefId, path: &parse::Path) -> Option<DefId> {
        let mut base = match &path.root {
            Some(parse::PathRoot::Crate) => self.curcrate,
            Some(parse::PathRoot::Root) => DefId(0),
            Some(parse::PathRoot::SelfPath) => cur_mod,
            Some(parse::PathRoot::Super) => {
                if let Some(Definition {
                    def: DefinitionInner::Module(md),
                    ..
                }) = self.defs.get_mut(&cur_mod)
                {
                    md.parent
                } else {
                    panic!("Expected a module for current module {:?}", cur_mod)
                }
            }
            Some(root) => panic!("Expected a module prefix for path, got {:?} instead", root),
            None => {
                let first_id = path
                    .components
                    .first()
                    .expect("No-prefix path contains no components");
                match first_id {
                    parse::PathComponent::Id(id) => {
                        if let Some(Definition {
                            def: DefinitionInner::Module(md),
                            ..
                        }) = self.defs.get_mut(&cur_mod)
                        {
                            if md.types.contains_key(id) {
                                cur_mod
                            } else {
                                DefId(0)
                            }
                        } else {
                            panic!("Expected a module for current module {:?}", cur_mod)
                        }
                    }
                    parse::PathComponent::Generics(_) => {
                        panic!("Got generics at start of a no-prefix path")
                    }
                }
            }
        };

        for comp in &path.components {
            match comp {
                parse::PathComponent::Id(id) => {
                    if let Some(Definition {
                        def: DefinitionInner::Module(md),
                        ..
                    }) = self.defs.get(&base)
                    {
                        base = *md.types.get(id)?
                    } else if base == DefId(0) {
                        base = *self.crates.get(id)?
                    } else {
                        panic!(
                            "Expected a module for current module {:?}, got {:?}",
                            base,
                            self.defs.get(&base)
                        )
                    }

                    if let Some(def) = self.defs.get(&base) {
                        if !self.visible_from(cur_mod, def.visible_from) {
                            return None;
                        }
                    }
                }
                parse::PathComponent::Generics(generics) => todo!("generics {generics:?}"),
            }
        }
        Some(base)
    }

    pub fn find_type(&mut self, cur_mod: DefId, path: &parse::Path) -> Option<&mut Definition> {
        let defid = self.find_type_def(cur_mod, path)?;

        self.defs.get_mut(&defid)
    }
}

pub fn convert_visibility(
    defs: &mut Definitions,
    cur_mod: DefId,
    vis: &parse::Visibility,
) -> Option<DefId> {
    match vis {
        Visibility::None => None,
        Visibility::Pub => Some(DefId(0)),
        Visibility::Priv => Some(cur_mod),
        Visibility::Crate => Some(defs.curcrate),
        Visibility::Super => {
            if let Definition {
                def: DefinitionInner::Module(md),
                ..
            } = defs.get_definition(cur_mod)
            {
                Some(md.parent)
            } else {
                panic!("Expected a mod for current module {:?}", cur_mod)
            }
        }
        Visibility::In(path) => Some(defs.find_mod(cur_mod, path).unwrap()),
    }
}

pub fn collect_submods(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId) {
    for i in &ast_mod.items {
        match i {
            parse::Item::Mod {
                name,
                content: Some(md),
                vis,
                ..
            } => {
                let inner_md = Module {
                    parent: sema_mod,
                    types: HashMap::new(),
                    values: HashMap::new(),
                };

                let visible_from = convert_visibility(defs, sema_mod, vis).unwrap_or(DefId(0));

                let submod_id = defs.insert_type(
                    sema_mod,
                    name,
                    Definition {
                        attrs: md.attrs.clone(),
                        visible_from,
                        def: DefinitionInner::Module(inner_md),
                    },
                );

                collect_submods(defs, md, submod_id);
            }
            _ => {}
        }
    }
}

pub fn collect_type_names(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId) {
    for i in &ast_mod.items {
        match i {
            parse::Item::Mod {
                name,
                content: Some(md),
                ..
            } => {
                let next_mod = defs.find_type_in_mod(sema_mod, name).unwrap();
                collect_type_names(defs, md, next_mod);
            }
            parse::Item::TypeAlias {
                name, vis, attrs, ..
            } => {
                let def = DefinitionInner::IncompleteAlias;
                let vis = convert_visibility(defs, sema_mod, vis).unwrap_or(sema_mod);
                defs.insert_type(
                    sema_mod,
                    name,
                    Definition {
                        visible_from: vis,
                        attrs: attrs.clone(),
                        def,
                    },
                );
            }
            parse::Item::Type(ty) => {
                let def = DefinitionInner::IncompleteType;
                let vis = convert_visibility(defs, sema_mod, &ty.vis).unwrap_or(sema_mod);
                let defid = defs.insert_type(
                    sema_mod,
                    &ty.name,
                    Definition {
                        visible_from: vis,
                        attrs: ty.attrs.clone(),
                        def,
                    },
                );
                for attr in &ty.attrs {
                    match attr {
                        parse::Meta::KeyValue(path, meta) => {
                            if matches_simple_path!(path, lang) {
                                match &**meta {
                                    parse::Meta::String(id) => {
                                        if let Some(lang) = LangItem::from_item_name(id) {
                                            if lang.target() != LangItemTarget::Adt {
                                                panic!(
                                                    "Cannot apply #[lang = \"{}\"] to struct {}",
                                                    lang.name(),
                                                    ty.name
                                                )
                                            }
                                            if let Some(existing) =
                                                defs.lang_items.insert(lang, defid)
                                            {
                                                panic!("Duplicate Lang Item {}. Already applied to {:?}",id,existing)
                                            }
                                        } else {
                                            panic!("Unknown lang item {}", id)
                                        }
                                    }
                                    m => panic!("Invalid argument for #[lang] attribute: {:?}", m),
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            parse::Item::Adt {
                name, attrs, vis, ..
            } => {
                let def = DefinitionInner::IncompleteType;
                let vis = convert_visibility(defs, sema_mod, vis).unwrap_or(sema_mod);
                let defid = defs.insert_type(
                    sema_mod,
                    name,
                    Definition {
                        visible_from: vis,
                        attrs: attrs.clone(),
                        def,
                    },
                );
                for attr in attrs {
                    match attr {
                        parse::Meta::KeyValue(path, meta) => {
                            if matches_simple_path!(path, lang) {
                                match &**meta {
                                    parse::Meta::String(id) => {
                                        if let Some(lang) = LangItem::from_item_name(id) {
                                            if lang.target() != LangItemTarget::Adt {
                                                panic!(
                                                    "Cannot apply #[lang = \"{}\"] to enum {}",
                                                    lang.name(),
                                                    name
                                                )
                                            }
                                            if let Some(existing) =
                                                defs.lang_items.insert(lang, defid)
                                            {
                                                panic!("Duplicate Lang Item {}. Already applied to {:?}",id,existing)
                                            }
                                        } else {
                                            panic!("Unknown lang item {}", id)
                                        }
                                    }
                                    m => panic!("Invalid argument for #[lang] attribute: {:?}", m),
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn convert_type(defs: &mut Definitions, cur_mod: DefId, ty: &crate::parse::Type) -> Type {
    match ty {
        parse::Type::Name(path) => {
            if let Some(Definition { def: ty, .. }) = defs.find_type(cur_mod, path) {
                match ty {
                    DefinitionInner::IncompleteType | DefinitionInner::UserType(_) => {
                        let defid = defs.find_type_def(cur_mod, path).unwrap();

                        if defs.lang_items.get(&LangItem::Logic1164) == Some(&defid) {
                            Type::Logic(LogicType::Ieee1164)
                        } else if defs.lang_items.get(&LangItem::Logic1364) == Some(&defid) {
                            Type::Logic(LogicType::Ieee1364)
                        } else if defs.lang_items.get(&LangItem::LogicTri) == Some(&defid) {
                            Type::Logic(LogicType::Tristate)
                        } else {
                            Type::UserDef(defid)
                        }
                    }
                    DefinitionInner::IncompleteAlias => {
                        Type::IncompleteAlias(defs.find_type_def(cur_mod, path).unwrap())
                    }
                    _ => panic!("Required a type, got {:?}", path),
                }
            } else if path.root.is_none() && path.components.len() == 1 {
                match &path.components[0] {
                    parse::PathComponent::Id(id) => match &**id {
                        x if x.starts_with('i') => {
                            if let Ok(bits) = x[1..].parse::<u16>() {
                                Type::IntegerType(IntType {
                                    signed: true,
                                    logic: LogicType::Binary,
                                    bits,
                                })
                            } else {
                                panic!("Required a type, got {:?}", path)
                            }
                        }
                        x if x.starts_with('u') => {
                            if let Ok(bits) = x[1..].parse::<u16>() {
                                Type::IntegerType(IntType {
                                    signed: false,
                                    logic: LogicType::Binary,
                                    bits,
                                })
                            } else {
                                panic!("Required a type, got {:?}", path)
                            }
                        }
                        "bool" => Type::Logic(LogicType::Binary),
                        "char" => Type::Char,
                        _ => panic!("Required a type, got {:?}", path),
                    },
                    _ => panic!("Required a type, got {:?}", path),
                }
            } else {
                panic!("Required type, got {:?}", path)
            }
        }
        parse::Type::Pointer {
            mutability,
            underlying,
        } => {
            let underlying = convert_type(defs, cur_mod, underlying);
            Type::Pointer(*mutability, Box::new(underlying))
        }
        parse::Type::Never => Type::Never,
        parse::Type::Tuple(ast_types) => {
            let mut sema_types = ast_types
                .iter()
                .map(|ty| convert_type(defs, cur_mod, ty))
                .collect::<Vec<_>>();

            Type::Tuple(sema_types)
        }
        parse::Type::Wildcard => Type::Inferable,
        parse::Type::Reference {
            mutability,
            lifetime,
            underlying,
        } => todo!("reference"),
        parse::Type::Signal {
            direction,
            lifetime,
            underlying,
        } => todo!("signal"),
        parse::Type::Slice(ty) => Type::Slice(Box::new(convert_type(defs, cur_mod, ty))),
        parse::Type::Array(_, _) => todo!("array"),
    }
}

pub fn collect_value_names(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId) {
    for item in &ast_mod.items {
        match item {
            parse::Item::ExternBlock { attrs, abi, items } => todo!("extern block"),
            parse::Item::FnDeclaration {
                attrs,
                visibility,
                is_const,
                is_async,
                safety,
                name,
                params,
                return_ty,
                ..
            } => {
                let vis = convert_visibility(defs, sema_mod, visibility).unwrap_or(sema_mod);
                let sig = FunctionType {
                    async_ty: *is_async,
                    constness: if *is_const {
                        Mutability::Const
                    } else {
                        Mutability::Mut
                    },
                    safety: *safety,
                    params: params
                        .iter()
                        .map(|ty| convert_type(defs, sema_mod, ty.ty.as_ref().unwrap()))
                        .collect(),
                    retty: Box::new(
                        return_ty
                            .as_ref()
                            .map(|ty| convert_type(defs, sema_mod, ty))
                            .unwrap_or(Type::Tuple(Vec::new())),
                    ),
                };
                let def = DefinitionInner::IncompleteFunction(sig);

                defs.insert_value(
                    sema_mod,
                    name,
                    Definition {
                        visible_from: vis,
                        attrs: attrs.clone(),
                        def,
                    },
                );
            }
            parse::Item::MacroRules { .. }
            | parse::Item::Type(_)
            | parse::Item::Adt { .. }
            | parse::Item::TypeAlias { .. } => {}
            parse::Item::Trait { .. } => todo!("trait"),

            parse::Item::Mod {
                name,
                content: Some(md),
                ..
            } => {
                let next_mod = defs.find_type_in_mod(sema_mod, name).unwrap();
                collect_value_names(defs, md, next_mod);
            }
            parse::Item::Impl { .. } => todo!("impl block"),
            parse::Item::Static {
                name,
                ty,
                vis,
                attrs,
                ..
            } => {
                let ty = convert_type(defs, sema_mod, ty);
                match name {
                    Pattern::Ident(_, id) => {
                        let def = DefinitionInner::IncompleteStatic(ty);
                        let vis = convert_visibility(defs, sema_mod, vis).unwrap_or(sema_mod);
                        defs.insert_value(
                            sema_mod,
                            id,
                            Definition {
                                visible_from: vis,
                                attrs: attrs.clone(),
                                def,
                            },
                        );
                    }
                    pat => panic!("Invalid name for a static item: {:?}", pat),
                }
            }
            parse::Item::Const {
                name,
                ty,
                vis,
                attrs,
                ..
            } => {
                let ty = convert_type(defs, sema_mod, ty);
                match name {
                    Pattern::Ident(Mutability::Const, id) => {
                        let def = DefinitionInner::IncompleteConst(ty);
                        let vis = convert_visibility(defs, sema_mod, vis).unwrap_or(sema_mod);
                        defs.insert_value(
                            sema_mod,
                            id,
                            Definition {
                                visible_from: vis,
                                attrs: attrs.clone(),
                                def,
                            },
                        );
                    }
                    Pattern::Discard => {}
                    pat => panic!("Invalid name for a static item: {:?}", pat),
                }
            }
            parse::Item::Signal {
                attrs,
                vis,
                direction,
                name,
                ty,
            } => {
                let ty = convert_type(defs, sema_mod, ty);
                let def = DefinitionInner::Signal(ty, *direction);
                let visible_from = convert_visibility(defs, sema_mod, vis).unwrap_or(sema_mod);
                defs.insert_value(
                    sema_mod,
                    name,
                    Definition {
                        visible_from,
                        attrs: attrs.clone(),
                        def,
                    },
                );
            }
            _ => panic!("Unexpected Item"),
        }
    }
}

pub fn convert_top_const_expr(
    defs: &mut Definitions,
    cur_mod: DefId,
    expr: &parse::Expr,
) -> ConstVal {
    match expr {
        parse::Expr::Block(_) => todo!("block"),
        parse::Expr::LetExpr(_, _) => todo!("let expr"),
        parse::Expr::Id(path) => {
            let root = path.root.clone();
            let mut components = path.components.clone();
            let last = components.pop().unwrap(); // fixme: currently assumes no generics
            if root == None && components.len() == 0 {
                match last {
                    parse::PathComponent::Id(id) => {
                        if let Definition {
                            def: DefinitionInner::Module(md),
                            ..
                        } = defs.get_definition(cur_mod)
                        {
                            ConstVal::ConstDef(md.values[&id])
                        } else {
                            unreachable!()
                        }
                    }
                    _ => todo!("Handle generics properly"),
                }
            } else {
                let ty = defs
                    .find_type_def(cur_mod, &Path { root, components })
                    .unwrap_or_else(|| panic!("No such type {:?}", path));

                if defs.lang_items.get(&LangItem::Logic1164) == Some(&ty) {
                    match last {
                        parse::PathComponent::Id(id) => match &*id {
                            "StrongOne" => ConstVal::LogicVal(LogicVal::StrongOne),
                            "WeakOne" => ConstVal::LogicVal(LogicVal::WeakOne),
                            "StrongUnknown" => ConstVal::LogicVal(LogicVal::StrongUnknown),
                            "DontCare" => ConstVal::LogicVal(LogicVal::DontCare),
                            "Uninitialized" => ConstVal::LogicVal(LogicVal::Uninitialized),
                            "WeakUnknown" => ConstVal::LogicVal(LogicVal::WeakUnknown),
                            "HighImpedence" => ConstVal::LogicVal(LogicVal::HighImpedence),
                            "WeakZero" => ConstVal::LogicVal(LogicVal::WeakZero),
                            "StrongZero" => ConstVal::LogicVal(LogicVal::StrongZero),
                            _ => panic!("No such enumerator {:?}", path),
                        },
                        _ => panic!("Unexpected generics on struct {:?}", path),
                    }
                } else if defs.lang_items.get(&LangItem::Logic1364) == Some(&ty) {
                    match last {
                        parse::PathComponent::Id(id) => match &*id {
                            "One" => ConstVal::LogicVal(LogicVal::StrongOne),
                            "DontCare" => ConstVal::LogicVal(LogicVal::DontCare),
                            "HighImpedence" => ConstVal::LogicVal(LogicVal::HighImpedence),
                            "Zero" => ConstVal::LogicVal(LogicVal::StrongZero),
                            _ => panic!("No such enumerator {:?}", path),
                        },
                        _ => panic!("Unexpected generics on struct {:?}", path),
                    }
                } else if defs.lang_items.get(&LangItem::LogicTri) == Some(&ty) {
                    match last {
                        parse::PathComponent::Id(id) => match &*id {
                            "One" => ConstVal::LogicVal(LogicVal::StrongOne),
                            "HighImpedence" => ConstVal::LogicVal(LogicVal::HighImpedence),
                            "Zero" => ConstVal::LogicVal(LogicVal::StrongZero),
                            _ => panic!("No such enumerator {:?}", path),
                        },
                        _ => panic!("Unexpected generics on struct {:?}", path),
                    }
                } else {
                    match last {
                        parse::PathComponent::Id(id) => match &defs.get_definition(ty).def {
                            DefinitionInner::IncompleteType => panic!("Unresolved Type"),
                            DefinitionInner::IncompleteAlias => panic!("Unresolved Alias"),
                            DefinitionInner::IncompleteFunction(_)
                            | DefinitionInner::IncompleteStatic(_)
                            | DefinitionInner::IncompleteConst(_)
                            | DefinitionInner::Static { .. }
                            | DefinitionInner::Const { .. }
                            | DefinitionInner::Signal(_, _)
                            | DefinitionInner::HirFunction(_, _)
                            | DefinitionInner::ThirFunction(_, _)
                            | DefinitionInner::MirFunction(_, _) => panic!("Not a type"),
                            DefinitionInner::Module(_) => {
                                if let Some(val) = defs
                                    .find_val_in_mod(ty, &id)
                                    .filter(|&def| defs.check_visible(cur_mod, def))
                                {
                                    ConstVal::ConstDef(val)
                                } else if let Some(val) = defs
                                    .find_type_in_mod(ty, &id)
                                    .filter(|&def| defs.check_visible(cur_mod, def))
                                {
                                    todo!("Constructor")
                                } else {
                                    panic!("Could not find {:?}", path)
                                }
                            }
                            DefinitionInner::UserType(_) => todo!("Constructor"),
                            DefinitionInner::Alias(_) => todo!("Non-canonical type name"),
                            DefinitionInner::Empty => unreachable!(),
                        },
                        _ => todo!("Unexpected generics on module {:?}", path),
                    }
                }
            }
        }
        parse::Expr::FunctionCall { .. } => todo!(),
        parse::Expr::Cast(_, _) => todo!(),
        parse::Expr::StringLiteral(_, _) => todo!(),
        parse::Expr::CharLiteral(_, _) => todo!(),
        parse::Expr::Parentheses(expr) => convert_top_const_expr(defs, cur_mod, expr),
        parse::Expr::MacroExpansion { target, args } => todo!(),
        parse::Expr::IntLiteral(n) => ConstVal::ScalarConst(*n),
        parse::Expr::StructConstructor(_, _) => todo!(),
        parse::Expr::Field(_, _) => todo!(),
        parse::Expr::Await(_) => todo!(),
        parse::Expr::AwaitSignal(_, _) => todo!(),
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

pub fn convert_types(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId) {
    for item in &ast_mod.items {
        match item {
            parse::Item::ExternBlock { .. } => todo!("extern block"),
            parse::Item::Static { .. }
            | parse::Item::Const { .. }
            | parse::Item::Signal { .. }
            | parse::Item::FnDeclaration { .. } => {}
            parse::Item::Type(_) => todo!(
                "struct"
            ),
            parse::Item::Mod {
                name,
                vis,
                content: Some(md),
            } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                convert_types(defs, md, def);
            }
            parse::Item::Adt {
                attrs,
                name,
                generics,
                vis,
                variants,
            } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                let mut repr = Repr::Default;
                for attr in attrs {
                    match attr {
                        Meta::Group(id, inner) if matches_simple_path!(id, repr) => {
                            for i in inner {
                                match i {
                                    Meta::Ident(id) if matches_simple_path!(id, C) => {
                                        repr = Repr::Vhdl
                                    }
                                    Meta::Ident(id) if matches_simple_path!(id, vhdl) => {
                                        repr = Repr::Vhdl
                                    }
                                    Meta::Ident(id)
                                        if id.root.is_none() && id.idents.len() == 1 =>
                                    {
                                        let id = &id.idents[0];

                                        if id.starts_with('i') {
                                            let bits = id[1..]
                                                .parse::<u16>()
                                                .expect("Invalid repr attribute");
                                            repr = Repr::Int(IntType {
                                                signed: true,
                                                logic: LogicType::Binary,
                                                bits,
                                            });
                                        } else if id.starts_with('u') {
                                            let bits = id[1..]
                                                .parse::<u16>()
                                                .expect("Invalid repr attribute");
                                            repr = Repr::Int(IntType {
                                                signed: false,
                                                logic: LogicType::Binary,
                                                bits,
                                            });
                                        } else {
                                            panic!("Invalid repr attribute {}", id)
                                        }
                                    }
                                    m => panic!("Invalid repr atribute, {:?}", m),
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let mut variants = variants
                    .iter()
                    .map(|var| {
                        let attrs = var.attrs.clone();
                        let name = var.name.clone();

                        let ctor = match &var.ctor {
                            parse::StructBody::Unit => Constructor::Unit,
                            parse::StructBody::Tuple(fields) => {
                                let mut sema_fields = Vec::new();
                                for field in fields {
                                    let attrs = field.attrs.clone();
                                    let vis = DefId(0); // all enum fields are pub by default
                                    let ty = convert_type(defs, sema_mod, &field.ty);
                                    sema_fields.push(TupleField {
                                        visible_from: vis,
                                        attrs,
                                        ty,
                                    })
                                }
                                Constructor::Tuple(sema_fields)
                            }
                            parse::StructBody::Struct(fields) => {
                                let mut sema_fields = Vec::new();
                                for field in fields {
                                    let attrs = field.attrs.clone();
                                    let visible_from = DefId(0); // all enum fields are pub by default
                                    let ty = convert_type(defs, sema_mod, &field.ty);
                                    let name = field.name.to_string();
                                    sema_fields.push(StructField {
                                        visible_from,
                                        attrs,
                                        name,
                                        ty,
                                    });
                                }
                                Constructor::Struct(sema_fields)
                            }
                        };
                        let discrim = var.discrim.as_ref().map(|_| ConstVal::IncompleteExpr);
                        EnumVariant {
                            attrs,
                            name,
                            ctor,
                            discrim,
                        }
                    })
                    .collect::<Vec<_>>();
                defs.get_definition(def).def = DefinitionInner::UserType(UserType::Enum(variants));
            }
            parse::Item::TypeAlias {
                attrs,
                vis,
                name,
                generics,
                defn,
            } => todo!("Type alias"),
            parse::Item::Trait {
                attrs,
                vis,
                safety,
                auto,
                name,
                generics,
                supertraits,
                body,
            } => todo!(),
            parse::Item::Impl {
                attrs,
                safety,
                generics,
                tr,
                ty,
                body,
            } => todo!("impl block"),
            _ => unreachable!("invalid item"),
        }
    }
}

pub fn convert_items(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId) {
    for item in &ast_mod.items {
        match item {
            parse::Item::ExternBlock { attrs, abi, items } => todo!("extern"),
            parse::Item::FnDeclaration {
                name,
                block,
                params,
                ..
            } => {
                let def = defs.find_val_in_mod(sema_mod, name).unwrap();

                let fndef =
                    core::mem::replace(&mut defs.get_definition(def).def, DefinitionInner::Empty);

                let fnty = match fndef {
                    DefinitionInner::IncompleteFunction(fnty) => fnty,
                    item => unreachable!("Expected a function, got {:?}", item),
                };

                if let Some(block) = block{
                    let mut converter = HirConverter::new(defs,&fnty,sema_mod);
                    for ty in fnty.params.iter().cloned(){
                        converter.allocate_local(ty);
                    }
                    for item in block{
                        converter.convert_statement(item);
                    }
                    let stats = converter.into_function();
                    defs.get_definition(def).def = DefinitionInner::HirFunction(fnty, stats);
                }else{
                    todo!("extern fn")
                }                
            }
            parse::Item::MacroExpansion {
                attrs,
                target,
                args,
            } => unreachable!("macro expansion"),
            parse::Item::MacroRules {
                attrs,
                visibility,
                name,
                content,
            } => unreachable!("macro rules"),
            parse::Item::Type(_) => todo!(),
            parse::Item::Mod {
                name,
                content: Some(md),
                ..
            } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                convert_items(defs, md, def);
            }
            parse::Item::Adt { name, variants, .. } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();

                let mut sema_variants = if let Definition {
                    def: DefinitionInner::UserType(UserType::Enum(e)),
                    ..
                } = defs.get_definition(def)
                {
                    core::mem::take(e)
                } else {
                    unreachable!("Expected an adt")
                };

                for (sema_variant, ast_variant) in sema_variants.iter_mut().zip(variants) {
                    sema_variant.discrim = ast_variant
                        .discrim
                        .as_ref()
                        .map(|expr| convert_top_const_expr(defs, sema_mod, expr));
                }

                if let Definition {
                    def: DefinitionInner::UserType(UserType::Enum(e)),
                    ..
                } = defs.get_definition(def)
                {
                    *e = sema_variants;
                } else {
                    unreachable!("Expected an adt")
                }
            }
            parse::Item::TypeAlias {
                attrs,
                vis,
                name,
                generics,
                defn,
            } => todo!("type alias"),
            parse::Item::Trait {
                attrs,
                vis,
                safety,
                auto,
                name,
                generics,
                supertraits,
                body,
            } => todo!("trait"),
            parse::Item::Impl {
                attrs,
                safety,
                generics,
                tr,
                ty,
                body,
            } => todo!("impl"),
            parse::Item::Static { name, ty, init, .. } => {
                let (mt, name) = match name {
                    Pattern::Ident(mt, name) => (*mt, name),
                    pat => panic!("Invalid name for static {:?}", pat),
                };
                let def = defs.find_val_in_mod(sema_mod, name).unwrap();

                let ty = convert_type(defs, sema_mod, ty);

                let init = convert_top_const_expr(defs, sema_mod, init);

                defs.get_definition(def).def = DefinitionInner::Static {
                    ty,
                    mutability: mt,
                    init,
                };
            }
            parse::Item::Const {
                name: Pattern::Discard,
                ty,
                init,
                ..
            } => todo!("anon const"),
            parse::Item::Const { name, ty, init, .. } => {
                let name = match name {
                    Pattern::Ident(Mutability::Const, name) => name,
                    pat => panic!("Invalid name for const {:?}", pat),
                };

                let def = defs.find_val_in_mod(sema_mod, name).unwrap();

                let ty = convert_type(defs, sema_mod, ty);

                let val = convert_top_const_expr(defs, sema_mod, init.as_ref().unwrap());

                defs.get_definition(def).def = DefinitionInner::Const { ty, val };
            }
            parse::Item::Signal {
                attrs,
                vis,
                direction,
                name,
                ty,
            } => {}
            _ => panic!("Unexpected item"),
        }
    }
}

pub fn tycheck_crate(defs: &mut Definitions, sema_mod: DefId, ast_mod: &Mod){
    for item in &ast_mod.items{
        match item{
            parse::Item::FnDeclaration { name, .. } => {
                let defid = defs.find_val_in_mod(sema_mod, name).unwrap();

                let stats = replace_with::replace_with_or_abort_and_return(&mut defs.get_definition(defid).def, |f|{
                    match f{
                        DefinitionInner::HirFunction(fnty, stats) => (stats,DefinitionInner::IncompleteFunction(fnty)),
                        _ => unreachable!()
                    }
                });

                let mut tyinfo = tycheck::ScopeTypeInfo{
                    defs,
                    localmuts: FxHashMap::with_hasher(Default::default()),
                    localtys: FxHashMap::with_hasher(Default::default()),
                    parent: defid
                };

                let stats = stats.into_iter().map(|stat| tycheck::tycheck_stat(&mut tyinfo, stat, false)).collect::<Result<Vec<_>,_>>().unwrap();
                replace_with::replace_with_or_abort(&mut defs.get_definition(defid).def, |f|{
                    match f{
                        DefinitionInner::IncompleteFunction(fnty) => DefinitionInner::ThirFunction(fnty, stats),
                        _ => unreachable!()
                    }
                })
            }
            parse::Item::Mod { name, content: Some(md), .. } => {
                let defid = defs.find_type_in_mod(sema_mod, name).unwrap();
                tycheck_crate(defs, defid, md);   
            }
            _ => {}
        }
    }
}

pub fn lower_crate(defs: &mut Definitions, sema_mod: DefId, ast_mod: &Mod){
    for item in &ast_mod.items{
        match item{
            parse::Item::FnDeclaration { name, .. } => {
                let defid = defs.find_val_in_mod(sema_mod, name).unwrap();

                let (fnty,stats) = replace_with::replace_with_or_abort_and_return(&mut defs.get_definition(defid).def, |f|{
                    match f{
                        DefinitionInner::ThirFunction(fnty, stats) => ((fnty,stats),DefinitionInner::Empty),
                        _ => unreachable!()
                    }
                });

                let mut mir_builder = mir::FunctionConvert::new(&fnty, defs);

                for stat in stats{
                    mir_builder.convert_thir_stat(stat);
                }
                let mut blocks = mir_builder.build();

                for &opt in crate::opt::OPT_PASSES{
                    opt.transform_fn(&mut blocks, defs);
                }
                defs.get_definition(defid).def = DefinitionInner::MirFunction(fnty, blocks);
            }
            parse::Item::Mod { name, content: Some(md), .. } => {
                let defid = defs.find_type_in_mod(sema_mod, name).unwrap();
                tycheck_crate(defs, defid, md);   
            }
            _ => {}
        }
    }
}

pub fn analyze_crate(defs: &mut Definitions, root_mod: &Mod) {
    let root_defid = defs.next_defid();
    let sema_mod = Module {
        parent: DefId(0),
        types: HashMap::new(),
        values: HashMap::new(),
    };
    defs.defs.insert(
        root_defid,
        Definition {
            visible_from: DefId(0),
            attrs: root_mod.attrs.clone(),
            def: DefinitionInner::Module(sema_mod),
        },
    );
    defs.curcrate = root_defid;
    collect_submods(defs, root_mod, root_defid);
    collect_type_names(defs, root_mod, root_defid);
    collect_value_names(defs, root_mod, root_defid);
    convert_types(defs, root_mod, root_defid);
    convert_items(defs, root_mod, root_defid);
    tycheck_crate(defs, root_defid, root_mod);
    lower_crate(defs,root_defid,root_mod);
}
