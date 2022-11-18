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
    fmt::Write, convert,
};

pub use crate::ssa::SsaExpr;
pub use crate::parse::{Meta, Mutability, Safety, SignalDirection, AsyncFnTy};
use crate::{
    lang::{LangItem, LangItemTarget},
    parse::{self, Mod, Pattern, Visibility},
};

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefId(u64);

impl core::fmt::Debug for DefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_char('#')?;
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
pub enum UserType{
    Enum(Vec<EnumVariant>),
    Struct(Constructor),
    Union(Vec<StructField>)
}

#[derive(Clone, Debug)]
pub enum DefinitionInner {
    IncompleteType,
    IncompleteAlias,
    IncompleteFunction(FunctionType),
    IncompleteStatic(Type),
    IncompleteConst(Type),
    Signal(Type, SignalDirection),
    Module(Module),
    Enum(Vec<EnumVariant>)
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LogicType {
    Binary,
    Tristate,
    Ieee1364,
    Ieee1164,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct IntType {
    pub signed: bool,
    pub logic: LogicType,
    pub bits: u16,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Repr{
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
    FnItem(DefId),
    Signal(SignalDirection, SemaLifetime, Box<Type>),
    Tuple(Vec<Type>),
    Never,
    Pointer(Mutability, Box<Type>),
    Char,
    Inferable,
    Str,
    Slice(Box<Type>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleField{
    pub visible_from: DefId,
    pub attrs: Vec<Meta>,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField{
    pub visible_from: DefId,
    pub attrs: Vec<Meta>,
    pub name: String,
    pub ty: Type,
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constructor{
    Unit,
    Tuple(Vec<TupleField>),
    Struct(Vec<StructField>)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumVariant{
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionType {
    async_ty: AsyncFnTy,
    safety: Safety,
    constness: Mutability,
    params: Vec<Type>,
    retty: Box<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CtorConst{
    Unit,
    Tuple(Vec<ConstVal>),
    Struct(BTreeMap<String,ConstVal>)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstVal {
    ScalarConst(u128),
    LargeScalar(Vec<u8>),
    LogicVal(LogicVal),
    GenericParam(u32),
    ArrayLit(Vec<ConstVal>),
    ArrayRepeat {
        base: Box<ConstVal>,
        repeat: Box<ConstVal>,
    },
    ConstDef(DefId),
    Constructor{
        ty: DefId,
        name: Option<String>,
        inner: CtorConst,
    },
    Complex(Box<SsaExpr>)
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
    crates: HashMap<String, DefId>,
    lang_items: HashMap<LangItem, DefId>,
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            nextdefid: DefId(1),
            curcrate: DefId(0),
            defs: BTreeMap::new(),
            crates: HashMap::new(),
            lang_items: HashMap::new(),
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
                    DefinitionInner::IncompleteType => {
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
                let sig = FunctionType{
                    async_ty: *is_async,
                    constness: if *is_const{Mutability::Const}else{Mutability::Mut},
                    safety: *safety,
                    params: params.iter().map(|ty|{
                        convert_type(defs, sema_mod, ty.ty.as_ref().unwrap())
                    }).collect(),
                    retty: Box::new(return_ty.as_ref().map(|ty|convert_type(defs, sema_mod, ty)).unwrap_or(Type::Tuple(Vec::new())))
                };
                let def = DefinitionInner::IncompleteFunction(sig);

                defs.insert_value(sema_mod, name, Definition { visible_from: vis, attrs: attrs.clone(), def });
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


pub fn convert_top_const_expr(defs: &mut Definitions, cur_mod: DefId, expr: &parse::Expr) -> ConstVal{
    match expr{
        parse::Expr::Block(_) => todo!("block"),
        parse::Expr::LetExpr(_, _) => todo!("let expr"),
        parse::Expr::Id(path) => {
            let root = path.root.clone();
            let mut components = path.components.clone();
            let last = components.pop().unwrap(); // fixme: currently assumes no generics 
            let (def,ctor_name) = if root==None&&components.len()==0{
                match last{
                    parse::PathComponent::Id(id) => {
                        if let Some(Definition { def: DefinitionInner::Module(md),.. }) = defs.get_definition(cur_mod){
                            (md.values[&id],None)
                        }else{
                            unreachable!()
                        }
                    },
                    _ => todo!("Handle generics properly")
                }
            }else{
                let ty = defs.find_type_def(cur_mod, &Path{root,components}).unwrap_or_else(||{
                    panic!("No such type {:?}",path)
                });
            };
        },
        parse::Expr::FunctionCall { func, args } => todo!(),
        parse::Expr::Cast(_, _) => todo!(),
        parse::Expr::StringLiteral(_, _) => todo!(),
        parse::Expr::CharLiteral(_, _) => todo!(),
        parse::Expr::Parentheses(_) => todo!(),
        parse::Expr::MacroExpansion { target, args } => todo!(),
        parse::Expr::IntLiteral(_) => todo!(),
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

pub fn convert_types(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId){
    for item in &ast_mod.items{
        match item{
            parse::Item::ExternBlock { .. } => todo!("extern block"),
            parse::Item::Static { .. } |
            parse::Item::Const { .. } |
            parse::Item::Signal { .. } |
            parse::Item::FnDeclaration { .. } => {},
            parse::Item::Type(_) => todo!(),
            parse::Item::Mod { name, vis, content: Some(md) } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                convert_items(defs,md,def);
            },
            parse::Item::Adt { attrs, name, generics, vis, variants } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                let mut repr = Repr::Default;
                for attr in attrs{
                    match attr{
                        Meta::Group(id, inner) if matches_simple_path!(id,repr) => {
                            for i in inner{
                                match i{
                                    Meta::Ident(id) if matches_simple_path!(id,C) => repr = Repr::Vhdl,
                                    Meta::Ident(id) if matches_simple_path!(id,vhdl) => repr = Repr::Vhdl,
                                    Meta::Ident(id) if id.root.is_none()&&id.idents.len()==1 => {
                                        let id = &id.idents[0];

                                        if id.starts_with('i'){
                                            let bits = id[1..].parse::<u16>().expect("Invalid repr attribute");
                                            repr = Repr::Int(IntType{signed: true, logic: LogicType::Binary, bits});
                                        }else if id.starts_with('u'){
                                            let bits = id[1..].parse::<u16>().expect("Invalid repr attribute");
                                            repr = Repr::Int(IntType{signed: false, logic: LogicType::Binary, bits});
                                        }else{
                                            panic!("Invalid repr attribute {}",id)
                                        }
                                    }
                                    m => panic!("Invalid repr atribute, {:?}",m)
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let mut variants = variants.iter().map(|var|{
                    let attrs = var.attrs.clone();
                    let name = var.name.clone();
                    
                    let ctor = match &var.ctor{
                        parse::StructBody::Unit => Constructor::Unit,
                        parse::StructBody::Tuple(fields) => {
                            let mut sema_fields = Vec::new();
                            for field in fields{
                                let attrs = field.attrs.clone();
                                let vis = DefId(0); // all enum fields are pub by default
                                let ty = convert_type(defs, sema_mod, &field.ty);
                                sema_fields.push(TupleField{visible_from: vis, attrs, ty})
                            }
                            Constructor::Tuple(sema_fields)
                        }
                        parse::StructBody::Struct(fields) => {
                            let mut sema_fields = Vec::new();
                            for field in fields{
                                let attrs = field.attrs.clone();
                                let visible_from = DefId(0); // all enum fields are pub by default
                                let ty = convert_type(defs, sema_mod, &field.ty);
                                let name = field.name.to_string();
                                sema_fields.push(StructField{ visible_from, attrs, name, ty });
                            }
                            Constructor::Struct(sema_fields)
                        }
                    };
                    let discrim = var.discrim.as_ref().map(|expr| todo!());
                    EnumVariant{ attrs, name, ctor, discrim }
                }).collect::<Vec<_>>();
                defs.get_definition(def).def = DefinitionInner::Enum(variants);
            },
            parse::Item::TypeAlias { attrs, vis, name, generics, defn } => todo!(),
            parse::Item::Trait { attrs, vis, safety, auto, name, generics, supertraits, body } => todo!(),
            parse::Item::Impl { attrs, safety, generics, tr, ty, body } => todo!(),
            _ => unreachable!("invalid item")
        }
    }
}

pub fn convert_items(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId){
    for item in &ast_mod.items{
        match item{
            parse::Item::ExternBlock { attrs, abi, items } => todo!("extern"),
            parse::Item::FnDeclaration { attrs, visibility, is_const, is_async, safety, abi, name, generics, params, return_ty, block } => todo!(),
            parse::Item::MacroExpansion { attrs, target, args } => unreachable!("macro expansion"),
            parse::Item::MacroRules { attrs, visibility, name, content } => unreachable!("macro rules"),
            parse::Item::Type(_) => todo!(),
            parse::Item::Mod { name, content: Some(md), .. } => {
                let def = defs.find_type_in_mod(sema_mod, name).unwrap();
                convert_items(defs,md,def);
            },
            parse::Item::Adt { name, variants, attrs, .. } => {
                
            },
            parse::Item::TypeAlias { attrs, vis, name, generics, defn } => todo!("type alias"),
            parse::Item::Trait { attrs, vis, safety, auto, name, generics, supertraits, body } => todo!("trait"),
            parse::Item::Impl { attrs, safety, generics, tr, ty, body } => todo!("impl"),
            parse::Item::Static { attrs, vis, name, ty, init } => todo!("static"),
            parse::Item::Const { attrs, vis, name, ty, init } => todo!("const"),
            parse::Item::Signal { attrs, vis, direction, name, ty } => todo!("signal"),
            _ => panic!("Unexpected item")
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
    convert_items(defs, root_mod, root_defid);
}
