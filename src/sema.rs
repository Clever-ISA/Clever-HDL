
use std::{collections::{HashMap,BTreeMap}, fmt::Write};

use crate::{lang::LangItem, parse::{self, Mod}};
pub use crate::parse::{Mutability,SignalDirection,Safety};

#[derive(Copy,Clone,Hash,PartialEq,Eq, PartialOrd,Ord)]
pub struct DefId(u64);

impl core::fmt::Debug for DefId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        f.write_char('#')?;
        self.0.fmt(f)
    }
}

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

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub enum SemaLifetime{
    GenericParam(u32),
    Static,
    Erased,
    Region(u32)
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
    Signal(SignalDirection,SemaLifetime,Box<Type>)
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
    curcrate: DefId,
    defs: BTreeMap<DefId,Definition>,
    crates: HashMap<String,DefId>,
    lang_items: HashMap<LangItem,DefId>
}

impl Definitions{
    pub fn new() -> Self{
        Self { nextdefid: DefId(1), curcrate: DefId(0), defs: BTreeMap::new(), crates: HashMap::new(), lang_items: HashMap::new() }
    }
    pub fn next_defid(&mut self) -> DefId{
        let ret = self.nextdefid;
        self.nextdefid.0 = self.nextdefid.0.checked_add(1).expect("More than 2^64 definitions in a program.");
        ret
    }

    pub fn get_definition(&mut self, defid: DefId) -> &mut Definition{
        self.defs.get_mut(&defid).expect("Expected a definition")
    }

    pub fn insert_type(&mut self, targ_mod: DefId, name: &str, def: Definition) -> DefId{
        let defid = self.next_defid();
        if let Some(Definition::Module(md)) = self.defs.get_mut(&targ_mod){
            md.types.insert(name.to_string(),defid);
        }else{
            panic!("Expected a module for current module {:?}",targ_mod);
        }
        self.defs.insert(defid,def);

        defid
    }

    pub fn find_type_in_mod(&mut self, cur_mod: DefId, name: &str) -> Option<DefId>{
        if let Some(Definition::Module(md)) = self.defs.get(&cur_mod){
            md.types.get(name).copied()
        }else{
            panic!("Expected a module for current module {:?}",cur_mod);
        }
    }

    pub fn find_type(&mut self, cur_mod: DefId, path: &parse::Path) -> Option<&mut Definition>{
        let mut base = match &path.root{
            Some(parse::PathRoot::Crate) => self.curcrate,
            Some(parse::PathRoot::Root) => DefId(0),
            Some(parse::PathRoot::SelfPath) => cur_mod,
            Some(parse::PathRoot::Super) => {
                if let Some(Definition::Module(md)) = self.defs.get_mut(&cur_mod){
                    md.parent
                }else{
                    panic!("Expected a module for current module {:?}",cur_mod)
                }
            }
            Some(root) => panic!("Expected a module prefix for path, got {:?} instead",root),
            None => {
                let first_id = path.components.first().expect("No-prefix path contains no components");
                match first_id{
                    parse::PathComponent::Id(id) => {
                        if let Some(Definition::Module(md)) = self.defs.get_mut(&cur_mod){
                            if md.types.contains_key(id){
                                cur_mod
                            }else{
                                DefId(0)
                            }
                        }else{
                            panic!("Expected a module for current module {:?}",cur_mod)
                        }
                    },
                    parse::PathComponent::Generics(_) => panic!("Got generics at start of a no-prefix path"),
                }
            }
        };

        for comp in &path.components{
            match comp{
                parse::PathComponent::Id(id) => {
                    if let Some(Definition::Module(md)) = self.defs.get(&base){
                        base = *md.types.get(id)?
                    }else{
                        panic!("Expected a module for current module {:?}",cur_mod)
                    }
                },
                parse::PathComponent::Generics(generics) => todo!("generics {generics:?}"),
            }
        }

        self.defs.get_mut(&base)
    }
}


pub fn collect_submods(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId){
    for i in &ast_mod.items{
        match i{
            parse::Item::Mod { name, content: Some(md), .. } => {
                let inner_md = Module{parent: sema_mod,types: HashMap::new(), values: HashMap::new()};

                let submod_id = defs.insert_type(sema_mod, name, Definition::Module(inner_md));

                collect_submods(defs, md, submod_id);
            },
            _ => {}
        }
    }
}

pub fn collect_type_names(defs: &mut Definitions, ast_mod: &Mod, sema_mod: DefId){
    for i in &ast_mod.items{
        match i{
            parse::Item::Mod { name, content: Some(md),.. } => {
                let next_mod = defs.find_type_in_mod(sema_mod, name).unwrap();
                collect_type_names(defs, md, next_mod);
            }
            parse::Item::TypeAlias { name, .. } => {
                defs.insert_type(sema_mod, name, Definition::IncompleteAlias);
            }
            parse::Item::Type(ty) => {
                defs.insert_type(sema_mod, &ty.name,Definition::IncompleteType);
            }
            parse::Item::Adt { name, .. } => {
                defs.insert_type(sema_mod, name,Definition::IncompleteType);
            }
            _ => {}
        }
    }
}


pub fn analyze_crate(defs: &mut Definitions, root_mod: &Mod){
    let root_defid = defs.next_defid();
    let sema_mod = Module{
        parent: DefId(0),
        types: HashMap::new(),
        values: HashMap::new()
    };
    defs.defs.insert(root_defid, Definition::Module(sema_mod));
    defs.curcrate = root_defid;
    collect_submods(defs, root_mod, root_defid);
    collect_type_names(defs, root_mod, root_defid);
}