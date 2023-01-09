

use std::fmt::Write;

use crate::sema::{Type,DefId, LogicType};

use fxhash::FxHashMap;


fn hardcoded_sub(st: &[&str]) -> Option<&'static str>{
    match st{
        ["std"] => Some("St"),
        _ => None
    }
}

pub struct SubstitutionMap<'a>{
    subtab: FxHashMap<&'a [&'a str], u32>,
}

pub struct MangleCache{
    cache: FxHashMap<DefId, String>
}



impl MangleCache{
    pub fn new() -> Self{
        Self{cache: FxHashMap::with_hasher(Default::default())}
    }

    pub fn insert_type(&mut self, name: &[&str], def: DefId) {
        
    }
}

pub fn mangle_type(cache: &MangleCache,ty: &Type, out: &mut String){
    match ty{
        Type::Char => out.write_str("Di").unwrap(),
        Type::Never => out.write_str("u5never").unwrap(),
        Type::Tuple(tup) => {
            if tup.len()==0{
                out.write_str("u4unit").unwrap();
            }else{
                out.write_str("u5tupleI").unwrap();
                for ty in tup{
                    mangle_type(cache,ty,out);
                }
                out.write_str("E").unwrap();
            }
        }
        Type::Str => out.write_str("u5sliceIDuE").unwrap(),
        Type::Slice(ty) => {
            out.write_str("u5sliceE").unwrap();
            mangle_type(cache,ty,out);
            out.write_str("E").unwrap()
        }
        Type::UserDef(ty) => out.write_str(&cache.cache[ty]).unwrap(),
        _ => todo!()
    }
}