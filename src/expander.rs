use std::{collections::VecDeque, path::{PathBuf,Path}, io::ErrorKind};

use crate::{parse::Mod, ReadExt};





#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModuleSearch{
    base: PathBuf,
    stack: VecDeque<PathBuf>,
    curr_search: Option<PathBuf>,
}

impl ModuleSearch{
    pub fn new(base: PathBuf) -> ModuleSearch{
        ModuleSearch{base, stack: VecDeque::new(), curr_search: None}
    }

    pub fn error_search(&self) -> &Path{
        self.curr_search.as_deref().unwrap()
    }
}


pub fn find_modules(root: &mut Mod, search: &mut ModuleSearch) -> std::io::Result<()>{
    for decl in &mut root.items{
        match decl{
            crate::parse::Item::MacroExpansion { attrs, target, args } => todo!("expand macros"),
            crate::parse::Item::MacroRules { attrs, visibility, name, content } => todo!("macros"),
            crate::parse::Item::Mod { name, vis, content } => {
                if content.is_none(){
                    let mut path = search.base.clone();
                    path.push(&*name);

                    path.set_extension("rl");

                    eprintln!("Trying {} for {}",path.display(),name);

                    match std::fs::metadata(&path){
                        Ok(m) => {
                            search.curr_search = Some(path.clone());
                            let mut file = std::fs::File::open(&path)?;
                            let mut chars = utf::decode_utf8(std::iter::from_fn(|| file.read_single().ok()))
                                .map(|r|r.unwrap());

                            let toks = crate::lex::lex(&mut chars);

                            *content = Some(crate::parse::parse_mod(toks.into_iter(), Vec::new()));
                            search.curr_search = None;
                        }
                        _ => {
                            path.set_file_name(&*name);
                            path.push("mod.rl");
                            search.curr_search = Some(path.clone());
                            let mut file = std::fs::File::open(&path)?;
                            let mut chars = utf::decode_utf8(std::iter::from_fn(|| file.read_single().ok()))
                                .map(|r|r.unwrap());

                            let toks = crate::lex::lex(&mut chars);

                            *content = Some(crate::parse::parse_mod(toks.into_iter(), Vec::new()));
                            search.curr_search = None;
                        }
                    }
                }

                let content = content.as_mut().unwrap();
                search.stack.push_back(search.base.clone());
                search.base.push(&*name);
                find_modules(content,search)?;
                search.base = search.stack.pop_back().unwrap();
            },
            _ => {}
        }
    }
    Ok(())
}