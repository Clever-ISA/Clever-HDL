pub mod lex;
pub mod parse;
pub mod expander;
pub mod sema;
pub mod lang;

use std::{io::Read, path::PathBuf};

use expander::ModuleSearch;

use crate::sema::{Definitions, analyze_crate};

macro_rules! unwrap_or_error{
    ($val:expr, $($fmt:tt)*) => {
        match $val{
            Ok(v) => v,
            Err(e) => {
                eprintln!($($fmt)* , error = e);
                std::process::exit(1)
            }
        }
    }
}

fn print_help(prg_name: &str){
    eprintln!("Usage: {} [OPTIONS...] [--] <input file>\n",prg_name);
    eprintln!("Compiles Clever-HDL files");
    eprintln!("\n");
    eprintln!("OPTIONS:");
    eprintln!("\t--help, -h: Prints this message, and exits");
    eprintln!("\t--version, -V: Prints version information and exis");

}


pub trait ReadExt {
    fn read_single(&mut self) -> std::io::Result<u8>;
}

impl<R: Read> ReadExt for R {
    fn read_single(&mut self) -> std::io::Result<u8> {
        let mut val = 0u8;
        self.read_exact(core::slice::from_mut(&mut val))?;
        Ok(val)
    }
}

fn main() {
    let mut args = std::env::args();

    let prg_name = args.next().unwrap();

    let mut input_file = None::<String>;

    while let Some(arg) = args.next(){
        match &*arg{
            "--help" => {
                print_help(&prg_name);
                std::process::exit(0);
            }
            "--version" => {
                eprintln!("clever-hdl v{}",std::env!("CARGO_PKG_VERSION"));
                eprintln!("Copyright (C) 2022 Connor Horman");
                eprintln!("Released under the terms of the BSD-2 + Patent license");
                std::process::exit(1);
            }
            "--" => {
                input_file = Some(args.next().unwrap_or_else(||{
                    eprintln!("Expected an input file name");
                    std::process::exit(1);
                }));
                break;
            }
            x if x.starts_with("--") => {
                eprintln!("Unknown option {}. Run {} --help for list of options",x,prg_name);
                std::process::exit(1);
            }
            x if x.starts_with('-') => {
                for c in x[1..].chars(){
                    match c{
                        'h' => {
                            print_help(&prg_name);
                            std::process::exit(0);
                        }
                        'V' => {
                            eprintln!("clever-hdl v{}",std::env!("CARGO_PKG_VERSION"));
                            eprintln!("Copyright (C) 2022 Connor Horman");
                            eprintln!("Released under the terms of the BSD-2 + Patent license");
                            std::process::exit(1);
                        }
                        x => {
                            eprintln!("Unknown short option -{}. Run {} --help for list of options",x,prg_name);
                        }
                    }
                }
            }
            x => {
                input_file = Some(arg);
                break;
            }
        }
    }

    let input_file = input_file.unwrap_or_else(||{
        eprintln!("Expected an input file name");
        std::process::exit(1);
    });

    let mut file = unwrap_or_error!(std::fs::File::open(&input_file),"Failed to open {}: {error}",input_file);

    let mut chars = utf::decode_utf8(std::iter::from_fn(|| file.read_single().ok()))
        .map(|r|unwrap_or_error!(r,"Cannot Read {}: Invalid UTF-8 ({error:?})",input_file));

    let toks = lex::lex(&mut chars);

    let mut file = parse::parse_mod(toks.into_iter(), Vec::new());

    let input_file = PathBuf::from(input_file);

    let mut input_file = unwrap_or_error!(input_file.canonicalize(),"Cannot find path containing {}: {error}",input_file.display());
    input_file.pop();

    let mut search = ModuleSearch::new(input_file);

    unwrap_or_error!(expander::find_modules(&mut file, &mut search),"Cannot open {} for reading: {error}",search.error_search().display());
    
    let mut defs = Definitions::new();

    analyze_crate(&mut defs, &file);

    println!("All definitions: {:#?}",defs);

}
