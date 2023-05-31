use fxhash::FxHashMap;
use llhd::ty;

use crate::sema::DefId;

pub struct LlhdGenerator {
    mod_out: llhd::ir::Module,
    ty_map: FxHashMap<DefId, ty::Type>,
}
