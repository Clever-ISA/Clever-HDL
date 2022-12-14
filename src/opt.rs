use fxhash::FxHashSet;

use crate::sema::{mir::{BasicBlock, SsaTerminator}, Definitions};


pub trait OptimizationPass{
    fn name(&self) -> &'static str;
    fn transform_fn(&self, bbs: &mut Vec<BasicBlock>, defs: &mut Definitions);
}

pub struct EliminateStaticUnreached{

}

impl OptimizationPass for EliminateStaticUnreached{
    fn name(&self) -> &'static str{
        "eliminate-static-unreachable"
    }

    fn transform_fn(&self, bbs: &mut Vec<BasicBlock>, _: &mut Definitions) {
        let mut cfg = Vec::new();

        for bb in &*bbs{
            match &bb.term{
                SsaTerminator::Await(ssaawait) => {
                    cfg.push((bb.id,vec![ssaawait.resume_target.target_bb]))
                },
                SsaTerminator::AwaitSignal(sigawait) => {
                    cfg.push((bb.id,vec![sigawait.resume_target.target_bb]))
                }
                SsaTerminator::Branch(branch) => {
                    cfg.push((bb.id,vec![branch.if_true_target.target_bb,branch.else_target.target_bb]));
                }
                SsaTerminator::Jump(jmp) => {
                    cfg.push((bb.id,vec![jmp.target_bb]));
                }
                SsaTerminator::FunctionCall(call) => {
                    cfg.push((bb.id,vec![call.target.target_bb]))
                }
                SsaTerminator::TailCall(_) | SsaTerminator::Return(_) | SsaTerminator::Unreachable => {
                    cfg.push((bb.id,vec![]));
                }
                SsaTerminator::Switch(switch) => {
                    cfg.push((bb.id,switch.case_targets.iter().map(|(_,jmp)|jmp).chain(core::iter::once(&switch.default_target)).map(|targ|targ.target_bb).collect()))
                }
            }
        }

        
        loop{
            let mut used = FxHashSet::<u32>::with_hasher(Default::default());

            for (_,reached_targets) in &cfg{
                used.extend(reached_targets);
            }

            let mut old_cfg = core::mem::take(&mut cfg);
            cfg = old_cfg.iter().filter(|&(id,_)| used.contains(id)||(*id==0)).cloned().collect();
            if old_cfg.len()==cfg.len(){
                break
            }
        }

        let mut used = FxHashSet::<u32>::with_hasher(Default::default());

        for (_,reached_targets) in &cfg{
            used.extend(reached_targets);
        }


        bbs.retain(|elem| used.contains(&elem.id)||elem.id==0);
    }
}


pub static OPT_PASSES: &[&(dyn OptimizationPass+Sync)] = &[&EliminateStaticUnreached{}]; 