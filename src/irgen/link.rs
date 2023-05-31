#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Linkage {
    ExternDef,
    ExternDefWeak,
    Strong,
    LinkOnce,
    Weak,
    InlineStrong,
    Private,
}
