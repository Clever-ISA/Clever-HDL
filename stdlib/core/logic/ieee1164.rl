


#[lang = "logic9"]
pub enum Logic{
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

impl From<super::ieee1364::Logic> for Logic{
    fn from(v: super::ieee1364::Logic) -> Self{
        match v{
            super::ieee1364::Logic::One => Self::StrongOne,
            super::ieee1364::Logic::HighImpedence => Self::HighImpedence,
            super::ieee1364::Logic::DontCare => Self::DontCare,
            super::ieee1364::Logic::Zero => Self::StrongZero,
        }
    }
}

impl From<super::tristate::Logic> for Logic{
    fn from(v: super::tristate::Logic) -> Self{
        match v{
            super::tristate::Logic::One => Self::StrongOne,
            super::tristate::Logic::HighImpedence => Self::HighImpedence,
            super::tristate::Logic::Zero => Self::StrongZero,
        }
    }
}