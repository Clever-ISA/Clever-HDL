
#[lang = "logic4"]
pub enum Logic{
    One,
    DontCare,
    HighImpedence,
    Zero,
}

impl From<Logic3> for Logic{
    fn from(v: Logic3) -> Self{
        match v{
            Logic3::One => Self::One,
            Logic3::HighImpedence => Self::HighImpedence,
            Logic3::Zero => Self::Zero,
        }
    }
}