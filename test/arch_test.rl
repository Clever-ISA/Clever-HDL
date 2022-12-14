
#[lang = "logic9"]
pub enum Logic1164{
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

#[lang = "logic4"]
pub enum Logic1364{
    One,
    DontCare,
    HighImpedence,
    Zero,
}

#[lang = "logic3"]
pub enum LogicTristate{
    One,
    HighImpedence,
    Zero,
}


pub static FOO: LogicTristate = LogicTristate::One;
pub const BAR: i13 = 2048;


pub signal in G_CLOCK: bool;

pub proc main(){
    loop{G_CLOCK.await falling_edge}
}
