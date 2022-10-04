

pub mod ieee1164;
pub mod ieee1364;
pub mod tristate;

trait Sealed{}

pub trait LogicUnit : Sealed{}

impl LogicUnit for ieee1164::Logic{}
impl LogicUnit for ieee1364::Logic{}
impl LogicUnit for tristate::Logic{}
impl LogicUnit for bool{}
impl LogicUnit for u1{}