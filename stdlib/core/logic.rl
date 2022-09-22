

#[lang = "logic_9state"]
#[derive(Copy,Clone)]
pub enum Logic9{
    StrongZero,
    WeakZero,
    StrongUnknown,
    WeakUnknown,
    HighImpedence,
    DontCare,
    Unconnected,
    WeakOne,
    StrongOne,
}

#[lang = "logic_4state"]
#[derive(Copy,Clone)]
pub enum Logic4{
    None,
    False,
    True,
    Both
}

#[lang = "logic_3state"]
#[derive(Copy,Clone)]
pub enum Tristate{
    False,
    HighImpedence,
    True,
}

#[lang = "signal_trigger"]
#[derive(Copy,Clone)]
pub enum SignalTrigger{
    #[lang = "trigger_low"]
    LowSignal,
    #[lang = "trigger_fall"]
    FallingEdge,
    #[lang = "trigger_high"]
    HighSignal,
    #[lang = "trigger_rise"]
    RisingEdge,
}


#[sealed]
#[lang = "unit_logic"]
pub trait UnitLogic{}

impl UnitLogic for bool{}
impl UnitLogic for Tristate{}
impl UnitLogic for Logic4{}
impl UnitLogic for Logic9{}
impl UnitLogic for u1{}

#[lang = "signal_future"]
pub trait Signal<const TRIGGER: SignalTrigger>{
    fn poll_edge(&mut self,ctx: &mut core::future::Context) -> core::future::Poll<()>;
}

impl<const TRIGGER: SignalTrigger, T: UnitLogic> Signal for &in T{
    fn poll_edge(&mut self, ctx: &mut core::future::Context) -> core::future::Poll<()>{
        match TRIGGER{
            SignalTrigger::LowSignal => (*self).await low_signal,
            SignalTrigger::FallingEdge => (*self).await falling_edge,
            SignalTrigger::HighSignal => (*self).await high_signal,
            SignalTrigger::RisingEdge => (*self).await rising_edge,
        }
    }
}

impl<const TRIGGER: SignalTrigger, T: UnitLogic> Signal for &inout T{
    fn poll_edge(&mut self, ctx: &mut core::future::Context) -> core::future::Poll<()>{
        match TRIGGER{
            SignalTrigger::LowSignal => (*self).await low_signal,
            SignalTrigger::FallingEdge => (*self).await falling_edge,
            SignalTrigger::HighSignal => (*self).await high_signal,
            SignalTrigger::RisingEdge => (*self).await rising_edge,
        }
    }
}