pub trait Increment{
    /// `++self`
    fn increment_and_fetch(&mut self) -> Self;
    /// `self++`
    fn fetch_and_increment(&mut self) -> Self;
    /// `--self`
    fn decrement_and_fetch(&mut self) -> Self;
    /// `self--`
    fn fetch_and_decrement(&mut self) -> Self;
}

macro_rules! impl_increment{
    ($($tys:ty),* $(,)?) => {
        $(
            impl Increment for $tys{
                fn increment_and_fetch(&mut self) -> Self{
                    *self += 1;
                    *self
                }
                fn fetch_and_increment(&mut self) -> Self{
                    let val = *self;
                    *self += 1;
                    val
                }

                fn decrement_and_fetch(&mut self) -> Self{
                    *self -= 1;
                    *self
                }
                fn fetch_and_decrement(&mut self) -> Self{
                    let val = *self;
                    *self -= 1;
                    val
                }
            }
        )*
    }
}

impl_increment!(i8, i16, i32, i64, i128, u8, u16, u32,u64, u128,isize,usize);