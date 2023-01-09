#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LangItemTarget {
    Function,
    Adt,
    Constructor,
    Struct,
    Trait,
    AssociatedType,
    Item,
    Impl,
}

use LangItemTarget::*;

macro_rules! def_lang_items{
    [$($lang_name:ident : {$enum:ident @ $targ:expr}),* $(,)?] => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[repr(u32)]
        pub enum LangItem{
            $($enum),*
        }

        impl LangItem{
            pub fn from_item_name(name: &str) -> Option<LangItem>{
                match name{
                    $(::core::stringify!($lang_name) => Some(Self::$enum),)*
                    _ => None,
                }
            }

            pub const fn name(self) -> &'static str{
                match self{
                    $(Self::$enum => ::core::stringify!($lang_name)),*
                }
            }

            pub const fn target(self) -> LangItemTarget{
                match self{
                    $(Self::$enum => $targ),*
                }
            }
        }
    }
}

def_lang_items![
    sized: {Sized @ Trait},
    copy: {Copy @ Trait},
    clone: {Clone @ Trait},
    drop: {Drop @ Trait},
    destruct: {Destruct @ Trait},
    fn_once: {FnOnce @ Trait},
    fn_mut: {FnMut @ Trait},
    fn: {Fn @ Trait},
    deref: {Deref @ Trait},
    deref_mut: {DerefMut @ Trait},
    logic3: {LogicTri @ Adt},
    logic4: {Logic1364 @ Adt},
    logic9: {Logic1164 @ Adt},
    phantom_data: {PhantomData @ Adt},
    sync: {Sync @ Trait},
    main: {Main @ Function},
    manually_drop: {ManuallyDrop @ Struct},
];

impl LangItem{
    pub const fn builtin_impl_traits() -> &'static [LangItem]{
        &[Self::Copy, Self::Clone, Self::Destruct]
    }
}