use crate::{
    lex::Span,
    sema::{
        mir::{BinaryOp, SsaVarId, UnaryOp},
        DefId, FieldName, LogicType, LogicVal, Mutability, SignalDirection,
    },
    strings::Symbol,
};

use super::link::Linkage;

macro_rules! impl_visitor_defaults{
    ($($trait:path => {$(fn $visitor_fn:ident(&mut self $(,$($pname:ident : $ty:ty ),* $(,)?)?) $(-> $ret_ty:ty)?;)*})*) => {
        $(impl<V: $trait + ?Sized> $trait for &mut V{
            $(
                #[inline]
                fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)?{
                    <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                }
            )*
        }

        impl<V: $trait + ?Sized> $trait for Box<V>{
            $(
                #[inline]
                fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)?{
                    <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                }
            )*
        }

        impl<V: $trait> $trait for Option<V>{
            $(
                #[inline]
                fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)?{
                    match self{
                        Some(this) => <V as $trait>::$visitor_fn(this, $($($pname),*)?),
                        None => core::default::Default::default()
                    }

                }
            )*
        }
    )*
    }
}

pub trait DefinitionVisitor {
    /// Visit's the definition of a type, either a type alias or a user-defined type
    fn visit_type_definition(&mut self) -> Option<Box<dyn TypeDefinitionVisitor + '_>>;
    fn visit_impl_block(&mut self) -> Option<Box<dyn ImplBlockVisitor + '_>>;
    fn visit_global_value(&mut self) -> Option<Box<dyn GlobalValueVisitor + '_>>;
    fn visit_const_value(&mut self) -> Option<Box<dyn ConstValueVisitor + '_>>;
    fn visit_function_def(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>>;
}

pub trait TypeDefinitionVisitor {
    fn visit_alias(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_struct(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
    fn visit_union(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
    fn visit_enum(&mut self) -> Option<Box<dyn EnumVisitor + '_>>;
}

pub trait ImplBlockVisitor {
    fn visit_trait(&mut self, trdef: DefId);

    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;

    fn visit_associated_const(
        &mut self,
        name: &str,
        def: DefId,
    ) -> Option<Box<dyn ConstValueVisitor + '_>>;

    fn visit_associated_type(
        &mut self,
        name: &str,
        def: DefId,
    ) -> Option<Box<dyn TypeVisitor + '_>>;

    fn visit_method(&mut self, name: &str, def: DefId) -> Option<Box<dyn FunctionDefVisitor + '_>>;
}

pub trait GlobalValueVisitor {
    fn visit_signal_direction(&mut self, sigdir: SignalDirection);
    fn visit_mutability(&mut self, mutab: Mutability);
    fn visit_linkage(&mut self, link: Linkage);
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_initializer(&mut self) -> Option<Box<dyn ConstValueVisitor + '_>>;
}

pub trait ConstValueVisitor {
    fn visit_reference_linkage(&mut self, link: Linkage);
    fn visit_literal_value(&mut self, val: u128);
    fn visit_const_value(&mut self, defid: DefId);
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_unary_operator(&mut self, uop: UnaryOp) -> Option<Box<dyn ConstValueVisitor + '_>>;
    fn visit_binary_operator(
        &mut self,
        binop: BinaryOp,
    ) -> (
        Option<Box<dyn ConstValueVisitor + '_>>,
        Option<Box<dyn ConstValueVisitor + '_>>,
    );
    fn visit_ctor(&mut self, basety: DefId) -> Option<Box<dyn ConstructorValueVisitor + '_>>;
}

pub trait TypeVisitor {
    fn visit_reference(&mut self, mutability: Mutability) -> Option<Box<dyn TypeVisitor + '_>>;

    fn visit_pointer(&mut self, mutability: Mutability) -> Option<Box<dyn TypeVisitor + '_>>;

    fn visit_signal(&mut self, sigdir: SignalDirection) -> Option<Box<dyn TypeVisitor + '_>>;

    fn visit_function_pointer(&mut self) -> Option<Box<dyn FunctionTypeVisitor + '_>>;

    fn visit_tuple(&mut self) -> Option<Box<dyn TupleTypeVisitor + '_>>;

    fn visit_logic_type(&mut self) -> Option<Box<dyn LogicTypeVisitor + '_>>;

    fn visit_named_type(&mut self) -> Option<Box<dyn NamedTypeVisitor + '_>>;

    fn visit_array(&mut self) -> Option<Box<dyn ArrayTypeVisitor + '_>>;

    fn visit_str(&mut self);

    fn visit_char(&mut self);
}

pub trait ConstructorVisitor {
    fn visit_unit_ctor(&mut self);
    fn visit_tuple_ctor(&mut self) -> Option<Box<dyn TupleConstructorVisitor + '_>>;
    fn visit_struct_ctor(&mut self) -> Option<Box<dyn StructConstructorVisitor + '_>>;
}

pub trait EnumVisitor {
    fn visit_base_ty(&mut self) -> Option<Box<dyn LogicTypeVisitor + '_>>;
    fn visit_variant(&mut self) -> Option<Box<dyn VariantVisitor + '_>>;
}

pub trait VariantVisitor {
    fn visit_name(&mut self, name: Symbol);
    fn visit_discrim(&mut self) -> Option<Box<dyn DiscriminantVisitor + '_>>;
    fn visit_ctor(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
}

pub trait DiscriminantVisitor {
    fn visit_int(&mut self, val: i128);
    fn visit_logic_val(&mut self, val: LogicVal);
}

pub trait FunctionDefVisitor {
    fn visit_reciever(&mut self, lnum: SsaVarId);
    fn visit_signature(&mut self) -> Option<Box<dyn FunctionTypeVisitor + '_>>;
    fn visit_local_ty(&mut self, lnum: SsaVarId) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_local_debug(&mut self, lnum: SsaVarId) -> Option<Box<dyn LocalDebugInfoVisitor + '_>>;
    fn visit_basic_block(&mut self, bbid: u32) -> Option<Box<dyn BasicBlockVisitor + '_>>;
}

pub trait ConstructorValueVisitor {
    fn visit_base_ty(&mut self, defid: DefId);
    fn visit_variant(&mut self, var: Symbol);
    fn visit_field(&mut self, field_name: FieldName) -> Option<Box<dyn ConstValueVisitor + '_>>;
}

pub trait FunctionTypeVisitor {
    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
}

pub trait TupleTypeVisitor {
    fn visit_element(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
}

pub trait ArrayTypeVisitor {
    fn visit_element_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_extent(&mut self, len: u64);
}

pub trait LogicTypeVisitor {
    fn visit_logic_type(&mut self, ty: LogicType);
    fn visit_width(&mut self, bits: u16);
    fn visit_signed(&mut self, signed: bool);
}

pub trait NamedTypeVisitor {
    fn visit_defid(&mut self, def: DefId);
}

pub trait TupleConstructorVisitor {
    fn visit_field(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
}

pub trait StructConstructorVisitor {
    fn visit_field(&mut self, name: Symbol) -> Option<Box<dyn TypeVisitor + '_>>;
}

pub trait LocalDebugInfoVisitor {
    fn visit_name(&mut self, name: Symbol);
    fn visit_decl_span(&mut self, span: Span);
}

pub trait BasicBlockVisitor {
    fn visit_statement(&mut self) -> Option<Box<dyn SsaStatementVisitor + '_>>;
    fn visit_terminator(&mut self) -> Option<Box<dyn SsaTerminatorVisitor + '_>>;
}

pub trait SsaStatementVisitor {
    fn visit_declare(&mut self, varid: SsaVarId) -> Option<Box<dyn DeclareStatementVisitor + '_>>;
    fn visit_store_dead(&mut self, varid: SsaVarId);
    fn visit_pin(&mut self, varid: SsaVarId);
}

pub trait SsaTerminatorVisitor {
    fn visit_unreachable(&mut self);
}

pub trait DeclareStatementVisitor {
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_init(&mut self) -> Option<Box<dyn ExpressionVisitor + '_>>;
}

pub trait ExpressionVisitor {}

impl_visitor_defaults! {
    DefinitionVisitor => {
        fn visit_type_definition(&mut self) -> Option<Box<dyn TypeDefinitionVisitor + '_>>;
        fn visit_impl_block(&mut self) -> Option<Box<dyn ImplBlockVisitor + '_>>;
        fn visit_global_value(&mut self) -> Option<Box<dyn GlobalValueVisitor + '_>>;
        fn visit_const_value(&mut self) -> Option<Box<dyn ConstValueVisitor + '_>>;
        fn visit_function_def(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>>;
    }
    TypeDefinitionVisitor => {
        fn visit_alias(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_struct(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
        fn visit_union(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
        fn visit_enum(&mut self) -> Option<Box<dyn EnumVisitor + '_>>;
    }
    ImplBlockVisitor => {
        fn visit_trait(&mut self, trdef: DefId);

        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;

        fn visit_associated_const(&mut self, name: &str, def: DefId) -> Option<Box<dyn ConstValueVisitor + '_>>;

        fn visit_associated_type(&mut self, name: &str, def: DefId) -> Option<Box<dyn TypeVisitor + '_>>;

        fn visit_method(&mut self, name: &str, def: DefId) -> Option<Box<dyn FunctionDefVisitor + '_>>;
    }
    GlobalValueVisitor => {
        fn visit_signal_direction(&mut self, sigdir: SignalDirection);
        fn visit_mutability(&mut self, mutab: Mutability);
        fn visit_linkage(&mut self, link: Linkage);
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_initializer(&mut self) -> Option<Box<dyn ConstValueVisitor + '_>>;
    }
    TypeVisitor => {
        fn visit_reference(&mut self, mutability: Mutability) -> Option<Box<dyn TypeVisitor + '_>>;

        fn visit_pointer(&mut self, mutability: Mutability) -> Option<Box<dyn TypeVisitor + '_>>;

        fn visit_signal(&mut self, sigdir: SignalDirection) -> Option<Box<dyn TypeVisitor + '_>>;

        fn visit_function_pointer(&mut self) -> Option<Box<dyn FunctionTypeVisitor + '_>>;

        fn visit_tuple(&mut self) -> Option<Box<dyn TupleTypeVisitor + '_>>;

        fn visit_logic_type(&mut self) -> Option<Box<dyn LogicTypeVisitor + '_>>;

        fn visit_named_type(&mut self) -> Option<Box<dyn NamedTypeVisitor + '_>>;

        fn visit_array(&mut self) -> Option<Box<dyn ArrayTypeVisitor + '_>>;

        fn visit_str(&mut self);

        fn visit_char(&mut self);
    }
    ConstructorVisitor => {
        fn visit_unit_ctor(&mut self);
        fn visit_tuple_ctor(&mut self) -> Option<Box<dyn TupleConstructorVisitor + '_>>;
        fn visit_struct_ctor(&mut self) -> Option<Box<dyn StructConstructorVisitor + '_>>;
    }
    EnumVisitor => {
        fn visit_base_ty(&mut self) -> Option<Box<dyn LogicTypeVisitor + '_>>;
        fn visit_variant(&mut self) -> Option<Box<dyn VariantVisitor + '_>>;
    }
    VariantVisitor => {
        fn visit_name(&mut self, name: Symbol);
        fn visit_discrim(&mut self) -> Option<Box<dyn DiscriminantVisitor + '_>>;
        fn visit_ctor(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>>;
    }
    DiscriminantVisitor => {
        fn visit_int(&mut self, val: i128);
        fn visit_logic_val(&mut self, val: LogicVal);
    }
    FunctionDefVisitor => {
        fn visit_reciever(&mut self, lnum: SsaVarId);
        fn visit_signature(&mut self) -> Option<Box<dyn FunctionTypeVisitor + '_>>;
        fn visit_local_ty(&mut self, lnum: SsaVarId) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_basic_block(&mut self, bbid: u32) -> Option<Box<dyn BasicBlockVisitor + '_>>;
        fn visit_local_debug(&mut self, lnum: SsaVarId) -> Option<Box<dyn LocalDebugInfoVisitor + '_>>;
    }
    ConstructorValueVisitor => {
        fn visit_base_ty(&mut self, defid: DefId);
        fn visit_variant(&mut self, var: Symbol);
        fn visit_field(&mut self,field_name: FieldName) -> Option<Box<dyn ConstValueVisitor + '_>>;
    }
    FunctionTypeVisitor => {
        fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }
    TupleTypeVisitor => {
        fn visit_element(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }
    ArrayTypeVisitor => {
        fn visit_element_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_extent(&mut self, len: u64);
    }
    LogicTypeVisitor => {
        fn visit_logic_type(&mut self, ty: LogicType);
        fn visit_width(&mut self, bits: u16);
        fn visit_signed(&mut self, signed: bool);
    }
    NamedTypeVisitor => {
        fn visit_defid(&mut self, def: DefId);
    }
    TupleConstructorVisitor => {
        fn visit_field(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }
    StructConstructorVisitor => {
        fn visit_field(&mut self, name: Symbol) -> Option<Box<dyn TypeVisitor + '_>>;
    }

    LocalDebugInfoVisitor => {
        fn visit_name(&mut self, name: Symbol);
        fn visit_decl_span(&mut self, span: Span);
    }
    BasicBlockVisitor => {
        fn visit_statement(&mut self) -> Option<Box<dyn SsaStatementVisitor + '_>>;
        fn visit_terminator(&mut self) -> Option<Box<dyn SsaTerminatorVisitor + '_>>;
    }
    SsaStatementVisitor => {
        fn visit_declare(&mut self, varid: SsaVarId) -> Option<Box<dyn DeclareStatementVisitor + '_>>;
        fn visit_store_dead(&mut self, varid: SsaVarId);
        fn visit_pin(&mut self, varid: SsaVarId);
    }
    SsaTerminatorVisitor => {
        fn visit_unreachable(&mut self);
    }

    DeclareStatementVisitor => {
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_init(&mut self) -> Option<Box<dyn ExpressionVisitor + '_>>;
    }
    ExpressionVisitor =>  {}
}
