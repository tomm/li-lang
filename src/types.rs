#[derive(Debug, PartialEq, Eq)]
pub struct StructMember<'ctx> {
    name: &'ctx str,
    type_: &'ctx Type<'ctx>,
    offset: u32, /* bytes */
}

#[derive(Debug, Eq)]
pub enum Type<'ctx> {
    Unknown,
    Never,
    Void,
    Bool,
    U8,
    I8,
    U16,
    I16,
    Array(&'ctx Type<'ctx>, usize /* elems */),
    Func(Vec<&'ctx Type<'ctx>>, &'ctx Type<'ctx>),
    Ptr(&'ctx Type<'ctx>),
    Struct(Vec<StructMember<'ctx>>),
}

impl<'ctx> PartialEq for Type<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Unknown => false,
            Type::Never => match other {
                Type::Never => true,
                _ => false,
            },
            Type::Void => match other {
                Type::Void => true,
                _ => false,
            },
            Type::Bool => match other {
                Type::Bool => true,
                _ => false,
            },
            Type::U8 => match other {
                Type::U8 => true,
                _ => false,
            },
            Type::I8 => match other {
                Type::I8 => true,
                _ => false,
            },
            Type::U16 => match other {
                Type::U16 => true,
                _ => false,
            },
            Type::I16 => match other {
                Type::I16 => true,
                _ => false,
            },
            Type::Array(cont1, len1) => match other {
                Type::Array(cont2, len2) => len1 == len2 && cont1 == cont2,
                _ => false,
            },
            Type::Ptr(cont1) => match other {
                Type::Ptr(cont2) => cont1 == cont2,
                _ => false,
            },
            Type::Func(args1, ret1) => match other {
                Type::Func(args2, ret2) => args1 == args2 && ret1 == ret2,
                _ => false,
            },
            struct1 @ Type::Struct(..) => {
                // nominal typing. XXX is this the right way?
                struct1 as *const Type == other as *const Type
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() {
        let _u8 = Type::U8;
        let _u8p = Type::Ptr(&_u8);
        let _u8a = Type::Array(&_u8, 10);
        let _s = Type::Struct(vec![StructMember {
            name: &"x",
            type_: &_u8,
            offset: 0,
        }]);
        let _f = Type::Func(vec![&_u8, &_u8p], &_u8a);
        assert!(_u8 == Type::U8);
        assert!(_u8 != Type::U16);
        assert!(_u8p == Type::Ptr(&Type::U8));
        assert!(_u8a == Type::Array(&Type::U8, 10));
        assert!(_s == _s);
        // nominal typing
        assert!(
            _s != Type::Struct(vec![StructMember {
                name: &"x",
                type_: &_u8,
                offset: 0
            }])
        );
        assert!(_f == _f);
        assert!(
            _f == Type::Func(
                vec![&Type::U8, &Type::Ptr(&Type::U8)],
                &Type::Array(&Type::U8, 10)
            )
        );
        assert!(
            _f != Type::Func(
                vec![&Type::U8, &Type::Ptr(&Type::U16)],
                &Type::Array(&Type::U8, 9)
            )
        );
    }
}
