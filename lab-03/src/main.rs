#[derive(Clone, Copy, Debug, PartialEq)]
enum Var {
    X,
    Y,
    Z,
}

impl Var {
    fn to_string(&self) -> String {
        match self {
            Var::X => "X".into(),
            Var::Y => "Y".into(),
            Var::Z => "Z".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Const {
    Numeric(i64),
    Named(String),
}

impl Const {
    fn to_string(&self) -> String {
        match self {
            Const::Numeric(i) => format!("{i}"),
            Const::Named(s) => s.clone(),
        }
    }
}

#[derive(Clone, Debug)]
enum E {
    Add(Box<E>, Box<E>),
    Neg(Box<E>),
    Mul(Box<E>, Box<E>),
    Inv(Box<E>),
    Const(Const),
    Func { name: String, arg: Box<E> },
    Var(Var),
}

impl E {
    fn add(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        let e = E::Add(lhs, rhs);
        Box::new(e)
    }

    fn neg(arg: Box<Self>) -> Box<Self> {
        let e = E::Neg(arg);
        Box::new(e)
    }

    fn mul(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        let e = E::Mul(lhs, rhs);
        Box::new(e)
    }

    fn inv(arg: Box<Self>) -> Box<Self> {
        let e = E::Inv(arg);
        Box::new(e)
    }

    fn constant(arg: Const) -> Box<Self> {
        let e = E::Const(arg);
        Box::new(e)
    }

    fn func(name: String, arg: Box<Self>) -> Box<Self> {
        let e = E::Func { name, arg };
        Box::new(e)
    }

    fn var(arg: Var) -> Box<Self> {
        let e = E::Var(arg);
        Box::new(e)
    }

    fn to_string(&self) -> String {
        match self {
            E::Add(lhs, rhs) => format!("({} + {})", lhs.to_string(), rhs.to_string()),
            E::Neg(arg) => format!("-({})", arg.to_string()),
            E::Mul(lhs, rhs) => format!("({} * {})", lhs.to_string(), rhs.to_string()),
            E::Inv(arg) => format!("1/({})", arg.to_string()),
            E::Const(c) => c.to_string(),
            E::Func { name, arg } => format!("{name}({})", arg.to_string()),
            E::Var(var) => var.to_string(),
        }
    }

    fn arg_count(&self) -> u32 {
        match self {
            E::Const(_) | E::Var(_) => 0,
            E::Add(_, _) | E::Mul(_, _) => 2,
            _ => 1,
        }
    }

    fn diff(self, by: Var) -> Box<Self> {
        match self {
            E::Add(lhs, rhs) => Self::add(lhs.diff(by), rhs.diff(by)),
            E::Neg(arg) => Self::neg(arg.diff(by)),
            E::Mul(lhs, rhs) => Self::add(
                Self::mul(lhs.clone().diff(by), rhs.clone()),
                Self::mul(lhs, rhs.diff(by)),
            ),
            E::Inv(arg) => {
                let mul = E::mul(arg.clone(), arg.clone());
                let inv = E::inv(mul);
                let neg = E::neg(inv);
                E::mul(neg, arg.diff(by))
            }
            E::Const(_) => E::constant(Const::Numeric(0)),
            E::Func { name, arg } => {
                let new_name = format!("{name}_{}", by.to_string());
                let func = E::func(new_name, arg.clone());
                let new_arg = arg.diff(by);
                E::mul(func, new_arg)
            }
            E::Var(v) if v == by => E::constant(Const::Numeric(1)),
            E::Var(_) => E::constant(Const::Numeric(0)),
        }
    }

    fn unpack_inv_inv(self) -> Option<Box<Self>> {
        if let E::Inv(inv) = self {
            if let E::Inv(inv) = *inv {
                return Some(inv);
            }
        }
        None
    }

    fn uninv(self: Box<Self>) -> Box<Self> {
        let mut root = self.clone();
        let mut new_root = self.clone();
        while let Some(unpacked) = root.unpack_inv_inv() {
            root = unpacked;
            new_root = root.clone();
        }
        new_root
    }

    fn unpack_neg_neg(self) -> Option<Box<Self>> {
        if let E::Neg(first_neg) = self
            && let E::Neg(neg) = *first_neg
        {
            return Some(neg);
        }
        None
    }

    fn unneg(self: Box<Self>) -> Box<Self> {
        let mut root = self.clone();
        let mut new_root = self.clone();
        while let Some(unpacked) = root.unpack_neg_neg() {
            root = unpacked;
            new_root = root.clone();
        }
        new_root
    }

    fn substitute(self, name: &str, value: Box<Self>) -> Box<Self> {
        match self {
            E::Add(lhs, rhs) => E::add(
                lhs.substitute(name, value.clone()),
                rhs.substitute(name, value),
            ),
            E::Neg(arg) => E::neg(arg.substitute(name, value)),
            E::Mul(lhs, rhs) => E::mul(
                lhs.substitute(name, value.clone()),
                rhs.substitute(name, value),
            ),
            E::Inv(arg) => arg.substitute(name, value),
            E::Const(c) if Const::Named(name.to_string()) == c => value,
            E::Const(c) => E::constant(c),
            E::Func {
                name: func_name,
                arg,
            } => {
                let arg = arg.substitute(name, value);
                E::func(func_name, arg)
            }
            E::Var(var) => E::var(var),
        }
    }
}

fn main() {
    let x = Var::X;
    let y = Var::Y;
    let add = E::add(E::var(x), E::var(y));
    let inv = E::inv(add);
    let c = Const::Named("a".into());
    let neg = E::neg(inv);
    let mul = E::mul(neg, E::constant(c));
    let mul_unneg = mul.unneg();
    let mul_uninv = mul_unneg.uninv();
    let z = Var::Z;
    let next = E::add(E::var(z), mul_uninv);
    let nc = Const::Numeric(12);
    let next = E::mul(next, E::constant(nc));
    let f = E::func("f".into(), next);
    let f = f.substitute("a", E::constant(Const::Numeric(12)));
    let f = f.diff(x);
    println!("{}", f.to_string());
    println!("args count: {}", f.arg_count());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const_to_string() {
        let c_num = Const::Numeric(42);
        let c_name = Const::Named("a".into());
        assert_eq!(c_num.to_string(), "42");
        assert_eq!(c_name.to_string(), "a");
    }

    #[test]
    fn test_var_to_string() {
        assert_eq!(Var::X.to_string(), "X");
        assert_eq!(Var::Y.to_string(), "Y");
        assert_eq!(Var::Z.to_string(), "Z");
    }

    #[test]
    fn test_builder_constant_var() {
        let e_const = E::constant(Const::Numeric(5));
        let e_var = E::var(Var::X);
        assert_eq!(e_const.to_string(), "5");
        assert_eq!(e_var.to_string(), "X");
    }

    #[test]
    fn test_builder_add() {
        let expr = E::add(E::constant(Const::Numeric(2)), E::var(Var::X));
        assert_eq!(expr.to_string(), "(2 + X)");
    }

    #[test]
    fn test_builder_neg() {
        let expr = E::neg(E::var(Var::X));
        assert_eq!(expr.to_string(), "-(X)");
    }

    #[test]
    fn test_builder_mul() {
        let expr = E::mul(E::var(Var::X), E::var(Var::Y));
        assert_eq!(expr.to_string(), "(X * Y)");
    }

    #[test]
    fn test_builder_inv() {
        let expr = E::inv(E::var(Var::X));
        assert_eq!(expr.to_string(), "1/(X)");
    }

    #[test]
    fn test_builder_func() {
        let expr = E::func("f".into(), E::var(Var::X));
        assert_eq!(expr.to_string(), "f(X)");
    }

    #[test]
    fn test_expr_to_string_complex() {
        let expr1 = E::add(E::constant(Const::Numeric(2)), E::var(Var::X));
        let expr2 = E::mul(E::neg(E::var(Var::Y)), E::inv(E::var(Var::Z)));
        let complex = E::add(
            E::func("f".into(), expr1.clone()),
            E::func("g".into(), expr2.clone()),
        );
        assert_eq!(complex.to_string(), "(f((2 + X)) + g((-(Y) * 1/(Z))))");
    }

    #[test]
    fn test_diff_add_vars() {
        let expr = E::add(E::var(Var::X), E::var(Var::Y));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "(1 + 0)");
    }

    #[test]
    fn test_unpack_inv_inv() {
        let double_inv = E::inv(E::inv(E::var(Var::X)));
        let inner = double_inv.clone().unpack_inv_inv().unwrap();
        assert_eq!(inner.to_string(), "X");
    }

    #[test]
    fn test_unpack_neg_neg() {
        let double_neg = E::neg(E::neg(E::neg(E::neg(E::neg(E::var(Var::Y))))));
        let inner = double_neg.clone().unneg();
        assert_eq!(inner.to_string(), "-(Y)");
    }

    #[test]
    fn test_simplify_double_inv() {
        let double_inv = E::inv(E::inv(E::var(Var::X)));
        let simplified = double_inv.uninv();
        assert_eq!(simplified.to_string(), "X");
    }

    #[test]
    fn test_simplify_double_neg() {
        let double_neg = E::neg(E::neg(E::var(Var::X)));
        let simplified = double_neg.unneg();
        assert_eq!(simplified.to_string(), "X");
    }

    #[test]
    fn test_substitute_named_constant() {
        let expr = E::add(E::constant(Const::Named("a".into())), E::var(Var::X));
        let substituted = expr.substitute("a", E::constant(Const::Numeric(10)));
        assert_eq!(substituted.to_string(), "(10 + X)");
    }

    #[test]
    fn test_substitute_deep() {
        let expr = E::mul(
            E::constant(Const::Named("a".into())),
            E::func("f".into(), E::constant(Const::Named("a".into()))),
        );
        let substituted = expr.substitute("a", E::constant(Const::Numeric(3)));
        assert_eq!(substituted.to_string(), "(3 * f(3))");
    }

    #[test]
    fn test_diff_neg() {
        let expr = E::neg(E::var(Var::X));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "-(1)");
    }

    #[test]
    fn test_diff_mul() {
        let expr = E::mul(E::var(Var::X), E::var(Var::Y));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "((1 * Y) + (X * 0))");
    }

    #[test]
    fn test_diff_inv() {
        let expr = E::inv(E::var(Var::X));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "(-(1/((X * X))) * 1)");
    }

    #[test]
    fn test_diff_const_numeric() {
        let expr = E::constant(Const::Numeric(7));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "0");
    }

    #[test]
    fn test_diff_const_named() {
        let expr = E::constant(Const::Named("a".into()));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "0");
    }

    #[test]
    fn test_diff_func() {
        let expr = E::func("f".into(), E::var(Var::X));
        let d = expr.diff(Var::X);
        assert_eq!(d.to_string(), "(f_X(X) * 1)");
    }

    #[test]
    fn test_diff_var_same() {
        let d = E::var(Var::X).diff(Var::X);
        assert_eq!(d.to_string(), "1");
    }

    #[test]
    fn test_diff_var_other() {
        let d = E::var(Var::Y).diff(Var::X);
        assert_eq!(d.to_string(), "0");
    }

    #[test]
    fn test_diff_big_expression() {
        // (((X + -(Y)) * 1/(Z)) + (f((X * Y)) + g(1/(X))))
        let part1 = E::add(E::var(Var::X), E::neg(E::var(Var::Y)));
        let part2 = E::inv(E::var(Var::Z));
        let a = E::mul(part1.clone(), part2.clone());
        let xy = E::mul(E::var(Var::X), E::var(Var::Y));
        let b = E::func("f".into(), xy);
        let inv_x = E::inv(E::var(Var::X));
        let c = E::func("g".into(), inv_x);
        let big = E::add(a.clone(), E::add(b.clone(), c.clone()));

        assert_eq!(
            big.to_string(),
            "(((X + -(Y)) * 1/(Z)) + (f((X * Y)) + g(1/(X))))"
        );

        let d = big.diff(Var::X);
        assert_eq!(
            d.to_string(),
            "((((1 + -(0)) * 1/(Z)) + ((X + -(Y)) * (-(1/((Z * Z))) * 0))) + ((f_X((X * Y)) * ((1 * Y) + (X * 0))) + (g_X(1/(X)) * (-(1/((X * X))) * 1))))"
        );
    }

    #[test]
    fn test_arg_count_zeroary() {
        assert_eq!(E::constant(Const::Numeric(1)).arg_count(), 0);
        assert_eq!(E::var(Var::X).arg_count(), 0);
    }

    #[test]
    fn test_arg_count_unary() {
        assert_eq!(E::neg(E::var(Var::X)).arg_count(), 1);
        assert_eq!(E::inv(E::var(Var::X)).arg_count(), 1);
        assert_eq!(E::func("f".into(), E::var(Var::X)).arg_count(), 1);
    }

    #[test]
    fn test_arg_count_binary() {
        assert_eq!(E::add(E::var(Var::X), E::var(Var::Y)).arg_count(), 2);
        assert_eq!(E::mul(E::var(Var::X), E::var(Var::Z)).arg_count(), 2);
    }
}
