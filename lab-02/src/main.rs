#[derive(Debug, Clone)]
struct NumberWithUnit {
    unit: String,
    value: f64,
}

impl NumberWithUnit {
    fn unitless(value: f64) -> Self {
        NumberWithUnit {
            unit: "".into(),
            value,
        }
    }

    fn with_unit(value: f64, unit: String) -> Self {
        Self { unit, value }
    }

    fn with_unit_from(other: Self, value: f64) -> Self {
        Self {
            unit: other.unit,
            value,
        }
    }

    fn add(self, other: Self) -> Self {
        if self.unit != other.unit {
            panic!("Units dont match({}, {})", self.unit, other.unit);
        }
        NumberWithUnit {
            unit: self.unit,
            value: self.value + other.value,
        }
    }

    fn mul(self, other: Self) -> Self {
        let value = self.value * other.value;
        let unit = format!("{}*{}", self.unit, other.unit);
        NumberWithUnit { unit, value }
    }

    fn div(self, other: Self) -> Self {
        let value = self.value / other.value;
        let unit = format!("{}/{}", self.unit, other.unit);
        NumberWithUnit { unit, value }
    }

    fn add_in_place(&mut self, other: &Self) {
        if self.unit != other.unit {
            panic!("Units dont match({}, {})", self.unit, other.unit);
        }
        self.value += other.value;
    }

    fn mul_in_place(&mut self, other: &Self) {
        self.value *= other.value;
        self.unit = format!("{}*{}", self.unit, other.unit);
    }

    fn div_in_place(&mut self, other: &Self) {
        self.value /= other.value;
        self.unit = format!("{}/{}", self.unit, other.unit);
    }
}

fn mul_vals(values: &[NumberWithUnit]) -> NumberWithUnit {
    values[1..].iter().fold(values[0].clone(), |mut acc, x| {
        acc.mul_in_place(x);
        acc
    })
}

fn mul_vals_vec(values: Vec<NumberWithUnit>) -> NumberWithUnit {
    values[1..].iter().fold(values[0].clone(), |mut acc, x| {
        acc.mul_in_place(x);
        acc
    })
}

fn main() {
    let unit_less = NumberWithUnit::unitless(1.5);
    println!("Unit less: {:?}", unit_less);
    let with_unit = NumberWithUnit::with_unit(2.5, "UNIT".into());
    println!("With unit: {:?}", with_unit);
    let with_unit_from = NumberWithUnit::with_unit_from(with_unit, 3.5);
    println!("With unit from: {:?}", with_unit_from);

    let m1 = NumberWithUnit::with_unit(5.5, "m".into());
    let m2 = NumberWithUnit::with_unit(8.5, "m".into());
    let m_added = m1.add(m2);
    println!("added: {:?}", m_added);

    let mut a1 = NumberWithUnit::with_unit(5.5, "a".into());
    let a2 = NumberWithUnit::with_unit(8.5, "a".into());
    a1.add_in_place(&a2);
    println!("added in place: {:?}", a1);

    let l = NumberWithUnit::with_unit(15.0, "l".into());
    let p = NumberWithUnit::with_unit(1.0, "p".into());
    let lp = l.mul(p);
    println!("multiplied: {:?}", lp);

    let mut k = NumberWithUnit::with_unit(15.0, "k".into());
    let u = NumberWithUnit::with_unit(2.0, "u".into());
    k.mul_in_place(&u);
    println!("multiplied in place: {:?}", k);

    let m = NumberWithUnit::with_unit(10.0, "m".into());
    let s = NumberWithUnit::with_unit(10.0, "s".into());
    let m_s = m.div(s);
    println!("divided: {:?}", m_s);

    let mut m = NumberWithUnit::with_unit(10.0, "m".into());
    let s = NumberWithUnit::with_unit(10.0, "s".into());
    m.div_in_place(&s);
    println!("divided in place: {:?}", m);

    let v = vec![
        NumberWithUnit::with_unit(5.5, "p".into()),
        NumberWithUnit::with_unit(55.5, "o".into()),
        NumberWithUnit::with_unit(1.0, "l".into()),
        NumberWithUnit::with_unit(2.0, "s".into()),
        NumberWithUnit::with_unit(-3.0, "k".into()),
        NumberWithUnit::with_unit(0.0, "a".into()),
    ];

    let res1 = mul_vals(&v);
    println!("first result: {:?}", res1);
    let res2 = mul_vals(&v);
    println!("second result: {:?}", res2);

    let res1_vec = mul_vals_vec(v.clone());
    println!("first result vec: {:?}", res1_vec);
    let res2_vec = mul_vals_vec(v);
    println!("second result vec: {:?}", res2_vec);

    let string = String::from("string");
    let str_slice = "slice";

    let ds1 = DoubleString::from_strs(&string, str_slice);
    ds1.show();

    let ds2 = DoubleString::from_strings(&string, &str_slice.to_string());
    ds2.show();
}

struct DoubleString(String, String);

impl DoubleString {
    fn from_strs(str_1: &str, str_2: &str) -> Self {
        DoubleString(str_1.into(), str_2.into())
    }

    fn from_strings(str_1: &String, str_2: &String) -> Self {
        DoubleString(str_1.to_owned(), str_2.to_owned())
    }

    fn show(&self) {
        println!("({}, {})", self.0, self.1);
    }
}
