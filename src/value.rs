use std::mem;
use std::{fmt, hash::Hash, hash::Hasher, rc::Rc};

use crate::vm::ExeState;

const SHORT_STR_MAX: usize = 14;
const MID_STR_MAX: usize = 48 - 1;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Interger(i64),
    Float(f64),
    ShortStr(u8, [u8; SHORT_STR_MAX]),
    MidStr(Rc<(u8, [u8; MID_STR_MAX])>),
    LongStr(Rc<Vec<u8>>),
    Function(fn(&mut ExeState) -> i32),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Interger(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::ShortStr(len, buf) => {
                write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize]))
            }
            Value::MidStr(s) => write!(f, "{}", String::from_utf8_lossy(&s.1[..s.0 as usize])),
            Value::LongStr(s) => write!(f, "{}", String::from_utf8_lossy(&s)),
            Value::Function(_) => write!(f, "function"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(b1), Value::Boolean(b2)) => *b1 == *b2,
            (Value::Interger(i1), Value::Interger(i2)) => *i1 == *i2,
            (Value::Float(f1), Value::Float(f2)) => *f1 == *f2,
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => {
                s1[..*len1 as usize] == s2[..*len2 as usize]
            }
            (Value::MidStr(s1), Value::MidStr(s2)) => {
                s1.1[..s1.0 as usize] == s2.1[..s2.0 as usize]
            }
            (Value::LongStr(s1), Value::LongStr(s2)) => s1 == s2,
            (Value::Function(f1), Value::Function(f2)) => std::ptr::eq(f1, f2),
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Nil => (),
            Self::Boolean(v) => v.hash(state),
            Value::Interger(v) => v.hash(state),
            Value::Float(v) => unsafe { mem::transmute::<f64, u64>(*v).hash(state) },
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::MidStr(v) => v.1[..v.0 as usize].hash(state),
            Value::LongStr(v) => v.hash(state),
            Value::Function(v) => (*v as *const usize).hash(state),
        }
    }
}

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Value::Nil
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Interger(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<&[u8]> for Value {
    fn from(value: &[u8]) -> Self {
        vec_to_short_mid_str(value).unwrap_or(Value::LongStr(Rc::new(value.to_vec())))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        value.as_bytes().into()
    }
}

impl From<Vec<u8>> for Value {
    fn from(value: Vec<u8>) -> Self {
        vec_to_short_mid_str(&value).unwrap_or(Value::LongStr(Rc::new(value)))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        value.into_bytes().into()
    }
}

fn vec_to_short_mid_str(v: &[u8]) -> Option<Value> {
    let len = v.len();
    if len <= SHORT_STR_MAX {
        let mut buf = [0; SHORT_STR_MAX];
        buf[..len].copy_from_slice(&v);
        Some(Value::ShortStr(len as u8, buf))
    } else if len <= MID_STR_MAX {
        let mut buf = [0; MID_STR_MAX];
        buf[..len].copy_from_slice(&v);
        Some(Value::MidStr(Rc::new((len as u8, buf))))
    } else {
        None
    }
}

impl<'a> From<&'a Value> for &'a [u8] {
    fn from(value: &'a Value) -> Self {
        match value {
            Value::ShortStr(len, buf) => &buf[..*len as usize],
            Value::MidStr(s) => &s.1[..s.0 as usize],
            Value::LongStr(s) => s,
            _ => panic!("invalid string value"),
        }
    }
}

impl<'a> From<&'a Value> for &'a str {
    fn from(value: &'a Value) -> Self {
        std::str::from_utf8(value.into()).unwrap()
    }
}

impl From<&Value> for String {
    fn from(value: &Value) -> Self {
        String::from_utf8_lossy(value.into()).to_string()
    }
}
