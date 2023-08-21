use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

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
    Table(Rc<RefCell<Table>>),
    Function(fn(&mut ExeState) -> i32),
}

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

impl Table {
    pub fn new(narray: usize, nmap: usize) -> Self {
        Table {
            array: Vec::with_capacity(narray),
            map: HashMap::with_capacity(nmap),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Interger(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v:?}"),
            Value::ShortStr(len, buf) => {
                write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize]))
            }
            Value::MidStr(v) => write!(f, "{}", String::from_utf8_lossy(&v.1[..v.0 as usize])),
            Value::LongStr(v) => write!(f, "{}", String::from_utf8_lossy(&v)),
            Value::Table(v) => write!(f, "table: {:?}", Rc::as_ptr(v)),
            Value::Function(_) => write!(f, "function"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Interger(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v:?}"),
            Value::ShortStr(len, buf) => {
                write!(f, "SS: '{}'", String::from_utf8_lossy(&buf[..*len as usize]))
            }
            Value::MidStr(v) => {
                write!(f, "MS: '{}'", String::from_utf8_lossy(&v.1[..v.0 as usize]))
            }
            Value::LongStr(v) => write!(f, "LS: '{}'", String::from_utf8_lossy(&v)),
            Value::Table(v) => {
                let v = v.borrow();
                write!(f, "table: {}:{}", v.array.len(), v.map.len())
            }
            Value::Function(_) => write!(f, "function"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(v1), Value::Boolean(v2)) => *v1 == *v2,
            (Value::Interger(v1), Value::Interger(v2)) => *v1 == *v2,
            (Value::Float(v1), Value::Float(v2)) => *v1 == *v2,
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => s1[..*len1 as usize] == s2[..*len2 as usize],
            (Value::MidStr(s1), Value::MidStr(s2)) => s1.1[..s1.0 as usize] == s2.1[..s2.0 as usize],
            (Value::LongStr(s1), Value::LongStr(s2)) => s1 == s2,
            (Value::Table(v1), Value::Table(v2)) => Rc::ptr_eq(v1, v2),
            (Value::Function(v1), Value::Function(v2)) => std::ptr::eq(v1, v2),
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
            Value::Float(v) => unsafe { mem::transmute::<f64, i64>(*v).hash(state) },
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::MidStr(v) => v.1[..v.0 as usize].hash(state),
            Value::LongStr(v) => v.hash(state),
            Value::Table(v) => Rc::as_ptr(v).hash(state),
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
