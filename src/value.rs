use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

use crate::parse::FuncProto;
use crate::utils::ftoi;
use crate::vm::ExeState;

const SHORT_STR_MAX: usize = 14; // sizeof(Value) - 1(tag) - 1(len)
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
    RustFunction(fn(&mut ExeState) -> i32),
    LuaFunction(Rc<FuncProto>),
}

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

impl Table {
    pub fn new(narray: usize, nmap: usize) -> Self {
        Self {
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
            Value::ShortStr(len, buf) => write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::MidStr(v) => write!(f, "{}", String::from_utf8_lossy(&v.1[..v.0 as usize])),
            Value::LongStr(v) => write!(f, "{}", String::from_utf8_lossy(&v)),
            Value::Table(v) => write!(f, "table: {:?}", Rc::as_ptr(v)),
            Value::RustFunction(_) => write!(f, "function"),
            Value::LuaFunction(v) => write!(f, "function: {:?}", Rc::as_ptr(v)),
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
            Value::ShortStr(len, buf) => write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::MidStr(v) => write!(f, "{}", String::from_utf8_lossy(&v.1[..v.0 as usize])),
            Value::LongStr(v) => write!(f, "{}", String::from_utf8_lossy(&v)),
            Value::Table(v) => {
                let v = v.borrow();
                write!(f, "table: {}:{}", v.array.len(), v.map.len())
            }
            Value::RustFunction(_) => write!(f, "function"),
            Value::LuaFunction(_) => write!(f, "Lua function",),
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
            (Value::RustFunction(v1), Value::RustFunction(v2)) => std::ptr::eq(v1, v2),
            (Value::LuaFunction(v1), Value::LuaFunction(v2)) => Rc::as_ptr(v1) == Rc::as_ptr(v2),
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Interger(v1), Value::Interger(v2)) => Some(v1.cmp(v2)),
            (Value::Interger(v1), Value::Float(v2)) => (*v1 as f64).partial_cmp(v2),
            (Value::Float(v1), Value::Interger(v2)) => v1.partial_cmp(&(*v2 as f64)),
            (Value::Float(v1), Value::Float(v2)) => v1.partial_cmp(v2),
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => Some(s1[..*len1 as usize].cmp(&s2[..*len2 as usize])),
            (Value::ShortStr(len1, s1), Value::MidStr(s2)) => Some(s1[..*len1 as usize].cmp(&s2.1[..s2.0 as usize])),
            (Value::ShortStr(len1, s1), Value::LongStr(s2)) => Some(s1[..*len1 as usize].cmp(s2)),
            (Value::MidStr(s1), Value::ShortStr(len2, s2)) => Some(s1.1[..s1.0 as usize].cmp(&s2[..*len2 as usize])),
            (Value::MidStr(s1), Value::MidStr(s2)) => Some(s1.1[..s1.0 as usize].cmp(&s2.1[..s2.0 as usize])),
            (Value::MidStr(s1), Value::LongStr(s2)) => Some(s1.1[..s1.0 as usize].cmp(s2)),
            (Value::LongStr(s1), Value::ShortStr(len2, s2)) => Some(s1.as_ref().as_slice().cmp(&s2[..*len2 as usize])),
            (Value::LongStr(s1), Value::MidStr(s2)) => Some(s1.as_ref().as_slice().cmp(&s2.1[..s2.0 as usize])),
            (Value::LongStr(s1), Value::LongStr(s2)) => Some(s1.cmp(s2)),
            (_, _) => None,
        }
    }
}

impl Value {
    pub fn same(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other) && self == other
    }

    pub fn ty(&self) -> &'static str {
        match self {
            &Value::Nil => "nil",
            &Value::Boolean(_) => "bool",
            &Value::Interger(_) => "number",
            &Value::Float(_) => "number",
            &Value::ShortStr(_, _) => "string",
            &Value::MidStr(_) => "string",
            &Value::LongStr(_) => "string",
            &Value::Table(_) => "table",
            &Value::RustFunction(_) => "function",
            &Value::LuaFunction(_) => "function",
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Nil => (),
            Self::Boolean(v) => v.hash(state),
            Value::Interger(v) => v.hash(state),
            &Value::Float(v) => {
                if let Some(v) = ftoi(v) {
                    v.hash(state)
                } else {
                    (v.to_bits() as i64).hash(state)
                }
            }
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::MidStr(v) => v.1[..v.0 as usize].hash(state),
            Value::LongStr(v) => v.hash(state),
            Value::Table(v) => Rc::as_ptr(v).hash(state),
            Value::RustFunction(v) => (*v as *const usize).hash(state),
            Value::LuaFunction(v) => Rc::as_ptr(v).hash(state),
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

impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        !matches!(value, Value::Nil | Value::Boolean(false))
    }
}
