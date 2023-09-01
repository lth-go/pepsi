use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::rc::Rc;

use crate::bytecode::ByteCode;
use crate::parse::ParseProto;
use crate::utils::ftoi;
use crate::value::{Table, Value};

fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[state.func_index + 1]);
    0
}

pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
    func_index: usize,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert(String::from("print"), Value::Function(lib_print));

        Self {
            globals,
            stack: Vec::new(),
            func_index: 0,
        }
    }

    pub fn execute<R: Read>(&mut self, proto: &ParseProto<R>) {
        let mut pc = 0;
        while pc < proto.byte_codes.len() {
            println!("  [{pc}]\t{:?}", proto.byte_codes[pc]);
            match proto.byte_codes[pc] {
                ByteCode::GetGlobal(dst, name) => {
                    let name: &str = (&proto.constants[name as usize]).into();
                    let v = self.globals.get(name).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::SetGlobal(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = self.stack[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::SetGlobalConst(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = proto.constants[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::LoadNil(dst, n) => {
                    self.fill_stack(dst as usize, n as usize);
                }
                ByteCode::LoadBool(dst, b) => {
                    self.set_stack(dst, Value::Boolean(b));
                }
                ByteCode::LoadInt(dst, i) => {
                    self.set_stack(dst, Value::Interger(i as i64));
                }
                ByteCode::Move(dst, src) => {
                    let v = self.stack[src as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(t, i, v) => {
                    let value = self.stack[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetIntConst(t, i, v) => {
                    let value = proto.constants[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetFieldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTable(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTableConst(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = table as usize + 1;
                    if let Value::Table(table) = self.stack[table as usize].clone() {
                        let values = self.stack.drain(ivalue..ivalue + n as usize);
                        table.borrow_mut().array.extend(values);
                    } else {
                        panic!("not table");
                    }
                }
                ByteCode::GetInt(dst, t, k) => {
                    let value = self.get_table_int(t, k as i64);
                    self.set_stack(dst, value);
                }
                ByteCode::GetField(dst, t, k) => {
                    let key = &proto.constants[k as usize];
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                }
                ByteCode::GetTable(dst, t, k) => {
                    let key = &self.stack[k as usize];
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                }
                ByteCode::Test(icond, jmp) => {
                    let cond = &self.stack[icond as usize];
                    if matches!(cond, Value::Nil | Value::Boolean(false)) {
                        pc = (pc as isize + jmp as isize) as usize;
                    }
                }
                ByteCode::Jump(jmp) => {
                    pc = (pc as isize + jmp as isize) as usize;
                }
                ByteCode::ForPrepare(dst, jmp) => {
                    if let (&Value::Interger(mut i), &Value::Interger(step)) = (&self.stack[dst as usize], &self.stack[dst as usize + 2]) {
                        if step == 0 {
                            panic!("0 step in numerical for")
                        }

                        let limit = match self.stack[dst as usize + 1] {
                            Value::Interger(limit) => limit,
                            Value::Float(limit) => {
                                let limit = for_int_limit(limit, step > 0, &mut i);
                                self.set_stack(dst + 1, Value::Interger(limit));
                                limit
                            }
                            _ => panic!("invalid limit type"),
                        };
                        if !for_check(i, limit, step > 0) {
                            pc += jmp as usize;
                        }
                    } else {
                        let i = self.make_float(dst);
                        let limit = self.make_float(dst + 1);
                        let step = self.make_float(dst + 2);
                        if step == 0.0 {
                            panic!("0 step in numerical for")
                        }
                        if !for_check(i, limit, step > 0.0) {
                            pc += jmp as usize;
                        }
                    }
                }
                ByteCode::ForLoop(dst, jmp) => match self.stack[dst as usize] {
                    Value::Interger(i) => {
                        let limit = self.read_int(dst + 1);
                        let step = self.read_int(dst + 2);
                        let i = i + step;
                        if for_check(i, limit, step > 0) {
                            self.set_stack(dst, Value::Interger(i));
                            pc -= jmp as usize;
                        }
                    }
                    Value::Float(f) => {
                        let limit = self.read_float(dst + 1);
                        let step = self.read_float(dst + 2);
                        let i = f + step;
                        if for_check(i, limit, step > 0.0) {
                            self.set_stack(dst, Value::Float(i));
                            pc -= jmp as usize;
                        }
                    }
                    _ => panic!("todo"),
                },
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}")
                    }
                }
                ByteCode::Neg(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Interger(v) => Value::Interger(-v),
                        Value::Float(v) => Value::Float(-v),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::Not(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Nil => Value::Boolean(true),
                        Value::Boolean(v) => Value::Boolean(!v),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::BitNot(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Interger(v) => Value::Interger(!v),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::Len(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::ShortStr(len, _) => Value::Interger(*len as i64),
                        Value::MidStr(s) => Value::Interger(s.0 as i64),
                        Value::LongStr(s) => Value::Interger(s.len() as i64),
                        Value::Table(v) => Value::Interger(v.borrow().array.len() as i64),
                        _ => panic!("invalid #"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Add(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &self.stack[b as usize], |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::AddConst(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::AddInt(dst, a, b) => {
                    let v = exp_binop_int(&self.stack[a as usize], b, |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::Sub(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &self.stack[b as usize], |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::SubConst(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::SubInt(dst, a, b) => {
                    let v = exp_binop_int(&self.stack[a as usize], b, |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::Mul(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &self.stack[b as usize], |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::MulConst(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::MulInt(dst, a, b) => {
                    let v = exp_binop_int(&self.stack[a as usize], b, |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::Mod(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &self.stack[b as usize], |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::ModConst(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::ModInt(dst, a, b) => {
                    let v = exp_binop_int(&self.stack[a as usize], b, |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::Idiv(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &self.stack[b as usize], |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::IdivConst(dst, a, b) => {
                    let v = exp_binop(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::IdivInt(dst, a, b) => {
                    let v = exp_binop_int(&self.stack[a as usize], b, |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::Div(dst, a, b) => {
                    let v = exp_binop_f(&self.stack[a as usize], &self.stack[b as usize], |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::DivConst(dst, a, b) => {
                    let v = exp_binop_f(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::DivInt(dst, a, b) => {
                    let v = exp_binop_int_f(&self.stack[a as usize], b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::Pow(dst, a, b) => {
                    let v = exp_binop_f(&self.stack[a as usize], &self.stack[b as usize], |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::PowConst(dst, a, b) => {
                    let v = exp_binop_f(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::PowInt(dst, a, b) => {
                    let v = exp_binop_int_f(&self.stack[a as usize], b, |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::BitAnd(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitAndConst(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitAndInt(dst, a, b) => {
                    let v = exp_binop_int_i(&self.stack[a as usize], b, |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOr(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOrConst(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOrInt(dst, a, b) => {
                    let v = exp_binop_int_i(&self.stack[a as usize], b, |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXor(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXorConst(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXorInt(dst, a, b) => {
                    let v = exp_binop_int_i(&self.stack[a as usize], b, |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftL(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftLConst(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftLInt(dst, a, b) => {
                    let v = exp_binop_int_i(&self.stack[a as usize], b, |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftR(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftRConst(dst, a, b) => {
                    let v = exp_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftRInt(dst, a, b) => {
                    let v = exp_binop_int_i(&self.stack[a as usize], b, |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::Concat(dst, a, b) => {
                    let v = exp_concat(&self.stack[a as usize], &self.stack[b as usize]);
                    self.set_stack(dst, v);
                }
                ByteCode::ConcatConst(dst, a, b) => {
                    let v = exp_concat(&self.stack[a as usize], &proto.constants[b as usize]);
                    self.set_stack(dst, v);
                }
                ByteCode::ConcatInt(dst, a, b) => {
                    let v = exp_concat(&self.stack[a as usize], &Value::Interger(b as i64));
                    self.set_stack(dst, v);
                }
            }

            pc += 1;
        }
    }

    fn set_stack(&mut self, dst: u8, v: Value) {
        let dst = dst as usize;

        match dst.cmp(&self.stack.len()) {
            Ordering::Equal => self.stack.push(v),
            Ordering::Less => self.stack[dst] = v,
            Ordering::Greater => panic!("fail in set_stack"),
        }
    }

    fn fill_stack(&mut self, begin: usize, num: usize) {
        let end = begin + num;
        let len = self.stack.len();
        if begin < len {
            self.stack[begin..len].fill(Value::Nil);
        }
        if end > len {
            self.stack.resize(end, Value::Nil);
        }
    }

    fn set_table(&mut self, t: u8, key: Value, value: Value) {
        match &key {
            Value::Interger(v) => self.set_table_int(t, *v, value),
            _ => self.do_set_table(t, key, value),
        }
    }

    fn set_table_int(&mut self, t: u8, i: i64, value: Value) {
        if let Value::Table(table) = &self.stack[t as usize] {
            let mut table = table.borrow_mut();
            if i > 0 && (i < 4 || i < table.array.capacity() as i64 * 2) {
                set_vec(&mut table.array, i as usize - 1, value);
            } else {
                table.map.insert(Value::Interger(i), value);
            }
        } else {
            panic!("invalid table")
        }
    }

    fn do_set_table(&mut self, t: u8, key: Value, value: Value) {
        if let Value::Table(table) = &self.stack[t as usize] {
            table.borrow_mut().map.insert(key, value);
        } else {
            panic!("invalid table");
        }
    }

    fn get_table(&self, t: u8, key: &Value) -> Value {
        match key {
            Value::Interger(i) => self.get_table_int(t, *i),
            _ => self.do_get_table(t, key),
        }
    }

    fn get_table_int(&self, t: u8, i: i64) -> Value {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            table.array.get(i as usize - 1).unwrap_or_else(|| table.map.get(&Value::Interger(i)).unwrap_or(&Value::Nil)).clone()
        } else {
            panic!("set invalid table");
        }
    }

    fn do_get_table(&self, t: u8, key: &Value) -> Value {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            table.map.get(key).unwrap_or(&Value::Nil).clone()
        } else {
            panic!("set invalid table")
        }
    }

    fn make_float(&mut self, dst: u8) -> f64 {
        match self.stack[dst as usize] {
            Value::Float(v) => v,
            Value::Interger(i) => {
                let f = i as f64;
                self.set_stack(dst, Value::Float(f));
                f
            }
            ref v => panic!("not number {v:?}"),
        }
    }

    fn read_int(&self, dst: u8) -> i64 {
        if let Value::Interger(i) = self.stack[dst as usize] {
            i
        } else {
            panic!("invalid interger");
        }
    }

    fn read_float(&self, dst: u8) -> f64 {
        if let Value::Float(f) = self.stack[dst as usize] {
            f
        } else {
            panic!("invalid float");
        }
    }
}

fn set_vec(vec: &mut Vec<Value>, i: usize, value: Value) {
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, Value::Nil);
            vec.push(value);
        }
    }
}

fn exp_binop(v1: &Value, v2: &Value, arith_i: fn(i64, i64) -> i64, arith_f: fn(f64, f64) -> f64) -> Value {
    match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => Value::Interger(arith_i(*a, *b)),
        (Value::Interger(a), Value::Float(b)) => Value::Float(arith_f(*a as f64, *b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(arith_f(*a, *b)),
        (Value::Float(a), Value::Interger(b)) => Value::Float(arith_f(*a, *b as f64)),
        _ => todo!("meta"),
    }
}

fn exp_binop_int(v1: &Value, v2: u8, arith_i: fn(i64, i64) -> i64, arith_f: fn(f64, f64) -> f64) -> Value {
    match v1 {
        Value::Interger(a) => Value::Interger(arith_i(*a, v2 as i64)),
        Value::Float(a) => Value::Float(arith_f(*a, v2 as f64)),
        _ => todo!("meta"),
    }
}

fn exp_binop_f(v1: &Value, v2: &Value, arith_f: fn(f64, f64) -> f64) -> Value {
    let (f1, f2) = match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => (*a as f64, *b as f64),
        (Value::Interger(a), Value::Float(b)) => (*a as f64, *b),
        (Value::Float(a), Value::Float(b)) => (*a, *b),
        (Value::Float(a), Value::Interger(b)) => (*a, *b as f64),
        _ => todo!("meta"),
    };

    Value::Float(arith_f(f1, f2))
}

fn exp_binop_int_f(v1: &Value, v2: u8, arith_f: fn(f64, f64) -> f64) -> Value {
    let f1 = match v1 {
        Value::Interger(i1) => *i1 as f64,
        Value::Float(f1) => *f1,
        _ => todo!("meta"),
    };

    Value::Float(arith_f(f1, v2 as f64))
}

fn exp_binop_i(v1: &Value, v2: &Value, arith_i: fn(i64, i64) -> i64) -> Value {
    let (i1, i2) = match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => (*a, *b),
        (Value::Interger(a), Value::Float(b)) => (*a, ftoi(*b).unwrap()),
        (Value::Float(a), Value::Float(b)) => (ftoi(*a).unwrap(), ftoi(*b).unwrap()),
        (Value::Float(a), Value::Interger(b)) => (ftoi(*a).unwrap(), *b),
        (_, _) => todo!("meta"),
    };

    Value::Interger(arith_i(i1, i2))
}

fn exp_binop_int_i(v1: &Value, v2: u8, arith_i: fn(i64, i64) -> i64) -> Value {
    let i1 = match v1 {
        Value::Interger(i1) => *i1,
        Value::Float(f1) => ftoi(*f1).unwrap(),
        _ => todo!("meta"),
    };

    Value::Interger(arith_i(i1, v2 as i64))
}

fn exp_concat(v1: &Value, v2: &Value) -> Value {
    let mut b1: Vec<u8> = Vec::new();

    let v1 = match v1 {
        Value::Interger(i) => {
            write!(&mut b1, "{}", i).unwrap();
            b1.as_slice()
        }
        Value::Float(f) => {
            write!(&mut b1, "{}", f).unwrap();
            b1.as_slice()
        }
        _ => v1.into(),
    };

    let mut b2: Vec<u8> = Vec::new();

    let v2 = match v2 {
        Value::Interger(i) => {
            write!(&mut b2, "{}", i).unwrap();
            b2.as_slice()
        }
        Value::Float(f) => {
            write!(&mut b2, "{}", f).unwrap();
            b2.as_slice()
        }
        _ => v2.into(),
    };

    [v1, v2].concat().into()
}

fn for_check<T: PartialOrd>(i: T, limit: T, is_step_positive: bool) -> bool {
    if is_step_positive {
        i <= limit
    } else {
        i >= limit
    }
}

fn for_int_limit(limit: f64, is_step_positive: bool, i: &mut i64) -> i64 {
    if is_step_positive {
        if limit < i64::MIN as f64 {
            *i = 0;
            -1
        } else {
            limit.floor() as i64
        }
    } else {
        if limit > i64::MAX as f64 {
            *i = 0;
            1
        } else {
            limit.ceil() as i64
        }
    }
}
