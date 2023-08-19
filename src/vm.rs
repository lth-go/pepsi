use std::cmp::Ordering;
use std::collections::HashMap;

use crate::bytecode::ByteCode;
use crate::parse::ParseProto;
use crate::value::Value;

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

        ExeState {
            globals,
            stack: Vec::new(),
            func_index: 0,
        }
    }

    pub fn execute(&mut self, proto: &ParseProto) {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, name) => {
                    let name = &proto.constants[name as usize];

                    if let Value::String(key) = name {
                        let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                        self.set_stack(dst, v);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::SetGlobal(name, src) => {
                    let name = proto.constants[name as usize].clone();
                    if let Value::String(key) = name {
                        let value = self.stack[src as usize].clone();
                        self.globals.insert(key, value);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::SetGlobalConst(name, src) => {
                    let name = proto.constants[name as usize].clone();
                    if let Value::String(key) = name {
                        let value = proto.constants[src as usize].clone();
                        self.globals.insert(key, value);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::SetGlobalGlobal(name, src) => {
                    let name = proto.constants[name as usize].clone();
                    if let Value::String(key) = name {
                        let src = &proto.constants[src as usize];
                        if let Value::String(src) = src {
                            let value = self.globals.get(src).unwrap_or(&Value::Nil).clone();
                            self.globals.insert(key, value);
                        } else {
                            panic!("invalid global key: {key:?}");
                        }
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::LoadNil(dst) => {
                    self.set_stack(dst, Value::Nil);
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
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}")
                    }
                }
            }
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
}
