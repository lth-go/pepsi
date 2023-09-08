use std::cell::RefCell;
use std::cmp::Ordering;
use std::io::Write;
use std::rc::Rc;

use crate::bytecode::ByteCode;
use crate::parse::{FuncProto, UpIndex};
use crate::utils::{ftoi, set_vec};
use crate::value::{Table, Value};

fn lib_print(state: &mut ExeState) -> i32 {
    for i in 1..=state.get_top() {
        if i != 1 {
            print!("\t");
        }
        print!("{}", state.get::<&Value>(i).to_string());
    }
    println!("");
    0
}

fn lib_type(state: &mut ExeState) -> i32 {
    let ty = state.get::<&Value>(1).ty();
    state.push(ty);
    1
}

fn test_new_counter(state: &mut ExeState) -> i32 {
    let mut i: i32 = 0;

    let c = move |_: &mut ExeState| {
        i += 1;
        println!("counter: {i}");
        0
    };

    state.push(Value::RustClosure(Rc::new(RefCell::new(Box::new(c)))));
    1
}

fn ipairs_aux(state: &mut ExeState) -> i32 {
    let table = match state.get::<&Value>(1) {
        Value::Table(t) => t.borrow(),
        _ => panic!("ipairs non-table"),
    };

    let i: i64 = state.get(2);
    if i < 0 || i as usize >= table.array.len() {
        return 0;
    }

    let v = table.array[i as usize].clone();
    drop(table);

    state.push(i + 1);
    state.push(v);
    2
}

fn ipairs(state: &mut ExeState) -> i32 {
    state.push(Value::RustFunction(ipairs_aux));
    state.push(state.get::<&Value>(1).clone());
    state.push(0);
    3
}

#[derive(Debug, PartialEq)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    fn get<'a>(&'a self, stack: &'a Vec<Value>) -> &'a Value {
        match self {
            Upvalue::Open(v) => &stack[*v],
            Upvalue::Closed(v) => &v,
        }
    }

    fn set(&mut self, stack: &mut Vec<Value>, value: Value) {
        match self {
            Upvalue::Open(v) => stack[*v] = value,
            Upvalue::Closed(v) => *v = value,
        }
    }
}

struct OpenBroker {
    ilocal: usize,
    broker: Rc<RefCell<Upvalue>>,
}

impl From<usize> for OpenBroker {
    fn from(value: usize) -> Self {
        OpenBroker {
            ilocal: value,
            broker: Rc::new(RefCell::new(Upvalue::Open(value))),
        }
    }
}

pub struct LuaClosure {
    proto: Rc<FuncProto>,
    upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

pub struct ExeState {
    stack: Vec<Value>,
    base: usize,
}

impl ExeState {
    pub fn new() -> Self {
        let mut env = Table::new(0, 0);
        env.map.insert("print".into(), Value::RustFunction(lib_print));
        env.map.insert("type".into(), Value::RustFunction(lib_type));
        env.map.insert("ipairs".into(), Value::RustFunction(ipairs));
        env.map.insert("new_counter".into(), Value::RustFunction(test_new_counter));

        Self {
            stack: vec![Value::Nil, Value::Table(Rc::new(RefCell::new(env)))],
            base: 1,
        }
    }

    pub fn execute(&mut self, proto: &FuncProto, upvalues: &Vec<Rc<RefCell<Upvalue>>>) -> usize {
        let mut open_brokers: Vec<OpenBroker> = Vec::new();

        if self.stack.len() - self.base < proto.nparam {
            self.fill_stack_nil(0, proto.nparam)
        }

        let varargs = if proto.has_varargs { self.stack.drain(self.base + proto.nparam..).collect() } else { Vec::new() };

        let mut pc = 0;

        loop {
            println!("  [{pc}]\t{:?}", proto.byte_codes[pc]);
            match proto.byte_codes[pc] {
                ByteCode::GetUpvalue(dst, src) => {
                    let v = upvalues[src as usize].borrow().get(&self.stack).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::SetUpvalue(dst, src) => {
                    let v = self.get_stack(src).clone();
                    upvalues[dst as usize].borrow_mut().set(&mut self.stack, v);
                }
                ByteCode::SetUpvalueConst(dst, src) => {
                    let v = proto.constants[src as usize].clone();
                    upvalues[dst as usize].borrow_mut().set(&mut self.stack, v);
                }
                ByteCode::Close(ilocal) => {
                    let ilocal = self.base + ilocal as usize;
                    let from = open_brokers.binary_search_by_key(&ilocal, |b| b.ilocal).unwrap_or_else(|i| i);
                    self.close_brokers(open_brokers.drain(from..));
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::LoadNil(dst, n) => {
                    let begin = self.base + dst as usize;
                    if begin < self.stack.len() {
                        self.stack[begin..].fill(Value::Nil);
                    }
                    self.fill_stack_nil(dst, n as usize);
                }
                ByteCode::LoadBool(dst, b) => {
                    self.set_stack(dst, Value::Boolean(b));
                }
                ByteCode::LoadInt(dst, i) => {
                    self.set_stack(dst, Value::Interger(i as i64));
                }
                ByteCode::Move(dst, src) => {
                    let v = self.get_stack(src).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(t, i, v) => {
                    let value = self.get_stack(v).clone();
                    self.get_stack(t).new_index_array(i as i64, value);
                }
                ByteCode::SetIntConst(t, i, v) => {
                    let value = proto.constants[v as usize].clone();
                    self.get_stack(t).new_index_array(i as i64, value);
                }
                ByteCode::SetField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.get_stack(v).clone();
                    self.get_stack(t).new_index(key, value);
                }
                ByteCode::SetFieldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.get_stack(t).new_index(key, value);
                }
                ByteCode::SetTable(t, k, v) => {
                    let key = self.get_stack(k).clone();
                    let value = self.get_stack(v).clone();
                    self.get_stack(t).new_index(key, value);
                }
                ByteCode::SetTableConst(t, k, v) => {
                    let key = self.get_stack(k).clone();
                    let value = proto.constants[v as usize].clone();
                    self.get_stack(t).new_index(key, value);
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = self.base + table as usize + 1;
                    if let Value::Table(table) = self.get_stack(table).clone() {
                        let end = if n == 0 { self.stack.len() } else { ivalue + n as usize };
                        let values = self.stack.drain(ivalue..end);
                        table.borrow_mut().array.extend(values);
                    } else {
                        panic!("not table");
                    }
                }
                ByteCode::GetInt(dst, t, k) => {
                    let value = self.get_stack(t).index_array(k as i64);
                    self.set_stack(dst, value);
                }
                ByteCode::GetField(dst, t, k) => {
                    let key = &proto.constants[k as usize];
                    let value = self.get_stack(t).index(key);
                    self.set_stack(dst, value);
                }
                ByteCode::GetFieldSelf(dst, t, k) => {
                    let table = self.get_stack(t).clone();
                    let key = &proto.constants[k as usize];
                    let value = table.index(key).clone();
                    self.set_stack(dst, value);
                    self.set_stack(dst + 1, table);
                }
                ByteCode::GetTable(dst, t, k) => {
                    let key = self.get_stack(k);
                    let value = self.get_stack(t).index(key);
                    self.set_stack(dst, value);
                }
                ByteCode::SetUpField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.get_stack(v).clone();
                    upvalues[t as usize].borrow().get(&self.stack).new_index(key, value);
                }
                ByteCode::SetUpFidldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    upvalues[t as usize].borrow().get(&self.stack).new_index(key, value);
                }
                ByteCode::GetUpField(dst, t, k) => {
                    let key = &proto.constants[k as usize];
                    let value = upvalues[t as usize].borrow().get(&self.stack).index(key).clone();
                    self.set_stack(dst, value);
                }
                ByteCode::TestAndJump(icond, jmp) => {
                    if self.get_stack(icond).into() {
                        pc = (pc as isize + jmp as isize) as usize;
                    }
                }
                ByteCode::TestOrJump(icond, jmp) => {
                    if self.get_stack(icond).into() {
                    } else {
                        pc = (pc as isize + jmp as isize) as usize;
                    }
                }
                ByteCode::TestAndSetJump(dst, icond, jmp) => {
                    let condition = self.get_stack(icond);
                    if condition.into() {
                        self.set_stack(dst, condition.clone());
                        pc += jmp as usize;
                    }
                }
                ByteCode::TestOrSetJump(dst, icond, jmp) => {
                    let condition = self.get_stack(icond);
                    if condition.into() {
                    } else {
                        self.set_stack(dst, condition.clone());
                        pc += jmp as usize;
                    }
                }
                ByteCode::Jump(jmp) => {
                    pc = (pc as isize + jmp as isize) as usize;
                }
                ByteCode::ForPrepare(dst, jmp) => {
                    if let (&Value::Interger(mut i), &Value::Interger(step)) = (self.get_stack(dst), self.get_stack(dst + 2)) {
                        if step == 0 {
                            panic!("0 step in numerical for")
                        }

                        let limit = match self.get_stack(dst + 1) {
                            &Value::Interger(limit) => limit,
                            &Value::Float(limit) => {
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
                ByteCode::ForLoop(dst, jmp) => match self.get_stack(dst) {
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
                ByteCode::ForCallLoop(iter, nvar, jmp) => {
                    let nret = self.call_function(iter, 2 + 1);
                    let iret = self.stack.len() - nret;

                    if nret > 0 && self.stack[iret] != Value::Nil {
                        let first_ret = self.stack[iret].clone();
                        self.set_stack(iter + 2, first_ret);

                        self.stack.drain(self.base + iter as usize + 3..iret);
                        self.fill_stack_nil(iter + 3, nvar as usize);

                        pc -= jmp as usize;
                    } else if jmp == 0 {
                        pc += 1;
                    }
                }
                ByteCode::Closure(dst, inner) => {
                    let Value::LuaFunction(inner_proto) = proto.constants[inner as usize].clone() else {
                        panic!("must be funcProto");
                    };

                    let inner_upvalues = inner_proto
                        .upindexes
                        .iter()
                        .map(|up| match up {
                            &UpIndex::Upvalue(iup) => upvalues[iup].clone(),
                            &UpIndex::Local(ilocal) => {
                                let ilocal = self.base + ilocal;
                                let iob = open_brokers.binary_search_by_key(&ilocal, |b| b.ilocal).unwrap_or_else(|i| {
                                    open_brokers.insert(i, OpenBroker::from(ilocal));
                                    i
                                });
                                open_brokers[iob].broker.clone()
                            }
                        })
                        .collect();

                    let c = LuaClosure {
                        upvalues: inner_upvalues,
                        proto: inner_proto,
                    };

                    self.set_stack(dst, Value::LuaClosure(Rc::new(c)))
                }
                ByteCode::Call(func, narg_plus, want_nret) => {
                    let nret = self.call_function(func, narg_plus);

                    let iret = self.stack.len() - nret;

                    self.stack.drain(self.base + func as usize..iret);

                    let want_nret = want_nret as usize;
                    if nret < want_nret {
                        self.fill_stack_nil(func, want_nret - nret)
                    }
                }
                ByteCode::CallSet(dst, func, narg_plus) => {
                    let nret = self.call_function(func, narg_plus);

                    if nret == 0 {
                        self.set_stack(dst, Value::Nil);
                    } else {
                        let iret = self.stack.len() - nret;
                        self.stack.swap(self.base + dst as usize, iret)
                    }

                    self.stack.truncate(self.base + func as usize + 1);
                }
                ByteCode::TailCall(func, narg_plus) => {
                    self.close_brokers(open_brokers);

                    self.stack.drain(self.base - 1..self.base + func as usize);

                    return self.do_call_function(narg_plus);
                }
                ByteCode::Return(iret, nret) => {
                    self.close_brokers(open_brokers);

                    let iret = self.base + iret as usize;
                    if nret == 0 {
                        return self.stack.len() - iret;
                    } else {
                        self.stack.truncate(iret + nret as usize);
                        return nret as usize;
                    }
                }
                ByteCode::Return0 => {
                    self.close_brokers(open_brokers);
                    return 0;
                }
                ByteCode::VarArgs(dst, want) => {
                    self.stack.truncate(self.base + dst as usize);

                    let len = varargs.len();
                    let want = want as usize;
                    if want == 0 {
                        self.stack.extend_from_slice(&varargs);
                    } else if want > len {
                        self.stack.extend_from_slice(&varargs);
                        self.fill_stack_nil(dst, want);
                    } else {
                        self.stack.extend_from_slice(&varargs[..want]);
                    }
                }
                ByteCode::Neg(dst, src) => {
                    let value = match self.get_stack(src) {
                        Value::Interger(v) => Value::Interger(-v),
                        Value::Float(v) => Value::Float(-v),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::Not(dst, src) => {
                    let value = match self.get_stack(src) {
                        Value::Nil => Value::Boolean(true),
                        Value::Boolean(v) => Value::Boolean(!v),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::BitNot(dst, src) => {
                    let value = match self.get_stack(src) {
                        Value::Interger(v) => Value::Interger(!v),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value)
                }
                ByteCode::Len(dst, src) => {
                    let value = match self.get_stack(src) {
                        Value::ShortStr(len, _) => Value::Interger(*len as i64),
                        Value::MidStr(s) => Value::Interger(s.0 as i64),
                        Value::LongStr(s) => Value::Interger(s.len() as i64),
                        Value::Table(v) => Value::Interger(v.borrow().array.len() as i64),
                        _ => panic!("invalid #"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Add(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), self.get_stack(b), |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::AddConst(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), &proto.constants[b as usize], |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::AddInt(dst, a, b) => {
                    let v = exe_binop_int(self.get_stack(a), b, |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, v)
                }
                ByteCode::Sub(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), self.get_stack(b), |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::SubConst(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), &proto.constants[b as usize], |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::SubInt(dst, a, b) => {
                    let v = exe_binop_int(self.get_stack(a), b, |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, v)
                }
                ByteCode::Mul(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), self.get_stack(b), |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::MulConst(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), &proto.constants[b as usize], |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::MulInt(dst, a, b) => {
                    let v = exe_binop_int(self.get_stack(a), b, |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, v)
                }
                ByteCode::Mod(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), self.get_stack(b), |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::ModConst(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), &proto.constants[b as usize], |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::ModInt(dst, a, b) => {
                    let v = exe_binop_int(self.get_stack(a), b, |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, v)
                }
                ByteCode::Idiv(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), self.get_stack(b), |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::IdivConst(dst, a, b) => {
                    let v = exe_binop(self.get_stack(a), &proto.constants[b as usize], |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::IdivInt(dst, a, b) => {
                    let v = exe_binop_int(self.get_stack(a), b, |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::Div(dst, a, b) => {
                    let v = exe_binop_f(self.get_stack(a), self.get_stack(b), |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::DivConst(dst, a, b) => {
                    let v = exe_binop_f(self.get_stack(a), &proto.constants[b as usize], |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::DivInt(dst, a, b) => {
                    let v = exe_binop_int_f(self.get_stack(a), b, |a, b| a / b);
                    self.set_stack(dst, v)
                }
                ByteCode::Pow(dst, a, b) => {
                    let v = exe_binop_f(self.get_stack(a), self.get_stack(b), |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::PowConst(dst, a, b) => {
                    let v = exe_binop_f(self.get_stack(a), &proto.constants[b as usize], |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::PowInt(dst, a, b) => {
                    let v = exe_binop_int_f(self.get_stack(a), b, |a, b| a.powf(b));
                    self.set_stack(dst, v)
                }
                ByteCode::BitAnd(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), self.get_stack(b), |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitAndConst(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), &proto.constants[b as usize], |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitAndInt(dst, a, b) => {
                    let v = exe_binop_int_i(self.get_stack(a), b, |a, b| a & b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOr(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), self.get_stack(b), |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOrConst(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), &proto.constants[b as usize], |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitOrInt(dst, a, b) => {
                    let v = exe_binop_int_i(self.get_stack(a), b, |a, b| a | b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXor(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), self.get_stack(b), |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXorConst(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), &proto.constants[b as usize], |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::BitXorInt(dst, a, b) => {
                    let v = exe_binop_int_i(self.get_stack(a), b, |a, b| a ^ b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftL(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), self.get_stack(b), |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftLConst(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), &proto.constants[b as usize], |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftLInt(dst, a, b) => {
                    let v = exe_binop_int_i(self.get_stack(a), b, |a, b| a << b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftR(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), self.get_stack(b), |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftRConst(dst, a, b) => {
                    let v = exe_binop_i(self.get_stack(a), &proto.constants[b as usize], |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::ShiftRInt(dst, a, b) => {
                    let v = exe_binop_int_i(self.get_stack(a), b, |a, b| a >> b);
                    self.set_stack(dst, v)
                }
                ByteCode::Equal(a, b, r) => {
                    if (self.get_stack(a) == self.get_stack(b)) == r {
                        pc += 1;
                    }
                }
                ByteCode::EqualConst(a, b, r) => {
                    if (self.get_stack(a) == &proto.constants[b as usize]) == r {
                        pc += 1;
                    }
                }
                ByteCode::EqualInt(a, b, r) => {
                    if let &Value::Interger(ii) = self.get_stack(a) {
                        if (ii == b as i64) == r {
                            pc += 1;
                        }
                    }
                }
                ByteCode::NotEq(a, b, r) => {
                    if (self.get_stack(a) != self.get_stack(b)) == r {
                        pc += 1;
                    }
                }
                ByteCode::NotEqConst(a, b, r) => {
                    if (self.get_stack(a) != &proto.constants[b as usize]) == r {
                        pc += 1;
                    }
                }
                ByteCode::NotEqInt(a, b, r) => {
                    if let &Value::Interger(ii) = self.get_stack(a) {
                        if (ii != b as i64) == r {
                            pc += 1;
                        }
                    }
                }
                ByteCode::LesEq(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if !matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::LesEqConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if !matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::LesEqInt(a, b, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Interger(v) => v,
                        &Value::Float(v) => v as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a <= b as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEq(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if !matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEqConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if !matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEqInt(a, b, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Interger(v) => v,
                        &Value::Float(v) => v as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a >= b as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::Less(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::LessConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::LessInt(a, b, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Interger(v) => v,
                        &Value::Float(v) => v as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a < b as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::Greater(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreaterConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreaterInt(a, b, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Interger(v) => v,
                        &Value::Float(v) => v as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a > b as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::SetFalseSkip(dst) => {
                    self.set_stack(dst, Value::Boolean(false));
                    pc += 1;
                }
                ByteCode::Concat(dst, a, b) => {
                    let v = exe_concat(self.get_stack(a), self.get_stack(b));
                    self.set_stack(dst, v);
                }
                ByteCode::ConcatConst(dst, a, b) => {
                    let v = exe_concat(self.get_stack(a), &proto.constants[b as usize]);
                    self.set_stack(dst, v);
                }
                ByteCode::ConcatInt(dst, a, b) => {
                    let v = exe_concat(self.get_stack(a), &Value::Interger(b as i64));
                    self.set_stack(dst, v);
                }
            }

            pc += 1;
        }
    }

    fn get_stack(&self, dst: u8) -> &Value {
        &self.stack[self.base + dst as usize]
    }

    fn set_stack(&mut self, dst: u8, v: Value) {
        set_vec(&mut self.stack, self.base + dst as usize, v);
    }

    fn fill_stack_nil(&mut self, base: u8, to: usize) {
        self.stack.resize(self.base + base as usize + to, Value::Nil);
    }

    // fn set_table(&mut self, t: u8, key: Value, value: Value) {
    //     match &key {
    //         Value::Interger(v) => self.set_table_int(t, *v, value),
    //         _ => self.do_set_table(t, key, value),
    //     }
    // }

    // fn set_table_int(&mut self, t: u8, i: i64, value: Value) {
    //     if let Value::Table(table) = self.get_stack(t) {
    //         let mut table = table.borrow_mut();
    //         if i > 0 && (i < 4 || i < table.array.capacity() as i64 * 2) {
    //             set_vec(&mut table.array, i as usize - 1, value);
    //         } else {
    //             table.map.insert(Value::Interger(i), value);
    //         }
    //     } else {
    //         panic!("invalid table")
    //     }
    // }

    // fn do_set_table(&mut self, t: u8, key: Value, value: Value) {
    //     if let Value::Table(table) = self.get_stack(t) {
    //         table.borrow_mut().map.insert(key, value);
    //     } else {
    //         panic!("invalid table");
    //     }
    // }

    // fn get_table(&self, t: u8, key: &Value) -> Value {
    //     match key {
    //         Value::Interger(i) => self.get_table_int(t, *i),
    //         _ => self.do_get_table(t, key),
    //     }
    // }

    // fn get_table_int(&self, t: u8, i: i64) -> Value {
    //     if let Value::Table(table) = self.get_stack(t) {
    //         let table = table.borrow();
    //         table.array.get(i as usize - 1).unwrap_or_else(|| table.map.get(&Value::Interger(i)).unwrap_or(&Value::Nil)).clone()
    //     } else {
    //         panic!("set invalid table");
    //     }
    // }

    // fn do_get_table(&self, t: u8, key: &Value) -> Value {
    //     if let Value::Table(table) = self.get_stack(t) {
    //         let table = table.borrow();
    //         table.map.get(key).unwrap_or(&Value::Nil).clone()
    //     } else {
    //         panic!("set invalid table")
    //     }
    // }

    fn call_function(&mut self, func: u8, narg_plus: u8) -> usize {
        self.base += func as usize + 1;
        let nret = self.do_call_function(narg_plus);
        self.base -= func as usize + 1;
        nret
    }

    fn do_call_function(&mut self, narg_plus: u8) -> usize {
        if narg_plus != 0 {
            self.stack.truncate(self.base + narg_plus as usize - 1);
        }

        match self.stack[self.base - 1].clone() {
            Value::RustFunction(f) => f(self) as usize,
            Value::RustClosure(f) => f.borrow_mut()(self) as usize,
            Value::LuaFunction(f) => self.execute(&f, &Vec::new()),
            Value::LuaClosure(f) => self.execute(&f.proto, &f.upvalues),
            v => panic!("invalid function: {v:?}"),
        }
    }

    fn close_brokers(&self, open_brokers: impl IntoIterator<Item = OpenBroker>) {
        for OpenBroker { ilocal, broker } in open_brokers {
            let openi = broker.replace(Upvalue::Closed(self.stack[ilocal].clone()));
            debug_assert_eq!(openi, Upvalue::Open(ilocal));
        }
    }

    fn make_float(&mut self, dst: u8) -> f64 {
        match self.get_stack(dst) {
            &Value::Float(v) => v,
            &Value::Interger(i) => {
                let f = i as f64;
                self.set_stack(dst, Value::Float(f));
                f
            }
            ref v => panic!("not number {v:?}"),
        }
    }

    fn read_int(&self, dst: u8) -> i64 {
        if let &Value::Interger(i) = self.get_stack(dst) {
            i
        } else {
            panic!("invalid interger");
        }
    }

    fn read_float(&self, dst: u8) -> f64 {
        if let &Value::Float(f) = self.get_stack(dst) {
            f
        } else {
            panic!("invalid float");
        }
    }
}

impl<'a> ExeState {
    pub fn get_top(&self) -> usize {
        self.stack.len() - self.base
    }

    pub fn get<T>(&'a self, i: usize) -> T
    where
        T: From<&'a Value>,
    {
        (&self.stack[self.base + i - 1]).into()
    }

    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into())
    }
}

fn exe_binop(v1: &Value, v2: &Value, arith_i: fn(i64, i64) -> i64, arith_f: fn(f64, f64) -> f64) -> Value {
    match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => Value::Interger(arith_i(*a, *b)),
        (Value::Interger(a), Value::Float(b)) => Value::Float(arith_f(*a as f64, *b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(arith_f(*a, *b)),
        (Value::Float(a), Value::Interger(b)) => Value::Float(arith_f(*a, *b as f64)),
        _ => todo!("meta"),
    }
}

fn exe_binop_int(v1: &Value, v2: u8, arith_i: fn(i64, i64) -> i64, arith_f: fn(f64, f64) -> f64) -> Value {
    match v1 {
        Value::Interger(a) => Value::Interger(arith_i(*a, v2 as i64)),
        Value::Float(a) => Value::Float(arith_f(*a, v2 as f64)),
        _ => todo!("meta"),
    }
}

fn exe_binop_f(v1: &Value, v2: &Value, arith_f: fn(f64, f64) -> f64) -> Value {
    let (f1, f2) = match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => (*a as f64, *b as f64),
        (Value::Interger(a), Value::Float(b)) => (*a as f64, *b),
        (Value::Float(a), Value::Float(b)) => (*a, *b),
        (Value::Float(a), Value::Interger(b)) => (*a, *b as f64),
        _ => todo!("meta"),
    };

    Value::Float(arith_f(f1, f2))
}

fn exe_binop_int_f(v1: &Value, v2: u8, arith_f: fn(f64, f64) -> f64) -> Value {
    let f1 = match v1 {
        Value::Interger(i1) => *i1 as f64,
        Value::Float(f1) => *f1,
        _ => todo!("meta"),
    };

    Value::Float(arith_f(f1, v2 as f64))
}

fn exe_binop_i(v1: &Value, v2: &Value, arith_i: fn(i64, i64) -> i64) -> Value {
    let (i1, i2) = match (v1, v2) {
        (Value::Interger(a), Value::Interger(b)) => (*a, *b),
        (Value::Interger(a), Value::Float(b)) => (*a, ftoi(*b).unwrap()),
        (Value::Float(a), Value::Float(b)) => (ftoi(*a).unwrap(), ftoi(*b).unwrap()),
        (Value::Float(a), Value::Interger(b)) => (ftoi(*a).unwrap(), *b),
        (_, _) => todo!("meta"),
    };

    Value::Interger(arith_i(i1, i2))
}

fn exe_binop_int_i(v1: &Value, v2: u8, arith_i: fn(i64, i64) -> i64) -> Value {
    let i1 = match v1 {
        Value::Interger(i1) => *i1,
        Value::Float(f1) => ftoi(*f1).unwrap(),
        _ => todo!("meta"),
    };

    Value::Interger(arith_i(i1, v2 as i64))
}

fn exe_concat(v1: &Value, v2: &Value) -> Value {
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
