use super::*;

use std::collections::HashMap;

pub enum MalAtomCompRef<'a> {
    Var,
    Func(MalFuncCallTemplate<'a>),
}

pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    current: HashMap<&'a str, MalAtomCompRef<'a>>,
}

impl<'a> Env<'a> {
    pub fn new(outer: Option<&'a Env>) -> Self {
        Env {
            outer,
            current: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &'a str, val: MalAtomCompRef<'a>) {
        self.current.insert(name, val);
    }

    pub fn find(&self, name: &'a str) -> Option<&'a MalAtomCompRef> {
        let v = self.current.get(name);
        if let (None, Some(outer)) = (v, self.outer) {
            outer.find(name)
        } else {
            v
        }
    }
}
