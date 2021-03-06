use super::*;

use std::collections::HashMap;

#[derive(Clone)]
pub enum MalAtomCompRef {
    Var(String),
    Func(MalFuncCallTemplate),
}

pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    current: HashMap<String, MalAtomCompRef>,
}

impl<'a> Env<'a> {
    pub fn new(outer: Option<&'a Env>) -> Self {
        Env {
            outer,
            current: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, val: MalAtomCompRef) {
        self.current.insert(name, val);
    }

    pub fn find(&self, name: &'a str) -> Option<MalAtomCompRef> {
        let v = self.current.get(name);
        if let (None, Some(outer)) = (v, self.outer) {
            outer.find(name)
        } else {
            v.cloned()
        }
    }
}
