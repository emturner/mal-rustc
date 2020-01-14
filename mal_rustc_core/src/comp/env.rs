use std::collections::HashMap;

#[derive(Clone)]
pub enum MalAtomCompRef {
    Var(String),
    Func(String),
}

pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    current: HashMap<String, MalAtomCompRef>,
    new_vars: HashMap<String, bool>,
}

impl<'a> Env<'a> {
    pub fn new(outer: Option<&'a Env>) -> Self {
        Env {
            outer,
            current: HashMap::new(),
            new_vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, val: MalAtomCompRef) {
        let existed_in_current = self.current.insert(name.clone(), val).is_some();

        if !existed_in_current {
            let exists_in_outer = self.outer.map_or(false, |e| e.find(&name).is_some());
            self.new_vars.insert(name, !exists_in_outer);
        }
    }

    pub fn find(&self, name: &'a str) -> Option<MalAtomCompRef> {
        let v = self.current.get(name);
        if let (None, Some(outer)) = (v, self.outer) {
            outer.find(name)
        } else {
            v.cloned()
        }
    }

    pub fn get_new_vars(&mut self) -> Vec<(String, bool)> {
        self.new_vars.drain().collect()
    }
}
