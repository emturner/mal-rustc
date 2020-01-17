use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Clone)]
pub enum MalAtomCompRef {
    Var(String),
    Func(String),
}

pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    current: HashMap<String, String>,
    new_vars: HashMap<String, bool>,
    new_captures: HashSet<String>,
}

impl<'a> Env<'a> {
    pub fn new(outer: Option<&'a Env>) -> Self {
        Env {
            outer,
            current: HashMap::new(),
            new_vars: HashMap::new(),
            new_captures: HashSet::new(),
        }
    }

    pub fn set(&mut self, name: String, val: String, is_arg: bool) {
        let existed_in_current = self.current.insert(name.clone(), val.clone()).is_some();

        if !existed_in_current {
            let exists_in_outer = self
                .outer
                .map_or(false, |e| e.find_inner(&name, true).0.is_some());

            if !is_arg && exists_in_outer {
                self.new_captures.insert(val);
            }
            self.new_vars.insert(name, !exists_in_outer);
        }
    }

    pub fn find<'b>(&mut self, name: &'b str) -> Option<String> {
        let (v, found_in_outer) = self.find_inner(name, false);
        if let Some(name) = &v {
            if found_in_outer {
                self.new_captures.insert(name.clone());
            }
        }
        v
    }

    pub fn get_new_vars(&mut self) -> Vec<(String, bool)> {
        self.new_vars.drain().collect()
    }

    pub fn get_new_captures(&mut self) -> Vec<String> {
        self.new_captures.drain().collect()
    }

    fn find_inner(&self, name: &str, is_outer: bool) -> (Option<String>, bool) {
        let v = self.current.get(name);
        match (v, self.outer) {
            (None, Some(outer)) => outer.find_inner(name, true),
            (Some(_), None) => (v.cloned(), false), // this means we're referencing a builtin! don't want to capture it
            _ => (v.cloned(), is_outer),
        }
    }
}
