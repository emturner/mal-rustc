use std::collections::LinkedList;

#[derive(Debug, PartialEq, Eq)]
pub enum MalAtom<'a> {
    Nil,
    Bool(bool),
    Special(&'a str),
    // ideally this would be &'a str, but we need to escape the control characters
    // so need to allocate a new string
    String(String),
    Int(i32),
    List(LinkedList<MalAtom<'a>>),
}
