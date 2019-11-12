use std::collections::LinkedList;

#[derive(Debug, PartialEq, Eq)]
pub enum MalAtom<'a> {
    Nil,
    Bool(bool),
    Special(&'a str),
    String(&'a str),
    Int(i32),
    List(LinkedList<MalAtom<'a>>)
}