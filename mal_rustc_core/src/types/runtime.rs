use std::collections::HashMap;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq)]
pub enum MalAtom<'a> {
    Nil,
    Bool(bool),
    Special(&'a str),
    // ideally this would be &'a str, but we need to escape the control characters
    // so need to allocate a new string
    String(String),
    Int(i64),
    Symbol(&'a str),
    SExp(Vec<MalAtom<'a>>),
    Vector(Vec<MalAtom<'a>>),
    Keyword(&'a str),
    HashMap(HashMap<String, MalAtom<'a>>),
}

impl<'a> Display for MalAtom<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MalAtom::Nil => write!(f, "nil"),
            MalAtom::Bool(b) => b.fmt(f),
            MalAtom::Special(s) => s.fmt(f),
            MalAtom::String(s) => format!(
                "\"{}\"",
                s.replace("\\", "\\\\")
                    .replace("\n", "\\n")
                    .replace("\"", "\\\"")
            )
            .fmt(f),
            MalAtom::Int(i) => i.fmt(f),
            MalAtom::Symbol(s) => s.fmt(f),
            MalAtom::SExp(sexp) => {
                write!(f, "(")?;
                let exps = sexp
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<String>>()
                    .join(" ");
                exps.fmt(f)?;
                write!(f, ")")
            }
            MalAtom::Vector(v) => {
                write!(f, "[")?;
                let exps = v
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<String>>()
                    .join(" ");
                exps.fmt(f)?;
                write!(f, "]")
            }
            MalAtom::Keyword(k) => k.fmt(f),
            MalAtom::HashMap(h) => {
                write!(f, "{{")?;
                let exps = h
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "{} {}",
                            if k.starts_with(":") {
                                MalAtom::Keyword(k)
                            } else {
                                MalAtom::String(k.into())
                            },
                            v
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(" ");
                exps.fmt(f)?;
                write!(f, "}}")
            }
        }
    }
}