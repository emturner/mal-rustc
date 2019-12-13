use std::collections::HashMap;
use std::fmt::{self, Display};

pub type MalResult = Result<MalAtom, String>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MalAtom {
    Nil,
    Bool(bool),
    Special(String),
    // ideally this would be &'a str, but we need to escape the control characters
    // so need to allocate a new string
    String(String),
    Int(i64),
    Symbol(String),
    SExp(Vec<MalAtom>),
    Vector(Vec<MalAtom>),
    Keyword(String),
    HashMap(HashMap<String, MalAtom>),
}

impl<'a> Display for MalAtom {
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
                            if k.starts_with(':') {
                                MalAtom::Keyword(k.to_string())
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

impl<'a> std::ops::Add<&'a MalAtom> for MalAtom {
    type Output = MalResult;

    fn add(self, other: &'a MalAtom) -> MalResult {
        match (self, other) {
            (MalAtom::Int(s), MalAtom::Int(o)) => Ok(MalAtom::Int(s + o)),
            _ => Err("Expected two ints".into()),
        }
    }
}

pub fn mal_builtin_plus(args: Vec<&MalAtom>) -> MalResult {
    args.into_iter()
        .fold(Ok(MalAtom::Int(0)), |acc, next| acc? + next)
}

impl<'a> std::ops::Mul<&'a MalAtom> for MalAtom {
    type Output = MalResult;

    fn mul(self, other: &'a MalAtom) -> MalResult {
        match (self, other) {
            (MalAtom::Int(s), MalAtom::Int(o)) => Ok(MalAtom::Int(s * o)),
            _ => Err("Expected two ints".into()),
        }
    }
}

pub fn mal_builtin_mul(args: Vec<&MalAtom>) -> MalResult {
    args.into_iter()
        .fold(Ok(MalAtom::Int(1)), |acc, next| acc? * next)
}

impl<'a> std::ops::Div<&'a MalAtom> for MalAtom {
    type Output = MalResult;

    fn div(self, other: &'a MalAtom) -> MalResult {
        match (self, other) {
            (MalAtom::Int(s), MalAtom::Int(o)) => Ok(MalAtom::Int(s / o)),
            _ => Err("Expected two ints".into()),
        }
    }
}

pub fn mal_builtin_div(args: Vec<&MalAtom>) -> MalResult {
    if args.is_empty() {
        Ok(MalAtom::Int(1))
    } else {
        args[1..].into_iter()
            .fold(Ok(args[0].clone()), |acc, next| acc? / next)
    }
}

impl<'a> std::ops::Sub<&'a MalAtom> for MalAtom {
    type Output = MalResult;

    fn sub(self, other: &'a MalAtom) -> MalResult {
        match (self, other) {
            (MalAtom::Int(s), MalAtom::Int(o)) => Ok(MalAtom::Int(s - o)),
            _ => Err("Expected two ints".into()),
        }
    }
}

pub fn mal_builtin_sub(args: Vec<&MalAtom>) -> MalResult {
    if args.is_empty() { 
        Ok(MalAtom::Int(0))
    } else {
        args[1..].into_iter()
            .fold(Ok(args[0].clone()), |acc, next| acc? - next)
    }
}