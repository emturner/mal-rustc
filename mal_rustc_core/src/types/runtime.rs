use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub type MalResult = Result<MalAtom, String>;
pub type MalResultRef<'a> = Result<&'a MalAtom, String>;

pub const MAL_BUILTIN_PLUS: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_plus);
pub const MAL_BUILTIN_SUB: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_sub);
pub const MAL_BUILTIN_MUL: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_mul);
pub const MAL_BUILTIN_DIV: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_div);
// List functions
pub const MAL_BUILTIN_LIST: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_list);
pub const MAL_BUILTIN_IS_LIST: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_is_list);
pub const MAL_BUILTIN_IS_EMPTY: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_is_empty);
pub const MAL_BUILTIN_COUNT: &MalAtom = &MalAtom::BuiltinFunc(mal_builtin_count);

#[derive(Clone)]
pub enum MalAtom {
    // Variable has not currently been defined
    Undefined,
    Nil,
    Bool(bool),
    Special(Rc<String>),
    String(Rc<String>),
    Int(i64),
    Symbol(Rc<String>),
    List(Rc<Vec<MalAtom>>),
    Vector(Rc<Vec<MalAtom>>),
    Keyword(Rc<String>),
    HashMap(Rc<HashMap<String, MalAtom>>),
    Func(Rc<dyn Fn(&[&MalAtom]) -> MalResult>),
    BuiltinFunc(fn(&[&MalAtom]) -> MalResult),
}

impl MalAtom {
    pub fn is_defined(&self, name: &'static str) -> MalResultRef {
        match self {
            Self::Undefined => Err(format!("Exception: {} undefined", name)),
            _ => Ok(self),
        }
    }

    pub fn call(&self, args: &[&MalAtom]) -> MalResult {
        match self {
            Self::Func(f) => f(args),
            Self::BuiltinFunc(f) => f(args),
            _ => MalResult::Err("Exception: expected a function".to_string()),
        }
    }
}

impl fmt::Debug for MalAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &dyn fmt::Display).fmt(f)
    }
}

impl fmt::Display for MalAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MalAtom::Undefined => unreachable!(
                "Implementation Exception: MalAtom must be checked to be defined before printing"
            ),
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
            MalAtom::List(list) => {
                write!(f, "(")?;
                let exps = list
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
                                MalAtom::Keyword(Rc::new(k.to_string()))
                            } else {
                                MalAtom::String(Rc::new(k.to_string()))
                            },
                            v
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(" ");
                exps.fmt(f)?;
                write!(f, "}}")
            }
            MalAtom::Func(_) | MalAtom::BuiltinFunc(_) => write!(f, "#<function>"),
        }
    }
}

impl<'a> std::convert::Into<bool> for &'a MalAtom {
    fn into(self) -> bool {
        if let MalAtom::Bool(false) | MalAtom::Nil = self {
            false
        } else {
            true
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

pub fn mal_builtin_plus(args: &[&MalAtom]) -> MalResult {
    args.iter()
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

pub fn mal_builtin_mul(args: &[&MalAtom]) -> MalResult {
    args.iter()
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

pub fn mal_builtin_div(args: &[&MalAtom]) -> MalResult {
    if args.is_empty() {
        Ok(MalAtom::Int(1))
    } else {
        args[1..]
            .iter()
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

pub fn mal_builtin_sub(args: &[&MalAtom]) -> MalResult {
    if args.is_empty() {
        Ok(MalAtom::Int(0))
    } else {
        args[1..]
            .iter()
            .fold(Ok(args[0].clone()), |acc, next| acc? - next)
    }
}

pub fn mal_builtin_list(args: &[&MalAtom]) -> MalResult {
    Ok(MalAtom::List(Rc::new(
        args.iter().map(|a| (*a).clone()).collect()
    )))
}

pub fn mal_builtin_is_list(args: &[&MalAtom]) -> MalResult {
    match args.get(0) {
        Some(MalAtom::List(_)) => Ok(MalAtom::Bool(true)),
        Some(_) => Ok(MalAtom::Bool(false)),
        _ => Err("Exception: 'list?' requires at least one argument".to_string()),
    }
}

pub fn mal_builtin_is_empty(args: &[&MalAtom]) -> MalResult {
    match args.get(0) {
        Some(MalAtom::List(l)) => Ok(MalAtom::Bool(l.is_empty())),
        Some(MalAtom::Nil) => Ok(MalAtom::Bool(false)),
        Some(_) => Ok(MalAtom::Bool(true)),
        _ => Err("Exception: 'empty?' requires at least one argument".to_string()),
    }
}

pub fn mal_builtin_count(args: &[&MalAtom]) -> MalResult {
    match args.get(0) {
        Some(MalAtom::List(l)) => Ok(MalAtom::Int(l.len() as i64)),
        Some(MalAtom::Nil) => Ok(MalAtom::Int(0)),
        Some(_) => Ok(MalAtom::Int(1)),
        _ => Err("Exception: 'count?' requires at least one argument".to_string()),
    }
}