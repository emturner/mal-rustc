use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::*,
    combinator::*,
    error::{context, VerboseError},
    multi::*,
    sequence::*,
    IResult,
};

use crate::types::MalAtomComp;

use std::collections::HashMap;

type ParseResult<'a> = IResult<&'a str, MalAtomComp<'a>, VerboseError<&'a str>>;

pub fn parse_mal_atom(input: &str) -> ParseResult {
    context(
        "mal atom",
        preceded(
            alt((capture_whitespace, capture_comment)),
            alt((
                parse_bool,
                parse_int,
                parse_string,
                parse_nil,
                parse_special,
                parse_keyword,
                parse_symbol,
                parse_sexp,
                parse_vector,
                parse_hash_map,
            )),
        ),
    )(input)
}

fn parse_nil(input: &str) -> ParseResult {
    context(
        "nil",
        map(terminated(tag("nil"), peek(not(alphanumeric1))), |_| {
            MalAtomComp::Nil
        }),
    )(input)
}

fn parse_true(input: &str) -> ParseResult {
    context(
        "true",
        map(terminated(tag("true"), peek(not(alphanumeric1))), |_| {
            MalAtomComp::Bool(true)
        }),
    )(input)
}

fn parse_false(input: &str) -> ParseResult {
    context(
        "false",
        map(terminated(tag("false"), peek(not(alphanumeric1))), |_| {
            MalAtomComp::Bool(false)
        }),
    )(input)
}

fn parse_symbol(input: &str) -> ParseResult {
    context(
        "symbol",
        map(
            preceded(
                peek(not(parse_int)),
                take_while1(|c: char| {
                    (c.is_alphanumeric() || !"[]{}()'`~^@\",;:".contains(c)) && !c.is_whitespace()
                }),
            ),
            |i: &str| MalAtomComp::Symbol(i),
        ),
    )(input)
}

fn parse_keyword(input: &str) -> ParseResult {
    context(
        "keyword",
        preceded(
            peek(char(':')),
            map(take_till1(|c| "[]{}()'`~^@ ,".contains(c)), |i: &str| {
                MalAtomComp::Keyword(i)
            }),
        ),
    )(input)
}

fn parse_bool(input: &str) -> ParseResult {
    context("bool", alt((parse_true, parse_false)))(input)
}

fn parse_string(input: &str) -> ParseResult {
    context(
        "string",
        map(
            delimited(
                tag("\""),
                opt(escaped_transform(none_of("\"\\"), '\\', |input| {
                    alt((
                        map(tag("\\"), |_| "\\"),
                        map(tag("n"), |_| "\n"),
                        map(tag("\""), |_| "\""),
                    ))(input)
                })),
                context("UNBALANCED", cut(tag("\""))),
            ),
            |s| MalAtomComp::String(s.unwrap_or_else(|| "".into())),
        ),
    )(input)
}

fn parse_int(input: &str) -> ParseResult {
    context(
        "int",
        map(
            terminated(
                alt((
                    map_res(digit1, |i: &str| i.parse::<i64>().map(MalAtomComp::Int)),
                    map_res(preceded(tag("-"), digit1), |i: &str| {
                        i.parse::<i64>().map(|i| MalAtomComp::Int(-i))
                    }),
                )),
                peek(not(alpha1)),
            ),
            |o| o,
        ),
    )(input)
}

// captures whitespace & commans (which count as whitespace in mal)
fn capture_whitespace(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    context(
        "whitespace",
        map(many0(alt((space1, tag(","), line_ending))), |_| ()),
    )(input)
}

// captures comment
fn capture_comment(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    context("comment", map(preceded(tag(";"), not_line_ending), |_| ()))(input)
}

fn parse_special(input: &str) -> ParseResult {
    context(
        "special",
        alt((
            map(preceded(tag("~@"), parse_mal_atom), |i| {
                MalAtomComp::SExp(vec![MalAtomComp::Symbol("splice-unquote"), i])
            }),
            map(preceded(char('\''), parse_mal_atom), |i| {
                MalAtomComp::SExp(vec![MalAtomComp::Symbol("quote"), i])
            }),
            map(preceded(char('`'), parse_mal_atom), |i| {
                MalAtomComp::SExp(vec![MalAtomComp::Symbol("quasiquote"), i])
            }),
            map(preceded(char('~'), parse_mal_atom), |i| {
                MalAtomComp::SExp(vec![MalAtomComp::Symbol("unquote"), i])
            }),
            map(
                preceded(char('^'), pair(parse_mal_atom, parse_mal_atom)),
                |i| MalAtomComp::SExp(vec![MalAtomComp::Symbol("with-meta"), i.1, i.0]),
            ),
            map(preceded(char('@'), parse_mal_atom), |i| {
                MalAtomComp::SExp(vec![MalAtomComp::Symbol("deref"), i])
            }),
        )),
    )(input)
}

fn parse_sexp(input: &str) -> ParseResult {
    context(
        "sexp",
        map(
            delimited(
                char('('),
                many0(preceded(capture_whitespace, parse_mal_atom)),
                cut(preceded(
                    capture_whitespace,
                    context("UNBALANCED", char(')')),
                )),
            ),
            MalAtomComp::SExp,
        ),
    )(input)
}

fn parse_vector(input: &str) -> ParseResult {
    context(
        "vector",
        map(
            delimited(
                char('['),
                many0(preceded(capture_whitespace, parse_mal_atom)),
                cut(preceded(
                    capture_whitespace,
                    context("UNBALANCED", char(']')),
                )),
            ),
            MalAtomComp::Vector,
        ),
    )(input)
}

fn parse_hash_map(input: &str) -> ParseResult {
    context(
        "HashMap",
        map(
            delimited(
                char('{'),
                many0(pair(
                    preceded(
                        capture_whitespace,
                        preceded(
                            peek(parse_mal_atom),
                            cut(alt((parse_keyword, parse_string))),
                        ),
                    ),
                    preceded(capture_whitespace, cut(parse_mal_atom)),
                )),
                cut(preceded(
                    capture_whitespace,
                    context("UNBALANCED", char('}')),
                )),
            ),
            |h| {
                let mut hm = HashMap::new();

                for kv_pair in h {
                    match kv_pair {
                        (MalAtomComp::Keyword(k), a) => {
                            hm.insert(k.into(), a);
                        }
                        (MalAtomComp::String(s), a) => {
                            hm.insert(s, a);
                        }
                        _ => unreachable!(),
                    }
                }

                MalAtomComp::HashMap(hm)
            },
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_symbol_ok() {
        assert_eq!(
            Ok((" world", MalAtomComp::Symbol("hello"))),
            parse_symbol("hello world")
        );
    }

    #[test]
    fn capture_comment_ok() {
        assert_eq!(Ok(("", ())), capture_comment(";"));
        assert_eq!(Ok(("", ())), capture_comment("; hello comment"));
        assert_eq!(Ok(("\n ()", ())), capture_comment("; hello comment\n ()"));
        assert_eq!(
            Ok(("\r\n ()", ())),
            capture_comment("; hello comment\r\n ()")
        );
    }

    #[test]
    fn capture_whitespace_ok() {
        assert_eq!(Ok(("", ())), capture_whitespace(""));
        assert_eq!(Ok(("", ())), capture_whitespace(" "));
        assert_eq!(Ok(("", ())), capture_whitespace(","));
        assert_eq!(Ok(("", ())), capture_whitespace("  \t  ,,,,  \t\t  "));

        assert_eq!(Ok(("a", ())), capture_whitespace("\t , \ta"));
        assert_eq!(Ok(("a", ())), capture_whitespace("\t , \n , \r\n \ta"));
    }

    #[test]
    fn parse_int_ok() {
        assert_eq!(Ok(("", MalAtomComp::Int(0))), parse_int("0"));
        assert_eq!(Ok(("", MalAtomComp::Int(1304))), parse_int("1304"));

        assert_eq!(Ok(("", MalAtomComp::Int(-290))), parse_int("-290"));
    }

    #[test]
    fn parse_int_err() {
        assert!(parse_int("ab13").is_err());
        assert!(parse_int("124a").is_err());
    }

    #[test]
    fn parse_nil_ok() {
        assert_eq!(Ok(("", MalAtomComp::Nil)), parse_nil("nil"));
        assert_eq!(Ok((" ", MalAtomComp::Nil)), parse_nil("nil "));
        assert_eq!(Ok((",hey", MalAtomComp::Nil)), parse_nil("nil,hey"));
    }

    #[test]
    fn parse_nil_err() {
        assert!(parse_nil("nila").is_err());
        assert!(parse_nil("nil1").is_err());
        assert!(parse_nil("notnil").is_err());
    }

    #[test]
    fn parse_true_ok() {
        assert_eq!(Ok(("", MalAtomComp::Bool(true))), parse_true("true"));
        assert_eq!(Ok((" ", MalAtomComp::Bool(true))), parse_true("true "));
        assert_eq!(
            Ok((",hey", MalAtomComp::Bool(true))),
            parse_true("true,hey")
        );
    }

    #[test]
    fn parse_true_err() {
        assert!(parse_true("truea").is_err());
        assert!(parse_true("true1").is_err());
        assert!(parse_true("nottrue").is_err());
    }

    #[test]
    fn parse_false_ok() {
        assert_eq!(Ok(("", MalAtomComp::Bool(false))), parse_false("false"));
        assert_eq!(Ok((" ", MalAtomComp::Bool(false))), parse_false("false "));
        assert_eq!(
            Ok((",hey", MalAtomComp::Bool(false))),
            parse_false("false,hey")
        );
    }

    #[test]
    fn parse_false_err() {
        assert!(parse_false("falsea").is_err());
        assert!(parse_false("false1").is_err());
        assert!(parse_false("notfalse").is_err());
    }

    #[test]
    fn parse_book_ok() {
        assert_eq!(Ok(("", MalAtomComp::Bool(false))), parse_bool("false"));
        assert_eq!(Ok(("", MalAtomComp::Bool(true))), parse_bool("true"));
    }

    #[test]
    fn parse_string_ok() {
        assert_eq!(
            Ok(("", MalAtomComp::String("".into()))),
            parse_string("\"\"")
        );
        assert_eq!(
            Ok(("a", MalAtomComp::String("".into()))),
            parse_string("\"\"a")
        );
        assert_eq!(
            Ok(("", MalAtomComp::String("h3llo, World!".into()))),
            parse_string("\"h3llo, World!\"")
        );

        assert_eq!(
            Ok(("", MalAtomComp::String("\"".into()))),
            parse_string(r#""\"""#)
        );

        assert_eq!(
            Ok(("", MalAtomComp::String("\n".into()))),
            parse_string(r#""\n""#)
        );

        assert_eq!(
            Ok(("", MalAtomComp::String("\\".into()))),
            parse_string(r#""\\""#)
        );
    }
}
