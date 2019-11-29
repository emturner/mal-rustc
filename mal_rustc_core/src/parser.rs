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

use crate::types::runtime::MalAtom;

use std::collections::HashMap;

type ParseResult<'a> = IResult<&'a str, MalAtom<'a>, VerboseError<&'a str>>;

pub fn parse_mal_atom<'a>(input: &'a str) -> ParseResult<'a> {
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

fn parse_nil<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "nil",
        map(terminated(tag("nil"), peek(not(alphanumeric1))), |_| {
            MalAtom::Nil
        }),
    )(input)
}

fn parse_true<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "true",
        map(terminated(tag("true"), peek(not(alphanumeric1))), |_| {
            MalAtom::Bool(true)
        }),
    )(input)
}

fn parse_false<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "false",
        map(terminated(tag("false"), peek(not(alphanumeric1))), |_| {
            MalAtom::Bool(false)
        }),
    )(input)
}

fn parse_symbol<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "symbol",
        map(
            preceded(
                peek(not(parse_int)),
                take_while1(|c: char| {
                    (c.is_alphanumeric() || !"[]{}()'`~^@\",;:".contains(c)) && !c.is_whitespace()
                }),
            ),
            |i: &str| MalAtom::Symbol(i),
        ),
    )(input)
}

fn parse_keyword<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "keyword",
        preceded(
            peek(char(':')),
            map(take_till1(|c| "[]{}()'`~^@ ,".contains(c)), |i: &'a str| {
                MalAtom::Keyword(i)
            }),
        ),
    )(input)
}

fn parse_bool<'a>(input: &'a str) -> ParseResult<'a> {
    context("bool", alt((parse_true, parse_false)))(input)
}

fn parse_string<'a>(input: &'a str) -> ParseResult<'a> {
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
            |s| MalAtom::String(s.unwrap_or("".into())),
        ),
    )(input)
}

fn parse_int<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "int",
        map(
            terminated(
                alt((
                    map_res(digit1, |i: &str| i.parse::<i64>().map(MalAtom::Int)),
                    map_res(preceded(tag("-"), digit1), |i: &str| {
                        i.parse::<i64>().map(|i| MalAtom::Int(-i))
                    }),
                )),
                peek(not(alpha1)),
            ),
            |o| o,
        ),
    )(input)
}

// captures whitespace & commans (which count as whitespace in mal)
fn capture_whitespace<'a>(input: &'a str) -> IResult<&'a str, (), VerboseError<&'a str>> {
    context(
        "whitespace",
        map(many0(alt((space1, tag(","), line_ending))), |_| ()),
    )(input)
}

// captures comment
fn capture_comment<'a>(input: &'a str) -> IResult<&'a str, (), VerboseError<&'a str>> {
    context("comment", map(preceded(tag(";"), not_line_ending), |_| ()))(input)
}

fn parse_special<'a>(input: &'a str) -> ParseResult<'a> {
    context(
        "special",
        alt((
            map(preceded(tag("~@"), parse_mal_atom), |i| {
                MalAtom::SExp(vec![MalAtom::Symbol("splice-unquote"), i])
            }),
            map(preceded(char('\''), parse_mal_atom), |i| {
                MalAtom::SExp(vec![MalAtom::Symbol("quote"), i])
            }),
            map(preceded(char('`'), parse_mal_atom), |i| {
                MalAtom::SExp(vec![MalAtom::Symbol("quasiquote"), i])
            }),
            map(preceded(char('~'), parse_mal_atom), |i| {
                MalAtom::SExp(vec![MalAtom::Symbol("unquote"), i])
            }),
            map(
                preceded(char('^'), pair(parse_mal_atom, parse_mal_atom)),
                |i| MalAtom::SExp(vec![MalAtom::Symbol("with-meta"), i.1, i.0]),
            ),
            map(preceded(char('@'), parse_mal_atom), |i| {
                MalAtom::SExp(vec![MalAtom::Symbol("deref"), i])
            }),
        )),
    )(input)
}

fn parse_sexp<'a>(input: &'a str) -> ParseResult<'a> {
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
            |s| MalAtom::SExp(s),
        ),
    )(input)
}

fn parse_vector<'a>(input: &'a str) -> ParseResult<'a> {
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
            |v| MalAtom::Vector(v),
        ),
    )(input)
}

fn parse_hash_map<'a>(input: &'a str) -> ParseResult<'a> {
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
                        (MalAtom::Keyword(k), a) => {
                            hm.insert(k.into(), a);
                        }
                        (MalAtom::String(s), a) => {
                            hm.insert(s, a);
                        }
                        _ => unreachable!(),
                    }
                }

                MalAtom::HashMap(hm)
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
            Ok((" world", MalAtom::Symbol("hello"))),
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
    fn parse_special_ok() {
        assert_eq!(Ok(("", MalAtom::Special("~@"))), parse_special("~@"));
        assert_eq!(Ok(("", MalAtom::Special("'"))), parse_special("'"));
        assert_eq!(Ok(("", MalAtom::Special("`"))), parse_special("`"));
        assert_eq!(Ok(("", MalAtom::Special("~"))), parse_special("~"));
        assert_eq!(Ok(("", MalAtom::Special("^"))), parse_special("^"));
        assert_eq!(Ok(("", MalAtom::Special("@"))), parse_special("@"));
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
        assert_eq!(Ok(("", MalAtom::Int(0))), parse_int("0"));
        assert_eq!(Ok(("", MalAtom::Int(1304))), parse_int("1304"));

        assert_eq!(Ok(("", MalAtom::Int(-290))), parse_int("-290"));
    }

    #[test]
    fn parse_int_err() {
        assert!(parse_int("ab13").is_err());
        assert!(parse_int("124a").is_err());
    }

    #[test]
    fn parse_nil_ok() {
        assert_eq!(Ok(("", MalAtom::Nil)), parse_nil("nil"));
        assert_eq!(Ok((" ", MalAtom::Nil)), parse_nil("nil "));
        assert_eq!(Ok((",hey", MalAtom::Nil)), parse_nil("nil,hey"));
    }

    #[test]
    fn parse_nil_err() {
        assert!(parse_nil("nila").is_err());
        assert!(parse_nil("nil1").is_err());
        assert!(parse_nil("notnil").is_err());
    }

    #[test]
    fn parse_true_ok() {
        assert_eq!(Ok(("", MalAtom::Bool(true))), parse_true("true"));
        assert_eq!(Ok((" ", MalAtom::Bool(true))), parse_true("true "));
        assert_eq!(Ok((",hey", MalAtom::Bool(true))), parse_true("true,hey"));
    }

    #[test]
    fn parse_true_err() {
        assert!(parse_true("truea").is_err());
        assert!(parse_true("true1").is_err());
        assert!(parse_true("nottrue").is_err());
    }

    #[test]
    fn parse_false_ok() {
        assert_eq!(Ok(("", MalAtom::Bool(false))), parse_false("false"));
        assert_eq!(Ok((" ", MalAtom::Bool(false))), parse_false("false "));
        assert_eq!(Ok((",hey", MalAtom::Bool(false))), parse_false("false,hey"));
    }

    #[test]
    fn parse_false_err() {
        assert!(parse_false("falsea").is_err());
        assert!(parse_false("false1").is_err());
        assert!(parse_false("notfalse").is_err());
    }

    #[test]
    fn parse_book_ok() {
        assert_eq!(Ok(("", MalAtom::Bool(false))), parse_bool("false"));
        assert_eq!(Ok(("", MalAtom::Bool(true))), parse_bool("true"));
    }

    #[test]
    fn parse_string_ok() {
        assert_eq!(Ok(("", MalAtom::String("".into()))), parse_string("\"\""));
        assert_eq!(Ok(("a", MalAtom::String("".into()))), parse_string("\"\"a"));
        assert_eq!(
            Ok(("", MalAtom::String("h3llo, World!".into()))),
            parse_string("\"h3llo, World!\"")
        );

        assert_eq!(
            Ok(("", MalAtom::String("\"".into()))),
            parse_string(r#""\"""#)
        );

        assert_eq!(
            Ok(("", MalAtom::String("\n".into()))),
            parse_string(r#""\n""#)
        );

        assert_eq!(
            Ok(("", MalAtom::String("\\".into()))),
            parse_string(r#""\\""#)
        );
    }
}
