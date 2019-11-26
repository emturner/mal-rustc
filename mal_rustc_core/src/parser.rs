use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::*,
    combinator::*,
    error::{context, VerboseError},
    multi::*,
    sequence::*,
    IResult,
};

use std::iter::FromIterator;

use crate::types::MalAtom;

type ParseResult<'a> = IResult<&'a str, MalAtom<'a>, VerboseError<&'a str>>;

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
                tag("\""),
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
    context("whitespace", map(many0(alt((space1, tag(",")))), |_| ()))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capture_whitespace_ok() {
        assert_eq!(Ok(("", ())), capture_whitespace(""));
        assert_eq!(Ok(("", ())), capture_whitespace(" "));
        assert_eq!(Ok(("", ())), capture_whitespace(","));
        assert_eq!(Ok(("", ())), capture_whitespace("  \t  ,,,,  \t\t  "));

        assert_eq!(Ok(("a", ())), capture_whitespace("\t , \ta"));
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
