use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::*,
    combinator::*,
    error::{context, VerboseError},
    multi::many0,
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

#[cfg(test)]
mod tests {
    use super::*;

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
