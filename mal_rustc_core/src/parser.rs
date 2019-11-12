use nom::{
    bytes::complete::tag,
    character::complete::*,
    combinator::*,
    error::{context, VerboseError},
    sequence::*,
    IResult,
};

use crate::types::MalAtom;

fn parse_nil<'a>(input: &'a str) -> IResult<&'a str, MalAtom, VerboseError<&'a str>> {
    context(
        "nil",
        map(terminated(tag("nil"), peek(not(alphanumeric1))), |_| {
            MalAtom::Nil
        }),
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
}
