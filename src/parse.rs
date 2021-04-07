use nom::{error::ParseError, IResult, Parser};

// Gets an object from the first parser,
// then matches an object from the second parser and does not discard it.
pub fn terminated_spare<I, O1, O2, E: ParseError<I>, F, G>(
    mut first: F,
    mut second: G,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
    I: Clone,
{
    move |input: I| {
        let (input, o1) = first.parse(input)?;
        second.parse(input.clone()).map(|(_, _)| (input, o1))
    }
}
