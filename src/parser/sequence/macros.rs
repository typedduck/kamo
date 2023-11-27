/// Macro to check if the sequence is infinite. If it is, it will return an
/// error. The check fails if the parser does not consume any input.
///
/// # Examples
///
/// Note that the macro will return an error if the parser does not consume any
/// input. But it will not return an end of file error, even if the parser is
/// at the end of the input. In order to check for end of file, the parser
/// must do actual progress.
/// 
/// ```rust
/// # use drake::{
/// #     infinite_loop_check,
/// #     parser::{
/// #         prelude::*, SequenceError, code, Input, Position, Span
/// #    }
/// # };
/// fn parser<'a>(input: Input<'a>) -> ParseResult<'a, usize> {
///     let mut acc = 0;
///     let mut cursor = input;
///     let mut element = ascii::alpha0;
///     
///     while let Ok((_, next)) = element.parse(cursor) {
///         infinite_loop_check!(cursor, next);
///         acc += 1;
///         cursor = next;
///     }
///     Ok((acc, cursor))
/// }
/// 
/// assert_eq!(parser(Input::new("abc1")), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_INFINITE,
///     SequenceError::Infinite
/// )));
/// assert_eq!(parser(Input::new("abc")), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_INFINITE,
///     SequenceError::Infinite
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_INFINITE,
///     SequenceError::Infinite
/// )));
/// ```
#[macro_export]
macro_rules! infinite_loop_check {
    ($start:expr, $next:expr) => {
        // if $start.is_eof() {
        //     return Err($crate::parser::ParseError::eof($next.position()));
        // }
        if $start.len() == $next.len() {
            return Err($crate::parser::ParseError::new(
                $next,
                $crate::parser::code::ERR_INFINITE,
                $crate::parser::SequenceError::Infinite,
            ));
        }
    };
}
