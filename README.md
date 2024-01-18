# Kamo

![Build Status](https://img.shields.io/github/actions/workflow/status/typedduck/kamo/rust.yml)
[![Crates.io](https://img.shields.io/crates/v/kamo)](https://crates.io/crates/kamo)
[![Crates.io](https://img.shields.io/crates/d/kamo)](https://crates.io/crates/kamo)

Kamo (カモ) is japanese for duck.

* This is a supplement to the second part of the Kamo project: memory management
and runtime values. It adds procedural macros to parse s-expressions into
runtime values. The macros are defined in the crate
[`kamo-macros`](https://crates.io/crates/kamo-macros).

* This is the release of the second part of the Kamo project: memory management
and runtime values.

* The first part was the parser combinator library.

## Memory Management Module `kamo::mem`

This module implements automatic memory management. Values are allocated in a
mutator which holds an arena allocator for each type. Memory collection is done
by a mark and sweep garbage collector.

The mutator implementation is thread local. The mutator holds a vector of arena
allocators for each type. The arena allocator is implemented as a vector of
buckets. Each bucket holds a vector of slots. The slots are used to store the
values. Buckets have a fixed size and are allocated on the heap. The buckets are
allocated on demand when the slots are full.

The hierarchy of types is as follows:

* Mutator contains Arenas.
* Arena contains Buckets.
* Bucket contains Slots.
* Slot contains a value.
* a value is represented by a Pointer.

Values managed by the mutator must be traceable. The mutator uses the
`Trace` trait to trace the values. The `Trace` trait is implemented for all
types that can contain values managed by the mutator.

Pointers use reference counting to keep track of the number of references to
the value. When the number of references reaches zero the value may be subject
to garbage collection. To avoid cycles `Pair`s, also known as `Cons` cells, and
`Vector`s do not hold locks to their elements. Instead they are traced by the
mutator when memory collection is done. This is the reason why a drop of the
reference count of a pointer to zero does not immediately free the value.

The garbage collector is implemented as a mark and sweep collector. All values
which hold a lock are roots. The garbage collector traces all roots and marks
all values that are reachable from the roots. All values that are not marked
are unreachable and are freed. Tracing is done iteratively by repeatedly
calling the `trace` method on the values and adding the values to a work list.
This continues until the work list is empty.

### `std::alloc::Allocator` Trait

The mutator uses the global allocator to allocate memory for buckets, arenas,
vectors and strings etc. Dynamic memory allocation is done by the allocator
provided by the `std::alloc` crate. Since the implementation of the allocator
provided by the standard library is highly optimized it would be a waste of
effort to reimplement it.

As a consequence the mutator is not able to free memory when the global
allocator fails to allocate memory. This is because the mutator does not know
which memory is allocated by the global allocator. The mutator only knows which
memory is allocated by its arenas. Collection of memory is triggered only by the
allocation pressure. 

When the allocator trait is stabilized the mutator will be able to use the
the trait to mitigate allocation failures by freeing memory when the global
allocator fails.

## Runtime Values Module `kamo::value`

This module implements runtime values. Values can either hold immediate values
or pointers to values allocated in the mutator.

The following immediate values are supported:

* `nil`, the empty list
* `bool`
* `char`, a Unicode scalar value
* `i64`
* `f64`

The following pointer values are supported:

* `pair`, a pair of values, also known as `Cons` cell
* `symbol`, a symbol, an interned string
* `string`, a UTF-8 string
* `vector`, a vector of values
* `bytevector`, a vector of bytes

Value provides a safe interface to the values in the runtime. It is implemented
as a wrapper around `ValueKind`. It provides methods to convert the value into a
Rust type, to check the type of the value, and to visit the value. Heap values
are automatically locked and unlocked when necessary.

Value implements the visitor pattern. The `Visitor` trait is used to visit the
value. When implementing the `Visitor` trait it must be considered that the
value may be cyclic. The visitor must keep track of the values it has already
visited to avoid infinite loops.

The `Visitor` trait is used to implement printing of values. The `Value` itself
does not implement printing. Instead the `Display` trait is implemented for
a specific vistor. This makes it possible to implement different printing
strategies for values.

An implementation of the `Visitor` trait is provided to print values in a
Scheme-like syntax.

## Parser Combinator Library `kamo::parser`

This module implements a parser combinator library. The library is focused
on parsing UTF-8 text in a safe and mostly zero-copy way. It is designed to
be used to implement parsers for programming languages. It is not designed
to be used to parse binary data.

One design goal of this library is to make it easy to write parsers that keep
track of the position of the input and the cause of the error. The position is
tracked automatically by its offset in bytes from the begining of the input and
the line and column number in UTF-8 characters. The cause of the error is
tracked by a stack of causes in the error. The stack is used to keep track of
multiple causes of one error. The cause is added to the stack when a parser
fails and holds the position, error code and a message. The error code is used
to identify the cause of the error. Typically parsers wich take other parsers as
input will add a cause to the stack when the input parser fails.

### Example

Example of a parser that parses a Scheme-like byte-vector and returns it as
`Vec<u8>`.

The grammar is defined as follows:

```text
ByteVector = "#u8(" Byte* ')'
Byte       = <any exact integer between 0 and 255>
```

The parser is defined as follows:

```rust
use kamo::parser::{code, prelude::*, Input, ParseError, ParseResult, Span};

fn main() {
    assert_eq!(
        parse_bytevec(Input::new("#u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)")),
        Ok((
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
            Input::new("")
        ))
    );
}

fn parse_bytevec(input: Input<'_>) -> ParseResult<'_, Vec<u8>> {
    context_as(
        delimited(
            pair(tag("#u8("), ascii::whitespace0),
            list0(parse_bytevec_value, ascii::whitespace1),
            context_as(
                preceded(ascii::whitespace0, char(')')),
                code::ERR_CUSTOM + 2,
                "expecting a closing parenthesis: )",
            ),
        ),
        code::ERR_CUSTOM + 1,
        "expecting a byte vector: #u8(<byte>*)",
    )(input)
}

fn parse_bytevec_value(input: Input<'_>) -> ParseResult<'_, u8> {
    let result = preceded(
        ascii::whitespace0,
        any((
            parse_binary_natural,
            parse_octal_natural,
            parse_decimal_natural,
            parse_hexadecimal_natural,
            parse_natural,
        )),
    )(input);

    match result {
        Ok((value, cursor)) => {
            if value > u8::MAX as u64 {
                Err(ParseError::new(
                    Span::new(input.position(), cursor.position()),
                    code::ERR_CUSTOM + 3,
                    "expecting a byte value: 0..255",
                ))
            } else {
                Ok((value as u8, cursor))
            }
        }
        Err(err) => Err(err),
    }
}

#[inline]
fn parse_binary_natural(input: Input<'_>) -> ParseResult<u64> {
    preceded(tag("#b"), literal::natural(literal::Radix::Binary))(input)
}

#[inline]
fn parse_octal_natural(input: Input<'_>) -> ParseResult<u64> {
    preceded(tag("#o"), literal::natural(literal::Radix::Octal))(input)
}

#[inline]
fn parse_decimal_natural(input: Input<'_>) -> ParseResult<u64> {
    preceded(tag("#d"), parse_natural)(input)
}

#[inline]
fn parse_hexadecimal_natural(input: Input<'_>) -> ParseResult<u64> {
    preceded(tag("#x"), literal::natural(literal::Radix::Hexadecimal))(input)
}

#[inline]
fn parse_natural(input: Input<'_>) -> ParseResult<u64> {
    literal::natural(literal::Radix::Decimal)(input)
}
```

## Macros

The macros are used to parse a string literal into a single or an array of
`kamo::value::Value`s. The macros are defined as follows:

- `sexpr!(<mutator>, <expression>)` - A macro for parsing a single
  s-expression from a string. It returns a `kamo::value::Value`.
- `sexpr_file!(<mutator>, <filename>)` - A macro for parsing multiple
  s-expressions from a file. It returns an array of `kamo::value::Value`s.
  The array may be empty.
 
- `sexpr_script!(<mutator>, <expressions>)` - A macro for parsing multiple
  s-expressions from a string. It returns an array of `kamo::value::Value`s.
  The array may be empty.
 
The macros all take an optional `MutatorRef` identifier. This is used to allocate
values on the heap. If the expression does not contain any values that need to
be allocated on the heap, then the `Mutator` identifier can be omitted.
 
The syntax for the macros is as defined by the Scheme standard R7RS for the
`read` procedure. The syntactic definition is the `<datum>` in section
["7.1.2 External representations"](https://standards.scheme.org/official/r7rs.pdf)
of the standard.
 
The syntax deviations from the standard are:
 
- The extactness (`#e` and `#i`) of numbers is not supported. Floating-point
  numbers are always inexact and integers are always exact.
 
- Numbers may only be signed 64-bit integers or IEEE 754 double precision
  floating-point numbers. The standard allows for arbitrary precision
  integers, rationals and complex numbers.
 
- Labels are not supported.
 
- The `#;` comment syntax is only supported in the macros which parse
  multiple s-expressions. The `#;` comment syntax may not be nested.
 
- Character literals defined by a hex escape sequence may have 1 to 6
  digits. The standard excepts 1 or more digits. The code must be a valid
  Unicode code point.
 
The parser is implemented with the [`pest`](https://crates.io/crates/pest)
crate. The grammar is defined in `src/sexpr/sexpr.pest`. This is necessary
because the combination parser library defined in `kamo::parser` cannot be
used here. It would be cyclic dependency. There will be an implementation
of a parser for s-expressions in the `kamo::form` module in the future. It
will be based on the `kamo::parser` crate and will be used by the scheme
interpreter.
 
## Examples
 
```rust
use kamo::{mem::Mutator, sexpr, value::{print, Value}};
 
let m = Mutator::new_ref();
let value = sexpr!(m, "(1 2 3)");
 
assert_eq!(print(value).to_string(), "(1 2 3)");
```
 
```rust
use kamo::{sexpr_file, value::{print, Value}};
 
let m = Mutator::new_ref();
let values = sexpr_file!("tests/sexpr/values.scm");
 
assert_eq!(values.len(), 3);
assert_eq!(print(values[0].clone()).to_string(), "()");
assert_eq!(print(values[1].clone()).to_string(), "100");
assert_eq!(print(values[2].clone()).to_string(), "#t");

let values: &[Value] = &sexpr_file!("tests/sexpr/empty.scm");
assert_eq!(values.len(), 0);
```
 
```rust
use kamo::{mem::Mutator, sexpr_script, value::{print, Value}};
 
let m = Mutator::new_ref();
let values = sexpr_script!(m, "(define a 1)\n(define b 2)\n(+ a b)");
 
assert_eq!(values.len(), 3);
assert_eq!(print(values[0].clone()).to_string(), "(define a 1)");
assert_eq!(print(values[1].clone()).to_string(), "(define b 2)");
assert_eq!(print(values[2].clone()).to_string(), "(+ a b)");

let values: &[Value] = &sexpr_script!("");
 
assert_eq!(values.len(), 0);
```

## Feature List

- [x] Module `kamo::parser` for parsing UTF-8 text. A parser combinator
    library for parsing UTF-8 text in a safe and mostly zero-copy way.
- [x] Module `kamo::mem` for automatic memory management. Values are allocated
    in a mutator which holds an arena allocator for each type. Memory collection
    is done by a mark and sweep garbage collector.
- [x] Module `kamo::value` for values. Values can either hold immediate values
    or pointers to values allocated in the mutator.
- [ ] Module `kamo::eval` for evaluation. The evaluator processes an AST, which
    is an symbolic expression tree, and evaluates it to an intermediate
    representation. The intermediate representation can then be interpreted or
    compiled to a target representation. The interpreter is generic and can be
    used to interpret any intermediate representation.
- [ ] Module `kamo::types` for types. The type system is used to infer the
    types of the intermediate representation and the AST.
- [ ] Module `kamo::repl` for a read-eval-print-loop. The REPL is used to
    interactively evaluate expressions and is generic and can be used to
    evaluate any intermediate representation.
- [ ] Module `kamo::lang::scheme` for the Scheme language. The Scheme language
    is implemented as a library on top of the `kamo` modules. It implements a
    subset of the R7RS standard.

