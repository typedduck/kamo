use crate::{
    env::{Environmental, ScopeGuard},
    mem::Pointer,
    parser::{Code, Input},
    types::{parser::text, Type},
    value::{self, Next, Value, ValueKind},
};

use super::{Parameters, Result, TypeCheckError};

/// This trait is used to implement a type checker for a specific language.
///
/// The type checker is used to check the types of values in an AST. It is
/// implemented for a specific language and is used to check the types of
/// values in the AST of that language.
///
/// The main entry point of the type checker is the
/// [`TypeChecker::check_expression()`] method. It is used to check the type of
/// a single value in the AST. The method is called recursively to check the
/// types of all values in the AST.
///
/// For a concrete implementation of a type checker for a specific language,
/// the method [`TypeChecker::check_invocation()`] must be implemented. It is
/// used to check the type of an invocation in the AST. An invocation has the
/// form `(<operator> <operand> ...)`. The operator must be a symbol. The
/// operands are not checked, but passed as-is. The type of the operands should
/// be determined by calling [`TypeChecker::check_expression()`] recursively or
/// according to the implementation of the operator, if it is for example a
/// special form.
///
/// For a full implementation of a type checker for an imaginary language, see
/// the tests in this module. The type checker is called `TestChecker` and is
/// located in the `tests` module.
pub trait TypeChecker<'a, const ECO: Code>: Environmental<'a> {
    /// Checks the type of the given invocation.
    ///
    /// An invocation has the form `(<operator> <operand> ...)`. The operator
    /// must be a symbol. The operands are not checked, but passed as-is. The
    /// type of the operands should be determined by calling
    /// [`TypeChecker::check_expression()`] recursively or according to the
    /// implementation of the operator, if it is for example a special form.
    ///
    /// Returns the type of the invocation.
    ///
    /// Since everything in s-expressions is definied in terms of invocations,
    /// this method does the heavy lifting of type checking. It is the main
    /// entry point of type checking for a specific AST of a language. It must
    /// be implemented by the concrete type checker.
    ///
    /// In Scheme for example, the type checker would check against the
    /// syntax-rules of the special form and then check the types of the
    /// operands against the types of the pattern variables.
    ///
    /// An example implementation of a concrete type checker for an imaginary
    /// language can be found in the tests of this module. It is called
    /// `TestChecker` and is located in the `tests` module of this module.
    ///
    /// # Errors
    ///
    /// If the operator is not a symbol or is not defined in the current
    /// environment or is unknown, then an error is returned. If the operator is
    /// a known operator, but the operands do not match the expected types, then
    /// an error is returned.
    fn check_invocation(&mut self, operator: Value<'a>, operands: &[Value<'a>]) -> Result<Type>;

    /// Returns the type of the given expression.
    ///
    /// This is the main entry point of the type checker. It is used to check
    /// the type of a single expression.
    ///
    /// The expression is either a single value or a list of values. If it is a
    /// list, then it must have the form `(<operator> <operand> ...)` and is
    /// passed to [`TypeChecker::check_invocation()`] for further processing.
    /// The operands are not checked, but passed as-is. Checking of the operands
    /// is deferred to [`TypeChecker::check_invocation()`].
    ///
    /// Symbols are resolved to their values before being passed recursively to
    /// [`TypeChecker::check_expression()`].
    ///
    /// # Errors
    ///
    /// If the expression is not a value or an invalid invocation, then an error
    /// is returned. If the expression is a symbol, but is not defined in the
    /// current environment, then an error is returned.
    fn check_expression(&mut self, value: Value<'a>) -> Result<Type> {
        match value.kind() {
            ValueKind::Nil => Ok(Type::nil()),
            ValueKind::Bool(_) => Ok(Type::boolean()),
            ValueKind::Char(_) => Ok(Type::character()),
            ValueKind::Integer(_) => Ok(Type::integer()),
            ValueKind::Float(_) => Ok(Type::float()),
            ValueKind::Pair(_, pair) => {
                let pair = Pointer::from(*pair);
                let operator = pair.car().to_owned();
                let mut operands = vec![];
                let mut pair = pair.next();

                loop {
                    match pair {
                        Next::Pair(cursor) => {
                            operands.push(cursor.car().to_owned());
                            pair = cursor.next();
                        }
                        Next::Nil => {
                            return self.check_invocation(operator, &operands);
                        }
                        Next::Dot(_) => {
                            return Err(TypeCheckError::InproperList);
                        }
                    }
                }
            }
            ValueKind::String(_, _) => Ok(Type::string()),
            ValueKind::Symbol(_, _) => self.resolve_symbol_type(value),
            ValueKind::Bytevec(_, _) => Ok(Type::binary()),
            ValueKind::Vector(_, _) => Ok(Type::vector()),
            ValueKind::Type(_, _) => Ok(Type::typedef()),
        }
    }

    /// Checks the type of the given declaration.
    ///
    /// The declaration has a declared type, which will be associated with the
    /// given name in the current environment and scope. The value is optional.
    /// If it is given, then it is checked against the declared type.
    ///
    /// Returns the declared type.
    ///
    /// # Note
    ///
    /// This method has the side effect of inserting the declaration into the
    /// current scope of the environment. It may shadow or overwrite existing
    /// declarations.
    ///
    /// If successful, the value is bound to the given name in the current
    /// environment and scope.
    ///
    /// # Errors
    ///
    /// If the value is given and is not a valid expression or the type does not
    /// match the declared type, then an error is returned.
    fn check_declaration(
        &mut self,
        name: impl AsRef<str>,
        declared: Type,
        value: Option<Value<'a>>,
    ) -> Result<Type> {
        if let Some(ref value) = value {
            let inferred = self.check_expression(value.to_owned())?;
            self.expect_type(&inferred, &declared, value)?;
        }

        self.env()
            .borrow_mut()
            .define(name.as_ref(), declared.clone(), value);

        Ok(declared)
    }

    /// Checks the type of the given operand.
    ///
    /// The operand is checked against the given list of allowed types. If the
    /// operand is not of any of the allowed types, then an error is returned.
    /// Otherwise, the type of the operand is returned. The type of the operand
    /// is determined by calling [`TypeChecker::check_expression()`]
    /// recursively.
    ///
    /// # Errors
    ///
    /// If the operand is not a valid expression or the type of the operand does
    /// not match any of the allowed types, then an error is returned.
    fn check_operand(
        &mut self,
        operator: &Pointer<'a, Box<str>>,
        operand: Value<'a>,
        allowed: &[Type],
    ) -> Result<Type> {
        let ty = self.check_expression(operand)?;

        if allowed.contains(&ty) {
            Ok(ty)
        } else {
            Err(TypeCheckError::UnexpectedOperatorType(
                operator.as_ref().to_owned(),
                ty,
                allowed.to_vec(),
            ))
        }
    }

    /// Checks the type of the given assignment. If successful, returns the type
    /// of the value. No actual assignment is performed.
    ///
    /// # Errors
    ///
    /// If the binding is not defined in the current environment or the type of
    /// the value does not match the declared type of the binding, then an
    /// error is returned.
    fn check_assignment(&mut self, name: impl AsRef<str>, value: Value<'a>) -> Result<Type> {
        let binding = self
            .env()
            .borrow()
            .lookup(name.as_ref())
            .ok_or_else(|| TypeCheckError::UndefinedVariable(name.as_ref().to_owned()))?;
        let declared = binding.typedef().clone();
        let actual = self.check_expression(value.clone())?;

        self.expect_type(&actual, &declared, &value)?;
        Ok(actual)
    }

    /// Checks the type of the given block. The expressions in the block are
    /// checked in a new scope. Returns the type of the last expression in the
    /// block. If the block is empty, then the type is `void`.
    ///
    /// Calls [`TypeChecker::check_sequence()`] to check the expressions in the
    /// block.
    ///
    /// Variables declared in the block will not persist after the block has
    /// been checked.
    ///
    /// # Errors
    ///
    /// If any of the expressions in the block are not valid, then an error is
    /// returned.
    fn check_block(&mut self, exprs: &[Value<'a>]) -> Result<Type> {
        let _guard = ScopeGuard::new(self.env().clone());
        self.check_sequence(exprs)
    }

    /// Checks the type of the given sequence. Returns the type of the last
    /// expression in the sequence. If the sequence is empty, then the type is
    /// `void`. The expressions in the sequence are checked in the current
    /// scope.
    ///
    /// # Note
    ///
    /// As a side effect, binding declarations in the sequence are inserted
    /// into the current scope. They may shadow or overwrite existing
    /// declarations. Declarations in the global scope will persist after the
    /// sequence has been checked.
    ///
    /// # Errors
    ///
    /// If any of the expressions in the sequence are not valid, then an error
    /// is returned.
    fn check_sequence(&mut self, exprs: &[Value<'a>]) -> Result<Type> {
        let mut ty = Type::void();

        for expr in exprs {
            ty = self.check_expression(expr.to_owned())?;
        }

        Ok(ty)
    }

    /// Checks the type of the given lambda declaration. The parameters and body
    /// are checked in a new scope. Returns the type of the lambda.
    ///
    /// Calls [`TypeChecker::check_parameter_list()`] to check the
    /// parameters of the lambda. Calls [`TypeChecker::check_expression()`] to
    /// check the body of the lambda. Calls
    /// [`TypeChecker::expect_return_type()`] to check the return type.
    ///
    /// Variables declared in the lambda will not persist after the lambda has
    /// been checked.
    ///
    /// # Errors
    ///
    /// If the parameters or the body of the lambda are not valid, then an error
    /// is returned. If the type of the body does not match the return type an
    /// error is returned.
    fn check_lambda(
        &mut self,
        params: Parameters<'a>,
        rettype: Type,
        body: Value<'a>,
    ) -> Result<Type> {
        // Set up the scope for the function body with the parameters
        let _guard = params.define(&self.env());
        let body = self.check_expression(body.clone())?;

        self.expect_return_type(&body, &rettype, "lambda")?;
        Ok(Type::from((&params, &rettype)))
    }

    /// Checks the type of the given parameter list. Returns the parameters of
    /// the function as a [`Parameters`] struct.
    ///
    /// The paramaters are not inserted into the current scope. They are only
    /// checked for correctness. To add the parameters to a new scope, use
    /// [`Parameters::define()`].
    ///
    /// Expects the parameters to be a list of parameter declarations. The list
    /// may be empty. Each parameter declaration is processed by
    /// [`TypeChecker::check_parameter()`].
    ///
    /// # Errors
    ///
    /// If the parameter declarations are not valid, then an error is returned.
    fn check_parameter_list(&self, params: Value<'a>) -> Result<Parameters<'a>> {
        let mut cursor = match params.kind() {
            ValueKind::Pair(_, pair) => Pointer::from(*pair),
            ValueKind::Nil => return Ok(Parameters::default()),
            _ => return Err(TypeCheckError::MalformedParameterList),
        };
        let mut params = vec![];
        let mut varg = None;

        loop {
            let param = cursor.car().to_owned();
            let (name, ty) = self.check_parameter(param)?;

            params.push((name, ty));
            match cursor.next() {
                Next::Pair(param) => cursor = param,
                Next::Nil => break,
                Next::Dot(param) => {
                    let (name, ty) = self.check_parameter(param)?;

                    varg = Some((name, ty));
                    break;
                }
            };
        }

        Ok(Parameters {
            fixed: params,
            variadic: varg,
        })
    }

    /// Checks the type of the given parameter. Returns the name and type of the
    /// parameter. The parameter must have the form `(<symbol> <type>)` or
    /// `(<symbol> . <type>)`.
    ///
    /// The name of the parameter must be a symbol. The type of the parameter
    /// may be a symbol or a type declaration. If the type is a symbol, then it
    /// is resolved to a type by calling [`TypeChecker::resolve_type()`].
    ///
    /// # Errors
    ///
    /// If the parameter is not a valid parameter declaration, then an error is
    /// returned.
    fn check_parameter(&self, param: Value<'a>) -> Result<(Pointer<'a, Box<str>>, Type)> {
        let param = param
            .as_pair_ptr()
            .ok_or_else(|| TypeCheckError::MalformedParameter)?;
        let name = param
            .car()
            .as_symbol_ptr()
            .ok_or_else(|| TypeCheckError::MalformedParameter)?;
        let ty = match param.next() {
            Next::Pair(cursor) => {
                if cursor.next() != Next::Nil {
                    return Err(TypeCheckError::MalformedParameter);
                }
                self.resolve_type(cursor.car().to_owned())?
            }
            Next::Dot(value) => self.resolve_type(value.clone())?,
            Next::Nil => {
                return Err(TypeCheckError::MalformedParameter);
            }
        };

        Ok((name, ty))
    }

    /// Resolves the given symbol in the current environment, moving up the
    /// scope chain if necessary. Returns the type of the associated binding.
    ///
    /// # Errors
    ///
    /// If the symbol is not a valid symbol or is not defined in the current
    /// environment, then an error is returned.
    fn resolve_symbol_type(&self, value: Value<'a>) -> Result<Type> {
        let name = value.as_symbol_ptr().ok_or(TypeCheckError::NotASymbol)?;
        let binding = self
            .env()
            .borrow()
            .lookup(name.as_ref())
            .ok_or_else(|| TypeCheckError::UndefinedVariable(name.as_ref().to_owned()))?;

        Ok(binding.typedef().to_owned())
    }

    /// Resolves the given symbol to a type by parsing its string value. If this
    /// fails recursively looks up the symbol in the current environment and
    /// resolves its type.
    ///
    /// Calls [`TypeChecker::parse_type()`] to parse the string value into a
    /// type.
    ///
    /// # Errors
    ///
    /// If the symbol is not a valid symbol, is not defined in the current
    /// environment or the string value cannot be parsed into a type, then an
    /// error is returned.
    fn resolve_type(&self, value: Value<'a>) -> Result<Type> {
        let value = value.as_symbol_ptr().ok_or(TypeCheckError::NotASymbol)?;
        let ty = self.parse_type(&*value);

        if ty.is_err() {
            let binding = self.env().borrow().lookup(value.as_ref());

            if let Some(binding) = binding {
                let ty = binding
                    .value()
                    .ok_or_else(|| TypeCheckError::UnboundVariable(value.as_ref().to_owned()))?;

                if let Some(ty) = ty.as_type_ptr() {
                    return Ok(ty.as_ref().to_owned());
                }
                return self.resolve_type(ty.to_owned());
            }
        }
        ty
    }

    /// Parses the given string value into a type. If the string value is not a
    /// valid type, then an error is returned. This method is used to resolve
    /// explicit type declarations in the form of symbols.
    ///
    /// It also resolves named types defined in the current environment.
    ///
    /// # Errors
    ///
    /// If the string value is not a valid type, then an error is returned.
    fn parse_type(&self, value: impl AsRef<str>) -> Result<Type> {
        let value = value.as_ref();
        let input = Input::new(value);
        let (ty, _) = text::parse::<ECO>(Some(self.env()))(input)?;

        Ok(ty)
    }

    /// Checks the actual type against the expected type.
    ///
    /// # Errors
    ///
    /// If the actual type does not match the expected type, then an error is
    /// returned.
    fn expect_type<'b>(
        &self,
        actual: &'b Type,
        expected: &Type,
        value: &Value<'a>,
    ) -> Result<&'b Type> {
        if actual == expected {
            Ok(actual)
        } else {
            let value = value::print(value.to_owned()).to_string();

            Err(TypeCheckError::ExpectedType(
                expected.to_owned(),
                actual.to_owned(),
                value,
            ))
        }
    }

    /// Checks the actual return type against the expected return type.
    ///
    /// # Errors
    ///
    /// If the actual return type does not match the expected return type, then
    /// an error is returned.
    fn expect_return_type<'b>(
        &self,
        actual: &'b Type,
        expected: &Type,
        name: &str,
    ) -> Result<&'b Type> {
        if actual == expected {
            Ok(actual)
        } else {
            Err(TypeCheckError::ExpectedReturnType(
                expected.to_owned(),
                actual.to_owned(),
                name.to_owned(),
            ))
        }
    }

    /// Checks the arity of the given operator.
    ///
    /// # Errors
    ///
    /// If the expected arity does not match the found arity, then an error is
    /// returned.
    fn expect_arity(&self, operator: &str, expected: usize, found: usize) -> Result<()> {
        if expected == found {
            Ok(())
        } else {
            Err(TypeCheckError::ArityMismatch(
                operator.to_owned(),
                expected,
                found,
            ))
        }
    }

    /// Checks the minimum arity of the given operator.
    ///
    /// # Errors
    ///
    /// If found arity is less than the expected minimum arity, then an error is
    /// returned.
    fn expect_min_arity(&self, operator: &str, expected: usize, found: usize) -> Result<()> {
        if expected <= found {
            Ok(())
        } else {
            Err(TypeCheckError::MinArityMismatch(
                operator.to_owned(),
                expected,
                found,
            ))
        }
    }

    /// Checks the maximum arity of the given operator.
    ///
    /// # Errors
    ///
    /// If the found arity exceeds the expected maximum arity, then an error is
    /// returned.
    fn expect_max_arity(&self, operator: &str, expected: usize, found: usize) -> Result<()> {
        if expected >= found {
            Ok(())
        } else {
            Err(TypeCheckError::MaxArityMismatch(
                operator.to_owned(),
                expected,
                found,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use kamo_macros::{sexpr, sexpr_file};

    use crate::{
        env::{Environment, EnvironmentRef},
        mem::{Mutator, Pointer},
    };

    use super::*;

    #[allow(unused)]
    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    pub enum Operand {
        Single,
        Multiple,
    }

    struct TestChecker<'a> {
        env: EnvironmentRef<'a>,
    }

    impl<'a> TestChecker<'a> {
        fn new() -> Self {
            let mut tc = Self {
                env: Environment::new_ref(Mutator::new_ref()),
            };

            tc.check_declaration("sum", "fn(int, int -> int)".parse().unwrap(), None)
                .unwrap();
            tc.check_declaration("square", "fn(int -> int)".parse().unwrap(), None)
                .unwrap();
            tc.check_declaration("pi", "fn(void -> float)".parse().unwrap(), None)
                .unwrap();
            tc
        }

        fn operator_types(oper: &str, arity: Operand) -> Result<&Vec<Type>> {
            let operator = OPERATORS
                .get(&(oper, arity))
                .ok_or_else(|| TypeCheckError::UnknownOperator(oper.to_owned()))?;

            Ok(operator)
        }

        fn invoke_call(
            &mut self,
            tag: &str,
            fntype: &Type,
            operands: &[Value<'a>],
        ) -> Result<Type> {
            let fntype = fntype
                .as_lambda()
                .ok_or_else(|| TypeCheckError::NotAFunction(fntype.clone()))?;
            let args = operands
                .iter()
                .map(|operand| self.check_expression(operand.to_owned()))
                .collect::<Result<Vec<_>>>()?;

            self.expect_arity(tag, fntype.params.len(), operands.len())?;

            for (i, (expected, actual)) in fntype.params.iter().zip(args.iter()).enumerate() {
                self.expect_type(actual, expected, &operands[i])?;
            }
            Ok(fntype.result)
        }
    }

    impl<'a> Environmental<'a> for TestChecker<'a> {
        #[inline]
        fn env(&self) -> EnvironmentRef<'a> {
            self.env.clone()
        }
    }

    impl<'a> TypeChecker<'a, 0> for TestChecker<'a> {
        #[allow(clippy::too_many_lines)]
        fn check_invocation(
            &mut self,
            operator: Value<'a>,
            operands: &[Value<'a>],
        ) -> Result<Type> {
            if let Some(tag) = operator.as_symbol_ptr() {
                match tag.as_ref() {
                    "+" | "-" | "*" | "/" => {
                        self.expect_min_arity(tag.as_ref(), 2, operands.len())?;

                        let allowed = Self::operator_types(tag.as_ref(), Operand::Multiple)?;
                        let opertypes = operands
                            .iter()
                            .map(|operand| self.check_operand(&tag, operand.to_owned(), allowed))
                            .collect::<Result<Vec<_>>>()?;
                        let mut iter = operands.iter().zip(opertypes.iter());
                        let (_, lhs_type) = iter.next().unwrap();

                        for (rhs, rhs_type) in iter {
                            self.expect_type(rhs_type, lhs_type, rhs)?;
                        }

                        Ok(lhs_type.to_owned())
                    }
                    "<" | "<=" | "=" | "!=" | ">=" | ">" => {
                        self.expect_min_arity(tag.as_ref(), 2, operands.len())?;

                        let opertypes = operands
                            .iter()
                            .map(|operand| self.check_expression(operand.to_owned()))
                            .collect::<Result<Vec<_>>>()?;
                        let mut iter = operands.iter().zip(opertypes.iter());
                        let (_, lhs_type) = iter.next().unwrap();

                        for (rhs, rhs_type) in iter {
                            self.expect_type(rhs_type, lhs_type, rhs)?;
                        }

                        Ok(Type::boolean())
                    }
                    "var" => {
                        self.expect_arity(tag.as_ref(), 2, operands.len())?;

                        let (name, declared) = match operands[0].kind() {
                            ValueKind::Symbol(_, symbol) => (Pointer::from(*symbol), None),
                            ValueKind::Pair(_, _) => self
                                .check_parameter(operands[0].clone())
                                .map(|(name, ty)| (name, Some(ty)))?,
                            _ => return Err(TypeCheckError::NotASymbol),
                        };
                        let value = operands[1].clone();
                        let declared = if let Some(declared) = declared {
                            declared
                        } else {
                            self.check_expression(value.clone())?
                        };

                        self.check_declaration(&*name, declared, Some(value))
                    }
                    "set" => {
                        self.expect_arity(tag.as_ref(), 2, operands.len())?;

                        let name = operands[0]
                            .as_symbol_ptr()
                            .ok_or(TypeCheckError::NotASymbol)?;
                        let value = operands[1].clone();

                        self.check_assignment(&*name, value)
                    }
                    "begin" => self.check_block(operands),
                    "if" => {
                        self.expect_min_arity(tag.as_ref(), 2, operands.len())?;
                        self.expect_max_arity(tag.as_ref(), 3, operands.len())?;

                        let opertypes = operands
                            .iter()
                            .map(|operand| self.check_expression(operand.to_owned()))
                            .collect::<Result<Vec<_>>>()?;
                        let mut iter = operands.iter().zip(opertypes.iter());
                        let (condition, condition_type) = iter.next().unwrap();
                        let (_, consequent_type) = iter.next().unwrap();

                        self.expect_type(condition_type, &Type::boolean(), condition)?;
                        if let Some((alternative, alternative_type)) = iter.next() {
                            self.expect_type(alternative_type, consequent_type, alternative)
                                .cloned()
                        } else if !consequent_type.is_filled() {
                            Ok(consequent_type.to_owned())
                        } else {
                            Ok(Type::option(consequent_type.to_owned()).expect("optional type"))
                        }
                    }
                    "while" => {
                        self.expect_min_arity(tag.as_ref(), 2, operands.len())?;

                        let condition = &operands[0];
                        let condition_type = self.check_expression(operands[0].clone())?;

                        self.expect_type(&condition_type, &Type::boolean(), condition)?;
                        self.check_block(&operands[1..])
                    }
                    "def" => {
                        self.expect_arity(tag.as_ref(), 5, operands.len())?;

                        let name = operands[0]
                            .as_symbol_ptr()
                            .ok_or(TypeCheckError::NotASymbol)?
                            .clone();
                        let params = self.check_parameter_list(operands[1].clone())?;
                        let rettag = operands[2]
                            .as_symbol_ptr()
                            .ok_or(TypeCheckError::NotASymbol)?;
                        let rettype = self.resolve_type(operands[3].clone())?;
                        let fntype = Type::from((&params, &rettype));
                        let body = operands[4].clone();

                        if rettag.as_ref() != "->" {
                            return Err(TypeCheckError::MalformedReturn);
                        }

                        // To allow recursive functions, we first declare the
                        // function. For the type checker we only need the
                        // the declared type.
                        self.check_declaration(&*name, fntype, None)?;
                        self.check_lambda(params, rettype, body)
                    }
                    "lambda" => {
                        self.expect_arity(tag.as_ref(), 4, operands.len())?;

                        let params = self.check_parameter_list(operands[0].clone())?;
                        let rettag = operands[1]
                            .as_symbol_ptr()
                            .ok_or(TypeCheckError::NotASymbol)?;
                        let rettype = self.resolve_type(operands[2].clone())?;
                        let body = operands[3].clone();

                        if rettag.as_ref() != "->" {
                            return Err(TypeCheckError::MalformedReturn);
                        }

                        self.check_lambda(params, rettype, body)
                    }
                    "type" => {
                        self.expect_arity(tag.as_ref(), 2, operands.len())?;

                        let name = operands[0]
                            .as_symbol_ptr()
                            .ok_or(TypeCheckError::NotASymbol)?
                            .clone();
                        let ty = self.resolve_type(operands[1].clone())?;
                        let ty = Value::new_type(self.env().borrow().mutator(), ty);

                        self.check_declaration(name.as_ref(), Type::typedef(), Some(ty))
                    }
                    _ => {
                        let fntype = self.resolve_symbol_type(operator.clone())?;
                        self.invoke_call(tag.as_ref(), &fntype, operands)
                    }
                }
            } else {
                let fntype = self.check_expression(operator)?;
                self.invoke_call("lambda", &fntype, operands)
            }
        }
    }

    lazy_static! {
        static ref OPERATORS: HashMap<(&'static str, Operand), Vec<Type>> = {
            let mut map = HashMap::new();
            map.insert(
                ("+", Operand::Multiple),
                vec![Type::integer(), Type::float(), Type::string()],
            );
            map.insert(
                ("-", Operand::Multiple),
                vec![Type::integer(), Type::float()],
            );
            map.insert(
                ("*", Operand::Multiple),
                vec![Type::integer(), Type::float()],
            );
            map.insert(
                ("/", Operand::Multiple),
                vec![Type::integer(), Type::float()],
            );
            map
        };
    }

    #[test]
    fn self_eval() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();

        let value = sexpr!(m, "#t");
        assert_eq!(tc.check_expression(value), Ok(Type::boolean()));

        let value = sexpr!(m, r#"#\a"#);
        assert_eq!(tc.check_expression(value), Ok(Type::character()));

        let value = sexpr!(m, r#"100"#);
        assert_eq!(tc.check_expression(value), Ok(Type::integer()));

        let value = sexpr!(m, r#".5"#);
        assert_eq!(tc.check_expression(value), Ok(Type::float()));

        let value = sexpr!(m, r#""Hello World!""#);
        assert_eq!(tc.check_expression(value), Ok(Type::string()));

        let value = sexpr!(m, r#"#u8(1 2 3)"#);
        assert_eq!(tc.check_expression(value), Ok(Type::binary()));
    }

    #[test]
    fn math_operators_success() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();

        let list = sexpr!(m, "(+ 1 2 3)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "(- 1 2)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "(* 1 2)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "(/ 1 2)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, r#"(+ "1" "2")"#);
        assert_eq!(tc.check_expression(list), Ok(Type::string()));
    }

    #[test]
    fn math_operators_failure() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();

        let list = sexpr!(m, r#"(+ 1 "2" 3)"#);
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::ExpectedType(
                Type::integer(),
                Type::string(),
                "\"2\"".to_owned()
            ))
        );

        let list = sexpr!(m, r#"(- "1" "2")"#);
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::UnexpectedOperatorType(
                "-".to_owned(),
                Type::string(),
                vec![Type::integer(), Type::float()]
            ))
        );

        let list = sexpr!(m, r#"(- 1 "2")"#);
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::UnexpectedOperatorType(
                "-".to_owned(),
                Type::string(),
                vec![Type::integer(), Type::float()]
            ))
        );

        let list = sexpr!(m, r#"(- "1" 2)"#);
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::UnexpectedOperatorType(
                "-".to_owned(),
                Type::string(),
                vec![Type::integer(), Type::float()]
            ))
        );
    }

    #[test]
    fn bindings_success() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();

        let list = sexpr!(m, "(var a 1)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "(var b .5)");
        assert_eq!(tc.check_expression(list), Ok(Type::float()));

        let list = sexpr!(m, "(var (c int) 100)");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "(var (d float) .5)");
        assert_eq!(tc.check_expression(list), Ok(Type::float()));

        let list = sexpr!(m, "a");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "b");
        assert_eq!(tc.check_expression(list), Ok(Type::float()));

        let list = sexpr!(m, "c");
        assert_eq!(tc.check_expression(list), Ok(Type::integer()));

        let list = sexpr!(m, "d");
        assert_eq!(tc.check_expression(list), Ok(Type::float()));
    }

    #[test]
    fn bindings_failure() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();

        let list = sexpr!(m, "(var (a int) .5)");
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::ExpectedType(
                Type::integer(),
                Type::float(),
                "0.5".to_owned()
            ))
        );

        let list = sexpr!(m, "(var (b float) 100)");
        assert_eq!(
            tc.check_expression(list),
            Err(TypeCheckError::ExpectedType(
                Type::float(),
                Type::integer(),
                "100".to_owned()
            ))
        );
    }

    lazy_static! {
        static ref BLOCKS: [Type; 8] = [
            Type::void(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
        ];
    }

    #[test]
    fn block_success() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/blocks-success.scm");

        assert_eq!(exprs.len(), BLOCKS.len());

        for (n, (expr, expected)) in exprs.iter().zip(BLOCKS.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "block expression {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref IFS: [Type; 2] = [Type::integer(), Type::option(Type::integer()).unwrap()];
    }

    #[test]
    fn if_success() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/if-success.scm");

        assert_eq!(exprs.len(), IFS.len());

        for (n, (expr, expected)) in exprs.iter().zip(IFS.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "if expression {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref WHILES: [Type; 1] = [Type::integer()];
    }

    #[test]
    fn while_success() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/while-success.scm");

        assert_eq!(exprs.len(), WHILES.len());

        for (n, (expr, expected)) in exprs.iter().zip(WHILES.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "while expression {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref FUNCS: [Type; 12] = [
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
            Type::lambda([], None, Type::float()).unwrap(),
            Type::float(),
            Type::integer(),
            Type::lambda(
                [Type::integer(), Type::integer()],
                None,
                Type::lambda([Type::integer()], None, Type::integer()).unwrap()
            )
            .unwrap(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
        ];
    }

    #[allow(clippy::approx_constant)]
    #[test]
    fn user_defined_functions() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/user-defined-functions.scm");

        assert_eq!(exprs.len(), FUNCS.len());

        for (n, (expr, expected)) in exprs.iter().zip(FUNCS.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "user defined function {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref BUILTINS: [Type; 3] = [Type::integer(), Type::integer(), Type::float(),];
    }

    #[test]
    fn built_in_functions() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/built-in-functions.scm");

        assert_eq!(exprs.len(), BUILTINS.len());

        for (n, (expr, expected)) in exprs.iter().zip(BUILTINS.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "built-in function {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref LAMBDAS: [Type; 6] = [
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::lambda(
                [Type::lambda([Type::integer()], None, Type::integer()).unwrap()],
                None,
                Type::integer()
            )
            .unwrap(),
            Type::integer(),
            Type::integer(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
        ];
    }

    #[test]
    fn lambda_functions() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/lambda-functions.scm");

        assert_eq!(exprs.len(), LAMBDAS.len());

        for (n, (expr, expected)) in exprs.iter().zip(LAMBDAS.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "lambda function {} failed",
                n + 1
            );
        }
    }

    lazy_static! {
        static ref TYPES: [Type; 9] = [
            Type::typedef(),
            Type::typedef(),
            Type::typedef(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
            Type::lambda([Type::integer()], None, Type::integer()).unwrap(),
            Type::integer(),
            Type::integer(),
            Type::integer(),
        ];
    }

    #[test]
    fn type_declarations() {
        let mut tc = TestChecker::new();
        let m = tc.env().borrow().mutator().clone();
        let exprs = sexpr_file!(m, "tests/types/type-declarations.scm");

        assert_eq!(exprs.len(), TYPES.len());

        for (n, (expr, expected)) in exprs.iter().zip(TYPES.iter()).enumerate() {
            assert_eq!(
                tc.check_expression(expr.to_owned()),
                Ok(expected.to_owned()),
                "type declaration {} failed",
                n + 1
            );
        }
    }
}
