use crate::{
    env::{EnvironmentRef, ScopeGuard},
    mem::Pointer,
    types::Type,
};

/// Defines the parameters of a lambda expression.
///
/// * `positional` is a list of the positional parameters. May be filled or
/// optional types.
/// * `variadic` is the name of the variadic parameter, if any. May be filled
/// or optional type.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Parameters<'a> {
    /// Positional parameters and their types.
    pub fixed: Vec<(Pointer<'a, Box<str>>, Type)>,
    /// Variadic parameter and its type if any.
    pub variadic: Option<(Pointer<'a, Box<str>>, Type)>,
}

impl<'a> Parameters<'a> {
    /// Return the number of parameters including the variadic parameter.
    #[must_use]
    #[inline]
    pub fn len(&self) -> usize {
        self.fixed.len() + usize::from(self.variadic.is_some())
    }

    /// Return `true` if there are no parameters.
    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.variadic.is_none()
    }

    /// Return `true` if there is a variadic parameter.
    #[must_use]
    #[inline]
    pub const fn is_variadic(&self) -> bool {
        self.variadic.is_some()
    }

    /// Define the parameters in a new scope. Returns a scope guard which pops
    /// the scope when dropped.
    #[must_use]
    pub fn define(&self, env: &EnvironmentRef<'a>) -> ScopeGuard<'a> {
        let guard = ScopeGuard::new(env.clone());
        let mut env = env.borrow_mut();

        for (name, typedef) in &self.fixed {
            env.define(name.as_ref(), typedef.to_owned(), None);
        }
        if let Some((name, typedef)) = &self.variadic {
            env.define(name.as_ref(), typedef.to_owned(), None);
        }
        guard
    }
}
