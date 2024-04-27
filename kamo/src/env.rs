//! # Runtime environment of an interpreter
//!
//! This module is available when the `types` feature is enabled.
//!
//! The environment defines the scopes and bindings of an interpreter. The
//! environment is used to store the bindings of variables and functions. During
//! the execution or compilation of a program, the environment is used to lookup
//! the bindings of variables and functions. The environment is also used to
//! define new bindings.
//!
//! The [`TypeChecker`](crate::types::TypeChecker) uses the environment to store
//! or to lookup the types of variables and functions.
//!
//! ## Example
//!
//! ```rust
//! use kamo::env::{Environment, EnvironmentRef};
//! use kamo::mem::Mutator;
//! use kamo::types::Type;
//!
//! let mut mutator = Mutator::new_ref();
//! let mut env = Environment::new_ref(mutator.clone());
//!
//! // Define a binding in the global scope.
//! let (level, index) = env.borrow_mut().define("x", Type::integer(), None);
//!
//! // Lookup the binding.
//! let binding = env.borrow().lookup("x").unwrap();
//! assert_eq!(binding.access(), (level, index));
//! assert_eq!(binding.typedef(), &Type::integer());
//! assert!(binding.value().is_none());
//! ```
use std::{cell::RefCell, collections::BTreeMap, ops::Deref, rc::Rc};

use crate::{
    mem::{Mutator, MutatorRef, Pointer},
    types::Type,
    value::Value,
};

/// The `Environmental` trait is implemented by types which have an environment
/// associated with them. This is used to make it easier to pass around
/// environments.
pub trait Environmental<'a> {
    /// Get a reference to the environment.
    fn env(&self) -> EnvironmentRef<'a>;
}

/* #region EnvironmentRef */

/// A reference to an environment. This is a wrapper around a reference counted
/// reference to a mutable reference cell. This is used to make it easier to
/// pass around environments.
#[derive(Clone, Debug, PartialEq)]
pub struct EnvironmentRef<'a>(Rc<RefCell<Environment<'a>>>);

impl<'a> Deref for EnvironmentRef<'a> {
    type Target = RefCell<Environment<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/* #endregion */

/* #region Environment */

/// The environment is a stack of scopes. Each scope is a mapping from names to
/// indices, types and optional values. The indices are used to access the
/// bindings in the activation stack at run-time. The optional values are used
/// to store the values of bindings which are known at compile-time.
///
/// The environment also contains the global scope. The global scope is the
/// first in the stack. It is used to store global bindings. The global scope
/// is always present and cannot be popped. The environment also contains a
/// reference to the mutator. The mutator is used to allocate new values and
/// store them in the environment. The mutator is shared between all scopes.
#[derive(Debug, PartialEq)]
pub struct Environment<'a> {
    scopes: Vec<Scope<'a>>,
    // Fields in structs are dropped in the order they are declared. This is
    // important because the mutator must be dropped last.
    mutator: MutatorRef<'a>,
}

impl<'a> Environment<'a> {
    /// Create a new environment. The global scope is always present.
    #[must_use]
    #[inline]
    pub fn new(m: MutatorRef<'a>) -> Self {
        Self {
            scopes: vec![BTreeMap::new()],
            mutator: m,
        }
    }

    /// Create a new environment reference.
    #[must_use]
    #[inline]
    pub fn new_ref(m: MutatorRef<'a>) -> EnvironmentRef<'a> {
        EnvironmentRef(Rc::new(RefCell::new(Self::new(m))))
    }

    /// Convert the environment into a reference.
    #[must_use]
    #[inline]
    pub fn into_ref(self) -> EnvironmentRef<'a> {
        EnvironmentRef(Rc::new(RefCell::new(self)))
    }

    /// Get the global scope.
    ///
    /// # Panics
    ///
    /// Panics if the global scope is not present. This should never happen. If
    /// the global scope is not present, the environment is in an invalid state.
    #[must_use]
    #[inline]
    pub fn global_scope(&self) -> &Scope<'a> {
        self.scopes.first().expect("global scope")
    }

    /// Get the mutator.
    #[must_use]
    #[inline]
    pub fn mutator(&self) -> MutatorRef<'a> {
        self.mutator.clone()
    }

    /// Get a reference to the top scope.
    ///
    /// # Panics
    ///
    /// Panics if the global scope is not present. This should never happen. If
    /// the global scope is not present, the environment is in an invalid state.
    #[must_use]
    #[inline]
    pub fn top_scope(&self) -> &Scope<'a> {
        self.scopes.last().expect("global scope")
    }

    /// Get a mutable reference to the top scope.
    ///
    /// # Panics
    ///
    /// Panics if the global scope is not present. This should never happen. If
    /// the global scope is not present, the environment is in an invalid state.
    #[must_use]
    #[inline]
    pub fn top_scope_mut(&mut self) -> &mut Scope<'a> {
        self.scopes.last_mut().expect("global scope")
    }

    /// Push a new scope.
    #[inline]
    pub fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    /// Pop the top scope. Returns `Some(Scope)` if the scope was popped, `None`
    /// if the global scope would be popped.
    ///
    /// # Panics
    ///
    /// Panics when compiled with debug assertions and the global scope would be
    /// popped. This should never happen in a correct program.
    #[inline]
    pub fn pop_scope(&mut self) -> Option<Scope<'a>> {
        debug_assert!(self.scopes.len() > 1, "cannot pop global scope");
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
        }
    }

    /// Define a binding in the top scope.
    ///
    /// If the top scope is the global scope, the binding is shadowed if it
    /// already exists. Previous definitions are not overwritten. The global
    /// binding defined will be unbound.
    ///
    /// Otherwise in a local scope the binding is overwritten if it already
    /// exists. The binding is defined in the top scope.
    ///
    /// If a binding value is given, it is the responsibility of the caller to
    /// ensure that the type of the value matches the type of the binding.
    ///
    /// Returns the level and index of the binding by which it can be accessed
    /// in the activation stack.
    ///
    /// # Panics
    ///
    /// Panics if the scope has more than `u32::MAX` bindings.
    pub fn define(
        &mut self,
        name: impl Into<String>,
        decl_type: Type,
        binding: Option<Value<'a>>,
    ) -> (u32, u32) {
        let name = name.into();

        if self.scopes.len() == 1 {
            let index = u32::try_from(self.scopes[0].len()).expect("scope overflow");

            self.scopes[0].insert(name, (index, decl_type, binding));
            return (0, index);
        }

        let scope = self.top_scope_mut();

        if let Some((index, var_type, var_binding)) = scope.get_mut(&name) {
            *var_type = decl_type;
            *var_binding = binding;
            (0, *index)
        } else {
            let index = u32::try_from(scope.len()).expect("scope overflow");

            scope.insert(name, (index, decl_type, binding));
            (0, index)
        }
    }

    /// Lookup a binding in the environment.
    ///
    /// The binding is searched in the scopes from top to bottom. The first
    /// [`Binding`] with the given name is returned. If no binding is found,
    /// `None` is returned.
    ///
    /// # Panics
    ///
    /// Panics if the level of the binding is greater than `u32::MAX`.
    pub fn lookup(&self, name: impl AsRef<str>) -> Option<Binding<'a>> {
        let name = name.as_ref();
        for (level, scope) in self.scopes.iter().enumerate().rev() {
            if let Some((index, var_type, var_binding)) = scope.get(name) {
                return Some(Binding {
                    level: u32::try_from(level).expect("level overflow"),
                    index: *index,
                    typedef: var_type.to_owned(),
                    value: var_binding.to_owned(),
                });
            }
        }
        None
    }

    /// Resets the environment to the global scope.
    pub fn reset(&mut self) {
        self.scopes.truncate(1);
    }
}

impl<'a> Default for Environment<'a> {
    #[inline]
    fn default() -> Self {
        Self::new(Mutator::new_ref())
    }
}

/* #endregion */

/// The scope is a mapping from names to indices, types and optional values. The
/// indices are used to access the bindings in the activation stack at
/// run-time. The optional values are used to store the values of bindings
/// which are known at compile-time.
pub type Scope<'a> = BTreeMap<String, (u32, Type, Option<Value<'a>>)>;

/* #region ScopeGuard */

/// This guard is used to push and pop scopes from the environment.
///
/// On creation, a new scope is pushed onto the environment. On drop, the
/// scope is popped from the environment. This ensures that the scope is
/// always popped from the environment, even if an error occurs.
#[derive(Clone, Debug, PartialEq)]
pub struct ScopeGuard<'a> {
    env: EnvironmentRef<'a>,
}

impl<'a> ScopeGuard<'a> {
    /// Creates a new scope guard for the given environment.
    #[must_use]
    pub fn new(env: EnvironmentRef<'a>) -> Self {
        env.borrow_mut().push_scope();
        Self { env }
    }
}

impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        self.env.borrow_mut().pop_scope();
    }
}

/* #endregion */

/* #region Binding */

/// Contains the the compile-time information about a binding, which is
/// returned by the lookup in the environment. The optional value is the value
/// of the binding, if it is known at compile-time.
///
/// It is garanteed that the binding is defined in the environment and that
/// the type corresponds to the type of the binding if it is bound.
#[derive(Clone, Debug, PartialEq)]
pub struct Binding<'a> {
    /// The level of the binding in the activation stack. Level 0 is the top
    /// level, i.e. the current frame. Level 1 is the level of the parent frame,
    /// and so on.
    level: u32,
    /// The index in the frame where the binding is stored at run-time.
    index: u32,
    /// The type of the binding.
    typedef: Type,
    /// The value of the binding, if it is known at compile-time.
    value: Option<Value<'a>>,
}

impl<'a> Binding<'a> {
    /// Get the level and index of the binding in the activation stack.
    #[must_use]
    #[inline]
    pub const fn access(&self) -> (u32, u32) {
        (self.level, self.index)
    }

    /// Get the level of the binding in the activation stack.
    #[must_use]
    #[inline]
    pub const fn level(&self) -> u32 {
        self.level
    }

    /// Get the index of the binding in the activation stack.
    #[must_use]
    #[inline]
    pub const fn index(&self) -> u32 {
        self.index
    }

    /// Get the type of the binding.
    #[must_use]
    #[inline]
    pub const fn typedef(&self) -> &Type {
        &self.typedef
    }

    /// Get the value of the binding, if it is known at compile-time.
    #[must_use]
    #[inline]
    pub const fn value(&self) -> Option<&Value<'a>> {
        self.value.as_ref()
    }
}

/* #endregion */
