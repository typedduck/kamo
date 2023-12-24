use super::{Pair, Vector};

/// A trait for visiting values.
/// 
/// This trait is used by the [`Value::accept()`](super::Value::accept) method
/// to implement the visitor-pattern. It is also used by the
/// [`SimplePrinter`](super::SimplePrinter) to print values.
/// 
/// # Note
/// 
/// The implementation of the visit-methods must consider that the value may be
/// circular. In order to prevent infinite recursion, the implementation should
/// check if the value has already been visited and if so, return early.
/// 
/// This mainly effects the [`visit_pair()`](Visitor::visit_pair) and
/// [`visit_vector()`](Visitor::visit_vector) methods.
pub trait Visitor {
    /// The type of the result returned by the visit-methods.
    type Result;

    /// Visit if the value is `Value::Nil`.
    fn visit_nil(&mut self) -> Self::Result;
    /// Visit if the value is `Value::Bool(true)`.
    fn visit_true(&mut self) -> Self::Result;
    /// Visit if the value is `Value::Bool(false)`.
    fn visit_false(&mut self) -> Self::Result;
    /// Visit if the value is `Value::Char`.
    fn visit_char(&mut self, value: char) -> Self::Result;
    /// Visit if the value is `Value::Integer`.
    fn visit_integer(&mut self, value: i64) -> Self::Result;
    /// Visit if the value is `Value::Float`.
    fn visit_float(&mut self, value: f64) -> Self::Result;
    /// Visit if the value is `Value::Pair`.
    /// 
    /// Pairs are the cons-cells of lists. They consist of a head and a tail.
    /// The accept-method does not visit the head and tail directly, but rather
    /// calls the [`visit_pair()`](Visitor::visit_pair) method. The
    /// implementation must take care of walking the list. This can involve
    /// recursion if the list is complex, for example if it is branched or
    /// circular.
    /// 
    /// # Note
    /// 
    /// The implementation of this method must consider that the pair may be
    /// circular. In order to prevent infinite recursion, the implementation
    /// should check if the pair has already been visited and if so, return
    /// early.
    fn visit_pair(&mut self, value: &Pair<'_>) -> Self::Result;
    /// Visit if the value is `Value::String`.
    fn visit_string(&mut self, value: &str) -> Self::Result;
    /// Visit if the value is `Value::Symbol`.
    fn visit_symbol(&mut self, value: &str) -> Self::Result;
    /// Visit if the value is `Value::Bytevec`.
    fn visit_bytevec(&mut self, value: &[u8]) -> Self::Result;
    /// Visit if the value is `Value::Vector`.
    /// 
    /// # Note
    /// 
    /// The implementation of this method must consider that the vector may be
    /// circular. In order to prevent infinite recursion, the implementation
    /// should check if the vector has already been visited and if so, return
    /// early.
    fn visit_vector(&mut self, value: &Vector<'_>) -> Self::Result;
}
