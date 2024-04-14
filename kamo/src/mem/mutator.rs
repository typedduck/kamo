use std::{cell::RefCell, collections::BTreeMap, fmt, ops::Deref, ptr::NonNull, rc::Rc};

#[cfg(feature = "types")]
use crate::types::Type;
use crate::{
    mem::TARGET,
    value::{ByteVector, Pair, SmartString, Value, Vector},
};

use super::{Arena, Pointer, Root, Slot, Stats, Trace};

/* #region MutatorRef */

/// A reference to a mutator. This is a wrapper around a reference counted
/// reference to a mutable reference cell. This is used to make it easier to
/// pass around mutators.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Debug, PartialEq)]
pub struct MutatorRef<'a>(Rc<RefCell<Mutator<'a>>>);

impl<'a> Deref for MutatorRef<'a> {
    type Target = RefCell<Mutator<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/* #endregion */

/// Default capacity of a [`Bucket`](crate::mem::Bucket) managed by an
/// [`Arena`].
pub const BUCKET_DEFAULT_SIZE: usize = 1024;
/// Capacity of a [`Bucket`](crate::mem::Bucket) managed by an [`Arena`] for
/// a large number of values. It is used for [`Pair`](crate::value::Pair)s, which
/// are usually allocated in large numbers.
pub const BUCKET_LARGE_SIZE: usize = 2048;
/// Capacity of a [`Bucket`](crate::mem::Bucket) managed by an [`Arena`] for
/// a small number of values. It is used for [`Vector`](crate::value::Vector)s,
/// which are usually allocated in small numbers.
pub const BUCKET_SMALL_SIZE: usize = 512;
/// Minimum allocation pressure. This is the number of allocations that can be
/// performed before a garbage collection is triggered.
pub const MIN_ALLOCATION_PRESSURE: usize = 1024;
/// Maximum allocation pressure is clamped to this value. This is the number of
/// allocations that can be performed at a maximum before a garbage collection
/// is triggered.
pub const MAX_ALLOCATION_PRESSURE: usize = 1024 * 1024;

/// A mutator is used to allocate values. It contains arenas for each type of
/// value. It also contains a symbol map which is used to intern symbols.
///
/// Values are automatically garbage collected when the allocation pressure is
/// reached. The allocation pressure is the number of allocations that can be
/// performed before a garbage collection is triggered. The allocation pressure
/// is calculated as half of the capacity of the arenas. The capacity of the
/// arenas is the number of values that can be allocated without expanding the
/// arenas.
///
/// Arenas are expanded when they are full. The capacity of the arenas is
/// extended by allocating buckets which are used to store the values. The
/// capacity of a bucket is fixed at compile time. The capacity of the arenas is
/// the number of buckets multiplied by the capacity of a bucket.
///
/// The values which can be allocated by the mutator are:
/// * [`BytevVector`](ByteVector) (bytevec)
/// * [`Pair`](Pair) (cons cell)
/// * [`SmartString`](SmartString) (string)
/// * [`Box<str>`](Box<str>) (symbol)
/// * [`Vector`](Vector) (vector)
///
/// The mutator can also allocate values which are used by the evaluator:
/// * [`Activation`](Activation) (activation frame)
/// * [`Procedure`](Procedure) (procedure)
///
/// In order to allocate the values for the evaluator, the `evaluate` feature
/// must be enabled.
///
/// All allocations are returned as [`Pointer`](Pointer)s. A pointer is a
/// reference to a value in the mutator. The pointer is reference counted and
/// can be cloned. The pointer is also used to trace values during garbage
/// collection. Almost all pointers can be converted to a value. The only
/// exception is the pointer to an activation frame. This is because the
/// activation frame should not be used outside of the evaluator.
///
/// The mutator is not `Send` and not `Sync`. This means that the mutator can
/// not be sent between threads and can not be shared between threads. This is
/// because the mutator is not thread safe. This means that the mutator should
/// only be used from one thread at a time. This is because the garbage
/// collection is not atomic. This means that the garbage collection can be
/// triggered while a value is being allocated. This can lead to dangling
/// pointers in a multithreaded environment.
///
/// This may change in the future.
///
/// ## Symbols
///
/// Symbols are interned. If the symbol already exists in the symbol map, the
/// existing symbol is returned. The symbol map only contains weak references to
/// the symbols. Therefore the symbol map does not own the symbols but it is
/// ensured that the symbols are not collected while they are interned and a
/// reference to the symbol exists.
///
/// If an existing symbol is returned, then the weak reference is upgraded to a
/// strong reference. This is safe because the symbol is still alive.
pub struct Mutator<'a> {
    bytevecs: Arena<'a, ByteVector, BUCKET_SMALL_SIZE>,
    pairs: Arena<'a, Pair<'a>, BUCKET_LARGE_SIZE>,
    strings: Arena<'a, SmartString, BUCKET_DEFAULT_SIZE>,
    symbol_map: BTreeMap<&'a str, NonNull<Slot<Box<str>>>>,
    symbols: Arena<'a, Box<str>, BUCKET_DEFAULT_SIZE>,
    vectors: Arena<'a, Vector<'a>, BUCKET_SMALL_SIZE>,
    #[cfg(feature = "types")]
    types: Arena<'a, Type, BUCKET_SMALL_SIZE>,
    allocations: usize,
    allocation_pressure: usize,
    garbage_collections: usize,
}

impl<'a> Mutator<'a> {
    /// Creates a new mutator.
    #[must_use]
    pub fn new() -> Self {
        log::debug!(target: TARGET, "Mutator: new mutator");
        let mut mutator = Self {
            bytevecs: Arena::new(),
            pairs: Arena::new(),
            strings: Arena::new(),
            symbols: Arena::new(),
            vectors: Arena::new(),
            #[cfg(feature = "types")]
            types: Arena::new(),
            symbol_map: BTreeMap::new(),
            allocations: 0,
            allocation_pressure: 0,
            garbage_collections: 0,
        };

        mutator.reset_pressure();
        mutator
    }

    /// Creates a new mutator reference.
    #[must_use]
    pub fn new_ref() -> MutatorRef<'a> {
        MutatorRef(Rc::new(RefCell::new(Self::new())))
    }

    /* #region Attributes */

    /// Returns the number of allocated values.
    #[allow(clippy::let_and_return)]
    #[must_use]
    pub fn len(&self) -> usize {
        let len = self.bytevecs.len()
            + self.pairs.len()
            + self.strings.len()
            + self.symbols.len()
            + self.vectors.len();
        #[cfg(feature = "types")]
        let len = len + self.types.len();

        len
    }

    /// Returns the total capacity of values that can be allocated.
    #[allow(clippy::let_and_return)]
    #[must_use]
    pub fn capacity(&self) -> usize {
        let capacity = self.bytevecs.capacity()
            + self.pairs.capacity()
            + self.strings.capacity()
            + self.symbols.capacity()
            + self.vectors.capacity();
        #[cfg(feature = "types")]
        let capacity = capacity + self.types.capacity();

        capacity
    }

    /// Returns the number of values that can be allocated without reallocating.
    #[allow(clippy::let_and_return)]
    #[must_use]
    pub fn available(&self) -> usize {
        let available = self.bytevecs.available()
            + self.pairs.available()
            + self.strings.available()
            + self.symbols.available()
            + self.vectors.available();
        #[cfg(feature = "types")]
        let available = available + self.types.available();

        available
    }

    /// Returns the current statistics of the mutator.
    #[inline]
    #[must_use]
    pub fn stats(&self) -> Stats {
        Stats {
            allocated: self.len(),
            allocated_bytevecs: self.bytevecs.len(),
            allocated_strings: self.strings.len(),
            allocated_symbols: self.symbols.len(),
            allocated_pairs: self.pairs.len(),
            allocated_vectors: self.vectors.len(),
            #[cfg(feature = "types")]
            allocated_types: self.types.len(),
            allocations: self.allocations,
            allocation_pressure: self.allocation_pressure,
            garbage_collections: self.garbage_collections,
        }
    }

    /// Returns `true` if no values have been allocated.
    #[allow(clippy::let_and_return)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        let empty = self.bytevecs.is_empty()
            && self.pairs.is_empty()
            && self.strings.is_empty()
            && self.symbols.is_empty()
            && self.vectors.is_empty();
        #[cfg(feature = "types")]
        let empty = empty && self.types.is_empty();

        empty
    }

    /* #endregion */

    /* #region Allocation */

    #[must_use]
    pub fn new_bytevec(&mut self, value: impl AsRef<[u8]>) -> Pointer<'a, ByteVector> {
        self.on_allocate();

        let value = self.bytevecs.alloc(value.as_ref().into());

        log::debug!(target: TARGET, "Mutator: new bytevec {:p}", value.as_ptr());
        value
    }

    #[must_use]
    pub fn new_pair(&mut self, head: Value<'a>, tail: Value<'a>) -> Pointer<'a, Pair<'a>> {
        self.on_allocate();

        let value = self.pairs.alloc(Pair::new(head, tail));

        log::debug!(target: TARGET, "Mutator: new pair {:p}", value.as_ptr());
        value
    }

    #[must_use]
    pub fn new_string(&mut self, value: impl AsRef<str>) -> Pointer<'a, SmartString> {
        self.on_allocate();

        let value = self.strings.alloc(value.as_ref().into());

        log::debug!(target: TARGET, "Mutator: new string {:p}", value.as_ptr());
        value
    }

    #[allow(clippy::missing_panics_doc)]
    #[must_use]
    pub fn new_symbol(&mut self, value: impl AsRef<str>) -> Pointer<'a, Box<str>> {
        let value = value.as_ref();

        // In the symbol map the symbols are interned. This means that if the
        // symbol already exists in the symbol map, the existing symbol is
        // returned. The symbol map only contains weak references to the
        // symbols. This means that the symbol map does not own the symbols but
        // it is ensured that the symbols are not collected while they are
        // interned.
        if let Some(value) = self.symbol_map.get(value).copied() {
            log::debug!(target: TARGET, "Mutator: using symbol {:p}", value.as_ptr());
            // The weak reference is upgraded to a strong reference. This is
            // safe because the symbol is still alive.
            Pointer::new(value)
        } else {
            self.on_allocate();

            let value: Box<str> = value.into();
            let key = std::ptr::addr_of!(*value);
            // This unwrap is safe because the key is a reference to the value
            // and the value is not null. It is a quirky way to get a reference
            // to the value without cloning the value. The reference to the key
            // is the same as the reference to the value. The lifetime of the
            // reference is the same as the lifetime of the value.
            let key = unsafe { key.as_ref() }.unwrap();
            let value = self.symbols.alloc(value);

            // The symbol map is only used to intern symbols and does not own
            // the symbols. Only weak references to the symbols are stored in
            // the symbol map. This is because the symbol map is not used to
            // trace symbols during garbage collection. The symbols are traced
            // by tracing the values that reference the symbols.
            //
            // The weak references are removed when the symbols are collected by
            // using the `sweep_and` function of the arena.
            self.symbol_map.insert(key, unsafe { value.as_inner() });
            log::debug!(target: TARGET, "Mutator: new symbol {:p}", value.as_ptr());
            value
        }
    }

    pub fn new_vector(&mut self, values: impl Into<Vec<Value<'a>>>) -> Pointer<'a, Vector<'a>> {
        self.on_allocate();

        let values = values.into();
        let len = values.len();
        let value = self.vectors.alloc(Vector::new(values));

        log::debug!(target: TARGET, "Mutator: new vector[{}] {:p}", len, value.as_ptr());
        value
    }

    #[cfg(feature = "types")]
    pub fn new_type(&mut self, value: Type) -> Pointer<'a, Type> {
        self.on_allocate();

        let value = self.types.alloc(value);

        log::debug!(target: TARGET, "Mutator: new type {:p}", value.as_ptr());
        value
    }

    /* #endregion */

    /* #region Coercion */

    /// Coerces the raw pointer into a valid bytevec pointer. Returns `None` if
    /// the pointer is not valid.
    #[inline]
    pub fn into_bytevec(
        &mut self,
        raw: NonNull<Slot<ByteVector>>,
    ) -> Option<Pointer<'a, ByteVector>> {
        self.bytevecs
            .is_valid_pointer(raw)
            .then(|| Pointer::new(raw))
    }

    /// Coerces the raw pointer into a valid pair pointer. Returns `None` if the
    /// pointer is not valid.
    #[inline]
    pub fn into_pair(&mut self, raw: NonNull<Slot<Pair<'a>>>) -> Option<Pointer<'a, Pair<'a>>> {
        self.pairs.is_valid_pointer(raw).then(|| Pointer::new(raw))
    }

    /// Coerces the raw pointer into a valid string pointer. Returns `None` if
    /// the pointer is not valid.
    #[inline]
    pub fn into_string(
        &mut self,
        raw: NonNull<Slot<SmartString>>,
    ) -> Option<Pointer<'a, SmartString>> {
        self.strings
            .is_valid_pointer(raw)
            .then(|| Pointer::new(raw))
    }

    /// Coerces the raw pointer into a valid symbol pointer. Returns `None` if
    /// the pointer is not valid.
    #[inline]
    pub fn into_symbol(&mut self, raw: NonNull<Slot<Box<str>>>) -> Option<Pointer<'a, Box<str>>> {
        self.symbols
            .is_valid_pointer(raw)
            .then(|| Pointer::new(raw))
    }

    /// Coerces the raw pointer into a valid vector pointer. Returns `None` if
    /// the pointer is not valid.
    #[inline]
    pub fn into_vector(
        &mut self,
        raw: NonNull<Slot<Vector<'a>>>,
    ) -> Option<Pointer<'a, Vector<'a>>> {
        self.vectors
            .is_valid_pointer(raw)
            .then(|| Pointer::new(raw))
    }

    #[cfg(feature = "types")]
    /// Coerces the raw pointer into a valid type pointer. Returns `None` if
    /// the pointer is not valid.
    #[inline]
    pub fn into_type(&mut self, raw: NonNull<Slot<Type>>) -> Option<Pointer<'a, Type>> {
        self.types.is_valid_pointer(raw).then(|| Pointer::new(raw))
    }

    /* #endregion */

    /* #region MemoryManagement */

    /// Collects garbage by marking and sweeping all values. Returns the number
    /// of collected values.
    ///
    /// Collecting garbage is done automatically when the allocation pressure is
    /// reached. The allocation pressure is the number of allocations that can
    /// be performed before a garbage collection is triggered. The allocation
    /// pressure is calculated as half of the capacity of the arenas. The
    /// capacity of the arenas is the number of values that can be allocated
    /// without expanding the arenas.
    ///
    /// In the current implementation garbage collection is not thread safe.
    /// This means that garbage collection should only be called from one thread
    /// at a time and the mutator should be thread local. This is because the
    /// garbage collection is not atomic. This means that the garbage collection
    /// can be triggered while a value is being allocated. This can lead to
    /// dangling pointers in a multithreaded environment.
    ///
    /// This may change in the future.
    ///
    /// Reclaiming memory is done in two phases:
    ///
    /// 1. Marking phase: All values that are reachable are marked. Reachable
    ///   values are values that are locked. These roots are traced and all
    ///   values that are reachable from these roots are marked. This is done
    ///   without recursion in an iterative manner. This is done for all arenas.
    ///
    /// 2. Sweeping phase: All values that are not marked are collected. This is
    ///   done for all arenas.
    ///
    /// The implemenation is a straightforward implementation of the mark and
    /// sweep algorithm.
    ///
    /// # Panics
    ///
    /// This function panics if the value in the symbol map is not occupied.
    /// This should never happen.
    pub fn collect_garbage(&mut self) -> usize {
        self.on_collect();

        let mut collected = 0;

        // Marking phase
        let mut pending = vec![];

        log::debug!(target: TARGET, "Mutator: marking arenas");
        self.bytevecs.mark(&mut pending);
        self.pairs.mark(&mut pending);
        self.strings.mark(&mut pending);
        self.symbols.mark(&mut pending);
        self.vectors.mark(&mut pending);
        #[cfg(feature = "types")]
        self.types.mark(&mut pending);

        log::debug!(target: TARGET, "Mutator: marking pending values");
        Self::mark_pending(pending);

        // Sweeping phase
        log::debug!(target: TARGET, "Mutator: sweeping arenas");
        #[cfg(feature = "types")]
        {
            collected += self.types.sweep();
        }
        collected += self.vectors.sweep();
        collected += self.symbols.sweep_and(|key| {
            let key = key.value().expect("not occupied").as_ref();
            self.symbol_map.remove(key);
        });
        collected += self.strings.sweep();
        collected += self.pairs.sweep();
        collected += self.bytevecs.sweep();
        log::debug!(target: TARGET, "Mutator: collected {} values", collected);

        collected
    }

    /// Mark all pending objects and their pending objects without recursion.
    fn mark_pending(mut pending: Vec<Root>) {
        while let Some(root) = pending.pop() {
            match root {
                Root::Bytevec(mut value) => unsafe { value.as_mut() }.mark(),
                Root::String(mut value) => unsafe { value.as_mut() }.mark(),
                Root::Symbol(mut value) => unsafe { value.as_mut() }.mark(),
                #[cfg(feature = "types")]
                Root::Type(mut value) => unsafe { value.as_mut() }.mark(),
                Root::Pair(mut value) => {
                    let entry = unsafe { value.as_mut() };

                    if !entry.is_marked() {
                        entry.mark();
                        entry.trace(&mut pending);
                    }
                }
                Root::Vector(mut value) => {
                    let entry = unsafe { value.as_mut() };

                    if !entry.is_marked() {
                        entry.mark();
                        entry.trace(&mut pending);
                    }
                }
            }
        }
    }

    #[inline]
    fn on_allocate(&mut self) {
        self.allocations += 1;

        log::debug!(target: TARGET,
            "Mutator: started allocating value, {}. allocation",
            self.allocations);
        if self.allocations > self.allocation_pressure {
            self.collect_garbage();
            self.allocations = 1;
        }
    }

    #[inline]
    fn on_collect(&mut self) {
        self.garbage_collections += 1;

        log::info!(target: TARGET,
            "Mutator: started garbage collection for {}. time",
            self.garbage_collections);
        self.reset_pressure();
    }

    #[inline]
    fn reset_pressure(&mut self) {
        let pressure = (self.bytevecs.capacity() / 2)
            + (self.pairs.capacity() / 2)
            + (self.strings.capacity() / 2)
            + (self.symbols.capacity() / 2)
            + (self.vectors.capacity() / 2);
        #[cfg(feature = "types")]
        let pressure = pressure + (self.types.capacity() / 2);

        self.allocations = 0;
        self.allocation_pressure = pressure.clamp(MIN_ALLOCATION_PRESSURE, MAX_ALLOCATION_PRESSURE);

        log::info!(target: TARGET,
            "Mutator: allocation pressure is set to {}",
            self.allocation_pressure);
    }

    /* #endregion */
}

impl<'a> Default for Mutator<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> PartialEq for Mutator<'a> {
    fn eq(&self, other: &Self) -> bool {
        let lhs = self as *const Self;
        let rhs = other as *const Self;
        std::ptr::eq(lhs, rhs)
    }
}

impl<'a> fmt::Debug for Mutator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = f.debug_struct("Mutator");
        let out = out
            .field("bytevecs", &self.bytevecs)
            .field("pairs", &self.pairs)
            .field("strings", &self.strings)
            .field("symbols", &self.symbols)
            .field("vectors", &self.vectors);
        #[cfg(feature = "types")]
        let out = out.field("types", &self.types);

        out.field("symbol_map", &self.symbol_map)
            .field("allocations", &self.allocations)
            .field("allocation_pressure", &self.allocation_pressure)
            .field("garbage_collections", &self.garbage_collections)
            .finish()
    }
}

impl<'a> Drop for Mutator<'a> {
    fn drop(&mut self) {
        log::debug!(target: TARGET, "Mutator: dropping mutator");
        self.symbol_map.clear();

        self.bytevecs.drop_all();
        self.pairs.drop_all();
        self.strings.drop_all();
        self.symbols.drop_all();
        self.vectors.drop_all();
        #[cfg(feature = "types")]
        self.types.drop_all();
    }
}
