/// Statistics about the memory usage of the [`Mutator`](crate::mem::Mutator).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Stats {
    /// The total number of allocated slots.
    pub allocated: usize,
    /// The total number of allocated byte-vectors.
    pub allocated_bytevecs: usize,
    /// The total number of allocated strings.
    pub allocated_strings: usize,
    /// The total number of allocated symbols.
    pub allocated_symbols: usize,
    /// The total number of allocated pairs.
    pub allocated_pairs: usize,
    /// The total number of allocated vectors.
    pub allocated_vectors: usize,
    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    /// The total number of allocated types.
    pub allocated_types: usize,
    /// The number of allocations since the last garbage collection.
    pub allocations: usize,
    /// The threshold of allocations until the next garbage collection.
    pub allocation_pressure: usize,
    /// The number of garbage collections.
    pub garbage_collections: usize,
}
