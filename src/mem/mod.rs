//! # Memory management
//! 
//! This module contains the memory management system. It is based on a
//! [`Mutator`], which is responsible for allocating and freeing values. The
//! mutator is used to allocate values on the heap. The mutator is also
//! responsible for garbage collection. The garbage collection is done by a
//! mark-and-sweep algorithm.
//! 
//! The memory management system is based on [`Arena`]s. An arena is a
//! collection of [`Bucket`]s. A bucket is a collection of [`Slot`]s. A slot is
//! a container for a value. The mutator allocates values by requesting a new
//! slot from the arena. For each type that is managed by the mutator an arena
//! is maintained. The mutator returns a [Pointer] to the slot, which can be
//! used to access the value. When the mutator is dropped, all values are freed
//! regardless of whether they are reachable or not.
//! 
//! It is the responsibility of the user to make sure that the mutator is not
//! dropped while there are still pointers to values. The mutator does not
//! maintain a reference count by itself. If it is required to pass the mutator
//! to multiple functions, it is recommended to wrap the mutator in a
//! [MutatorRef]. This type maintains a reference count and can be used to
//! safely access the mutator. A [MutatorRef] can be created by calling the
//! [Mutator::new_ref()] method.
//! 
//! The hierarchy of types is as follows:
//! 
//! - [Mutator] contains [Arena]s.
//! - [Arena] contains [Bucket]s.
//! - [Bucket] contains [Slot]s.
//! - [Slot] contains a value.
//! - a value is represented by a [Pointer].
//! 
//! The values must be [Trace]able, which means that they must implement the
//! [Trace] trait. This trait is used to mark values as reachable. The mutator
//! will call the [Trace::trace()] method on all values that are reachable. This
//! method must add all reachable values, which are reachable from this value,
//! to the list of reachable values. This is done iteratively until all
//! reachable values are traced. The reachable values are wrapped in a [Root]
//! and are stored in a vector of pending values.
//! 
//! When all reachable values are traced, the mutator will free all values that
//! are not reachable. This is done by by the [Arena::sweep()] method. This
//! method will iterate over all buckets and free all slots that are not marked.
//! 
//! A value is reachable if it is a root, or if it is reachable from a root.
//! Roots are [Slot]s that are locked. This means that they are not freed by the
//! mutator as long as they hold a lock. The mutator will lock a slot when it is
//! returned to the user in a [`Pointer`]. Slots get unlocked when they are
//! added to a [`Pair`](crate::value::Pair) or [`Vector`](crate::value::Vector).
//! These types may have cycles. If they would not unlock the slots, the slots
//! would never be freed. This mitigates the problem of cycles with reference
//! counting.

mod arena;
pub use arena::Arena;

mod bucket;
pub use bucket::Bucket;

mod mutator;
pub use mutator::*;

mod pointer;
pub use pointer::Pointer;

mod slot;
pub use slot::Slot;

mod stats;
pub use stats::Stats;

mod trace;
pub use trace::{Root, Trace, ToRoot};

/// The target passed to the logger.
pub const TARGET: &str = "kamo::mem";
