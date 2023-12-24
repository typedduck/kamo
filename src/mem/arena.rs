use std::{fmt, ptr::NonNull};

use super::{Bucket, Pointer, Root, Slot, Trace, TARGET};

/// An arena for allocating values.
///
/// An arena is a data structure that can be used to allocate values of the same
/// type. It is an array of [`Bucket`]s. Each bucket can hold a fixed number of
/// values, which is specified by the generic parameter `N`. The values are
/// managed by [`Slot`]s. Each slot can either be vacant or occupied. If a slot
/// is occupied, then it contains a value.
///
/// Allocating a value is done mostly in constant time. New buckets are pushed
/// onto the arena, if all buckets are full. Allocation in the top bucket is
/// done in constant time. If the top bucket is full, then the remaining buckets
/// are checked. This is done in linear time.
///
/// The arena allows the garbage collector to collect values that are no longer
/// reachable. Therefore, the arena implements the methods
/// [`mark()`](Arena::mark),
/// [`sweep()`](Arena::sweep) and
/// [`compact()`](Arena::compact). These methods are only available
/// if the type of the values implements the trait [`Trace`](crate::mem::Trace).
/// They are used by the garbage collector to mark and sweep the values in the
/// arena.
///
/// * [`mark()`](crate::mem::Arena::mark) it iterates over all buckets and marks
///   all values that are reachable. All traceable values are added to a list of
///   pending values. These pending values will be traced later by the garbage
///   collector. This is done to prevent stack overflows and recursive calls.
/// * [`sweep()`](crate::mem::Arena::sweep) collects all values that are not
///   marked. This method is called by the garbage collector after the mark
///   phase. The collected values are deallocated and the arena is updated. All
///   marked values are unmarked. If more than a quarter of the values are
///   collected, then the arena is compacted. This means that all empty buckets
///   are removed.
/// * [`compact()`](crate::mem::Arena::compact) removes all empty buckets from
///   the arena. Calling this method is only useful after a sweep.
///
/// The call-sequence of a garbage collector must be as follows:
///
/// 1. Call [`mark()`](Arena::mark) on each arena, which propagates the mark
///    phase to all buckets.
/// 2. Mark all pending roots until no more roots are pending.
/// 3. Call [`sweep()`](Arena::sweep) on each arena, which propagates the sweep
///    phase to all buckets.
///
/// Compacting the arena is done automatically after a sweep. This is safe,
/// because all reachable values in this arena where locked. This sequence must
/// be ensured to prevent memory leaks.
/// 
/// The lifetime `'a` is the lifetime of the [`Mutator`](crate::mem::Mutator).

pub struct Arena<'a, T: fmt::Debug, const N: usize> {
    buckets: Vec<Box<Bucket<'a, T, N>>>,
}

impl<'a, T: fmt::Debug, const N: usize> Arena<'a, T, N> {
    /// Creates a new empty arena.
    pub fn new() -> Self {
        log::debug!(target: TARGET,
            "Arena: new arena of {} with bucket size {}",
            std::any::type_name::<T>(), N);
        Self { buckets: vec![] }
    }

    /// Returns the number of entries in the arena that are in use.
    pub fn len(&self) -> usize {
        self.buckets.iter().fold(0, |x, acc| x + acc.len())
    }

    /// Returns the total capacity of the arena. This is the size of a bucket
    /// times the number of buckets.
    #[inline]
    pub fn capacity(&self) -> usize {
        N * self.buckets.len()
    }

    /// Returns the number of values that can be allocated without reallocating.
    pub fn available(&self) -> usize {
        self.buckets.iter().fold(0, |x, acc| x + acc.available())
    }

    /// Returns `true` if the arena is empty.
    pub fn is_empty(&self) -> bool {
        self.buckets.is_empty() || self.buckets.iter().all(|bucket| bucket.is_empty())
    }

    /// Returns `true` if the pointer points to a valid entry in the arena and
    /// the entry is occupied.
    pub fn is_valid_pointer(&self, ptr: NonNull<Slot<T>>) -> bool {
        self.buckets
            .iter()
            .any(|bucket| bucket.is_valid_pointer(ptr))
    }

    /// Allocates a new entry in the arena and returns the pointer to the entry.
    /// The returned pointer to the entry is already locked. The
    /// [`Mutator`](crate::mem::Mutator) must be owner of the arena.
    pub fn alloc(&mut self, value: T) -> Pointer<'a, T> {
        for (idx, bucket) in self.buckets.iter_mut().enumerate().rev() {
            if !bucket.is_full() {
                let value = bucket
                    .alloc(value)
                    .expect("bucket reports it is not full");
                log::debug!(target: TARGET,
                    "Arena: allocating value in arena at {:p} in bucket {}",
                    value.as_ptr(), idx);
                return value;
            }
        }

        let mut bucket = Bucket::new();
        let value = bucket
            .alloc(value)
            .expect("new bucket must be able to allocate");

        self.buckets.push(bucket);
        log::debug!(target: TARGET,
            "Arena: allocating value in arena at {:p} in new bucket {}",
            value.as_ptr(), self.buckets.len() - 1);
        value
    }
}

impl<'a, T: Trace<'a> + fmt::Debug, const N: usize> Arena<'a, T, N> {
    /// Marks all buckets. Adds all traceable values to the given list of
    /// pending values.
    pub fn mark(&mut self, pending: &mut Vec<Root<'a>>) {
        log::debug!(target: TARGET,
            "Arena: marking {} buckets of {}",
            self.buckets.len(), std::any::type_name::<T>());

        for bucket in &mut self.buckets {
            bucket.mark(pending);
        }

        log::debug!(target: TARGET, "Arena: {} pending values", pending.len());
    }

    /// Sweeps all buckets and returns the number of collected entries.
    #[inline]
    pub fn sweep(&mut self) -> usize {
        self.sweep_and(|_| {})
    }

    /// Sweeps all buckets and returns the number of collected entries. Calls
    /// the given function for each collectable entry.
    pub fn sweep_and(&mut self, mut f: impl FnMut(&mut Slot<T>)) -> usize {
        log::debug!(target: TARGET,
            "Arena: sweeping {} buckets of {}",
            self.buckets.len(), std::any::type_name::<T>());
        let mut collected = 0;

        for bucket in &mut self.buckets.iter_mut() {
            collected += bucket.sweep_and(&mut f);
        }
        log::debug!(target: TARGET,
            "Arena: sweeped {} buckets, collected {} values",
            self.buckets.len(), collected);
        if self.buckets.len() > 4 && collected > self.capacity() / 4 {
            self.compact();
        }
        collected
    }

    /// Compacts the arena by removing all empty buckets. This is only useful
    /// after a sweep.
    pub fn compact(&mut self) {
        let len = self.buckets.len();

        log::debug!(target: TARGET,
            "Arena: compacting {} buckets of {}",
            self.buckets.len(), std::any::type_name::<T>());
        self.buckets.retain(|bucket| !bucket.is_empty());
        log::debug!(target: TARGET,
            "Arena: compacted {} buckets, removed {} buckets",
            len, len - self.buckets.len());
    }
}

impl<'a, T: fmt::Debug, const N: usize> Default for Arena<'a, T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T: fmt::Debug, const N: usize> fmt::Debug for Arena<'a, T, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arena")
            .field("buckets", &self.buckets)
            .field("len", &self.len())
            .field("capacity", &self.capacity())
            .finish()
    }
}
