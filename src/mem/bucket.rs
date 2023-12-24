use std::{
    fmt,
    marker::PhantomData,
    mem::{size_of, MaybeUninit},
    ptr::NonNull,
};

use super::{Mutator, Pointer, Root, Slot, Trace, TARGET};

/// A bucket is a fixed size array of [`Slot`](crate::mem::Slot)s holding values
/// of type `T`.
///
/// Buckets are used to store values. Each bucket can store a fixed number of
/// values specified by the generic parameter `N`. Allocating a value is done in
/// constant time. The values are managed by [`Slot`](crate::mem::Slot)s. Each
/// slot can either be vacant or occupied. If a slot is occupied, then it
/// contains a value.
///
/// Buckets allow the garbage collector to collect values that are no longer
/// reachable. Therefore, buckets implement the methods
/// [`mark()`](crate::mem::Bucket::mark) and
/// [`sweep()`](crate::mem::Bucket::sweep). These methods are only available if
/// the type of the values implements the trait [`Trace`](crate::mem::Trace).
/// They are used by the garbage collector to mark and sweep the values in the
/// bucket.
///
/// * [`mark()`](crate::mem::Bucket::mark) marks all values that are reachable.
///   On this level a value is reachable if it is locked. The locked values are
///   marked and all traceable values are added to a list of pending values.
///   These pending values will be traced later. This is done to prevent stack
///   overflows and recursive calls. If a value is traced, then all of its
///   traceable values will be traced, too. This is done iteratively until all
///   reachable values are traced. Tracing is done by the
///   [`Mutator`](crate::mem::Mutator).
/// * [`sweep()`](crate::mem::Bucket::sweep) collects all values that are not
///   marked. This method is called by the [`Mutator`](crate::mem::Mutator)
///   after the mark phase of the garbage collector. The collected values are
///   deallocated and the bucket is updated. All marked values are unmarked.
///
/// The call-sequence of a garbage collector must be as follows:
///
/// 1. Call [`mark()`](crate::mem::Bucket::mark) on all buckets.
/// 2. Mark all pending roots until no more roots are pending.
/// 3. Call [`sweep()`](crate::mem::Bucket::sweep) on all buckets.
///
/// This sequence must be ensured to prevent memory leaks.
///
/// The lifetime `'a` is the lifetime of the [`Mutator`](crate::mem::Mutator)
/// and the [`Arena`](crate::mem::Arena) this bucket belongs to.
pub struct Bucket<'a, T: fmt::Debug, const N: usize> {
    entries: [Slot<T>; N],
    len: usize,
    next: usize,
    marker: PhantomData<Mutator<'a>>,
}

impl<'a, T: fmt::Debug, const N: usize> Bucket<'a, T, N> {
    /// Creates a new bucket.
    pub fn new() -> Box<Self> {
        log::debug!(target: TARGET,
            "Bucket: new bucket of {} with capacity of {} values",
            std::any::type_name::<T>(), N);
        let mut bucket = Self {
            entries: unsafe { MaybeUninit::zeroed().assume_init() },
            len: 0,
            next: 0,
            marker: PhantomData,
        };

        for i in 0..N {
            bucket.entries[i] = Slot::Vacant(i + 1);
        }
        bucket.into()
    }

    /// Returns the number of entries in the bucket in use.
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns the number of values that can be allocated.
    pub const fn available(&self) -> usize {
        N - self.len
    }

    /// Returns `true` if the bucket is full.
    pub const fn is_full(&self) -> bool {
        self.len == N
    }

    /// Returns `true` if the bucket is empty.
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns `true` if the given pointer points to a valid entry in the
    /// bucket and the entry is occupied.
    pub fn is_valid_pointer(&self, ptr: NonNull<Slot<T>>) -> bool {
        let addr = ptr.as_ptr() as usize;
        let start = &self.entries[0] as *const _ as usize;
        let end = &self.entries[N - 1] as *const _ as usize;

        if addr >= start && addr <= end && (addr - start) % size_of::<Slot<T>>() == 0 {
            unsafe { ptr.as_ref() }.is_occupied()
        } else {
            false
        }
    }

    /// Allocates a new entry in the bucket and returns a pointer to the entry.
    /// Returns `None` if the bucket is full. The [`Mutator`] must be the owner
    /// of the bucket.
    pub fn alloc(&mut self, value: T) -> Option<Pointer<'a, T>> {
        if self.len == N {
            log::debug!(target: TARGET, "Bucket: full, cannot allocate");
            return None;
        }

        let index = self.next;
        self.next = match self.entries[index] {
            Slot::Vacant(next) => next,
            _ => unreachable!(),
        };

        self.entries[index] = Slot::Occupied(1, value);
        self.len += 1;
        log::debug!(target: TARGET,
            "Bucket: allocated value at index 0x{:x}, {:p}",
            index, &mut self.entries[0] as *mut _);

        let ptr = NonNull::new(&mut self.entries[index]).expect("null-pointer");
        let ptr = unsafe { Pointer::new_unchecked(ptr) };
        Some(ptr)
    }
}

impl<'a, T: Trace<'a> + fmt::Debug, const N: usize> Bucket<'a, T, N> {
    /// Marks all entries that are locked. All locked entries are roots and must
    /// be traced. Extends the given vector with roots that are pending.
    /// Pending roots are entries which are reachable from a previously marked
    /// root but are not yet marked themselves.
    pub fn mark(&mut self, pending: &mut Vec<Root<'a>>) {
        let mut marked = 0;
        let pending_len = pending.len();

        // Mark all roots and collect pending objects
        for entry in self.entries.iter_mut() {
            if entry.is_marked() {
                continue;
            }
            if entry.is_locked() {
                entry.mark();
                entry.trace(pending);
                marked += 1;
            }
        }
        log::debug!(target: TARGET,
            "Bucket: marked {} values with pending {} values",
            marked, pending.len() - pending_len);
    }

    /// Unmarks all entries that are marked and collects all entries that are
    /// not marked. Returns the number of collected entries.
    #[inline]
    pub fn sweep(&mut self) -> usize {
        self.sweep_and(|_| {})
    }

    /// Sweeps all entries and returns the number of collected entries. Calls
    /// the given function for each collectable entry.
    pub fn sweep_and(&mut self, mut f: impl FnMut(&mut Slot<T>)) -> usize {
        let allocated = self.len;

        for (index, entry) in self.entries.iter_mut().enumerate() {
            if entry.is_marked() {
                entry.unmark();
            } else if entry.is_collectable() {
                f(entry);
                *entry = Slot::Vacant(self.next);
                self.next = index;
                self.len -= 1;
            }
        }
        log::debug!(target: TARGET,
            "Bucket: sweeped {} values and collected {} values",
            allocated, allocated - self.len);
        allocated - self.len
    }
}

impl<'a, T: fmt::Debug, const N: usize> fmt::Debug for Bucket<'a, T, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Bucket")
            .field("len", &self.len)
            .field("next", &self.next)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    // #[allow(clippy::needless_range_loop)]
    fn create() {
        let bucket = Bucket::<String, 512>::new();

        assert!(bucket.is_empty());
        assert!(!bucket.is_full());
        assert_eq!(bucket.len(), 0);
        assert_eq!(bucket.available(), 512);
        assert_eq!(bucket.next, 0);
        for (idx, entry) in bucket.entries.iter().enumerate() {
            assert_eq!(entry, &Slot::Vacant(idx + 1));
        }
    }

    #[test]
    fn allocate() {
        let mut bucket = Bucket::<String, 512>::new();

        for i in 0..512 {
            let value = bucket.alloc(format!("String {}", i));

            assert_eq!(
                value,
                Some(Pointer::new(NonNull::new(&mut bucket.entries[i]).unwrap()))
            );
            assert_eq!(bucket.len(), i + 1);
            assert_eq!(bucket.available(), 511 - i);
            assert_eq!(bucket.next, i + 1);
            assert_eq!(
                bucket.entries[i],
                Slot::Occupied(1, format!("String {}", i))
            );
        }
        assert!(bucket.is_full());
        assert!(!bucket.is_empty());
    }

    #[test]
    fn mark_sweep() {}
}
