use std::fmt;

use super::{Root, Trace, TARGET};

/// A slot in a [`Bucket`](crate::mem::Bucket).
/// 
/// A slot can be in one of four states:
/// 
/// - **Undefined**: An all zero slot. Is used as a placeholder for
///   uninitialized slots.
/// - **Vacant**: A slot which can be filled with a value. The `usize` is the
///   index of the next free slot in the [`Bucket`](crate::mem::Bucket). The
///   last slot in the vacant list is marked with the size of the bucket
///   [`Bucket::N`](crate::mem::Bucket).
/// - **Occupied**: A slot which is occupied by a value. The `usize` is the
///   number of locks on the slot. A lock-count of zero does not mean that the
///   slot is collectable. It only means that the slot is not locked. But it
///   still can be reachable from other slots.
/// - **Marked**: A slot which is occupied by a value and is marked to be in
///   use. The `usize` is the number of locks on the slot. In the sweep phase of
///   the garbage collector, all marked slots are unmarked.
/// 
/// The garbage collector uses the lock-count to determine if a slot is
/// collectable. These are the root slots. A slot is only then collectable if it
/// is not locked or not reachable from other slots. In the mark phase all
/// reachable slots are marked. In the sweep phase all collectable slots are
/// those which are occupied and have no locks.
/// 
/// If the genric type `T` implements [`Trace`](crate::mem::Trace), then the
/// slot will be traced in the mark phase of the garbage collector. Note that
/// the mutator requires that all managed values must be tracable.
#[derive(Debug)]
#[repr(u16)]
pub enum Slot<T: fmt::Debug> {
    /// An all zero slot. Is used as a placeholder for uninitialized slots.
    Undefined,
    /// A slot which can be filled with a value. The `usize` is the index of the
    /// next free slot in the [`Bucket`](crate::mem::Bucket). The last slot in
    /// the vacant list is marked with the size of the bucket
    /// [`Bucket::N`](crate::mem::Bucket).
    Vacant(usize),
    /// A slot which is occupied by a value. The `usize` is the number of locks
    /// on the slot. A lock-count of zero does not mean that the slot is
    /// collectable. It only means that the slot is not locked. But it still
    /// can be reachable from other slots.
    Occupied(u32, T),
    /// A slot which is occupied by a value and is marked to be in use. The
    /// `usize` is the number of locks on the slot. In the sweep phase of the
    /// garbage collector, all marked slots are unmarked.
    Marked(u32, T),
    /// A slot which is dropped. This is used to prevent invalid memory access
    /// and memory leaks. A dropped slot is not reachable from other slots and
    /// is not collectable. An unlock does nothing on a dropped slot. This state
    /// is only used when the [`Mutator`](crate::mem::Mutator) is dropped.
    Dropped,
}

impl<T: fmt::Debug> Slot<T> {
    /// Marks the slot as reachable. This is used in the mark phase of the
    /// garbage collector. Only a slot which is occupied can be marked.
    /// 
    /// All slots which have a lock will be marked by the garbage collector.
    /// Locked slots are the root values of the garbage collector.
    pub fn mark(&mut self) {
        *self = match std::mem::replace(self, Slot::Undefined) {
            Self::Occupied(locks, value) => {
                log::debug!(target: TARGET,
                    "Slot: marking at {:p} with {} locks",
                    &value as *const T, locks);
                Self::Marked(locks, value)
            }
            slot => slot,
        }
    }

    /// Unmarks the slot. This is used in the sweep phase of the garbage
    /// collector. Only a slot which is marked can be unmarked.
    pub fn unmark(&mut self) {
        *self = match std::mem::replace(self, Self::Undefined) {
            Self::Marked(locks, value) => {
                log::debug!(target: TARGET,
                    "Slot: unmarking at {:p} with {} locks",
                    &value as *const T, locks);
                Self::Occupied(locks, value)
            }
            slot => slot,
        }
    }

    /// Locks the slot. This is used to prevent the garbage collector from
    /// collecting the slot while it is in use. Only a slot which is occupied
    /// or marked can be locked.
    /// 
    /// # Panics
    /// 
    /// Panics if the lock-count overflows or if the slot is undefined or
    /// vacant. If this happens, then there is a bug in the garbage collector.
    pub fn lock(&mut self) {
        let ptr = self as *mut Self;

        match self {
            Self::Marked(locks, _) | Self::Occupied(locks, _) => {
                log::debug!(target: TARGET,
                    "Slot: locking {:p} with {} locks",
                    ptr, locks);
                *locks = locks.checked_add(1).expect("lock overflow");
            }
            Self::Dropped => panic!("attempt to lock dropped slot"),
            Self::Undefined => panic!("attempt to lock undefined slot"),
            Self::Vacant(_) => panic!("attempt to lock vacant slot"),
        }
    }

    /// Unlocks the slot. This is used to allow the garbage collector to
    /// collect the slot. A slot is only then collectable if it is not locked or
    /// not reachable from other slots. Only a slot which is occupied or marked
    /// can be unlocked.
    /// 
    /// # Panics
    /// 
    /// Panics if the lock-count underflows or if the slot is undefined or
    /// vacant. If this happens, then there is a bug in the garbage collector.
    pub fn unlock(&mut self) {
        let ptr = self as *mut Self;

        match self {
            Self::Marked(locks, _) | Self::Occupied(locks, _) => {
                log::debug!(target: TARGET,
                    "Slot: unlocking {:p} with {} locks",
                    ptr, locks);
                *locks = locks.checked_sub(1).expect("null-unlock");
            }
            Self::Dropped => (),
            Self::Undefined => panic!("attempt to unlock undefined slot"),
            Self::Vacant(_) => panic!("attempt to unlock vacant slot"),
        }
    }

    /// Returns a reference to the value of the slot if it is occupied or
    /// marked.
    #[inline]
    pub const fn value(&self) -> Option<&T> {
        match self {
            Self::Marked(_, value) => Some(value),
            Self::Occupied(_, value) => Some(value),
            _ => None,
        }
    }

    /// Returns a mutable reference to the value of the slot if it is occupied
    /// or marked.
    #[inline]
    pub fn value_mut(&mut self) -> Option<&mut T> {
        match self {
            Self::Marked(_, value) => Some(value),
            Self::Occupied(_, value) => Some(value),
            _ => None,
        }
    }

    /// Returns `true` if the slot is undefined.
    #[inline]
    pub const fn is_undefined(&self) -> bool {
        matches!(self, Self::Undefined)
    }

    /// Returns `true` if the slot is vacant.
    #[inline]
    pub const fn is_vacant(&self) -> bool {
        matches!(self, Self::Vacant(_))
    }

    /// Returns `true` if the slot is occupied or marked.
    #[inline]
    pub const fn is_occupied(&self) -> bool {
        matches!(self, Self::Occupied(_, _) | Self::Marked(_, _))
    }

    /// Returns `true` if the slot is marked.
    #[inline]
    pub const fn is_marked(&self) -> bool {
        matches!(self, Self::Marked(_, _))
    }

    /// Returns `true` if the slot is locked. Only a slot which is occupied or
    /// marked can be locked.
    #[inline]
    pub const fn is_locked(&self) -> bool {
        matches!(self, Self::Occupied(locks, _) | Self::Marked(locks, _) if *locks > 0)
    }

    /// Returns `true` if the slot is collectable. Only a slot which is
    /// occupied and has no locks can be collected. In the sweep phase of the
    /// garbage collector this is true for all slots which where not reached by
    /// the mark phase.
    #[inline]
    pub const fn is_collectable(&self) -> bool {
        matches!(self, Self::Occupied(locks, _) if *locks == 0)
    }
}

impl<'a, T: Trace<'a> + fmt::Debug> Trace<'a> for Slot<T> {
    #[inline]
    fn trace(&self, traced: &mut Vec<Root<'a>>) {
        if let Some(value) = self.value() {
            value.trace(traced)
        }
    }
}

impl<T: fmt::Debug> Default for Slot<T> {
    /// Returns `Slot::Undefined`.
    #[inline]
    fn default() -> Self {
        Self::Undefined
    }
}

impl<T: PartialEq + fmt::Debug> PartialEq for Slot<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Undefined, Self::Undefined) => true,
            (Self::Vacant(a), Self::Vacant(b)) => a == b,
            (Self::Occupied(_, a), Self::Occupied(_, b)) => a == b,
            (Self::Marked(_, a), Self::Marked(_, b)) => a == b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display() {
        assert_eq!(format!("{:?}", Slot::<i32>::default()), "Undefined");
        assert_eq!(format!("{:?}", Slot::<i32>::Vacant(0)), "Vacant(0)");
        assert_eq!(format!("{:?}", Slot::Occupied(0, 42)), "Occupied(0, 42)");
        assert_eq!(format!("{:?}", Slot::Marked(0, 42)), "Marked(0, 42)");
    }

    #[test]
    fn slot() {
        let mut slot = Slot::<u32>::default();
        assert!(slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(!slot.is_occupied());
        assert!(!slot.is_marked());
        assert!(!slot.is_locked());
        assert!(!slot.is_collectable());

        slot = Slot::Vacant(0);
        assert!(!slot.is_undefined());
        assert!(slot.is_vacant());
        assert!(!slot.is_occupied());
        assert!(!slot.is_marked());
        assert!(!slot.is_locked());
        assert!(!slot.is_collectable());

        slot = Slot::Occupied(0, 42);
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(!slot.is_marked());
        assert!(!slot.is_locked());
        assert!(slot.is_collectable());

        slot.lock();
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(!slot.is_marked());
        assert!(slot.is_locked());
        assert!(!slot.is_collectable());

        slot.unlock();
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(!slot.is_marked());
        assert!(!slot.is_locked());
        assert!(slot.is_collectable());

        slot = Slot::Marked(0, 42);
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(slot.is_marked());
        assert!(!slot.is_locked());
        assert!(!slot.is_collectable());

        slot.lock();
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(slot.is_marked());
        assert!(slot.is_locked());
        assert!(!slot.is_collectable());

        slot.unlock();
        assert!(!slot.is_undefined());
        assert!(!slot.is_vacant());
        assert!(slot.is_occupied());
        assert!(slot.is_marked());
        assert!(!slot.is_locked());
        assert!(!slot.is_collectable());
    }
}
