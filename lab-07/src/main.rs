use std::{
    borrow::Cow,
    cell::Cell,
    collections::VecDeque,
    ops::{Deref, DerefMut},
};

struct AustroHungarianGreeter {
    call_count: Cell<usize>,
    greetings: [&'static str; 3],
}

impl AustroHungarianGreeter {
    fn greet(&self) -> &'static str {
        self.call_count.update(|prev| prev + 1);
        let idx = (self.call_count.get() - 1) % 3;
        self.greetings[idx]
    }
}

impl Default for AustroHungarianGreeter {
    fn default() -> Self {
        Self {
            call_count: Cell::new(0),
            greetings: [
                "Es lebe der Kaiser!",
                "Möge uns der Kaiser schützen!",
                "Éljen Ferenc József császár!",
            ],
        }
    }
}

impl Drop for AustroHungarianGreeter {
    fn drop(&mut self) {
        println!("Ich habe {} mal gegrüßt", self.call_count.get());
    }
}

#[derive(Debug)]
enum HeapOrStack<T> {
    Stack(T),
    Heap(Box<T>),
}

impl<T> Deref for HeapOrStack<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            HeapOrStack::Stack(t) => &t,
            HeapOrStack::Heap(t) => t.as_ref(),
        }
    }
}

impl<T> DerefMut for HeapOrStack<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            HeapOrStack::Stack(t) => t,
            HeapOrStack::Heap(t) => t.as_mut(),
        }
    }
}

fn canon_head<'a>(xs: &'a VecDeque<i32>) -> Option<Cow<'a, VecDeque<i32>>> {
    if xs.front().unwrap_or(&1) & 1 == 1 {
        return Some(Cow::Borrowed(xs));
    }
    let mut changed = xs.clone();
    for _ in 0..xs.len() {
        if changed.front().unwrap() & 1 == 1 {
            return Some(Cow::Owned(changed));
        }
        let front = changed.pop_front().unwrap();
        changed.push_back(front);
    }
    None
}

fn main() {
    let greeter = AustroHungarianGreeter::default();
    for _ in 0..10 {
        println!("{}", greeter.greet());
    }
    drop(greeter);

    let mut on_stack = HeapOrStack::Stack(12);

    *on_stack.deref_mut() *= 12;
    println!("on stack: {on_stack:?}");

    let on_heap = HeapOrStack::Heap(Box::new(123));
    let on_heap_plus = *on_heap.deref() + 10;
    println!("on heap plus: {on_heap_plus:?}");

    let mut vec = VecDeque::new();
    for i in 2..=5 {
        vec.push_back(i);
    }
    let wow = canon_head(&vec);
    println!("{wow:?}");
}
