#![allow(dead_code)]

use std::any::Any;
use std::sync::{Arc, Mutex, mpsc};
use std::thread;

// 1

pub trait Shape {
    fn area(&self) -> f64;
}

#[derive(Debug, Clone, Copy)]
pub struct Rect {
    pub w: f64,
    pub h: f64,
}
#[derive(Debug, Clone, Copy)]
pub struct Circle {
    pub r: f64,
}

impl Shape for Rect {
    fn area(&self) -> f64 {
        self.w * self.h
    }
}
impl Shape for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.r * self.r
    }
}

/// Funkcja sumuje pola figur.
pub fn total_area_generic<T: Shape>(items: &[T]) -> f64 {
    items.iter().map(|s| s.area()).sum()
}

/// Funkcja sumuje pola figór przekazanych przez slice boxów z dynamicznymi implementorami `Shape`.
/// Uzupełnij brakujący argument do funkcji.
pub fn total_area_dyn(items: &[Box<dyn Shape>]) -> f64 {
    items.iter().map(|s| s.area()).sum()
}

// 2

/// Następujący trait nie jest object safe. Dlaczego?
/// Zmień go tak, by był, potencjalnie rezygnując z jego ogólności.
/// Następnie zaimplementuj go dla `Add`, `Mul` i zaimplementuj funkcję `apply_all_dyn`.
pub trait Transform {
    fn name(&self) -> &str;
    fn apply(&self, x: f64) -> f64;
}

// Transform dodaje `k` do `x`. `name` zwraca "add".
pub struct Add {
    pub k: f64,
}

impl Transform for Add {
    fn name(&self) -> &str {
        "add"
    }

    fn apply(&self, x: f64) -> f64 {
        self.k + x
    }
}

// Transform mnoży `x` przez `k`. `name` zwraca "mul".
pub struct Mul {
    pub k: f64,
}

impl Transform for Mul {
    fn name(&self) -> &str {
        "mul"
    }

    fn apply(&self, x: f64) -> f64 {
        self.k * x
    }
}

// Funkcja aplikuje `t` do każdego elementu `seq`
pub fn apply_all_dyn(seq: &mut [f64], t: &dyn Transform) {
    for s in seq {
        *s = t.apply(*s)
    }
}

// 4

// Funkcja sumuje wszystkie wartości typu i32 w `boxes`
pub fn sum_all_i32(boxes: &[Box<dyn Any>]) -> i32 {
    boxes.iter().filter_map(|b| b.downcast_ref::<i32>()).sum()
}

// 5

// Funkcja uruchamia wątek, który sumuje elementy w `v`, a następnie wyświetla wynik sumowania bezpośrednio
// w funkcji (tzn. nie w uruchomionym wątku). Użyj `std::thread::spawn`.
pub fn spawn_sum(v: Vec<i32>) {
    let sum = thread::spawn(move || v.iter().sum::<i32>()).join();
    match sum {
        Ok(sum) => println!("sum: {sum}"),
        Err(err) => eprintln!("failed to join thread: {err:?}"),
    }
}

// 6

// Funkcja uruchamia osobny wątek dla każdego elementu `parts`.
// Zawartość każdego z tych elementów jest sumowana i zwracana z wątku.
// Główny wątek następnie sumuje uzyskane sumu częściowe.
// Użyj `std::thread::scope`.
pub fn sum_scoped(parts: &[&[i32]]) -> i32 {
    thread::scope(|s| {
        let sums = parts
            .iter()
            .map(|part| s.spawn(|| part.iter().sum::<i32>()));
        sums.map(|t| t.join()).filter_map(|join| join.ok()).sum()
    })
}

// 7
// Funkcja tworzy muteks z liczbą 0.
// Uruchamia `n_threads` wątków. Każdy z nich wykonuje `iters` iteracji.
// W każdej iteracji mutex jest blokowany i do liczy dodawane jest 1.
// Wątki o indeksach parzystych panikują przy ostatniej iteracji.
// Spowoduje to zatrucie muteksu. Odblokowania go powinny więc odpowiednio obsłużyć zatruty mutex.
// Po uruchomieniu wątków funkcja wykonuje na wszystkich `join`,
// wypisując indeksy wątków, które spanikowały (używając wyniku `join`).
// Na koniec funkcja zwraca wartość zawartą w liczniku w mutexie.
pub fn parallel_increment(n_threads: usize, iters: usize) -> i64 {
    let m = Arc::new(Mutex::new(0));
    let handles = (0..n_threads).map(|id| {
        let m = m.clone();
        thread::spawn(move || {
            for i in 0..iters {
                if id.is_multiple_of(2) && i == iters - 1 {
                    panic!("last iter")
                }
                let mut val_mut = match m.lock() {
                    Ok(v) => v,
                    Err(e) => e.into_inner(),
                };
                *val_mut += 1;
            }
        })
    });
    for (id, h) in handles.enumerate() {
        match h.join() {
            Ok(_) => {}
            Err(e) => eprintln!("failed to join thread #{id} ({e:?})"),
        }
    }
    match m.lock() {
        Ok(val) => *val,
        Err(err) => *err.into_inner(),
    }
}

// 8
// Funkcja tworzy kanał `mpsc` i `threads` wątków.
// Kanał przyjmuje wiadomości typu `u32`.
// Każdy wątek wysyła do kanału po kolei liczby 1, 2, ..., n.
// Osobny wątek (nie wątek główny!) sumuje wszystkie wartości w kanale.
// Funkcja zwraca sumę obliczoną w tym wątku.
pub fn pipeline(n: i32, threads: usize) -> i32 {
    let (tx, rx) = mpsc::channel::<i32>();
    let tx_handles = (0..threads).map(|_| {
        let tx = tx.clone();

        thread::spawn(move || {
            for i in 1..=n {
                if let Err(e) = tx.send(i) {
                    eprintln!("failed to send number: {e:?}");
                }
            }
        })
    });
    let rx_thread = thread::spawn(move || {
        let mut count = 0;
        while let Ok(value) = rx.recv() {
            count += value;
        }
        count
    });

    for h in tx_handles {
        if let Err(e) = h.join() {
            eprintln!("failed to join thread: {e:?}");
        }
    }
    drop(tx);

    match rx_thread.join() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("failed to join thread: {e:?}");
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn approx_eq(a: f64, b: f64, eps: f64) -> bool {
        (a - b).abs() <= eps
    }

    #[test]
    fn test_total_area_generic_rects() {
        let rects = vec![Rect { w: 3.0, h: 4.0 }, Rect { w: 2.5, h: 1.2 }];
        let total = total_area_generic(&rects);
        assert!(approx_eq(total, 15.0, 1e-12));
    }

    #[test]
    fn test_total_area_generic_circles() {
        let circles = vec![Circle { r: 1.0 }, Circle { r: 2.0 }];
        let total = total_area_generic(&circles);
        let expected = std::f64::consts::PI * (1.0 + 4.0);
        assert!(approx_eq(total, expected, 1e-12));
    }

    #[test]
    fn test_total_area_dyn_mixed() {
        let items: Vec<Box<dyn Shape>> = vec![
            Box::new(Rect { w: 3.0, h: 4.0 }),
            Box::new(Circle { r: 1.0 }),
        ];
        let total = total_area_dyn(&items);
        let expected = 12.0 + std::f64::consts::PI * 1.0;
        assert!(approx_eq(total, expected, 1e-12));
    }

    #[test]
    fn test_transform_add_and_apply_all_dyn() {
        // After making Transform object-safe with apply(&self, f64) -> f64
        let mut seq = [1.0, 2.0, -3.0];
        let add = Add { k: 2.0 };
        apply_all_dyn(&mut seq, &add);
        assert!(approx_eq(seq[0], 3.0, 1e-12));
        assert!(approx_eq(seq[1], 4.0, 1e-12));
        assert!(approx_eq(seq[2], -1.0, 1e-12));
        assert_eq!(add.name(), "add");
    }

    #[test]
    fn test_transform_mul_and_apply_all_dyn() {
        let mut seq = [1.5, -2.0, 0.0];
        let mul = Mul { k: -2.0 };
        apply_all_dyn(&mut seq, &mul);
        assert!(approx_eq(seq[0], -3.0, 1e-12));
        assert!(approx_eq(seq[1], 4.0, 1e-12));
        assert!(approx_eq(seq[2], 0.0, 1e-12));
        assert_eq!(mul.name(), "mul");
    }

    #[test]
    fn test_sum_all_i32_mixed_any() {
        let boxes: Vec<Box<dyn Any>> = vec![
            Box::new(5_i32),
            Box::new(String::from("x")),
            Box::new(7_i32),
            Box::new(3_i64),
        ];
        let s = sum_all_i32(&boxes);
        assert_eq!(s, 12);
    }

    #[test]
    fn test_spawn_sum_large() {
        // We cannot easily assert stdout in unit tests without extra crates; just ensure it completes.
        let v: Vec<i32> = (1..=10_000).collect();
        spawn_sum(v);
    }

    #[test]
    fn test_sum_scoped_parts() {
        let a = [1, 2, 3];
        let b = [10];
        let c = [-5, 0];
        let parts: Vec<&[i32]> = vec![&a, &b, &c];
        let s = sum_scoped(&parts);
        assert_eq!(s, 11);
    }

    #[test]
    fn test_parallel_increment_poison_and_total() {
        // Even-index threads panic at last iteration; they miss exactly one increment.
        let total = parallel_increment(4, 5);
        // Expected increments = n_threads * iters - number_of_even_threads
        let expected = (4 * 5 - 2) as i64;
        assert_eq!(total, expected);
    }

    #[test]
    fn test_pipeline_small() {
        let s = pipeline(3, 2);
        assert_eq!(s, 12); // 2 * (1 + 2 + 3)
    }

    #[test]
    fn test_pipeline_medium() {
        let s = pipeline(10, 3);
        assert_eq!(s, 165); // 3 * 55
    }
}
