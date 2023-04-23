/// Provides random number generation.
///
/// # Examples
///
/// ```rust
/// use graph::rand;
///
/// let random = rand::get_random();
/// let random_ranged = rand::get_random_ranged(0, 10);
/// ```
pub fn get_random() -> usize {
    let mut buffer = [0; 16];
    getrandom::getrandom(&mut buffer).unwrap();

    let mut result = 0;
    for i in 0..std::mem::size_of::<usize>() {
        result |= (buffer[i] as usize) << (i * 8);
    }
    result
}

/// Returns a random number in the range [min, max).
///
/// # Examples
///
/// ```rust
/// use graph::rand;
///
/// let random_ranged = rand::get_random_ranged(0, 10);
/// assert!(random_ranged < 10);
/// ```
pub fn get_random_ranged(min: usize, max: usize) -> usize {
    get_random() % (max - min) + min
}

/// Shuffles a slice in-place.
pub fn shuffle<T>(slice: &mut [T]) {
    for i in 0..slice.len() {
        slice.swap(i, get_random_ranged(i, slice.len()));
    }
}
