/// Provides random number generation using the [`getrandom`](../../getrandom/index.html) crate
///
/// # Examples
///
/// ```rust
/// use graph::rrand;
///
/// let random = rrand::get_random();
/// let random_ranged = rrand::get_random_ranged(0, 10);
/// ```
pub fn get_random() -> usize {
    let mut buffer = vec![0; std::mem::size_of::<usize>()];
    getrandom::getrandom(&mut buffer).unwrap();

    let mut result = 0;
    for (i, byte) in buffer.iter().copied().enumerate() {
        result |= (byte as usize) << (i * 8);
    }
    result
}

/// Gets a random number generator ([`SmallRng`](../../rand/rngs/struct.SmallRng.html))
/// using a random number from the [`getrandom`](../../getrandom/index.html) crate
/// as seed.
pub fn get_rng() -> rand::rngs::SmallRng {
    use rand::rngs::SmallRng;
    use rand::SeedableRng;

    SmallRng::seed_from_u64(get_random() as u64)
}
