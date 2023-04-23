pub fn get_random() -> usize {
    let mut buffer = [0; 16];
    getrandom::getrandom(&mut buffer).unwrap();

    let mut result = 0;
    for i in 0..std::mem::size_of::<usize>() {
        result |= (buffer[i] as usize) << (i * 8);
    }
    result
}

pub fn get_random_ranged(min: usize, max: usize) -> usize {
    get_random() % (max - min) + min
}

pub fn shuffle<T>(slice: &mut [T]) {
    for i in 0..slice.len() {
        let j = get_random_ranged(i, slice.len());
        slice.swap(i, j);
    }
}
