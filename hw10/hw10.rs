pub fn find_min(a: &[i32]) -> i32 {
    *a.iter().min().unwrap()
}

pub fn swap(a: &mut i32, b: &mut i32) {
    std::mem::swap(a, b);
}

