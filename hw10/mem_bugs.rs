pub fn test1() {
    let ptr = Box::new(10);
    println!("Value: {}", *ptr); 
}

pub fn foo(t: &i32) -> &i32 {
    t 
}

pub fn test2() {
    let mut arr = [1, 2, 3, 4, 5];

    println!("Array: {:?}", arr);
}

pub fn test3() {
    let ptr = Box::new(20);
    println!("Value: {}", *ptr); 
}