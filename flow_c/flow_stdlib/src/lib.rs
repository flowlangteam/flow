// Flow Standard Library
// Core functionality for I/O, memory, strings, and more

// === I/O Functions ===

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_print(ptr: *const u8, len: usize) {
    unsafe {
        let slice = std::slice::from_raw_parts(ptr, len);
        if let Ok(s) = std::str::from_utf8(slice) {
            print!("{}", s);
        }
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_println(ptr: *const u8, len: usize) {
    unsafe {
        let slice = std::slice::from_raw_parts(ptr, len);
        if let Ok(s) = std::str::from_utf8(slice) {
            println!("{}", s);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_i64(value: i64) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_i32(value: i32) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_f64(value: f64) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_f32(value: f32) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_bool(value: bool) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_char(value: u32) {
    if let Some(c) = char::from_u32(value) {
        print!("{}", c);
    }
}

// Input functions
#[unsafe(no_mangle)]
pub extern "C" fn flow_read_line() -> *mut FlowString {
    use std::io::{self, BufRead};

    let mut input = String::new();
    if io::stdin().lock().read_line(&mut input).is_ok() {
        // Remove trailing newline
        if input.ends_with('\n') {
            input.pop();
            if input.ends_with('\r') {
                input.pop();
            }
        }

        let bytes = input.into_bytes();
        let len = bytes.len();
        let ptr = bytes.as_ptr();
        std::mem::forget(bytes);

        Box::into_raw(Box::new(FlowString {
            data: ptr,
            len,
            capacity: len,
        }))
    } else {
        std::ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_read_i64() -> i64 {
    use std::io::{self, BufRead};

    let mut input = String::new();
    if io::stdin().lock().read_line(&mut input).is_ok() {
        input.trim().parse().unwrap_or(0)
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_read_f64() -> f64 {
    use std::io::{self, BufRead};

    let mut input = String::new();
    if io::stdin().lock().read_line(&mut input).is_ok() {
        input.trim().parse().unwrap_or(0.0)
    } else {
        0.0
    }
}

// === Memory Management ===

#[repr(C)]
pub struct FlowString {
    pub data: *const u8,
    pub len: usize,
    pub capacity: usize,
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_string_new(data: *const u8, len: usize) -> *mut FlowString {
    unsafe {
        let slice = std::slice::from_raw_parts(data, len);
        let owned = slice.to_vec();
        let capacity = owned.capacity();
        let ptr = owned.as_ptr();
        std::mem::forget(owned);

        Box::into_raw(Box::new(FlowString {
            data: ptr,
            len,
            capacity,
        }))
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_string_free(s: *mut FlowString) {
    if !s.is_null() {
        unsafe {
            let string = Box::from_raw(s);
            let _ = Vec::from_raw_parts(string.data as *mut u8, string.len, string.capacity);
        }
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_string_concat(
    a: *const FlowString,
    b: *const FlowString,
) -> *mut FlowString {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let a_slice = std::slice::from_raw_parts((*a).data, (*a).len);
        let b_slice = std::slice::from_raw_parts((*b).data, (*b).len);

        let mut result = Vec::with_capacity(a_slice.len() + b_slice.len());
        result.extend_from_slice(a_slice);
        result.extend_from_slice(b_slice);

        let len = result.len();
        let capacity = result.capacity();
        let ptr = result.as_ptr();
        std::mem::forget(result);

        Box::into_raw(Box::new(FlowString {
            data: ptr,
            len,
            capacity,
        }))
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_string_len(s: *const FlowString) -> usize {
    if s.is_null() { 0 } else { unsafe { (*s).len } }
}

// Generic memory allocation
#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_alloc(size: usize, align: usize) -> *mut u8 {
    unsafe {
        let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
        std::alloc::alloc(layout)
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_alloc_zeroed(size: usize, align: usize) -> *mut u8 {
    unsafe {
        let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
        std::alloc::alloc_zeroed(layout)
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_free(ptr: *mut u8, size: usize, align: usize) {
    if !ptr.is_null() {
        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
            std::alloc::dealloc(ptr, layout);
        }
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_realloc(
    ptr: *mut u8,
    old_size: usize,
    new_size: usize,
    align: usize,
) -> *mut u8 {
    if ptr.is_null() {
        return unsafe { flow_alloc(new_size, align) };
    }

    unsafe {
        let old_layout = std::alloc::Layout::from_size_align_unchecked(old_size, align);
        std::alloc::realloc(ptr, old_layout, new_size)
    }
}

// === Array Operations ===

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_array_new(elem_size: usize, capacity: usize) -> *mut FlowArray {
    let total_size = elem_size * capacity;
    let data = unsafe { flow_alloc_zeroed(total_size, 8) };

    Box::into_raw(Box::new(FlowArray {
        data,
        len: 0,
        capacity,
        elem_size,
    }))
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_array_free(arr: *mut FlowArray) {
    if !arr.is_null() {
        unsafe {
            let array = Box::from_raw(arr);
            flow_free(array.data, array.capacity * array.elem_size, 8);
        }
    }
}

#[repr(C)]
pub struct FlowArray {
    pub data: *mut u8,
    pub len: usize,
    pub capacity: usize,
    pub elem_size: usize,
}

// === Math Functions ===

#[unsafe(no_mangle)]
pub extern "C" fn flow_abs_i64(x: i64) -> i64 {
    x.abs()
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_abs_f64(x: f64) -> f64 {
    x.abs()
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_min_i64(a: i64, b: i64) -> i64 {
    a.min(b)
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_max_i64(a: i64, b: i64) -> i64 {
    a.max(b)
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_min_f64(a: f64, b: f64) -> f64 {
    a.min(b)
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_max_f64(a: f64, b: f64) -> f64 {
    a.max(b)
}

// === Utility Functions ===

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_panic(msg: *const u8, len: usize) -> ! {
    unsafe {
        let slice = std::slice::from_raw_parts(msg, len);
        if let Ok(s) = std::str::from_utf8(slice) {
            panic!("{}", s);
        } else {
            panic!("Flow panic!");
        }
    }
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn flow_assert(condition: bool, msg: *const u8, len: usize) {
    if !condition {
        unsafe {
            flow_panic(msg, len);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_operations() {
        let data = b"Hello";
        unsafe {
            let s = flow_string_new(data.as_ptr(), data.len());
            assert_eq!(flow_string_len(s), 5);
            flow_string_free(s);
        }
    }

    #[test]
    fn test_math_functions() {
        assert_eq!(flow_abs_i64(-42), 42);
        assert_eq!(flow_min_i64(10, 20), 10);
        assert_eq!(flow_max_i64(10, 20), 20);
    }
}
