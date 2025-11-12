// Flow Runtime Library
// Provides built-in functions and runtime support

use std::alloc::{Layout, alloc, dealloc};

// String operations
#[unsafe(no_mangle)]
pub extern "C" fn flow_string_new(data: *const u8, len: usize) -> *mut FlowString {
    unsafe {
        let layout = Layout::new::<FlowString>();
        let ptr = alloc(layout) as *mut FlowString;

        let string_data = Vec::from_raw_parts(data as *mut u8, len, len);
        let boxed = Box::new(string_data.into_boxed_slice());

        (*ptr).data = Box::into_raw(boxed) as *const u8;
        (*ptr).len = len;

        ptr
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_string_free(s: *mut FlowString) {
    unsafe {
        if !s.is_null() {
            let layout = Layout::new::<FlowString>();
            dealloc(s as *mut u8, layout);
        }
    }
}

#[repr(C)]
pub struct FlowString {
    pub data: *const u8,
    pub len: usize,
}

// Print functions
#[unsafe(no_mangle)]
pub extern "C" fn flow_print_int(value: i64) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_float(value: f64) {
    println!("{}", value);
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_bool(value: i8) {
    println!("{}", if value != 0 { "true" } else { "false" });
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_print_string(s: *const FlowString) {
    unsafe {
        if !s.is_null() {
            let slice = std::slice::from_raw_parts((*s).data, (*s).len);
            let string = std::str::from_utf8_unchecked(slice);
            println!("{}", string);
        }
    }
}

// Memory allocation for structs
#[unsafe(no_mangle)]
pub extern "C" fn flow_alloc(size: usize) -> *mut u8 {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        alloc(layout)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn flow_free(ptr: *mut u8, size: usize) {
    unsafe {
        if !ptr.is_null() {
            let layout = Layout::from_size_align_unchecked(size, 8);
            dealloc(ptr, layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_creation() {
        let data = b"hello";
        let s = flow_string_new(data.as_ptr(), data.len());
        unsafe {
            assert_eq!((*s).len, 5);
            flow_string_free(s);
        }
    }
}
