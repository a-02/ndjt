#[no_mangle]
pub extern "C" fn rust_wrapper_add(x: u64, y: u64) -> u64 {
    x + y
}
