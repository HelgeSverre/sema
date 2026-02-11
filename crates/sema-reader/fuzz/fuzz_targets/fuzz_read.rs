#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        // Must not panic — Ok or Err are both fine
        let _ = sema_reader::read(input);
    }
});
