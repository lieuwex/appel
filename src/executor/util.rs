#[macro_export]
macro_rules! collect_point {
    () => {{
        use std::backtrace::Backtrace;

        if cfg!(debug_assertions) {
            let bt = Backtrace::force_capture();
            println!("collect point:\n{}", bt);
        }
    }};
}
