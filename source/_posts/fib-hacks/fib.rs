fn fib(n:uint) -> uint {
    if n < 2 { 1 } else { fib(n - 1) + fib(n - 2) }
}

fn timeit<T>(thunk:fn() -> T) -> (T,u64) {
    use std::time::get_time;
    use std::time::precise_time_ns;
    let start = precise_time_ns();
    let result = thunk();
    let finis = precise_time_ns();
    let delta = finis - start;
    return (result, delta);
}

fn report<T>(expr:&str, result:T, delta:u64) {
    let dsecs = delta / 1_000_000_000;
    let dnanos = (delta % 1_000_000_000) as uint;
    let dsuffix = (dnanos / 10_000_000);
    io::println(fmt!("%s: %10? elapsed: %?.%02us", expr, result, dsecs, dsuffix));

}

fn fib10() -> uint { fib(10) }
fn fib20() -> uint { fib(20) }
fn fib30() -> uint { fib(30) }
fn fib40() -> uint { fib(40) }
fn fib41() -> uint { fib(41) }
fn fib42() -> uint { fib(42) }

extern mod std;
fn main () {
    let (result1, delta1) = timeit(fib10);
    let (result2, delta2) = timeit(fib20);
    let (result3, delta3) = timeit(fib30);
    let (result4, delta4) = timeit(fib40);
    let (result5, delta5) = timeit(fib41);
    let (result6, delta6) = timeit(fib42);
    report("fib(10)", result1, delta1);
    report("fib(20)", result2, delta2);
    report("fib(30)", result3, delta3);
    report("fib(40)", result4, delta4);
    report("fib(41)", result5, delta5);
    report("fib(42)", result6, delta6);
}
