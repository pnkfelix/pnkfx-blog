#include <stdint.h>
#include <stdlib.h>
#include <CoreServices/CoreServices.h>
#include <mach/mach_time.h>

static uint32_t fib(uint32_t n) {
    return (n < 2) ? 1 : (fib(n-1) + fib(n-2));
}

template<typename T, typename U> struct pair {
    T t;
    U u;
};

template<typename T>
static pair<T, uint64_t> timeit(T thunk()) {
    uint64_t start = mach_absolute_time();
    T result = thunk();
    uint64_t finis = mach_absolute_time();
    uint64_t elapsed = finis - start;
    Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime*)&elapsed);
    pair<T, uint64_t> p;
    p.t = result;
    p.u = *(uint64_t*)&elapsedNano;
    return p;
}

static void report(const char *expr, uint32_t result, uint64_t delta) {
    uint32_t dsecs = delta / 1000000000;
    uint32_t dnanos = uint32_t(delta % 1000000000);
    uint32_t dsuffix = dnanos / 10000000;
    printf("%s: %10u elapsed: %u.%02us\n", expr, result, dsecs, dsuffix);
}

static uint32_t fib10() { return fib(10); }
static uint32_t fib20() { return fib(20); }
static uint32_t fib30() { return fib(30); }
static uint32_t fib40() { return fib(40); }
static uint32_t fib41() { return fib(41); }
static uint32_t fib42() { return fib(42); }
static uint32_t fib43() { return fib(43); }

int main() {
    pair<uint32_t, uint64_t> rd1 = timeit(fib10);
    pair<uint32_t, uint64_t> rd2 = timeit(fib20);
    pair<uint32_t, uint64_t> rd3 = timeit(fib30);
    pair<uint32_t, uint64_t> rd4 = timeit(fib40);
    pair<uint32_t, uint64_t> rd5 = timeit(fib41);
    pair<uint32_t, uint64_t> rd6 = timeit(fib42);
    pair<uint32_t, uint64_t> rd7 = timeit(fib43);
    report("fib(10)", rd1.t, rd1.u);
    report("fib(20)", rd2.t, rd2.u);
    report("fib(30)", rd3.t, rd3.u);
    report("fib(40)", rd4.t, rd4.u);
    report("fib(41)", rd5.t, rd5.u);
    report("fib(42)", rd6.t, rd6.u);
    report("fib(43)", rd7.t, rd7.u);
    return 0;
}
