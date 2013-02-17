test-all: test-larc test-rust test-cxx

test-larc: fib.fasl
	bash -c "time larceny -nobanner -- $<"

test-rust: rust-fib
	bash -c "time ./$<"

test-cxx: cxx-fib
	bash -c "time ./$<"

fib.fasl: fib.sch
	bash -c "time larceny -- -e '(compile-file \"$<\")' -e '(exit)'"

rust-fib: fib.rs
	bash -c "time rustc -o $@ $<"

cxx-fib: fib.cpp
	bash -c "time g++ -framework CoreServices -O2 -o $@ $<"
