test-all: test-larc test-rust test-cxx

clean:
	rm -f fib.fasl unsafe-fib.fasl rust-fib cxx-fib
	rm -rf rust-fib.dSYM

test-larc: fib.fasl unsafe-fib.fasl
	bash -c "time larceny -unsafe -nobanner -- unsafe-$<"
	bash -c "time larceny -nobanner -- $<"

test-rust: rust-fib
	bash -c "time ./$<"

test-cxx: cxx-fib
	bash -c "time ./$<"

unsafe-%.fasl: %.sch
	bash -c "time larceny -- -e '(compiler-switches (quote fast-unsafe))' -e '(compile-file \"$<\" \"$@\")' -e '(exit)'"

%.fasl: %.sch
	bash -c "time larceny -- -e '(compiler-switches (quote default))' -e '(compile-file \"$<\" \"$@\")' -e '(exit)'"

rust-%: %.rs
	bash -c "time rustc -o $@ $<"

cxx-%: %.cpp
	bash -c "time g++ -framework CoreServices -O2 -o $@ $<"
