test-one: test-rust
test-all: test-larc test-rust test-cxx test-clj test-node

clean:
	rm -f fib.fasl unsafe-fib.fasl rust-fib cxx-fib
	rm -rf classes
	rm -rf rust-fib.dSYM

CLJJAR:=$(shell clj -e '(-> clojure.lang.Atom (.getProtectionDomain) (.getCodeSource) (.getLocation) (.getPath))')

test-node: fib.js
	bash -c "time node $<"

test-clj: classes/fib.class
	bash -c "time java -cp $(CLJJAR):classes fib"

test-larc: fib.fasl unsafe-fib.fasl
	bash -c "time larceny -unsafe -nobanner -- unsafe-$<"
	bash -c "time larceny -nobanner -- $<"

test-rust: rust-fib
	bash -c "time ./$<"

test-cxx: cxx-fib
	bash -c "time ./$<"

classes/fib.class: fib.clj classes
	bash -c "time clj -e '(compile (quote fib))'"

classes:
	mkdir classes

unsafe-%.fasl: %.sch
	bash -c "time larceny -- -e '(compiler-switches (quote fast-unsafe))' -e '(compile-file \"$<\" \"$@\")' -e '(exit)'"

%.fasl: %.sch
	bash -c "time larceny -- -e '(compiler-switches (quote default))' -e '(compile-file \"$<\" \"$@\")' -e '(exit)'"

rust-%: %.rs
	bash -c "time rustc -o $@ $<"

cxx-%: %.cpp
	bash -c "time g++ -framework CoreServices -O2 -o $@ $<"
