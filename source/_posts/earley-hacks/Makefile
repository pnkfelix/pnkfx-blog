test-one: test-larc
test-all: test-larc test-rust test-cxx test-clj test-node

clean:
	rm -f *.fasl rust-earley cxx-earley
	rm -rf classes
	rm -rf rust-earley.dSYM

CLJJAR:=$(shell clj -e '(-> clojure.lang.Atom (.getProtectionDomain) (.getCodeSource) (.getLocation) (.getPath))')

test-node: earley.js
	bash -c "time node $<"

test-clj: classes/earley.class
	bash -c "time java -cp $(CLJJAR):classes earley"

test-larc: earley.fasl unsafe-earley.fasl
	bash -c "time larceny -unsafe -nobanner -- unsafe-$<"
	bash -c "time larceny -nobanner -- $<"

test-rust: rust-earley
	bash -c "time ./$<"

test-cxx: cxx-earley
	bash -c "time ./$<"

classes/earley.class: earley.clj classes
	bash -c "time clj -e '(compile (quote earley))'"

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
