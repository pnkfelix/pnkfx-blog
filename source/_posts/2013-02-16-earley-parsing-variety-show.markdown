---
layout: post
title: "Earley Parsing Variety Show"
date: 2013-02-16 18:06
comments: true
categories: 
---

I have been meaning to explore how one might implement a paritcular
algorithm atop a host of languages that have struck my fancy lately.

In particular, I have wanted to explore Clojure and Rust some more,
and perhaps compare them with e.g. (Larceny) Scheme and (Mozilla)
Javascript.

```scheme
;; fib : Nat -> Nat
(define (fib n)
  (cond ((< n 2) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
```

```javascript
// fib : Nat -> Nat
function fib(n)
  (< n 2) ? 1 : (fib(n - 1) + fib(n - 2))
```

```javascript
// fib : Nat -> Nat
function fib(n) {
  if (< n 2) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}
```

# Reminder of first thought on rust:
# This:
```transcript
fib.rs:7:8: 7:28 error: unresolved name
fib.rs:7     use std::time::get_time;
                 ^~~~~~~~~~~~~~~~~~~~
fib.rs:7:8: 7:28 error: failed to resolve import: std::time::get_time
fib.rs:7     use std::time::get_time;
                 ^~~~~~~~~~~~~~~~~~~~
error: failed to resolve imports
error: aborting due to 3 previous errors
```
# does not help remind me that I need this somewhere:
```rust
extern mod std;
```
# I think the error message could be improved here.
