---
layout: post
title: "An insight regarding DST grammar for Rust"
date: 2014-03-13 07:04
comments: true
categories: rust proglang
---

The Rust team has been discussing incorporating "dynamically-sized
types" into the static semantics for Rust.  Essentially the idea is to
allow code to describe and name static types whose size is only known
at Runtime.  E.g. the integer vector `[int, ..5]` is known at compile
time to have five elements, and is considered (statically) sized,
while the vector `[int]` has unknown size at compile time, and so that
type is called unsized.

## <a id="background_dst">Background: Dynamically Sized Types (DST)</a>

[There](http://smallcultfollowing.com/babysteps/blog/2013/04/30/dynamically-sized-types/)
is [a](http://smallcultfollowing.com/babysteps/blog/2013/06/06/reducing-dst-annotation/)
series [of](http://smallcultfollowing.com/babysteps/blog/2013/11/26/thoughts-on-dst-1/)
blog [posts](http://smallcultfollowing.com/babysteps/blog/2013/11/27/thoughts-on-dst-2)
about [dynamically](http://smallcultfollowing.com/babysteps/blog/2013/11/27/thoughts-on-dst-3/)
sized [types](http://smallcultfollowing.com/babysteps/blog/2013/12/02/thoughts-on-dst-4/)
on [niko's](http://smallcultfollowing.com/babysteps/blog/2014/01/05/dst-take-5/)
blog.
So I will not dive into the details too much here

The main points are that the compiler wants to know whether a type is
meant to always have a static size, or if it can potentially be
unsized.  In a language without type polymorphism, this might be easy
to determine directly from the parsed type expression (such as in the
vector examples I gave at the outset).  But once you add polymorphism,
things get a litle harder for the compiler.

Anyway, the plan drafted in Niko's
[DST, Take 5](http://smallcultfollowing.com/babysteps/blog/2014/01/05/dst-take-5/)
is to add an `unsized` keyword, and then use it as a marker to make
certain spots more general than they are by default.  The reasoning
here is that in the common case, you want a type parameter to
represent a sized type.  (Since there are certain operations you
cannot do with a value of an unsized type, such copying the value into
some other location, the compiler needs to know its size statically so
that it can allocate an appopriate amount of space for it.)

So under that scheme, to write type parameter of most general type,
e.g. for a `struct` definition that ends with an unsized field,
you need to write:
```rust
fn play() { ... }

extern { fn foo() { ... } }

struct Named<unsized T> {
    name: ~str,
    payload: T
}

fn foo<T>(&Named<T>) { ... }

fn bar<unsized T>(&Named<T>) { ... }
```

For defining a trait that can be implemented on any possible type,
including unsized ones, you would need to use the `unsized` keyword
somewhere there as well.  DST take 5 proposed
`trait Foo<unsized Self> : NormalBounds { ... }`
(or `trait Foo : unsized + NormalBounds { ... }`, but this is broken for
various reasons).
I had been suggesting `unsized trait Foo : NormalBounds { ... }`,
which Niko rightly objected to (since it is not the trait that is
unsized, but rather potentially its Self type).
Over the Rust work week last week I suggested
`trait Foo for unsized : NormalBounds` { ... }, which I think is the first
suggestion that Niko and myself could both stomach.  (The reasoning
behind the latter suggestion is that we write `impl Trait for
SelfType`, so it makes sense to put the generalization marker into the
same position, i.e. filling the placeholder in: `Trait for _`.)


Insight for DST
Givens:
* We are subdividing the universe of types into Type = Sized \union Unsized
* We have been concerned about use of the `unsized T` keyword in places because its meaning is "T may be unsized" but it reads like "T *is* unsized"
So, the core of the insight: do not use `unsized` as the keyword here.  Use `type`.  (Which huzzah we already have as a keyword.)  And add the rule, "If you *omit* the word type, that desugars to an implicit Sized bound."
Then `Sized` and `Unsized` really are just trait bounds.  We might not need to add any `unsized` keyword at all.  (Not 100% sure of that one, probably should still reserve it.)
So, here are examples, adapted from Niko's DST take 5.

