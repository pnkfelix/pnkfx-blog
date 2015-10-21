---
layout: post
title: "An insight regarding DST grammar for Rust"
date: 2014-03-13 07:04
comments: true
categories: rust proglang
---

Executive summary: `type` = `unsized` ⊎ `sized`, so we should use
`type` as our generalization marker, not `unsized`.

<!-- more -->

* <a href="#background_dst">Background: Dynamically Sized Types (DST)</a>
* <a href="#the_insight">The Insight: `type` is a better generalization marker</a>
* <a href="#the_examples">Examples ported from DST, Take 5</a>


## <a id="background_dst">Background: Dynamically Sized Types (DST)</a>

The Rust team has been discussing incorporating "dynamically-sized
types" into the static semantics for Rust.  Essentially the idea is to
allow code to describe and name static types whose size is only known
at Runtime.  E.g. the integer vector `[int, ..5]` is known at compile
time to have five elements, and is considered (statically) sized,
while the vector `[int]` has unknown size at compile time, and so that
type is called unsized.

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
["DST, Take 5"](http://smallcultfollowing.com/babysteps/blog/2014/01/05/dst-take-5/)
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
struct Named<unsized T> {
    name: ~str,
    payload: T
}

// Accepts solely *sized* Named<T>.
fn foo<T>(&Named<T>) { ... }

// Accepts both sized and *unsized* Named<T>
fn bar<unsized T>(&Named<T>) { ... }
```

That is, you need to use what I will call a "generalization" marker at
the spot where you bind a type variable, to indicate that the domain of
that type variable is more general than the common-case default of 
a sized type.

For defining a trait that can be implemented on any possible type,
including unsized ones, you would need to use the `unsized` keyword
somewhere there as well.  "DST, Take 5" proposed
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

## <a id="the_insight">The Insight: `type` is a better generalization marker</a>

One of the concerns that Niko has pointed out to me is that it is easy
to (mis)read `unsized T` as saying "`T` must be unsized".  But that is not
what it is saying; it is saying "`T` *can* be unsized"; you can still pass in
a sized type for `T`.

I was reflecting on that this morning, and I realized something:
The whole point of DST is to partition the type universe into (Sized ⊎ Unsized).
So if you want this construct to be more self-documenting, the
generalization marker should be using some name to describe that union
(Sized ⊎ Unsized), rather than the name `unsized`.

But we already have a very appropriate name for that union: `type`!

So that started me thinking: Why don't we use `type` as our generalization marker?
So the definition of `bar` in the example above would be written
```rust
fn bar<type T>(&Named<T>) { ... }
```
In fact, this can have a very simple explanation: If we keep the `Sized` trait bound,
then you can just say that
```rust
fn foo<T>(args, ...){ ... }
```
desugars to
```rust
fn foo<type T:Sized>(args, ...) { ... }
```
and in general, any type variable formal binding `<T:Bounds>` desugars
to `<type T:Sized+Bounds>`

I admit, when I first wrote this, I said "hmm, this looks a bit like
C++, is that a problem?"  But I'm coming to like it.  The biggest
problem I can foresee is that a developer might be confused about when
they are suppposed to write `foo<type T>` versus `foo<T>`.  But chances
are that someone who does not understand the distinction will *not*
suffer if they just guess the answer; if they over-generalize, either:

 * the code will compile successfully anyway, in which case there is
   no harm, except perhaps w.r.t. forward-compatibility of their
   library when they may have wished they had imposed the `Sized`
   bound, or

 * the compiler will flag a problem in their code, in which case
   hopefully our error messages will suggest to add a `:Sized` bound
   or to just not use `type` in the binding for `T`.

If they under-generalize, then they (or their library's clients) will
discover the problem when they apply `foo`.

For the trait case, it is a little less obvious what to do.
I think we could likewise write:
`trait Foo for type : NormalBounds`
for the maximally general case.
`trait Foo : NormalBounds` would then desugar to
`trait Foo for type : Sized + NormalBounds`

So the point is that you would only use the `type` keyword when you
wanted to explicitly say "I am generalizing over *all* types, not just
sized ones", and thus are opting into the additional constraints that
that scenario presents.

This approach wouldn't be so palatable under earlier envisioned
designs for DST where e.g. you were restricted to write explicitly
`unsized struct S { ... }` for structs that could end up being
unsized.  But at this point I think we have collectively decided that
such a restriction is unnecessary and undesired, so there is no worry
that someone might end up having to write `type struct S { ... }`,
which definitely looks nonsensical.

There is another potential advantage to this approach that I have not
explored much yet: we could also add an `Unsized` trait bound, and
allow people to write `<type X:Unsized>` for when they want to
restrict `X` to unsized types alone.  I am not sure whether this is
actual value in this, but it does not seem absurd to put in a special
case in the coherence checker to allow one to write
`impl<X:Sized> SomeTrait for X { ... }`
and
`impl<X:Unsized> SomeTrait for X { ... }`
in order to get full coverage of `SomeTrait` for all types.

Finally, another obvious (though obviously post Rust 1.0) direction
that this approach suggests is that if we decide to add
parameterization over constants, we can likewise use the `const`
keyword in the spot where I have written the generalization marker
`type`, e.g.
```rust
fn foo<const N:int>(nums: &[f64, ..N]) { ... }
```
(In this case `const` would not be a generalization marker but instead
a *kind* marker, since it is changing the domain of the parameter from
being that of a type to being some value within a type.)

## <a id="the_examples">Examples ported from DST, Take 5</a>

Here are the ported definitions of `Rc` and `RcData`.
(Update: had to turn off syntax highlighting to work-around a rendering bug on `*`.)
```text
struct Rc<type T> {
    ptr: \*RcData<T>,
    // (a dummy field, just for illustrative purposes)
    dummy: uint,
}

struct RcData<type T> {
    ref_count: uint,

    #[max_alignment]
    data: T
}

impl<type T> Drop for Rc<T> {
    fn drop<'a>(&'a mut self) {
        unsafe {
            intrinsics::drop(&mut (*self.ptr).data);
            libc::free(self.ptr);
        }
    }
}
```

Here is the `ImmDeref` example:
```rust
trait ImmDeref<type T> {
    fn deref<'a>(&'a self) -> &'a T;
}

impl<type T> ImmDeref<T> for Rc<T> {
    fn deref<'a>(&'a self) -> &'a T {
        unsafe {
            &(*self.ptr).data
        }
    }
}
```

(I think I need a wider variety of examples, but this is good enough for now.)
