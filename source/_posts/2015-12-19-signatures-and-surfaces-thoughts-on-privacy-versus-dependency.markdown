---
layout: post
title: "Surfaces and Signatures: Component Privacy versus Dependence"
date: 2015-12-19 22:30:00 +0100
comments: true
categories: rust proglang
---

I have had some thoughts on what *privacy* is used for in programming
languages, and how it differs from the notion of *dependence* between
modules (or at least compilation units) in a language like Rust.
And I thought I should share.

<!-- more -->

I have been working on an
RFC{% sidenote 'arr-eff-what' '<a href="https://github.com/rust-lang/rfcs/">Request For Comment</a>: A document used to propose significant changes to the Rust language or standard library.' %}
meant to increase the expressiveness of Rust's privacy construct
(the `pub` modifier), and in the process hopefully simplify the mental
model for what privacy means there.

However, I kept finding myself diving into regressions in my draft RFC
document: idealized hypothetical semantics for privacy, and
discussions of what motivates different aspects of that semantics.

Eventually I realized that such text was going to really bog down the
RFC itself (which is meant to describe a relatively simple language
change);
so I decided it was time for a blog
post{% sidenote 'gc-posts' 'Yes, I know that I also am overdue for the next chapter in my <a href="http://blog.pnkfx.org/blog/categories/gc/">GC blog post series</a>; it is coming.' a%},
if for no other reason than to provide a place for me to cut-and-paste
all those digressions.

{% marginblock %}
Bugs including:
"Trait re-exports fail due to privacy of containing module" ([#18241][Rust Issue 18241]),
"Rules governing references to private types in public APIs not enforced in impls" ([#28325][Rust Issue 28325])
"Type alias can be used to bypass privacy check" ([#28450][Rust Issue 28450]),
"Private trait's methods reachable through a public supertrait" ([#28514][Rust Issue 28514]),
"Non-exported type in exported type signature does not error" ([#29668][Rust Issue 29668]),
[Rust Issue 18241]: https://github.com/rust-lang/rust/issues/18241
[Rust Issue 28325]: https://github.com/rust-lang/rust/issues/28325
[Rust Issue 28450]: https://github.com/rust-lang/rust/issues/28450
[Rust Issue 28514]: https://github.com/rust-lang/rust/issues/28514
[Rust Issue 29668]: https://github.com/rust-lang/rust/issues/29668
{% endmarginblock %}
There are a number of bugs that have been filed against the privacy
checking in Rust; some are simply implementation issues, but the
comment threads in the issues make it clear that in some cases,
different people have very different mental models about how privacy
interacts with aliases (e.g. `type` declarations) and re-exports.

The existing privacy rules in Rust try to enforce two things:

 1. When an item references a path, all of the names on that path need to
    be visible (in terms of privacy) in the referencing context, and,

 2. Private items should not be exposed in the surface of public API's.

One might reasonably ask: What do I mean by "visible", or "surface"?

For Rust today, "visible" means "either (1.) public, via `pub`, (2.)
defined in the current module, or (3.) defined in a parent of the
current module."

But "surface" is a bit more subtle, and before we discuss it, I want
to talk a bit about the purpose of "visibility" in the first place.

## Digression: a dependence need not be visible

In a hypothetical idealized programming language (*not* Rust), and
under a particularly extreme reading of the term "private", changes to
definitions that are private to one module would have no effect on the
validity of pre-existing uses from other modules. Another way of
looking at this: changes to private definitions in one compilation
unit would not require other compilation units to be recompiled, and
will not cause programs that previously type-checked to stop
type-checking.

One form of this ideal is the following:

<script src="/javascripts/viz.js" charset="utf-8"></script>
<div id="extreme_private_calls"></div>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; node [shape="rect"]; subgraph cluster_1 { fn_a [label="pub fn a()"]; label="unit1"; } subgraph cluster_2 { fn_b [label="pub fn b()"]; fn_c [label="fn c()"]; fn_b -> fn_c [label="calls"]; label="unit2"; } fn_a -> fn_b [label="calls"]; }';
    var elem = document.getElementById("extreme_private_calls");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

In this picture, one can see that the `fn c()` is a private component
of "unit2": it may just be an implementation detail of the body of
`pub fn b()`, that the author of "unit2" can revise at will or
eliminate entirely, without requiring any changes to "unit1"
downstream.

A problem arises when one sees other kinds of composition, at least in
language like Rust, where values are directly embedded into their
containers.  For example, instead of function calls, imagine type
definitions:

<script src="/javascripts/viz.js" charset="utf-8"></script>
<div id="extreme_private_types"></div>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; node [shape="rect"]; subgraph cluster_1 { struct_a [label="pub struct A { \\l    b: unit2::B \\l}\\l"]; label="unit1"; } subgraph cluster_2 { struct_b [label="pub struct B { \\l    c: C \\l}\\l"]; struct_c [label="struct C {\\l    x: i32,\\l    y: i32\\l}\\l"]; label="unit2"; } struct_a -> struct_b [label="uses"]; struct_b -> struct_c[label="uses", constraint=false] }';
    var elem = document.getElementById("extreme_private_types");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

{% marginblock %}
In many other languages (e.g. Java, ML, Scheme), such
changes do not require recompiling the downstream crate, because
the members of structural types are just *references* to other heap-allocated
values, rather than being directly embedded in the allocated structure.
{% endmarginblock %}
In this situation, even though the `struct C` is not publicly
accessible outside of "unit2", changes to `struct C` will still
require the downstream "unit1" to be recompiled (because the contents
of `struct A`, and thus its size in bytes, may have changed along with
`struct C`).

So, what does it *mean* that `C` is "private", if there is still a
dependence from the contents of "unit1" on the supposedly private
definition of `struct C`?

My answer to this is to distinguish between *visibility* versus *dependency*.

In the above picture, `struct A` in "unit1" has a dependence on the
definition of `struct C` in "unit2". But `struct C` remains
*invisible* to `struct A`, in the sense that one cannot actually write
a direct reference to that type in the context of "unit1."

## What is visibility for?

Some basic definitions: An item is just as it is declared in the Rust
[reference manual][]: a component of a crate, located at a fixed path
(potentially at the "outermost" anonymous module) within the module
tree of the crate.

[reference manual]: https://doc.rust-lang.org/reference.html#items

Every item can be thought of as having some hidden implementation
component(s) along with an exposed surface API.

So, for example, in:
```rust
pub fn foo(x: Input) -> Output { Body }
```
the surface of `fn foo` includes `Input` and `Output`, while the `Body` is
hidden.

What I would like is to establish the following
invariant{% marginnote 'inv' 'Yes, this is basically a rephrasing of the second of the previously-stated pair of goals of the existing privacy rules.' %}
for the language: if an item `I` is accessible in context `C`, then the
surface for `I` does not expose anything that is inaccessible to `C`.

## Intuition behind what "surface" means

I am taking care to distinguish between the phrase "exposed surface
API" (more simply put, "surface API" or just "surface"), versus the
more common unqualified phrase "API", because some items have
components that I argue are part of the item's programming interface,
but are not part of the publicly exposed surface of the item (further
discussed in a [later section][why-not-same]).

The inutition behind the term "surface" is this:
The exposed surface of an item is all of the
components{% marginnote 'surface-components' '"components" means: types, methods, paths ... perhaps its easiest to just say "names."' %}
 that the client operation's context must be able to reference to in order to use this
item legally.

There are two halves to this, that are roughly analogous to the output
and input types of a function: ensuring that local reasoning holds,
and ensuring an interface is actually usable.

### Restricting output surface enables local reasoning

A function's return type is part of its exposed surface, because if
a module has decided that a type `T` should be inaccessible in some
outer context `C`, then we do not want a value of that type to flow
into `C` while still having the type
`T`.{% marginnote 'boxes' 'Of course if the type of the value is hidden, e.g. a `Box<PrivateType>` behind a `Box<PublicTrait>`, then that is fine as always.' %}

In other words, we wish to reject such code in order to enable
module authors to employ *local reasoning* about all possible
locations in the source code that the operations on instances of
`T` could be invoked.

This *is* a soundness criteria: People need to be able to employ
this kind of reasoning.

### Restricting input surface catches API mistakes

A function's input types are part of its exposed surface, because
without access to such types, the function is not callable.

In other words, we wish to reject such code in order to catch bugs
where a crate is *accidentally providing* a function without realizing
that it cannot actually be used in the contexts that the author wants
it available in.

This is not a soundness criteria; it is just a language usability one.{% marginnote 'prioritizing-halves' 'In the long run, I suspect that the local reasoning enabled by restricting the output surface is going to be more important than the benefits of restricting the input surface. I am not aware of any case where we actually need to <emph>choose</emph> between the two; I am more speaking of where we should direct our attention.' %}

## Why is a "surface" not the same as a signature?
[why-not-same]: #Why.is.a..surface..not.the.same.as.a.signature.

Intuitively, one might ask: "well, this is easy: the *signature* of
`fn foo` is `fn (Input) -> Output`; does that not suffice as the
description of the *surface* of `fn foo`?"

I am distinguishing the above notion of "surface" from the idea of a
"signature", for the following reason: To my mind, the signature
(e.g. of a type or a function) contains all of the meta-data needed to
check (in the current crate or in other crates) whether a item is
being used properly. Such a signature may include references to names
that are not actually accessible in the current context. Compare this
to the *surface*, which is the subset of the names of the signature
that *must* be accessible in any context where the item is itself
accessible.

One example of where this kind of thinking can be applied is
`where` clauses. A where-clause can reference things that are not
accessible outside of the module of the function.  I would consider
such a `where` clause to still be part of the function's signature
(e.g., I would expect the compiler to reject my attempt to call the
function if I violate the encoded constraint), but I do not
necessarily consider the types or traits within that where clause part
of the surface API, since there are hidden parts to the constraint
that I do not have access to in my calling module.

Here is a concrete example that runs in Rust 1.5:

```rust
mod a {
    struct S(&'static str);                 // private struct type S
    pub trait Trait { fn compute(&self) -> i32; }

    impl Trait for (i32, S) {
        fn compute(&self) -> i32 { self.0 + ((self.1).0.len() as i32) }
    }

    pub fn foo<X>(x: X) -> i32
        where (X, S): Trait // where clause refers to private type S
    {
        (x,S("hi")).compute()
    }
}

fn main() {
    println!("{}", a::foo(3));
}
```

There are other examples that we may want to support in the future.
For example, Rust (version 1.5) considers bounding a type parameter
directly via a private trait to be illegal, but we might reasonably
revise the rules to say that while such a bound is part of the
signature, it need not be part of the *surface*.

(A very similar construction is allowed in Rust 1.5: A `pub` trait
can have a private *parent* trait, which allows us to encode the
latter construction anyway: the surface area of a function does not
include the parent traits of bounds on its type parameters.)

That's a lot of text to read. Here is the kind of code I am talking
about:

```rust
mod a {
    struct S(String);                      // private type
    trait Trait { fn make_s(&self) -> S; } // private trait
    pub trait SubT: Trait { }              // public trait to placate rustc

    pub fn foo<X:SubT>(x: X) { // public fn that external code *can* use.
        let s: S = x.make_s();
        s.do_stuff();
    }

    // Impl trait for both () and i32, so clients can call `foo` on () or i32.
    impl Trait for () { fn make_s(&self) -> S { S(format!("():()")) } }
    impl Trait for i32 { fn make_s(&self) -> S { S(format!("{}:i32", self)) } }
    impl SubT for () {}
    impl SubT for i32 {}

    impl S { fn do_stuff(&self) { println!("stuff with {}", self.0); } }
}

fn main() {
    a::foo(());
    a::foo(3);
}
```

In short: the term "surface API" here is *not* synonymous with the
term "signature".

Assuming that you believe me that this new term, "surface API", is
actually warranted, you might now ask: "How does one determine the
surface API of an item?" That is one of those questions that may sound
trivial at first, but it is actually a bit subtle.

Let us explore.

### Some items can change their surface based on context

For some items, such as `fn` definitions, the surface API is the same
regardless of the context of where the item is used; for example, if a
function is visible to you, then its surface API is simply its
argument and return types, regardless of from where the function is
referenced.

However, the previous rule does not generally hold for most items; in
general, the exposed surface of a given item is dependent on the
context where that item is referenced.

The main examples of this are:

{% marginblock %}
All of these bullets are phrased as "can be hidden", i.e.,
the visibility may be restricted. However, in Rust today,
one can write: `mod a{struct X{pub y: i32}}`
I may want to generalize the statements here. (Then again, I
am not clear whether there is any way to actually *use* the
`y` field that has been exposed in this way.)
{% endmarginblock %}

 * `struct` fields can be hidden in a `struct`,

 * inherent methods can be hidden relative to the type they are attached to, and

 * items can be hidden in a `mod`.

In all cases where a surface component can be hidden in this
context-dependent fashion, there is an associated `pub`-modifier
present on the definition of that component.

As an example of how the surface of a `struct` is context dependent,
the following is legal:

```rust
mod a {
    #[derive(Default)]
    struct Priv(i32);

    pub mod b {
        #[derive(Default)]
        pub struct F {
            pub    x: i32,
                   y: ::a::Priv,
        }

        // ... accesses to F.{x,y} ...
    }
    // ... accesses to F.x ...
}

mod k {
  use a::b::F;
  // ... accesses to F and F.x ...
}
```

Within `mod b`, the surface API of `F` includes both the fields `x`
and `y`, which means that the use of the type `Priv` is okay, since
that is accessible from the context of `mod b`.

Elsewhere, such as within `mod k`, the surface API of `F` is just the
field `x`. But this is again okay, because the type of `x: i32` is
visible everywhere.

### Aliases and translucency

Some items, such as `type` aliases, `const` definitions, or rebinding
imports a la `use <path> as <ident>`, can act to introduce named aliases
to an item.

In such cases, the alias itself has its own associated visibility:

```rust
mod a {
    pub struct S(String); // public type
    type Alias1 = S;      // private alias to the type
}

pub use a::S as Alias2;   // public alias to the type
```

The surface of simple aliases is also simple: the surface of an
alias
is just the paths referenced on its right-hand side.

As a small additional wrinkle, type aliases can be type-parametric. In
general, the exposed surface of a type alias are the bounds on its
type parameters, plus the paths referenced on its left-hand side.

So, for example, according to the rules today:

```rust
mod bad_aliases {
    struct Private1(String); // private type
    pub type PubAlias1 = Private1; // ERROR: private type exposed in pub surface

    trait PrivateTrait { }
    pub type PubAlias2<X:PrivateTrait> = i32; // ERROR: private trait exposed in pub surface
}
```

The more interesting issue is how *other* surface APIs are influenced
when they reference an alias.

For example:

```rust
mod a {
    pub struct S(String); // public type
    type Alias1 = S;      // private alias to the type

    pub fn twice(s: Alias1) -> String { s.0 }
    //              ~~~~~~
    //                 |
    // Should a `pub fn` be able to reference a private alias,
    // if it points to a suitably public type (like `S` here)?
}

pub use a::S as Alias2;   // public alias to the type
```

Should it be legal for us to publicly export `fn twice` from `mod a`,
even though it's signature references a private type alias?

The language team recently [debated][] this topic, because
it was suggested that allowing this would [reduce breakage][]
from a pull request.

[reduce breakage]: https://github.com/rust-lang/rust/pull/29973#issuecomment-158686899
[debated]:         https://github.com/rust-lang/rust/pull/29973#issuecomment-165723770

The conclusion for now was to continue to disallow the reference to
the private alias in the signature of a public function.

However, there are similar cases that *are* allowed today (also
discussed on that same PR), mainly involving references to `const` paths
from types in such signatures.

```rust
mod a {
    const LEN: usize = 4;
    pub fn max(a: [i32; LEN]) -> i32 { a.iter().map(|i|*i).max().unwrap() }
    //                  ~~~
    //                   |
    // A reference to a private const in a public signature
    // is legal in Rust today.
}

fn main() {
    println!("{}", a::max([1,4,2,3]));
}
```

I have not made up my mind as to which option would be better here.
We may decide to leave things as they are, or loosen the rules for
type aliases (so that they act more like `const` in the latter code),
or we may tighten the rules for references to `const` (so that one
would have to make `LEN` in the above code `pub`).

Regardless of what path we take, I think it makes sense today for the
language specification to at least identify a high-level abstraction
here, rather than dealing with each alias-creating form like `type` or
`const` or `use` individually in an ad-hoc manner.

Namely, I want to pin down the idea of a *translucent name*. Such a
name is not part of the API surface where it occurs; instead, an
occurrence adds the surface of the alias statement itself to the API
surface.

So, as another artifical example, if we were to change the language so
that `type` aliases were *translucent* when determining the exposed
surface of an API, then we might have the following:

```rust
mod a { // (not legal Rust today)
    pub struct S(String); // public type
    pub trait Bound { type X; fn trait_method(&self) -> Self::X; }
    impl Bound for String { type X = String; fn trait_method(&self) -> String { self.clone() } }
    impl Bound for S { type X = String; fn trait_method(&self) -> String { self.0.clone() } }

    type Alias<T: Bound> = (T, T::X, S); // private Alias, with surface = {Bound, S}

    pub fn free_fun<T: Bound<X=String>>(a: Alias<T>) -> String
    //                 ~~~~~   ~~~~~~      ~~~~~        ~~~~~~
    //  free_fun has     |       |           |            |
    //     surface = { Bound, String, surface(Alias), String }
    //             = { Bound, String,    Bound, S   , String }
    //             = { Bound, S, String }
    //
    // which is compatible with `free_fn` being `pub`, because
    // `Bound`, `S`, and `String` are all `pub`.
    {
        format!("{}{}", a.0.trait_method(), (a.1).0)
    }
}
```

Note 1: Even though `Alias` is type-parameteric over `T`, that
parameter would not be considered part of its surface. Anyone using
the alias would have to have access to whatever type they plugged in
there, of course.

Note 2: Type parameter bounds not enforced on type aliases in Rust yet.

This computation and questions here would become a little more
interesting if we had restricted visibility access modifiers on
associated items in traits. However, we do not have to consider it:
All associated items are implicitly `pub`, and so we do not need to
worry about whether the `X` in a projection like `T::X` is visible.
All that matters is whether the trait `Bound` itself is visible (which
is already reflected in the surfaces where `Bound` is used).

## Conclusion

Okay, that was of a bit of a meandering tour through some
of the issues I have been thinking about.

The big ideas I want to stress are these:

 * The "surface" of an item can be different from the "signature" of
   that item.

 * Restricting the components in a surface of an item according to the
   visibility of that item (1.) enables local reasoning and (2.)
   catches API mistakes.
