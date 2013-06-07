---
layout: post
title: "Detective Work on Rust Closures"
date: 2013-06-07 10:17
comments: true
published: true
categories: rust proglang
---

* <a href="#background_fns">Background: Closures (recurring and otherwise)</a>
* <a href="#background_dst">Background: DST</a>
* <a href="#the_problem">The Problem: DST and Closures</a>
  * <a href="#is_fn_too_ugly">Is fn~ too ugly?</a>
  * <a href="#leveraging_proc">Leveraging a proc keyword/expression</a>
  * <a href="#other_ways_to_proc">Other ways to express proc</a>
    * <a href="#digression_on_macros">A Personal Digression on Macros</a>
  * <a href="#more_followup_on_procs_and_fns">More followups on procs and fns</a>
* <a href="#where_are_we">So where are we?</a>

I have recently been trying to keep myself abreast of a flurry of
discussion about reforming the design of Rust closures.  Niko has a
series of blog posts ([1], [2], [3], [4], [5], [6], [7], [8]); the
content of some of those posts were further discussed at Rust team
meetings ([11], [12], [13], [14], [15], [16]), and there have been some
more formalized proposals with their own set of discussions: ([9],
[10]).

There are also associated github issues ([17], [18], [19]), though
without sufficient context the discussion in the github issues may not
always be intelligible.

Some of the links above are more about "Dynamically Sized Types"
(DST), a related topic, as we shall see.

This post is my attempt to condense all of this information down into
something where I can see all the pieces at once, and discard the
red herrings along the way.

## <a id="background_fns">Background: Closures (recurring and otherwise)</a>

In Rust circa version 0.6, closures have three categories according to
the type system (`&fn`, `@fn`, and `~fn`), but [as Niko describes][5],
they can be divided into two kinds: by-reference closures and copying
closures.  By-reference closures are also referred to as
stack-allocated closures or sometimes "stack closure."
(There is also a orthogonal division of `once` closures, versus
closures that can be invoked more than once; some of these things are,
to my knowledge, only part of planned future implementation.  Niko
discusses them in the blog posts but I'm mostly sidestep them here.)

As Niko states in the first paragraph of [1], a stack closure is
allocated on the stack, and can refer to and manipulate the local
variables of the enclosing stack frame (by reference).

In Rust (as of version 0.6), one creates a stack-allocated closure by
writing an expression `|x ...| { ... }` within an expression context
dictating that it wants a closure of `&fn` type.  Analogously, a
closure allocated on the exchange-heap is expressed by putting the
expression into a context of `~fn` type, et cetera.  Since a
stack-allocated closure is currently expressed solely by use of `&fn`
type, Niko often uses the term `&fn` closure synonymously with
stack-allocated closure.

(However, Niko also points out (first section of
["Procedures, Continued"][5]) that one can borrow a `@fn` or `~fn` to
a `&fn`, so the type does *not* tell you whether you actually have a
by-reference or a copying-closure.)

Here is the example of an unsound function that Niko described in
his [Case of the Recurring Closure][1] post from 2013-04-30,
making use of higher-order functions to express a fixed-point
combinator:
```rust
struct R<'self> {
    // This struct is needed to create the
    // otherwise infinite type of a fn that
    // accepts itself as argument:
    c: &'self fn(&R)
}

fn innocent_looking_victim() {
    let mut vec = ~[1, 2, 3];
    conspirator(|f| {
        if vec.len() < 100 {
            vec.push(4);
            for vec.each |i| {
                f.c(&f)
            }
        }
    })
}

fn conspirator(f: &fn(&R)) {
    let r = R {c: f};
    f(&r)
}

```

As Niko explains, the vector `vec` is mutated while being traversed by
an iterator; this is illegal.  The closure `|f| { ... }` captures
a reference to `vec`, and Rust's borrow checker is not treating the argument
`f` as a potential source of aliases to `vec`, even though it does alias
`vec` because `f` ends up being bound to the closure `|f| { ... }`.

An important detail here is that the closure in question is
a stack-allocated closure.

Niko has described his solution to this problem in [1]; it would
entail adding some new rules about how `&fn` closures are invoked
and passed as parameters.  One of the main changes imposed by his
solution was that `&fn` closures would become non-aliasable;
this would ensure that one could not express the Y-combinator.
The restriction to ensure `&fn` closures are unaliasable interacts
with other proposals, as we shall see.
(Note that Rust does have a way of expressing a non-aliasable pointer
to `T` for any `T`: `&mut T`.)

## <a id="background_dst">Background: DST</a>

The heart of the Dynamically Sized Types proposal is the discrepancy
described in Niko's [DST, Revisited][2] post from 2013-04-30
(published contemporaneously with [Case of the Recurring Closure][1]).
Niko has been wrestling with the idea for a while, as one can see on
his posts from [2012-04-23][20] and [2012-04-27][21].

In Rust, vectors (and strings, which we will treat as a special case
of vectors) come in the following forms:

* dynamic-length: heap-allocated, carries its length N as part of its
  record structure.  Consists of some amount of meta-data, including
  the length word, followed by the inline-allocated array of N
  elements.  Expressed as `~[T]` and `@[T]` in Rust.
* slice: represents a substring of a vector; consists of two words: a
  pointer to the payload, and a length bound.  Expressed as `&[T]` in Rust.
* fixed-length: represents exactly N elements, where N is statically
  tracked at compile-time.  Consists of just the array of elements,
  `T[N]`, and nothing more.  Expressed as `[T, ..N]` in Rust.

Niko points out that a slice's two-word representation is quite
different from the representations of the other variants.  His
proposal is to unify the first two representations, by laying out
`~[T]` and `@[T]` as pairs of words (a pointer to the boxed elements
array, and a length).
(Niko claimed that this makes a `~[T]` and `@[T]` valid slices, "apart
from the box header"; it seems to me like the box header is quite
relevant here, unless the idea is that when you coerce a `@[T]` to a
slice, you increment the pointer value accordingly...)

Then, Niko classifies the types of Rust into two categories: Sized and
Unsized; i.e., size is statically known, versus size is tracked at
runtime (maybe the letters S and R would have been more appropriate
than S and U...).  The "unsized types" cannot themselves be assigned
as types of local variables, and you cannot have vectors of elements
of unsized type; this all stems from the fact that "unsized types" do
not have a static size.
(The "unsized types" are arguably not actually types; we might be
well-served by referring to them as "pretypes" or something).
But pointers to unsized types are valid types.  Such pointers are the
pairs of words discussed above, aka "fat pointers": `(payload, meta)`,
where `payload` is the pointer to the data, and `meta` is the
descriptor that includes some way to determine the size of the payload
(to support runtime bounds checks).

The fact that "unsized types" need to be treated specially leads to a
complication, discussed further in the post; how to differentiate
between type-parameterized code that works on both kinds of types,
versus typed-parameterized code that solely operates on sized types.
The method proposed in the post is to express the distinction via a
trait bound: the `Sized` bound would restrict the type parameter to
one of statically-known size; you would not be able to express
types like `[X, ..3]` (a fixed-length vector of 3 X'es), unless you
include the bound `X:Sized`.  (There is more on this restriction
and ways to ease it further down.)

One of the benefits of DST that Niko proposes early on is that
Traits and closures are other instances of unsized types, so that
Rust's type hierarchy could be presented uniformly like so:
```
T = S            // sized types
  | U            // unsized types
S = &'r T        // region ptr
  | @T           // managed ptr
  | ~T           // unique ptr
  | [S, ..N]     // fixed-length array
  | uint         // scalars
  | ...
U = [S]          // vectors
  | str          // string
  | Trait        // existential ("exists S:Trait.S")
  | fn(S*) -> S
```
(Note that the actual types assigned to expressions would be instances
of `S` according to this grammar.)


## <a id="the_problem">The Problem: DST and Closures</a>

So, from the "Case of the Recurring Closure", we saw that
`&fn` closures were to become non-copyable.
But under the DST proposal, generic code should be able to treat `&T`
the same for all `T`, *including* when `T` is some `fn(S*) -> S`.
These two criteria are not compatible; Niko has lots more explanation
in his corresponding post:
["Recurring Closures and Dynamically Sized Types"][3],
from 2013-05-13.

Niko's immediate proposals to resolve this were either:

* we write `&mut fn` instead of `&fn`.  `&mut T` for all `T`
  (including `fn (S ...) -> S`) is forced to be unaliasable by the
  borrow-checker, and so the hole goes away, or,
* we change notation, and move the sigils for closures *after* the fn,
  side-stepping the special treatment of `&fn` versus `&T` by getting
  rid of `&fn` and replacing it with `fn&`.

### <a id="is_fn_too_ugly">Is fn~ too ugly?</a>

Niko at first favored the latter, then he wrote a second post,
["Mutable Fn Alternatives"][4] on 2013-05-13,
which reconsidered whether `fn~` is too ugly, and included
new survey of the options:

* Maybe `&mut fn` is not that bad, or
* Maybe make *all* closures borrowed (i.e. stack-allocated), removing
  the need for any sigil, or
* Make `fn` denote stack-allocated closures,
  and replace `fn~` with a new keyword, like `proc`.  (This is a variation
  on the previous bullet.)

For the second and third bullets, the main point is: If you need to
capture state in a manner that cannot be expressed via the available
options (stack-allocated closure, or a `proc`, if present), then you
have to use an trait instead (i.e. an object or a record).
(I personally am not thrilled about losing the option of using
closures to express combinator libraries, a use case for `fn@`.)

### <a id="leveraging_proc">Leveraging a proc keyword/expression</a>

Then a third post, ["Procedures, Continued"][5] from
2013-05-15, refined the `proc` proposal a bit further.  As stated in
the background on closures, Rust has by-reference closures and copying
closures; the choice of which variant to construct is based on the
type expected by the context of the `|x ...| { ... }` expression.
In this post, Niko proposed that the distinction here deserves a
starker line between the two forms.  (In that post, he proposed both a
revision to English jargon and also to the Rust syntax; I'm going to
focus solely on the Rust syntax changes, and let those guide the
changes to my own jargon here.)

So Niko proposes distinguishing a by-reference closure from a copying
closure via keywords.  A stack-allocated closure would be constructed
solely via `fn`, and a copying closure would be constructed solely via
`proc`.  While discussing this proposal henceforth, I will refer to a
by-reference closure as an `fn`-closure and a copying closure as a
`proc`-closure.

The type hierarchy that Niko then provides for this is:
```
T = S               // sized types
  | U               // unsized types
S = fn(S*) -> S     // closures (*)
  | &'r T           // region ptr
  | @T              // managed ptr
  | ~T              // unique ptr
  | [S, ..N]        // fixed-length array
  | uint            // scalars
  | ...
U = [S]             // vectors
  | str             // string
  | Trait           // existential ("exists S:Trait.S")
  | proc(S*) -> S   // procedures (*)
```

Now, `fn`-closures are considered *sized types*, because they are
always represented by two words: a (borrowed) environment pointer (to
the stack in Niko's proposal, though perhaps it could be generalized
to point elsewhere) and a function pointer.  `proc`-closures are
*unsized types*, because their copied lexical environment is of some
dynamically-determined size that they must carry in their record
structure.

In this version of the proposal, `proc` can now be allocated to either
the exchange heap (`~proc`) or the task heap (`@proc`).  So this
brings back the ability to express combinator libraries.

Niko's post provides further detail, such as dissection of the `fn`
and `proc` closure types (which include important details like the
lifetime and trait *bounds* for the closed-over variables; this is
important since with a separate keyword, it is now reasonable for
different defaults to be chosen for two cases; useful for making the
common case succinct).  He also describes a couple variations on the
theme, including modeling `proc` closures via traits (i.e. boxed
traits are objects carrying virtual method dispatch tables), and then
expressing them via a `proc!` macro (which means they could be left
out of the core language).

### <a id="other_ways_to_proc">Other ways to express proc</a>

In his next post, ["Removing Procs"][6], Niko elaborates further
on the idea that `proc` need not be supported in the language at all.
Stack-allocated `fn`-closures would remain, expressed via `fn(S ...) -> T`,
and the language already supports raw (environment-less) function
pointers via `extern "ABI" fn(S ...) -> T`.
Niko points out two ways to re-express copying closures:

1. One could pass around function pointers along with
   records that carry the captured environment; this is basically
   [lambda-lifting][22] (the variant that turns the free variables
   into fields of a single environment structure, rather than passing each
   variable as a separate parameter), or
1. As stated earlier, (boxed) traits can used to express copying
   closures.

Niko surveyed how these patterns would look in his post, by considered
existing use cases of `@fn` and `~fn` in the standard libraries,
namely task spawning and futures.  Without more language support, the
lambda-lifting transformation requires that one list the captures
variables (at least once, though further repetitions can be avoided
via appropriate macro definitions).  I am personally hesistant to
approve of removing non stack-allocated closures wholesale, though
*if* it turns out that capture clauses are essentially unavoidable (or
if understanding behavior without them is unworkable), then my main
problem with the `proc!` macros (the explicit list of free variables)
would go away.

Alternatively, if the macro system were somehow extended to allow a
macro to query an expression for its free variables, then that might
help.

#### <a id="digression_on_macros">A Personal Digression on Macros</a>

Actually, this latter idea brings up a problem with the explicit list
of captured variables that I had not thought of before: some macros
may intentionally inject references to free variables, where the
injected free variables are not meant to be part of the public
interface of the macro (i.e., the macro is enforcing some protocol of
usage, and the variable is meant to be otherwise private to the module
where the macro is defined).  I know we do not currently have macros
exported from modules, but I thought it was supposed to be part of the
long term plans for Rust.

* Do we intend to disallow the use of such macros within copying closures?

* Will we require the modules to expose those variable names, solely
  so that they can be included on the lists of free variables?

* Or, if a macro could query an expression for its free variables
  (where even module-private identifiers might be included on such
  a list), that might help impose a usage discipline that would
  support a `proc!` macro,

* Or, this whole example might serve as an argument for keeping
  copying closures as a primitive linguistic construct.

Okay, end of digression.

### <a id="more_followup_on_procs_and_fns">More followups on procs and fns</a>

A few days passed, then Niko had a fourth post,
["More on Fns"][7], from 2013-06-03.
This proposal renamed of a proposed `Task` trait to `Thunk`, since
Niko felt that the concept at hand (an encapsulated function and the
parameters it needs) is better reflected by that name.

More importantly, given the immediately preceding digression, the form
`thunk { ... }` would automatically determine the captured variables
instead of requiring an explicit list; this sidesteps the whole question
of how to handle macros that inject new free variable references.

There is then much discussion of whether or not to support `once fn`s,
which I won't summarize here.  The important detail of the post is
that we do not necessarily have to list the captured variables
explicitly.

After a few more days, Niko had a followup on the related topic of
dynamically sized types (DST), ["Reducing DST Annotation"][8], from
2013-06-06.  It took into account an [investigation][23] by Ben Blum
on the implications of a `Sized` trait bound.  This led to Niko
exploring some alternatives to adopting DST with a `Sized` bound:

* Abandon DST altogether: Niko summarizes what DST still buys us, but
  also points out where it does not live up to its original promises.
* Make type parameters default to `Sized`, and adopt a different
  syntactic mechanism to distinguish `Sized` from `Unsized` (such as
  a keyword).
* Use some sort of inference: the type-checker can use properties
  of a function's parameter list to provide feedback
  on whether the type parameter has an implicit `Sized` bound.
  (Niko wonders if this approach is too clever; I am inclined to
   affirm that it is.)

## <a id="where_are_we">So where are we?</a>

The above summarizes the series of blog posts from Niko.  I had hoped
to get through the actual proposals (and maybe also the team meeting
notes), but at this point, it is late enough in the day and this post
is long enough that I think I will stop here.

The language is young, and I am a Rust novice.  So, grains of salt
for everyone:

* My intuition is that we should leave in copying closures in some form.
* The `thunk { ... }` expression might suffice, but ... I am not yet
  convinced that I would be satisfied using boxed traits
  to express the cases that need input arguments (like combinator
  libraries).
* I am not thrilled by the idea of writing out lists of free
  variables.  Of course, this is a systems programming language,
  and such a list may simply be the simplest way to accomplish
  certain goals (e.g. to indicate whether a referenced value
  is moved or copied).
* If we do require a list of free variables in our copying
  `proc`/`thunk`/etc, please ensure that the question of free
  variables injected by macro invocations is addressed.

[1]: http://smallcultfollowing.com/babysteps/blog/2013/04/30/the-case-of-the-recurring-closure/

[2]: http://smallcultfollowing.com/babysteps/blog/2013/04/30/dynamically-sized-types/

[3]: http://smallcultfollowing.com/babysteps/blog/2013/05/13/recurring-closures-and-dynamically-sized-types/

[4]: http://smallcultfollowing.com/babysteps/blog/2013/05/13/mutable-fn-alternatives/

[5]: http://smallcultfollowing.com/babysteps/blog/2013/05/14/procedures/

[6]: http://smallcultfollowing.com/babysteps/blog/2013/05/30/removing-procs/

[7]: http://smallcultfollowing.com/babysteps/blog/2013/06/03/more-on-fns/

[8]: http://smallcultfollowing.com/babysteps/blog/2013/06/06/reducing-dst-annotation/

[9]: https://github.com/mozilla/rust/wiki/Proposal-for-closure-reform

[10]: https://github.com/mozilla/rust/wiki/Proposal-for-closure-reform-%28specific%29

[11]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-04-30

[12]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-05-07

[13]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-05-14

[14]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-05-21

[15]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-05-28

[16]: https://github.com/mozilla/rust/wiki/Meeting-weekly-2013-06-04

[17]: https://github.com/mozilla/rust/issues/2202

[18]: https://github.com/mozilla/rust/issues/3569

[19]: https://github.com/mozilla/rust/issues/6308

[20]: http://smallcultfollowing.com/babysteps/blog/2012/04/23/vectors-strings-and-slices/

[21]: http://smallcultfollowing.com/babysteps/blog/2012/04/27/in-favor-of-types-of-unknown-size/

[22]: http://matt.might.net/articles/closure-conversion/

[23]: https://github.com/mozilla/rust/issues/6308#issuecomment-18880575