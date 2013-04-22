---
layout: post
title: "Designing syntax for associated items in Rust"
date: 2013-04-22 14:37
comments: true
published: true
categories: rust proglang syntax
---

* <a href="#background">Background</a>
  * <a href="#encodingunpalatable">Encoding via parameters is unpalatable</a>
* <a href="#rustspec">Rust-specific issues</a>
  * <a href="#nikoposts">Niko's blog posts</a>
  * <a href="#thinkbinding">Simpler syntax: What about binding?</a>
* <a href="#insight">Insight</a>
* <a href="#proposal">The proposed syntax for associated items in Rust</a>
* <a href="#futurework">What the proposal does not cover</a>

Executive summary: if you don't want or need the background information
or the discussion motivating the proposal, then just jump
straight to the <a href="#proposal">proposal</a> itself.

## <a id="background">Background</a>

Early in my experimentation with Rust, I thought a reasonable exercise
would be to take the simple C++ programs from
[Elements of Programming][1] (Stepanov and McJones), which make heavy yet
disciplined use of abstraction and C++ templates to encode various
mathematical concepts.  The early chapters of the book use templates
rather than classes as the means of code reuse, so translating those
examples seemed like a good way to exercise Rust's generic type and
trait systems.

However, almost immediately after starting the experiment, I encountered
a problem: code that makes heavy use of C++ templates is quite likely
to use particular features of C++ templates that are not a universal
part of another language's generic type system.

In particular, the code from Elements of Programming (hereby
abbreviated "EOP" in this post) almost immediately makes use of
"associated types", such as in the following definition for `distance`:

```c++
template<typename F>
    requires(Transformation(F))
DistanceType(F) distance(Domain(F) x, Domain(F) y, F f)
{
    // Precondition: $y$ is reachable from $x$ under $f$
    typedef DistanceType(F) N;
    N n(0);
    while (x != y) {
        x = f(x);
        n = n + N(1);
    }
    return n;
}
```

The interesting thing about the above code is that it is parameterized
over one type: <nobr>`F`,</nobr> but it uses other type expressions within the body
of the procedure, namely:

  * `Domain(F)`: this is a `type -> type` operator that, given a
    Transformation (which we can think of as some type classifying a
    set of `T -> T` functions for some type `T`), returns `T`.

  * `DistanceType(F)`: this is a `type -> type` operator that, given a
    Transformation, returns a numeric type (think `uint8_t`,
    `uint32_t`, `uintptr_t`, `BigNum`, etc) suitable for counting the
    minimum number of applications of the transformation necessary to
    get from any particular `T` value to some other `T` value.

(Operators like `DistanceType`, to my mind, only makes sense when you
 look at things simultaneously in terms of bytes of memory in the
 machine and also in terms of pure abstract mathematical values.  If
 you omit either perspective, then the operator appears either
 pointless or nonsensical.)

It also requires that `F` obeys a constraint, specified in the
`requires` clause; I am going to conveniently ignore this detail for
now.  (The C++ code for EOP even macro-expands `requires(..)` into whitespace,
so treating them as helpful comments for the time being is not absurd.)

Type expressions like `triple<A, B, C>` (assuming three type expressions
`A`, `B`, and `C`), are the bread-and-butter of any generic type
system.  But these `type -> type` operators are interesting.  How are
they implemented?  Here is a snippet from `type_functions.h` in the
EOP source code distribution:

```c++
template<typename F>
    requires(Transformation(F))
struct distance_type;

// If all transformations on a type T have the same distance type,
// then DistanceType(T) is defined and returns that type.

// For any fixed-size type T, there is an integral type of the same
// size that is a valid distance type for T.

#define DistanceType(T) typename distance_type< T >::type
```

This code is making use of a C-style macro to define a easy-to-read
interface for the `DistanceType` operator (the subset of C++ used
for EOP's textbook examples is meant to be LL(1)), but the implementation
of the operator is using C++'s template system to define a partial
mapping from types to (integral) types.  One can add new entries to
this mapping by defining a new template instantiation of
`struct distance_type<F>`, as illustrated in `tests.h` for the following
transformation `gen_orbit`:

```c++
template<typename I, typename N>
    requires(Integer(I) && Integer(N) && DistanceType(I) = N)
struct gen_orbit // transformation
{
    gen_orbit_predicate<I, N> p;
    gen_orbit(I x_0, N h, N c) : p(x_0, h, c)
    {
        // Precondition: h < N(MaximumValue(I)) && c < N(MaximumValue(I))
        // Precondition: !negative(h) && !negative(c)
    }
    I operator() (I x)
    {
        Assert(p(x));
        x = successor(x);
        if (x == p.x_0 + I(p.h) + I(p.c)) x = p.x_0 + I(p.h);
        return x; 
    }
};

template<typename I, typename N>
    requires(Integer(I) && Integer(N) && DistanceType(I) = N)
struct distance_type< gen_orbit<I, N> >
{
    typedef N type;
};
```

Thus, the definition of `gen_orbit` (including its instantiation of
`distance_type`) collaborates with the definition of `DistanceType` to
indicate that `DistanceType(gen_orbit<I, N>)` is `N`.  As one adds new
structs (classes) representing other transformations, one is expected
to instantiate `distance_type` (as well as a host of other
template-abstracted structs) accordingly.

----

So, what's the problem here?  Well, Rust, much like Java, does not
provide a way to define general `type -> type` mappings like
`DistanceType(F)`.

One can try to work around this via a code transformation and lift any
type of interest up to a generic class's parameter list, like this
example in Rust:
```rust
trait Transformation<DISTANCETYPE, DOMAIN> {
    fn apply(&self, elem: DOMAIN) -> DOMAIN;
}
```
or if you prefer Java:
```java
    interface Transformation<DISTANCETYPE, DOMAIN> {
        DOMAIN apply(DOMAIN elem);
    }
```


At first glance, one might think this does not look so bad; after all,
the `gen_orbit` struct similarly was parameterized over a domain `I`
and a distance type `N`.  However, the problem comes when one
then attempts to write a function like distance:

Rust:
```rust
fn distance<F: Transformation<DT, DOM> >(x: ???, y: ???, f: F) -> ??? {
    /* ... */
}
```

Java:
```java
    public static <F extends Transformation<DT, DOM> ??? distance(??? x, ??? y, F f) {
        /* ... */
    }
```

What do we put in for the `???` portions?  We already established that
we do not have general `type -> type` operators, so we cannot just
derive it form `F`.  And for that matter, where did `DT` and `DOM`
come from?  In Rust and Java, we cannot just make up fresh type
variables and then add constraints upon them after the fact.  The only
option is to make any type we wish to use an additional type parameter
to the generic method.

Rust:
```rust
fn distance<DT, DOM, F: Transformation<DT, DOM> >(x: DOM, y: DOM, f: F) -> DT {
    /* ... */
}
```

Java:
```java
    public static <DT, DOM, F extends Transformation<DT, DOM> >
        DT distance(DOM x, DOM y, F f) {
        /* ... */
    }
```

### <a id="encodingunpalatable">Encoding via parameters is unpalatable</a>

The Rust and Java results above are made barely readable by using
short (obscure) parameter names.  More troubling is the fact that this
pollution of the parameter list will bubble transitively backwards
through the callers of `distance` until we reach the point where `F`
is instantiated.  Any use of `Transformation` needs to be
parameterized in the same manner.

It also makes explicit instantiation of a parameterized method or
class quite painful.  (This pain is somewhat alleviated in the
presence of type-inference, at least in terms of what text ends up in
the final code, but I argue that that in this case the pain has in
fact been *shifted*: instead of having pain while reading the code,
one instead suffers when trying to wade through type-errors that
inevitably arise during the compile-edit cycle.)

If anything, the above presentation *understates* the problem, since:


  1. `Transformation` has only one argument in its domain, and its codomain
     is the same as its domain; many real traits with associated types
     are each likely to require multiple parameters.
  2. The above example has direct uses of `DOM` and `DT` in the domain
     and codomain, respectively, of `distance`.  However, *every* client
     of `Transformation` will be forced to be parameterized over `DOM`
     and `DT`; while it is likely that any client of `Transformation` is likely
     to need to refer to the type `DOM`, many are likely to not require
     use of the distance type `DT` in their public interface or even in
     the internals of their code.  Thus, our abstraction is not very abstract
     at all.
  3. As a follow-on to the previous point: We are only illustrating
     *one* added concept: `DistanceType`; each additional concept
     would require a new type parameter to be threaded through the
     parameter lists of all methods and classes.  This blows up to an
     unmaintainable mess fairly quickly, discouraging use of generics
     to define these abstractions (and instead relying on
     e.g. separate class-hierarchies).


----

## <a id="rustspec">Rust-specific issues</a>

I encountered this problem while porting EOP code to Rust.  After
wrestling with the type parameter lists for a while, I eventually
wised up and asked on the #rust IRC channel if there was a better
option.  Tim Chevalier informed me of the relevant terminology:
the feature I want is called "associated types access" (or often just
"associated types").
An associated type specifies a mapping from some type to another type.

"Associated type access" is listed as one of eight properties considered important in
"[A comparative study of language support for generic programming][3]"
(Garcia et al., 2003 [ACM][2]).
If you found <a href="#encodingunpalatable">the argument above</a> unconvincing,
you should read the Garcia paper for a completely different example motivated by
a Graph abstraction.

After I read the Garcia paper, I promptly filed [an RFC][4] on the
Rust github repository requesting support for Associated Type
Synonyms.  After this, I had several discussions with Niko Matsakis,
both over IRC and in person, on the problems that associated types
present for Rust.

### <a id="nikoposts">Niko's blog posts</a>

You can see Niko's thorough overview of the matter,
including his natural generalization of the topic from "associated
types" to "associated *items*", on his pair of blog posts ([part
I][5], [part II][6]).  The generalization to "associated items"
enables one to define, in addition to `type -> type` mappings as
illustrated <a href="#background">above</a>, also `type -> function`
mappings (called in some languages "static" functions) and `type ->
(constant) value` mappings, which may enable certain interesting
coding patterns, such as allowing a type representing a vector in a
multi-dimensional space to state, statically, how many dimensions
it carries.

The following are the specific points that Niko makes in his posts (some of 
are just pointing out artifacts of current Rust language syntax).

### Current Rust syntax focuses on deriving associated functions from traits

Rust does not currently offer general associated items, but it does
offer a kind of associated function access.

If a trait `T` defines a function `f` that returns `Self` (which means
that implementations of `T` are obligated to provide an implementation
of `f`), and one has a type `X` implementing that trait, then one can
derive `f`.

But in current Rust syntax, one does not write this derivation of `f`
as something attached to the type `X`; instead, one writes `T::f(..)`,
and the compiler is responsible for inferring which implementation of
the function `f` one is referring to, by using type-inference on the
context of the invocation `T::f(..)` to determine that the return type
of `f` must be `X` (and thus the `f` in question must be the one that
the type `X` implements to satisfy the obligation established by the
trait `T`).

#### <a id="nikoenctt1">Resolving ambiguities in general implies you need both the trait and type</a>

The choice of deriving a function's implementation from the trait
rather than the type is understandable when one considers that a
software system may have multiple traits `T`, `U`, `V`, ... that all
define a function of the same name (say `f`), and a type may be
specified as implementing more than one of these traits in a single
piece of code.  (It would be anti-modular to require every trait to
choose globally unique names for its set of associated functions).  So
to handle this case, one must provide some way to disambiguate which
`f` is being referenced.  Rust did so by making the trait expression
part of the invocation syntax.  Niko points out that if one switches
to a syntax where one derives `f` from the type
`X` (e.g. <nobr>"`X::f`"</nobr>) then one must tackle this problem in
some manner; in his first blog post, he suggests doing so by allowing
one to encode both the type and the trait in the referencing syntax
(e.g. <nobr>"`X::(T::f)`"</nobr> or <nobr>"`X::(U::f)`"</nobr>.

I dislike this syntax because I think it
would be confusing for a reader to comprehend the distinct roles of
the <nobr>"`::`"</nobr> path operator, both when learning the language
and when casually skimming Rust code in general.

#### Rust type expressions do not naturally fit into Rust path expressions

Niko also points out that when one wants to write `X::f` where `X` is
a type, it is not always the case that `X` is a type parameter; it
could be a concrete type known to the programmer, such as the type of
owned vecs of ints, denoted by the type expression
<nobr>`~[int]`.</nobr> So it seems natural to want to substitute such
a type expression for (the meta-variable) `X`.

But the syntax `~[int]::f` is not legal, because `~[int]` is
not a legitimate path component.  Niko describes a couple of
work-arounds, e.g. allowing one to wrap a type expression that appears
in a path expression with brackets, yielding: `<~[int]>::f`.

All of the work-arounds presented by Niko do require allowing
arbitrary type-expressions in some form to appear as a sub-expression,
which would complicate the parser in the Rust compiler (there has been
a slight push to try to *simplify* the path expression syntax, which
this would conflict with).

#### Further syntactic exploration of encoding trait and type

In his second blog post, Niko provides some alternative syntactic forms
for resolution:

  * `X::(T::f)`, as described <a href="#nikoenctt1">above</a>.

  * `T::f::<X>` (from "Functional-style name resolution (take 1)"); here
    `X` is a synthetic type parameter added to the type parameter list
    (if any) of `f`; so now we get to retain syntactic backwards
    compatibility.  Since Rust allows one to omit the explicit type
    instantiation `::<X, ...>` when the compiler is able to infer
    the instantiation, this would be a natural way to continue
    doing return-type based inference of the desired type, the way
    it does already.

  * `T::f::<for X>` as a way of distinguishing the synthetic parameter
    from other entries on the parameter list.

I have already stated my problems with the first option.

For the second option, I anticipate being personally confused by the
synthetic type parameter being injected into the type parameter list.
I understand the appeal of enabling the compiler to continue doing
heavy lifting and lighten the programmers syntactic load.  [Niko's
post][6] does a good job of laying out some of the unexpected
interactions of the synthetic type parameter with the other forms of
generic type parameterization.

The third option would reduce confusion somewhat, since the
synthetic parameter would receive special attention at points of type
instantiation, but I still think it is an abuse of the parameter list.

### <a id="thinkbinding">Simpler syntax: What about binding?</a>

So I set about trying to come up with another syntactic form
for associated item access.  My primary focus initially was:
all of these examples would be so much simpler, to my mind,
if we were able to go back to using a single identifier
for the relevant path component in the referencing form,
the way that C++ uses <nobr>`C::f`.</nobr>
How can Rust make its own analogous <nobr>`R::f`</nobr> (the "R" is for Rust).

Of course, we have already covered that this will be ambiguous if
`R` is a mere type (and it is of course ambiguous if `R` is just a trait).

But what if `R` is a way of referring to the type `X` and the trait `T`
together: the (type, trait) pairing (X,T)?  Clearly once one specifies the
pair, then it is easy to tell what items are associated with the pair.
Even a human without a sophisticated IDE would know in that case to try
invoking `grep`, searching for `impl T.* for X.*`; a compiler can do even better.

Another way of looking at this: What if we could introduce local names
for the impl that corresponds to the (type, trait) pairing.

So I started working on ideas all centering around a declaration
form like `let R = trait T for type X;` or `use impl R = T for X`
and other variations (I think Patrick Walton actually deserves credit
for that last one; we will revisit it later).  But Niko quickly pointed
the huge failing of all of these declaration forms: a very common
use case for associated *types* (remember, that was our original goal)
is for function signatures, like:
```rust
fn distance<F: Transformation>(x: Domain(F), y: Domain(F), f: F) -> DistanceType(F);

fn remove_edge<G: IncidenceGraph + EdgeCollection>(g: &mut G, e: Edge(G));
```
where `Domain(F)`, `DistanceType(F)`, and `Edge(G)` are replaced
with appropriately Rust-friendly syntactic forms.  There is no *place*
there to put a declaration form `let ...` or `use ...` that refers to
`F`.  The same applies for other parameterized forms, such as structs,
enums, and traits.

So, back to the drawing board.

----

## <a id="insight">An Insight</a>

Even though my attempt to solve this problem via a declaration form had
failed, I continued to focus on the fact that associated item access
is all about the (type, trait) pairing.  So how could I surmount the
parameterized signature wall?

After reflecting on the parameterized signature itself, I said, "where
is a natural place to put a binding from an identifier to a (type,
trait) pair?"  And this reduced to "where does the (type, trait) pair
come from?"  This was my insight: The parameterized signature
<nobr>`<X: T>`</nobr> *itself* is where the pairing is defined;
(or in the case of <nobr>`<X: T + U>`</nobr>: *pairings*).

My only problem was to put the identifier binding in there.  Once I
saw the pairing waiting right in the parameter list, the place for the
identifier became clear: in-between the type and the trait:
<nobr>`<X: R=T>`</nobr> binds `R` to the `impl T for X`;
for multiple traits, we have <nobr>`<X: R=T + R2=U>`</nobr>,
where `R` is bound as above, and `R2` is bound to the `impl U for X`.

And now we can consider writing our examples like so:
```rust
fn distance<F: T=Transformation>(x: T::Domain, y: T::Domain, f: F) -> T::DistanceType;

fn remove_edge<G: IncidenceGraph + EC=EdgeCollection >(g: &mut G, e: EC::Edge);
```

The other cute insight is this: the only time we need to add these
identifiers explicitly is when there are multiple trait bounds.
When there is a single trait bound <nobr>`<X:R=T>`</nobr>, the
identifier `X` is just as reasonable (or at least unambiguous) as `R`
is as a way to reference the impl.  So why not treat
<nobr>`<X:T>`</nobr> as an abbreviation for <nobr>`<X:X=T>`</nobr>:
boom!  The biggest potential complaint with this extension (namely,
the notational complexity of making people pepper their code with
explicit bindings of the impls) goes away!  And our first example becomes:

```rust
fn distance<F: Transformation>(x: F::Domain, y: F::Domain, f: F) -> F::DistanceType;
```
(our second example remains unchanged, since `G` has two trait bounds there, and
so `G` alone cannot unambiguously denote a (type, trait) pair.

Note also that this binding form does not suffice on its own; in
particular, if one wants to introduce a binding for a (type,trait)
pairing that does not appear in the generic parameter bounds of the
signature.  But the latter is exactly the case that *is* handled by a
declaration form such as those <a href="#thinkbinding">proposed earlier</a>!

So neither solution suffices on its own, but the two together cover
many use cases of interest.

----

## <a id="proposal">The proposed syntax for associated items in Rust</a>

So, with that insight explained, here is my proposal for associated items:

  1. A trait can now declare names for things besides methods.
     In terms of the grammar that John has been working on:

     ```
     trait_decl: TRAIT ident
                    (generic_decls)? (COLON trait_list)?
                    LBRACE trait_method* RBRACE ;
     ```

     is replaced with

     ```
     trait_decl: TRAIT ident
                    (generic_decls)? (COLON trait_list)?
                    LBRACE trait_item* RBRACE ;
     trait_item: trait_method | trait_constant | trait_type
     trait_type: TYPE ident (generic_decls)? SEMI
               | TYPE ident (generic_decls)? COLON boundseq SEMI ;
     trait_const: STATIC ident COLON ty SEMI ;
     ```

  2. The identifier bound by a trait types is in scope of its enclosing
     trait; trait method declarations and trait const declarations
     can reference it.

  3. Extend the Rust grammar to allow an optional binding of
     an identifier to a (type, trait) pair in a type parameter bound.
     In terms of the grammar:

     ```
     bound : STATIC_LIFETIME | trait | obsoletekind ;
     ```

     is replaced with:

     ```
     bound : STATIC_LIFETIME | trait | ident = trait | obsoletekind ;
     ```

  4. Extend the Rust grammar to allow a declaration binding 
     an identifier to a (type, trait) pair.
     In terms of the grammar, I *think* this is close to what I want:

     ```
     view_item : attrs_vis use ;
     ```

     is replaced with:

     ```
     view_item : attrs_vis use | USE impl ident = trait for ty ;
     ```

     Of potential interest, we do not allow visibility attributes
     on `use impl R = T for X;`, because these definitions are always
     local shorthands and thus private to the module.  (Maybe in
     the future we will see motivation to allow the bindings to
     be exposed, but I have not yet seen a motivation for this.)

     I am not attached to the particulars of the syntax above;
     in particular, if someone wants to throw in the `trait`
     and/or `type` keywords into the above to make the purpose
     all the more clear, I will not object.  More so if it is
     somehow *necessary* for disambiguation, but I do not
     anticipate that being the case.

  5. A bound of the form `R=T` (`ident = ty`) in the context of a `ty_param`
     production `X : ... [] ...` (`ident COLON bound + ... + [] + ... + bound`)
     (where `[]` denotes the contextual hole that the `R=T` is plugged into)
     is treated as binding `R` to the code defined by the `impl T for X`.
     The scope of the binding for `R` encompasses: the rest of the boundseq
     (to the right of the <nobr>`"R=T"`</nobr>) and the remainder of this decl
     that follows the generic_decls within which the <nobr>`R=T`</nobr> bound appears.

  6. This binding of `R` can shadow earlier bindings of the same identifier
     (either other impl-bindings, or module names).  It seems like this
     should be a reasonable thing to signal via a lint-warning.

  7. A path identifier component can now be an `R`, binding an `impl T for X`.

     So one can access trait items (see trait_item above) as R::item.
     Associated items can be type-parametric whenever the corresponding
     item could be type-parameteric when exported from a module.

  8. A boundseq with a single bound of variant `ty` above, where ty is
     itself of the form `ident` (i.e. the `<X:T>` case) is implicitly
     expanded into `<X:X=T>`.

----

## <a id="futurework">What the proposal does not cover</a>

There are cases of interest that are not covered by the above proposal.

Most obvious to me are situations where one wants to describe mutual
constraints *between* the items associated with type parameters.
(An example of this is provided by the `gen_orbit` example with
the constraint `DistanceType(I) = N`, and more generally much of the
content of the `requires(..)` clauses from EOP that I deliberately
ignored).  For the examples from EOP, C++ handles this by doing the
template instantiation blindly and applying the type checker to
code after concrete types have been substituted for the parameters;
this approach is not compatible with Rust's design where we want to
type-check a generic body of code in terms of the guarantees provided
by the trait-bounds, *not* delaying those checks until after
the concrete types have been plugged in.

Also, in the changes I proposed above to the Rust grammar (and
somewhat implicitly to its semantics), I deliberately constrained my
focus to the cases Niko described in his blog posts: types, functions,
and constants.  But one might consider further extensions, such as
allowing traits to define *other* traits.  (I found that subject hard
to wrap one's mind around, and I wanted to keep the focus limited for
Rust 1.0; we can leave generalizations of this approach for after Rust
1.0.)

Also, I'm not sure whether there is need and/or utility in further
generalizing this topic to [associated data families][7].  Again,
I want to limit the scope of the work to something we believe we can
accomplish for Rust 1.0.

What else have I missed?  Let me know, leave a comment.  (Or look
for me in the #rust irc channel.)


[1]:http://www.elementsofprogramming.com/

[2]:http://dl.acm.org/citation.cfm?id=949317

[3]:http://osl.iu.edu/publications/prints/2003/comparing_generic_programming03.pdf

[4]:https://github.com/mozilla/rust/issues/5033

[5]:http://www.smallcultfollowing.com/babysteps/blog/2013/04/02/associated-items/

[6]:http://www.smallcultfollowing.com/babysteps/blog/2013/04/03/associated-items-continued/

[7]:http://www.haskell.org/haskellwiki/GHC/Type_families#An_associated_data_type_example
