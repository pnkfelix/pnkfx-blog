---
layout: post
title: "Breaking News: Non-Lexical Lifetimes arrives for everyone"
date: 2019-06-26 12:54:29 +0200
comments: true
categories: 
---
Hey there everyone!

It has been literally years since I last posted to this blog.
I have been doing a bunch of Rust compiler work.
One big feature has been deployed: Non-Lexical Lifetimes (hereafter denoted "NLL").

The motivation for this blog post: The next version of Rust, 1.36, is
going to have NLL turned on for the 2015 edition. Going forward,
all editions of Rust will now use NLL.
That is a big change.
It is mostly a change that empowers developers;
but it also is a change that will cause some existing code to break.

This post is going to talk a little bit about what NLL is and why you
as a Rust developer should be excited about it as a feature
(because it enables programmers to express themselves more directly).

However, even though the main feature of NLL is to *increase*
the set of programs that Rust accepts,
the vast majority of this post will focus on cases where the
switch to NLL has caused the compiler to start
rejecting{% marginnote 'soundness-as-a-feature' 'Rejecting unsound code is also a Rust feature that I value highly; but it often fails to excite developers, especially when it causes their legacy code to stop compiling.' %}
code that it used to accept erroneously.

<!-- more -->

<script>
// See https://github.com/imathis/octopress/issues/424
$(document).ready(function(){
    $('body').addClass('collapse-sidebar');
});
</script>

## Table of Contents

 * [Background: What is NLL](#Background:.What.is.NLL)
 * [How was NLL implemented](#How.was.NLL.implemented)
 * [NLL uses a new MIR borrow-checker](#NLL.uses.a.new.MIR.borrow-checker)
 * [NLL resolves soundess bugs](#NLL.resolves.soundness.bugs)
 * [NLL deployment and the migration mode](#NLL.deployment.and.the.migration.mode)
 * [But what about the 2015 edition?](#But.what.about.the.2015.edition.)
 * [What are the soundness issues fixed by NLL?](#What.are.the.soundness.issues.fixed.by.NLL.)
   * [Closure fixes](#Closure.fixes)
   * [Match fixes](#Match.fixes)
   * [Corrections to the model of control-flow](#Corrections.to.the.model.of.control-flow)
   * [Fixed leaking into Drop](#Fixed.leaking.into.Drop)
   * [The "liveness check"](#The..liveness.check.)
   * [Outlawed partial assignment to moved values (for now)](#Outlawed.partial.assignment.to.moved.values..for.now.)
 * [Conclusion, Related Posts, and Thanks](#Conclusion..Related.Posts..and.Thanks)
 
## Background: What is NLL

NLL is a change to Rust's static model of *lifetimes*. Lifetimes are
approximations of the dynamic extent of values in a program. In
earlier versions of Rust, lifetimes had a close correspondence with
lexical scope.

```rust
fn main() {                               // SCOPE TREE
                                          //
    let mut names =                       // +- `names` scope start
        ["abe", "beth", "cory", "diane"]; // |
                                          // |
    let alias = &mut names[0];            // | +- `alias` scope start
                                          // | |
    *alias = "alex"; // <------------------------ write to `*alias`
                                          // | |
    println!("{}", names[0]); // <--------------- read of `names[0]`
                                          // | |
                                          // | +- `alias` scope end
                                          // +- `name` scope end
}
```

The above code would be rejected under the old lexical model: the
mutable-borrow `&mut names[0]` from line 6 would last until the end of
the scope of `alias`. Since `alias` is declared at the top-most block
that forms the body of `fn main`, its scope lasts until the end of
that body, and so when you would try to compile this code, you would get an error like this:

```text
error[E0502]: cannot borrow `names[..]` as immutable because it is also borrowed as mutable
  --> src/main.rs:10:20
   |
6  |     let alias = &mut names[0];
   |                      -------- mutable borrow occurs here
...
10 |     println!("{}", names[0]);
   |                    ^^^^^^^^ immutable borrow occurs here
...
14 | }
   | - mutable borrow ends here
```

But this compilation error does not correspond to an actual
unsoundness in the code: the read of `names[0]` always comes after the
last write through the aliasing reference. This error is an artifact
of the old model of lifetimes that was tightly wedded to lexical
scopes.

With NLL, the Rust compiler internally represents the fact that the
mutable-borrow `&mut names[0]` does not need to last all the way to
the end of the lexical scope of `alias`; the mutable-borrow only needs
to last until the final access through `alias` on line 6. This shorter
mutable-borrow no longer overlaps with the read-access of `names[0]`
on line 10, and thus the program is accepted by the compiler with NLL enabled.

You can see this in action by trying out the program in the Rust
[playpen][play eg-basic]; this link defaults to the 2018 edition of Rust,
which has had NLL enabled ever since its official [release][2018 release note].

[play eg-basic]: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=caa469acdfb3c093a9659bb6b4685a09

[2018 release note]: https://blog.rust-lang.org/2018/12/06/Rust-1.31-and-rust-2018.html#non-lexical-lifetimes

You can read more about the motivation for NLL, with several other examples, on
[Rust RFC 2094: NLL][RFC 2094].

[RFC 2094]: https://rust-lang.github.io/rfcs/2094-nll.html

## How was NLL implemented

In Rust, the "borrow checker" is a static analysis dedicated to enforcing rules like:
 "you cannot write through a shared reference",
 "you cannot write through an aliased mutable reference", or
 "references never outlive their data".


One crucial part of NLL's implementation was the switch from the
legacy AST borrow-checker to a new MIR borrow-checker.

### Background: What is MIR

The Rust compiler works by parsing a program into an Abstract Syntax Tree (AST).

{% marginblock %}
Before MIR, we tried to lessen these issues via two abstractions, called "Category, Mutabilty, Type" (CMT) and "ExprUseVisitor" (EUV). These hid away AST semantic details and reduced an analysis's concerns to a relatively small set of semantic effects.
Translating to MIR and analyzing that directly has been a superior approach, from a developer-efficiency point of view. MIR is used by more systems within the compiler; thus, MIR's encoding and processing are subject to more compiler developers' scrutiny. This tends to expose bugs more readily. Furthermore, since analyses are operating directly on MIR structures that have a concrete semantics, there is a lower barrier for entry for new compiler  developers seeking to help with MIR-based analyses. The CMT and EUV abstractions, in my opinion, were one reason that AST borrow-checker development was considered a black art.
{% endmarginblock %}
AST-based analyses can suffer from three issues:

  1. First, individual nodes in the AST for a Rust program can have a relatively complicated meaning.
     Examples: `recv.invoke()` may move or borrow `recv`, based on the method's signature;
     also, a `match` expression may move or borrow its input based on the form of pattern on the
     match arm that is executed.

  2. Second, there are many different varieties of AST nodes, and the
     quality of an analysis may depend on handling all of them. Some
     nodes may have similar semantics (e.g. `if` and `if let` can be
     desugared to `match`), but leveraging such similarity is not
     trivial.

  3. Third, the tree-structure of the AST does not directly mirror the control-flow of the
     program it represents. Any static analysis interested in the control-flow of an
     AST must build a model of that control-flow; analyses using that modeled control-flow
     take the risk that the modeled control-flow
     *diverges* from the actual control-flow that is actually implemented by the low-level code generator.
     This was a significant source of subtle bugs in the AST borrow-checker.

These factors make it difficult to implement and maintain compiler analyses that operate on the AST itself.
This in turn put limits on our ambition when designing such analyses.

Our solution to this suite of problems,
which plagued many aspects of the Rust compiler,
was the introduction of MIR.

MIR is the "Middle Intermediate Representation" of a Rust program.

After doing some initial AST-based static analyses,
the compiler translates from the AST representation into MIR.
After this translation, subsequent static analyses operate on the MIR.

The MIR is designed to be able to express the semantics of the Rust program
via a smaller-set of basic constructs,
while also encoding the information needed to do static analyses such as type-checking.

The MIR representation is a graph structure that directly corresponds to the program control-flow.
All control-flow, such as loops, breaks, or panics from function invocations, are explicitly encoded.
Complicated AST nodes like `match` are translated into compositions of simpler MIR nodes.

Since each MIR node is simple, and there is a smaller set of them to consider,
many MIR analyses are easier to implement and maintain than their analogous AST analyses,
addressing the first two issues above.

Since MIR directly encodes the program control-flow,
there is less risk of divergence between the control-flow model and the actual runtime behavior,
addressing the third issue above.

You can read more about the motivations for MIR, which go beyond those
listed here, on [Rust RFC 1211: MIR][RFC 1211].

[RFC 1211]: https://rust-lang.github.io/rfcs/1211-mir.html

## NLL uses a new MIR borrow-checker

As noted above, NLL uses a new borrow-checker that processes MIR rather than the AST.

The main motivation for developing NLL was improving the expressiveness of Rust as a language:
we wanted to enable developers to write code that more directly corresponded to their intuition about
when references were valid and whether aliasing might occur.
(It doesn't hurt that the new code was also more concise.)

Fundamentally, NLL requires operating on a control-flow graph
(that's what puts the "non-lexical" in "non-lexical lifetimes").

## NLL resolves soundness bugs

As noted above, there were a host of bugs in the old AST borrow-checker.
Unsound code was sometimes accepted by the compiler
because the AST borrow-checker was not reasoning correctly about the program's semantics.

Fixing such bugs was *not* a primary motivation for implementing NLL.
The fact that such bugs would be fixed effectively "for free" as a side-effect of operating on MIR
was considered a welcome side-benefit.

There was a social effect on development though, and it is probably worth noting:
Since we, the Rust compiler developers, knew that
the MIR borrow-checker was going to be the *primary borrow-checker* in the (relatively near) future,
there was *signficantly less* motivation to fix bugs in the (soon-to-be legacy) AST borrow-checker.

There are currently 8 issues in the Rust bug database that have been tagged as
soundness bugs that are [`NLL-fixed-by-NLL`][I-unsound NLL-fixed-by-NLL].
The response from the compiler developers for these bugs has usually been
one of either "it is not feasible to fix this in the AST borrow-checker"
or "it is not *worthwhile* to invest time fixing this in the AST borrow-checker."

[I-unsound NLL-fixed-by-NLL]: https://github.com/rust-lang/rust/issues?utf8=%E2%9C%93&q=is%3Aissue++label%3ANLL-fixed-by-NLL+label%3A%22I-unsound+%F0%9F%92%A5%22+

To be clear: I am in favor of such a response.
Which is unsurprising, since I have been one of the main voices behind such responses.

But it did mean that such code was allowed to persist in the wild.
Let us dive into that now.

## NLL deployment and the migration mode

NLL was a big change to the compiler. We knew it might suffer from latent bugs:
it might unexpectedly regress compile-times, or
it might incorrectly accept unsound code, or
it might incorrectly reject valid code as unsound.

Furthermore, as discussed in the previous section,
we knew there were instances of previously-accepted code that would be *correctly* rejected by NLL
as unsound
(due to soundness fixes introduced with MIR borrow-check).

So we knew that NLL could have a negative impact on the developer experience,
but we did not know what the scope of such an impact might be.
To reduce the risk here, we limited the initial deployment of the NLL feature solely
to Rust code targeting the [2018 edition of the language][2018 edition guide NLL].
Rust crates that did not opt into using the 2018 edition
would still be checked by the old AST borrow-check, warts and all.

[2018 edition guide NLL]: https://doc.rust-lang.org/stable/edition-guide/rust-2018/ownership-and-lifetimes/non-lexical-lifetimes.html

Furthermore, since we did not want our end users to hit a road-block
where an attempt to migrate a pre-existing crate from the 2015 edition to the 2018 edition
was stymied by borrow-check errors that they had never seen before,
we included a further safeguard: the *NLL migration mode*.

The idea behind the NLL migration mode is simple.
First run the MIR borrow-checker on the input crate (buffering any
diagnostics it issues as it runs);
if the code is accepted by the MIR borrow-checker, then borrow-checking is done.
If the MIR borrow-checker rejects the input crate, *don't* reject it yet;
ask the AST borrow-checker whether it would also reject the crate.
If the AST borrow-checker would accept the crate, then this represents a case
where NLL is rejecting code that the AST borrow-checker would have accepted.
The compiler assumes any such case is an instance of a pre-existing bug in the AST borrow-checker;
it then emits the previously-buffered diagnostics, *downgrading* any emitted errors to future-compatibility warnings.

The heart of this is that the end user still gets feedback about problems in their code,
but they can keep using their crates for the short-term.

{% marginblock %}
One more slight UX downside:
an end-user has to wait until both the MIR borrow-checker *and* the AST borrow-checker run
before seeing diagnostic feedback about errors in a given piece of code.
{% endmarginblock %}
The main downsides of the NLL migration-mode are

  * Soundness bugs in end-user crates might go unaddressed

  * The Rust developers cannot remove the legacy AST borrow-checker
    code base. (But we cannot remove it until all editions are using NLL anyway.)

{% marginblock %}
If you are using the Nightly compiler and want to try turning off migration mode yourself,
you can use `#![feature(nll)]` to do so.
{% endmarginblock %}
In the long-term, we do intend to turn off the migration mode.
With migration mode off, any errors issued by NLL will cause the compiler to reject the input crate,
regardless of what the AST borrow-checker used to say about it.
(We have not yet established a time-line for when the migration mode will be turned off.)

## But what about the 2015 edition?

Now that the 2018 edition has been out for about six months, we have
gained more confidence in the quality of the code underpinning NLL,
and we are taking the next step: turning on NLL for the 2015 edition.

As I said at the start of this post, the next Rust release, 1.36, is
going to have NLL turned on for all crates, regardless of edition.

As you might have guessed, we are only turning on the NLL migration mode.
If a crate has been unknowingly taking advantage of some soundness bug
in order to be accepted by the AST borrow-check, the initial impact of NLL
on that crate should be limited to future-compatibility warnings.

## What are the soundness issues fixed by NLL?

At this point, you may have questions:

 * "Is my crate going to be affected by this?",
 * "What do these warnings from NLL look like?", or
 * "What should I do to address these warnings when I see them?"

As I already mentioned, there are currently 8 issues tagged as
[soundness bugs fixed by NLL][I-unsound NLL-fixed-by-NLL].

What I want to do in the rest of this post is show concrete examples of
known crates that ran afoul of one of these soundness bugs. My hope in each case is to
show a reduced example of real code that breaks the rules set by the borrow-checker,
explain why breaking that rule is bad,
and then show how one might resolve the problem exposed by the compiler.

### Closure fixes

NLL fixes the handling of closures in various ways.

Here is an example reduced from (an outdated version of) the `bart` crate
([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=21b506be36f876dddfd523c72f8cc8f2)):

```rust
use std::fmt::{self, Write};

pub struct EscapingWriter<'a> { inner: &'a mut Write }

impl<'a> Write for EscapingWriter<'a> {
    fn write_str(&mut self, buf: &str) -> fmt::Result {
        // Sneaky use of String::split, capturing the separator:
        let mut separator = '_';
        for part in buf.split(|x| { separator = x; x == '"' }) {
            self.inner.write_str(part)?;
            match separator {
                '"' => self.inner.write_str("&quot;"),
                _ => Ok(()),
            }?;
        }

        Ok(())
    }
}
```

which now yields this diagnostic:

```text
warning[E0503]: cannot use `separator` because it was mutably borrowed
  --> src/lib.rs:12:17
   |
9  |         for part in buf.split(|x| { separator = x; x == '"' }) {
   |                     ------------------------------------------
   |                     |         |     |
   |                     |         |     borrow occurs due to use of `separator` in closure
   |                     |         borrow of `separator` occurs here
   |                     borrow later used here
...
12 |                 '"' => self.inner.write_str("&quot;"),
   |                 ^^^ use of borrowed `separator`
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

As the code notes, this was a sneaky use of `String::split`.
Too sneaky, in fact.
The closure passed to `buf.split` captures separator by `&mut`-reference;
it *has* to for this code to "work", since
the whole idea is to sneak in the mutable reference as a side-channel
for transmitting information out of the invocation of the closure.

The problem is that the closure ends up being stored in the `Split` iterator returned by the `split` call;
this is represented by the statement that the borrow is used by the invocation itself,
`buf.split(...)` that is highlighted and labeled "borrow later used here".
Since the `Split` iterator is what is providing the inputs to this `for`-loop,
the `&mut`-borrow of `separator` has to live *across* the body of the `for`-loop.

The Rust memory model does not allow you to have a `&mut`-borrow alive
across other accesses to its data, so this is rejected by the MIR borrow-checker.
The compiler issues an error, which is downgraded above to a warning by the migration mode (due to the old buggy AST borrow-checker accepting the code as written).

#### How can you fix this?

The fact that `separator` is a local bound outside of the loop is a wart:
In my opinion, this code really wants
bind the specific separator it found for *each* iteration of the loop.

In this specific case, it seems like the `str` API is a bit impoverished;
I see many variants of the `split` method,
but I see none that let you extract each of the delimiters that matched the given pattern while also extracting the portions of the string that did not match.

My answer, at least for now, is probably to rewrite the loop using [`match_indices`][str::match_indices] ([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=2b49e65b1134438104b43d1167101315)):

```rust
use std::fmt::{self, Write};

pub struct EscapingWriter<'a> { inner: &'a mut Write }

impl<'a> Write for EscapingWriter<'a> {
    fn write_str(&mut self, buf: &str) -> fmt::Result {
        let mut last_index = 0;
        for (index, delim) in buf.match_indices(|x| x == '\'') {
            self.inner.write_str(&buf[last_index..index])?;
            match delim {
                "\'" => self.inner.write_str("&quot;"),
                _ => Ok(()),
            }?;
            last_index = index;
        }
        self.inner.write_str(&buf[last_index..])?;

        Ok(())
    }
}
```


[str::match_indices]: https://doc.rust-lang.org/std/primitive.str.html#method.match_indices

You can read about other fixes to the handling of closures at the following issues:

 * [\#53432 "Bug: nested closure outlives borrowed value."][issue #53432]

### Match fixes

Another area that got a lot of scrutiny with the switch to MIR
borrow-check was the handling of `match` expressions.

Consider the following code ([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=fc1a4fbfd1ed9ad0282c292afe64d5ac)):

```rust
extern crate crossbeam;

fn main() {
    let zero = &0;
    let one = &1;
    let mut x = Some(zero);
    crossbeam::scope(|s| {
        if let Some(ref mut y) = x {
            s.spawn(move |_| { *y = one; }); 
            //                 ~~~~~~~~~
            //                     |
            match x { // <---- DATA RACE on discriminant of `x`!
                Some(_) => println!("saw some"),
                None => println!("saw none"),
            }
        }
    }).unwrap();
}
```

which now yields this diagnostic:

```text
warning[E0503]: cannot use `x` because it was mutably borrowed
  --> src/main.rs:13:17
   |
7  |     crossbeam::scope(|s| {
   |                       - has type `&crossbeam::thread::Scope<'1>`
8  |         if let Some(ref mut y) = x {
   |                     --------- borrow of `x.0` occurs here
9  |             s.spawn(move |_| { *y = one; }); 
   |             ------------------------------- argument requires that `x.0` is borrowed for `'1`
...
13 |                 Some(_) => println!("saw some"),
   |                 ^^^^^^^ use of borrowed `x.0`
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

Your first reaction, upon reading that, might be "there is no use of
`x.0` there. That match is only looking at the discriminant that
differentiates `Option::Some` from `Option::None`, not the data held
in the `Some`."

{% marginblock %}
The initial version of the MIR borrow-checker actually made the same erroneous assumption here.
But the soundness checks performed by MIRI exposed the flaw in reasoning here; see [issue #56797][].
{% endmarginblock %}
And indeed, the AST borrow-checker assumes that the discriminant for
an enum value is always stored at a memory location that is disjoint from the
fields of that enum variant.


But this assumption is not generally true; Rust is free to make use of
data fields to hold an enum discriminant. (The simplest example of
this: a `&T` is always a non-null pointer value, and therefore we can
represent an `Option<&T>::None` as null!)


This means that an access to the discriminant of an enum
*might*{% marginnote 'freedom of discrimination' 'I say "might" here because we want to maximize the freedom for future versions of the compiler in how it chooses to represent enum discriminants.' %}
access memory stored in the fields of the enum's variants.

In this case, the highlighted `Some(_)` labeled "use of borrowed `x.0`" is the
point where the discriminant (and thus the memory at the location of that field)
might be read. But that would be a data-race with the mutable borrow of
`x.0` that was stored in `y` and passed off to another thread.
The compiler issues an error (which is downgraded above to a warning by the migration mode).

In practice, what you're likely to see is a similar error on this variant of the above code ([play]()):

```rust
extern crate crossbeam;

fn main() {
    let zero = &0;
    let one = &1;
    let mut x = Some(zero);
    crossbeam::scope(|_| {
        if let Some(ref mut y) = x {
            match x {
                Some(_) => println!("saw some"),
                None => println!("saw none"),
            }
            *y = one;
        }
    }).unwrap();
}
```

which now yields this diagnostic:

```text
warning[E0503]: cannot use `x` because it was mutably borrowed
  --> src/main.rs:10:17
   |
8  |         if let Some(ref mut y) = x {
   |                     --------- borrow of `x.0` occurs here
9  |             match x {
10 |                 Some(_) => println!("saw some"),
   |                 ^^^^^^^ use of borrowed `x.0`
...
13 |             *y = one;
   |             -------- borrow later used here
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

This does not represent a data-race, but it does represent a violation
of the Rust memory model that is designed to prevent data races like
the one above.

#### How can you fix this?

One option is to change the code to avoid inspecting the discriminant a second time.
Another option is to reorganize the code so that the
final write to `*y` is guaranteed to precede the second match of `x`.

#### Fixes to match guards

Another instance of a problem with `match` that has been resolved by NLL is the handling of *match guards*.

Consider this code, reduced from the [htmlgrep crate][] ([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4246e291c64c2b3550a7f633bdbe85ca)):

[htmlgrep crate]: https://github.com/andreastt/htmlgrep

```rust
pub struct Matches;
pub struct Options { separator: char, max: Option<usize>, }

impl Matches { fn count(self) -> usize { 0 } }

fn print_count(matches: Matches, opts: Options) {
    match opts.max {
        Some(n) if matches.count() > n => print!("{}{}", n, opts.separator),
        Some(_) | None => print!("{}{}", matches.count(), opts.separator),
    }
}
```

The above code is accepted by AST borrow-check, but it has a huge mistake:
the `count` method takes `Matches` by value,
which means evaluating the first guard consumes it.
This code should *not* be allowed to invoke `matches.count()`
on the second match arm.

With NLL, we now see the following diagnostic:

```text
warning[E0382]: use of moved value: `matches`
  --> src/lib.rs:11:36
   |
8  | fn print_count(matches: Matches, opts: Options) {
   |                ------- move occurs because `matches` has type `Matches`, which does not implement the `Copy` trait
9  |     match opts.max {
10 |         Some(n) if matches.count() > n => print!("{}{}", n, opts.separator),
   |                    ------- value moved here
11 |         Some(_) | None => print!("{}{}", matches.count(), opts.separator),
   |                                          ^^^^^^^ value used here after move
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

#### How can you fix this?

In this case, the code is just wrong. Either the `count` method should
be taking `&self` rather tnan `self`, or it should calcuate
`matches.count()` once and save its value in a local variable before
entering the `match`.

----

For further discussion of various bugs in borrow-checking `match` that are
resolved by NLL, see the following issues on the Rust repository:

 * [\#27282 "Can mutate in match-arm using a closure"][issue #27282]
 * [\#45045 "binding-less matches on borrowed data are incorrectly allowed"][issue #45045]
 * [\#56797 "Matching on an enum should read the entire enum"][issue #56797]

### Corrections to the model of control-flow

As mentioned above, one of the problems with AST borrow-check
is that it used an internal model of control-flow that could diverge from
the actual control-flow of the generated program.

<!-- An easy example of this can be seen in this code from reduced from the [fractions_and_matrices][] crate on github.
[fractions_and_matrices]: https://github.com/AuroransSolis/fractions_and_matrices
git@github.com:AuroransSolis/fractions_and_matrices.git
-->

An easy example of this can be seen in this code from the [floriama/agents][] crate on github
([line permalink](https://github.com/floriama/agents/blob/4cbecde2dbb408266b94c1ac73aa7c2d405936f3/src/network.rs#L215)):

[floriama/agents]: https://github.com/floriama/agents/

```rust
self.graph.node_payload_mut(i_idx).physics.pos +=
    self.graph.node_payload(i_idx).physics.vel.scale(dt);
```

This used to be accepted, because the AST borrow-check used a
control-flow model that said `LHS += RHS` is evaluated like so:

 1. evaluate the right-hand side (RHS), putting its value into a temporary local.
 2. evaluate the left-hand side (LHS),
 3. call the `add_assign` method on the evaluated LHS, passing it the RHS.

But that does not agree with the actual semantics of Rust.
Rust actually evaluates the LHS *before* the RHS and then performs the
`add_assign` invocation.

So you can probably now see the problem with the code above: the AST
borrow-check allows the immutable borrow for the evaluation of the RHS.
Such an immutable borrow does not need to outlive that evaluation.
Thus, if the RHS were evaluated first, then the immutable borrow
would not overlap the `&mut`-borrow required for the evaluation of LHS.
But the LHS *is* evaluated first, and its `&mut`-borrow does need to last
up through and including the invocation of `add_assign`.

Thus, the line of code above now issues the following diagnostic:

```text
warning[E0502]: cannot borrow `self.graph` as immutable because it is also borrowed as mutable
   --> src/network.rs:216:17
    |
215 |               self.graph.node_payload_mut(i_idx).physics.pos +=
    |               ----------
    |               |
    |  _____________mutable borrow occurs here
    | |
216 | |                 self.graph.node_payload(i_idx).physics.vel.scale(dt);
    | |_________________^^^^^^^^^^_________________________________________- mutable borrow later used here
    |                   |
    |                   immutable borrow occurs here
    |
    = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
    = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

<!--
Even the simpler expression `LHS = RHS` has the same issue here:
the AST borrow-check models this as evaluating RHS before LHS,
but its actual behavior is that the LHS is evaluated first.

https://crater-reports.s3.amazonaws.com/pr-60914/try%23f45cc3094ee337acd688771b9234318046b0572d/gh/ethowitz.life/log.txt
-->

#### How can you fix this?


In this case, the common fix is to do the evaluation of the RHS before the LHS, like so:

```rust
let rhs = self.graph.node_payload(i_idx).physics.vel.scale(dt);
self.graph.node_payload_mut(i_idx).physics.pos += rhs;
```

{% marginblock %}
This text is a bit blasé about the effort associated with transforming
`LHS += RHS` into `let rhs = RHS; LHS += rhs`
In fact, there were bugs we fixed in NLL that would have required a similar pattern
to be deployed widely across many code bases; namely, cases where one did `foo.mutate(&*foo.field)`
would have been forced to be rewritten as `{ let rhs = &*foo.field; foo.mutate(rhs) }`
We did realize that for NLL to be adopted in practice, we needed to continue accepting such code.
This is what motivated the introduction of two-phase borrows aka [RFC 2025: "nested method calls"][RFC 2025].
{% endmarginblock %}
Of course, you should verify that such a reordering is actually valid in each such case;
The fact that this code rewrite is not semantics-preserving in general is why we did not
just have the compiler do it for you behind the scenes.

[RFC 2025]: https://rust-lang.github.io/rfcs/2025-nested-method-calls.html

----

For further discussion of these issues with modeling control-flow, see

 * [\#27868 "Inconsistent evaluation order for assignment operations"][issue #27868]

 * [\#38899 "borrowed referent of a `&T` sometimes incorrectly allowed"][issue #38899]

### Fixed leaking into Drop

Our next example is reduced from code found in the [galvanize][] crate on github :

[galvanize]: https://github.com/estebank/galvanize/blob/a9624a317d8664dced737b203d2712f4127eb90e/src/writer.rs#L121

```rust
struct Reader<'a, F> { file: &'a mut F }
struct Writer<'a, F> { file: &'a mut F }


impl<'a, F> Writer<'a, F> {
    pub fn as_reader(mut self) -> Result<Reader<'a, F>> {
        {
            let s = &mut self;
            s.finalize();
        }
        Reader::new(self.file)
    }
}
```

That looks pretty innocent, right?

Well, lets see what NLL has to say about it ([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=a0143f0e7bbf1bf0bfeb3eac4c6c8801)):

```text
warning[E0713]: borrow may still be in use when destructor runs
  --> src/lib.rs:12:21
   |
6  | impl<'a, F> Writer<'a, F> {
   |      -- lifetime `'a` defined here
...
12 |         Reader::new(self.file)
   |         ------------^^^^^^^^^- returning this value requires that `*self.file` is borrowed for `'a`
13 |     }
   |     - here, drop of `self` needs exclusive access to `*self.file`, because the type `Writer<'_, F>` implements the `Drop` trait
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future

```

Oh, yeah, I forgot to mention: there's an `impl Drop` on `Writer`, and that changes everything.

As the diagnostic explains, the destructor for `Writer` has exclusive access to its fields while it runs,
including the `&mut` borrow in `self.file`.

Therefore we cannot just hand off `self.file` to the new `Reader` while also allowing the destructor to run;
that would violate Rust's aliasing rules for `&mut`.

#### How can you fix this?

One approach is wrap an `Option` around the `&mut F` reference in `Writer`, and swap in `None` in the `as_reader` method.
This is sort of a drag though, since it means all the other `Writer` methods end up checking for `None`, just because of this usage here.

You might have noticed that `as_reader` is already doing the same work that `drop` does (namely, call `finalize` on the writer).
So it would be nice if we could move the file out of `self` and then `mem::forget(self)` to prevent its destructor from running.
Unfortunately, that does not work: `mem::forget` still wants a fully-formed value for its input.


{% marginblock %}
Note however that this API has not been stablized and is only
available in the Nightly channel.
{% endmarginblock %}
The previous idea does yield the basis for a valid second approach, though:
wrap `ManuallyDrop` around the `&mut F`, and use
 its [`take` method][manually-drop-take]
to extract the file before forgetting `self`. (This does require using `unsafe`1, though.)
Here what that looks like ([play](https://play.rust-lang.org/?version=nightly&mode=debug&edition=2018&gist=e8be2d4004746e6264ac87327f9a0454)):

```rust
    pub fn as_reader(mut self) -> Result<Reader<'a, F>> {
        {
            let s = &mut self;
            s.finalize();
        }
        unsafe {
            let r = Reader::new(ManuallyDrop::take(&mut self.file));
            std::mem::forget(self);
            r
        }
    }

```

[manually-drop-take]: https://doc.rust-lang.org/std/mem/struct.ManuallyDrop.html#method.take

----

For further discussion of how destructors interact with the
borrow-checking, see the following issues on the Rust repository:

 * [\#31567 "`Drop` and leaking `&mut` references"][issue #31567]

<!--
### Corrections to handling of unions

 * [\#45157 "Borrow checker unsoundness with unions"][issue #45157]

### Corrections to handling of const-promoted expressions

 * [\#46557 "Compiler accepts return mut ref to local var on no longer valid stackframe"][issue #46557]

 * [\#49955 "Garbage value when accessing a reference into a field/element of a const value."][issue #49955]
-->

<!--
### Corrections to region-checking
Here is an example reduced from (an outdated version of) the `compare-version` crate.
--Nope, not sure this is something to advertise; see https://github.com/rust-lang/rust/issues/62185
-->


### The "liveness check"

The liveness check enforces the rule: "any region which appears at the point P
must contain the point P."

The AST borrow-check did not actually enforce this
rule in all cases.

Here is an example reduced from an outdated version of the `enumflags2` crate ([play](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=522815bfdc2bbaba98b6ab6673b93937)):

```rust
pub trait RawBitFlags: Copy + Clone {
    type Type;
    fn flag_list() -> &'static [Self];
}

pub struct BitFlags<T: RawBitFlags> {
    val: T::Type,
}

impl<T: RawBitFlags> BitFlags<T> {
    fn iter(self) -> impl Iterator<Item = T> {
        T::flag_list().iter().cloned()
    }
}
```

which now yields this diagnostic:

```text
warning[E0310]: the parameter type `T` may not live long enough
  --> src/lib.rs:13:22
   |
13 |     fn iter(self) -> impl Iterator<Item = T> {
   |                      ^^^^^^^^^^^^^^^^^^^^^^^
   |
   = help: consider adding an explicit lifetime bound `T: 'static`...
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future

warning[E0310]: the parameter type `T` may not live long enough
  --> src/lib.rs:14:9
   |
14 |         T::flag_list().iter().cloned()
   |         ^^^^^^^^^^^^^^
   |
   = help: consider adding an explicit lifetime bound `T: 'static`...
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

Here, we see that the compiler is newly detecting a case that it had
previously missed, where the existential type returned by `iter` holds
instances of the type `T`.
The compiler wants the code to state up front that these values will
not carry any non-static references, because of the liveness rule:
since we do not have any constraint on where in this control-flow
this iterator will be used, any regions in its type must contain all
points in the control-flow.

This liveness rule was a change that Niko had spent some time
considering adding to the AST borrow-check as well, as documented in
[PR 55988: introduce "type must be valid for" into lexical region solver][PR 55988].
In the end, he found a narrower fix that addressed the most serious cases,
and we ended up falling back on our default plan of letting the remaining bugs in this area
just get fixed with the migration to NLL.

[PR 55988]: https://github.com/rust-lang/rust/pull/55988

----

You can see further discussion of this on:

 * [\#55756: Lifetimes bug on beta 1.31: the associated type may not live long enough][issue #55756]

### Outlawed partial assignment to moved values (for now)

One last thing to mention that is *not* a soundness fix, but does break some AST borrow-check code in the wild.
The NLL system finally fixed a bug that was uncovered over four years ago.


Here's one of the instances I found of this bug, from the `epub` crate:

```rust
...
let arnode = Rc::new(RefCell::new(node));
{
    let current = parents.last();
    if let Some(c) = current {
        c.borrow_mut().childs.push(arnode.clone());
        node.parent = Some(Rc::downgrade(&c));
    }
}
...
```

This now yields the following diagnostic:

```rust
warning[E0382]: assign to part of moved value: `node`
  --> src/xmlutils.rs:68:29
   |
53 |                     let mut node = XMLNode {
   |                         -------- move occurs because `node` has type `xmlutils::XMLNode`, which does not implement the `Copy` trait
...
62 |                     let arnode = Rc::new(RefCell::new(node));
   |                                                       ---- value moved here
...
68 |                             node.parent = Some(Rc::downgrade(&c));
   |                             ^^^^^^^^^^^ value partially assigned here after move
   |
   = warning: this error has been downgraded to a warning for backwards compatibility with previous releases
   = warning: this represents potential undefined behavior in your code and this warning will become a hard error in the future
```

I freely admit that this *does not* represent undefined behavior of any
sort. The language design team's decision to outlaw this code was not based
on catching soundness errors; the motivation was for overall [consistency in the language](https://github.com/rust-lang/rust/issues/21232#issuecomment-427152371).

However, I do suspect that this often will catch a bug in the code when it arises. (For example, in this case: did the author actually intend to mutate the field on `arparent` intead?)

(The basic problem is that if you initialize a field of an
uninitialized struct, you currently cannot read from that field. So
doing such an initialization serves almost no purpose.)

#### How can you fix this?

Your main options here are one of the following:

 1. stop writing to the fields in question, or
 2. re-initialize the struct value before doing a write, or
 3. restructure the code so that you use locals instead of a single struct.

----

For much further discussion of how we ended up outlawing this coding
construct, and some hypothetical plans for treating it more
respectfully in the future, see:

 * [\#21232: borrow-checker allows partial reinit of struct that has been moved away, but no use of it.][issue #21232]

 * [\#54986: NLL: Reject partial init of uninitialized record (struct or tuple)][issue #54986]

 * [\#54987: Goal: Accept partial initialization + use of records created via such][issue #54987]

 * [\#60450: assign to part of possibly uninitialized variable is flagged as UB (but only in 2018 edition)][issue #60450]


## Conclusion, Related Posts, and Thanks

{% marginblock %}
I might update this post in the future if I encounter other
significant regressions that are calling out to be documented in this
manner. But for now I am hoping that this list will suffice to entertain you all.
{% endmarginblock %}
That's all of the AST borrow-check regressions that I had the stomach to try to categorize
in this post. 

If you are interested in learning more about the technology underpinning NLL,
and the process we used to develop it over the course of [six years][issue #6393], I highly
recommend Niko's [series][niko NLL] of posts on the subject.

[niko NLL]: http://smallcultfollowing.com/babysteps/categories/#NLL

And, as you might be able to tell, there is still more work to be done!
For example, the original plans for NLL included a goal known in the NLL RFC as
[Problem Case \#3][problem case 3]. We ended up scaling back our ambitions
in order to ensure that we could deploy a viable product with the 2018 edition;
but that just means there is opportunity to add support for this case, and many
more, in future versions of the Rust compiler.

Having said that, doing the exercise of writing this post has made me appreciate
what a long journey we have had with getting NLL out the door. There have been
so many subtle problems to resolve, and so many great volunteers who stepped
up to the plate to assist us.
I want to give shout-outs specifically to the members, both new and old, of the NLL working group (aka WG-NLL):
qmx, davidtwco, lqd, matthewjasper, spastorino, and blitzerr.
We *literally* could not have done this without your help.

[problem case 3]: https://rust-lang.github.io/rfcs/2094-nll.html#problem-case-3-conditional-control-flow-across-functions

[issue #6393]:  https://github.com/rust-lang/rust/issues/6393
[issue #21232]: https://github.com/rust-lang/rust/issues/21232
[issue #27868]: https://github.com/rust-lang/rust/issues/27868
[issue #27282]: https://github.com/rust-lang/rust/issues/27282
[issue #31567]: https://github.com/rust-lang/rust/issues/31567
[issue #38899]: https://github.com/rust-lang/rust/issues/38899
[issue #45157]: https://github.com/rust-lang/rust/issues/45157
[issue #45045]: https://github.com/rust-lang/rust/issues/45045
[issue #46557]: https://github.com/rust-lang/rust/issues/46557
[issue #49955]: https://github.com/rust-lang/rust/issues/49955
[issue #53432]: https://github.com/rust-lang/rust/issues/53432
[issue #54986]: https://github.com/rust-lang/rust/issues/54986
[issue #54987]: https://github.com/rust-lang/rust/issues/54987
[issue #55756]: https://github.com/rust-lang/rust/issues/55756
[issue #56797]: https://github.com/rust-lang/rust/issues/56797
[issue #60450]: https://github.com/rust-lang/rust/issues/60450
