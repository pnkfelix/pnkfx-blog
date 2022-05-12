---
layout: post
title: "Why I use a debugger"
date: 2022-01-10 11:12:57 -0500
comments: false
categories: rust debugging
---
I have been thinking about the Rust debugging experience, and I want to try
tease apart what value a debugger provides to me.

<!-- more -->

Last year, I gave a talk at [QCon][] that promoted the use of [`rr`][] as a tool
for exploring Rust code. I wanted to convey an *excitement* about debugging,
about using this tool to dive into the weeds of your program's behavior.

[QCon]: https://plus.qconferences.com/plus2021/presentation/reversible-debugging-rr
[`rr`]: https://rr-project.org/

Since then, I have continued to talk to developers, promoting `rr` (and the
associated service [pernos.co][]). And I heard Rust developers say that they are
happy with their experience using instrumentation such as logging statements or
the [tracing][] infrastructure.

[pernos.co]: https://pernos.co/
[tracing]: https://tracing.rs/tracing/

I have to agree: Logging statements *are* useful, and sometimes they are all you
need. Here is a great quote from "The Practice of Programming", by Kernighan and
Pike:

> As personal choice, we tend not to use debuggers beyond getting a stack trace or the value of a variable or two. One reason is that it is easy to get lost in details of complicated data structures and control flow; we find stepping through a program less productive than thinking harder and adding output statements and self-checking code at critical places. Clicking over statements takes longer than scanning the output of judiciously-placed displays. It takes less time to decide where to put print statements than to single-step to the critical section of code, even assuming we know where that is. More important, debugging statements stay with the program; debugging sessions are transient.

This quote is apt for many reasons; I will revisit it again before the end of
this post.

I do not know the typical way that people use debuggers. But I can imagine it
looks much like the image depicted above: you start your program, you use a
single-step command to go through it line by line, and you examine raw memory
dumps to try to figure out what the machine is doing. After that initial
exploration, you might set breakpoints at specific lines of code, and then tell
the debugger to continue running until it hits any of those breakpoints.

{% marginblock %}
Advanced debugger users will set *conditional* breakpoints, which only halt the
program if a specific predicate holds when it hits the breakpoint, or even
*scripted* breakpoints, which will automatically execute a series of debugger
commands each time they hit the breakpoint. One neat trick is that you often can
end a script with `continue`, so that it seems as though the program never
stopped at all, except for the side-effects on the debugger itself (such as
setting *other* breakpoints).
{% endmarginblock %}

But that typical usage pattern doesn't really provide much value over just
adding log statements. If all you want to do is learn what steps the computer
took, and what data was in certain variables at those steps, printing to a log
*is* a better way to achieve that.

So, why am I advocating for more people to include a debugger as one of the tools
to keep ready on their Rust development utility belt?

There are four main features that a debugger gives me that are not always
fulfilled by adding log statements:

 * [object code inspection](#Object.Code.Inspection),
 * [raw control-flow](#Raw.Control-Flow),
 * [memory exploration](#Memory.Exploration), and
 * [hardware watchpoints](#Hardware.Watchpoints).

Of the four, i think hardware watchpoints are the best example of a capability
that is both broadly useful and not typically available outside of a debugger.
I will delve into each of these in turn below.

## Object Code Inspection

By "object code inspection", I mean that I can attach a debugger to any binary,
without having to recompile anything. When your workflow is to add new print
statements each time you want to inspect something new, that means editing your
code and recompiling it. That edit-recompile-debug cycle has two problems: it can
be slow (especially the recompilation step). Worse still, the edits can cause
bugs to mysteriously go away, aka the ["observer effect"][Heisenbugs].
(To be fair, using a debugger can also cause those [Heisenbugs][] to change
behavior. This is where tools like [`rr`][] can really shine.)

[Heisenbugs]: https://en.wikipedia.org/wiki/Heisenbug

How valuable is "object code inspection"? I have given two reasons to avoid the
edit-recompile steps: reduced latency, and reducing the observer effect.

*Honestly:* It is not a frequent need for my current work. Much of my work
involves either fixing bugs that reproduce readily in the face of source code
changes, or adding features (which inherently requires source code changes).

{% marginblock %}
However, in practice this can be very
painful: We do not have much support for building an unoptimized `rustc` (it has
a *very* long bootstrap time), but once you enable optimizations, the debugging
experience degrades quite a bit in Rust today.
{% endmarginblock %}

There is one important exception here: `rustc` itself has a long bootstrap time;
so I have often *tried* to use a debugger to debug `rustc` itself, rather than
rely on an edit-recompile-debug cycle. 

## Raw Control-Flow

By "raw control-flow", I mean that one can step through the individual assembly
code instructions to see exactly what the machine is executing.

For example, at a method invocation, one can step forward and discover how
exactly to which function that invocation dispatches as it goes through `Deref`
implementations until it finally reaches the target method.

As another example, one can step through the the instructions corresponding to
the subcomponents of a `match` pattern. This way, one might discover which parts
matched and which failed to match, and in what order they were evaluated.

Here is a concrete example of the latter:

```rust
struct P { a: char, b: char, c: &'static str }

fn main() {
    let p = P { a: 'h', b: 'i', c: "world" };
    foo(p);
}

fn foo(p: P) {
    match p {
        P { b: 'i', a: 'y', c } => println!("yi, {}", c),
        P { a: 'h', b, c } => println!("h{}, {}", b, c),
        _ => panic!("unmatched"),
    }
}
```

After compiling the above with debuginfo enabled and running it under `gdb`,
I can investigate the body of `foo` while its running:

```
(gdb) step
boom::foo (p=...) at boom.rs:9
9	    match p {
(gdb) disassemble
Dump of assembler code for function _ZN4boom3foo17h3035bcff6328b12bE:
   0x000055555555db70 <+0>:	sub    $0x128,%rsp
   0x000055555555db77 <+7>:	mov    %rdi,0x38(%rsp)
=> 0x000055555555db7c <+12>:	cmpl   $0x69,0x14(%rdi)
   0x000055555555db80 <+16>:	jne    0x55555555db8d <_ZN4boom3foo17h3035bcff6328b12bE+29>
   0x000055555555db82 <+18>:	mov    0x38(%rsp),%rax
   0x000055555555db87 <+23>:	cmpl   $0x79,0x10(%rax)
   0x000055555555db8b <+27>:	je     0x55555555db9d <_ZN4boom3foo17h3035bcff6328b12bE+45>
   0x000055555555db8d <+29>:	mov    0x38(%rsp),%rax
   0x000055555555db92 <+34>:	cmpl   $0x68,0x10(%rax)
   0x000055555555db96 <+38>:	je     0x55555555dbeb <_ZN4boom3foo17h3035bcff6328b12bE+123>
   0x000055555555db98 <+40>:	jmp    0x55555555dc6e <_ZN4boom3foo17h3035bcff6328b12bE+254>
```

In the above, we can see three comparison instructions: a comparison of `0x14(%rdi)` with `$0x69` (the character 'i'), then a comparison of `0x10(%rax)` with `$0x79` (the character 'y'). If they're both equal, then it jumps to `0x55555555db9d` (which is presumably the first arm's body). If they're not, then it does a comparison of `0x10(%rax)` with `$0x68` (the character 'h'), and if *that* is equal, then it jumps to `0x55555555dbeb` (which is preumably the second arm's body).

{% marginblock %}
The people I imagine doing this kind of investigation are the ones working out the low-level semantic guarantees when it comes to investigating
things like [rust#87520](https://github.com/rust-lang/rust/issues/87520).
{% endmarginblock %}
Pretty low-level details, admittedly. Someone *new* to Rust cannot be expected to do this kind of investigation to gain insight into the language.

In short, this seems like a case where Kernighan and Pike's quote above is
especially apt: we can easily get lost in the details of the
control flow as rendered here.

So, how valuable is "raw control-flow"?

*Honestly:* The more experience I gain in a language or a particular project,
the less and less I find myself relying on this kind of spulunking. The
evaluation order of `match` shouldn't matter in most cases, so I would not
optimize the debugging UX for that. Seeing how method dispatch resolves *is*
useful, but in practice logging statements provide enough insight that people
rarely find themselves wanting to step through the individual assembly
instructions.

Insight into the raw control-flow is probably most useful for someone peeking
into low-level details of the semantics of the language (or a given project,
especially a code base with tricky macros that generate control-flow
structures). One might hope that that group generalizes to beginners who are
learning the language. But I am not certain that raw control-flow will
efficiently provide practical insight. For example, consider the following:

```rust
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct D(&'static str);

fn main() {
    let d = D("my name is D");
    println!("{:?}", d);
}

impl Drop for D {
    fn drop(&mut self) {
        println!("Goodbye, {}", self.0);
    }
}
```

A newcomer to Rust who is not familiar with the destructor system might think
that the above code only prints a single line of text. Running the code (even
outside of a debugger) will show that to be incorrect. But: Will stepping
through it in a debugger provide the insight about where the `<D as Drop>::drop`
call comes from? It is not clear to me that it would.


## Memory Exploration

By "memory exploration", I mean that when my debugger is attached to a paused
program, I can inspect values on the stack or heap, walking down the nodes of a
binary tree or the entries in a hashtable. A good debugger will do some amount
of memory traversal and semantic interpretation on my behalf, using type
information from the program's associated debuginfo to determine how best to
interpret the bits it finds and print it nicely (e.g. showing comma-separated
array elements).

How valuable is "memory exploration"?

*Honestly:* In Rust, one can add `#[derive(Debug)]` to type definitions to get
methods that print the structure for their values. The debugger printouts were
useful when the language didn't provide it out of the box (like in C and C++),
but they less value in Rust.

{% marginblock %}
Another counter-argument to this supposed first benefit: In a language like Rust,
anyone can use `unsafe` code to transmute the value to raw bits, and subsequently print them. This power is not solely in the debugger's hands.
{% endmarginblock %}
One benefit of using a debugger here, rather than just using log statements, is
that a debugger will let you immediately see the concrete representation (such
as concrete hashtable layout) in places where the developer has chosen a more
abstract view for the `Debug` trait implementation. But again we hear echos of
the Kernighan and Pike quote: Seeing that concrete representation may be crucial
in some scenarios, but in many cases it will just be unnecessary detail
distracting you from the problem at hand.

A second benefit with respect to memory exploration is that a debugger will also
let you inspect values that were not part of the original log statement. But,
one can view that as a special case of bypassing the edit-recompile cycle, which
we already discussed above. If the edit-recompile cycle is short, then there is
little cost to adding a new log statement that includes the value of interest.

## Hardware Watchpoints

So we have come to perhaps my *favorite* debugger feature: Hardware watchpoints.

This is the main feature that most programming languages do not provide.

The idea is simple: the gdb command `watch -location <place>` will
break the next time the memory at the given place is modified.
For example, `watch -location array[i]` will evaluate `&array[i]` to
its address, and then the next time `array[i]` is changed, the debugger
will stop and print out both the old value and the new value.

Consider the following (buggy) code for merging two sorted sequences into a
single sorted sequence:

```rust
fn main() {
    let mut r = [99, 98, 97, 96, 95];
    merge(&mut r, &[10, 30], &[20, 40, 60]);
    dbg!(r);
}

// Supposedly merges sorted `a` and `b` into  `recv`.
fn merge(recv: &mut [i32], a: &[i32], b: &[i32]) {
    let mut ai = a.iter();
    let mut bi = b.iter();
    let (mut a_cur, mut b_cur) = (ai.next(), bi.next());
    for i in 0..recv.len() {
        match (a_cur, b_cur) {
            (Some(x), Some(y)) => {
                if *x <= *y {
                    recv[i] = *x;
                    a_cur = ai.next();
                } else {
                    recv[i] = *y;
                    a_cur = ai.next();
                }
            }
            (Some(x), None) => {
                recv[i] = *x;
                a_cur = ai.next();
            }
            (None, Some(y)) => {
                recv[i] = *y;
                b_cur = bi.next();
            }
            (None, None) => {
                return;
            }
        }
    }
}
```

When I run the code above, I get the output:

```
[boom.rs:4] r = [
    10,
    20,
    20,
    40,
    60,
]
```

which is not quite right! The third element of the generated array is 20,
instead of 30.

This toy example is small enough that you might be able to see the bug from
immediate inspection. Adding more test cases to the run might also make the
problem apparent. But I am going to use it to illustrate how to use a hardware
watchpoint to jump immediately to the point in the control flow where the third
element of the `recv` array is overwritten.

After starting up the debugger, we let it evaluate the line that creates
the destination array:

```
(gdb) start
Temporary breakpoint 1, boom::main () at boom.rs:2
2	    let mut r = [99, 98, 97, 96, 95];
(gdb) next
3	    merge(&mut r, &[10, 30], &[20, 40, 60]);
(gdb) 
```

Next, lets first check that we understand what we are looking at:

```
(gdb) print r
$1 = [99, 98, 97, 96, 95]
(gdb) print r[2]
$2 = 97
```

Yep: We have our destination array `r`, and it currently has 97 as its
third element `r[2]`.

So we tell the debugger that we want it to break as soon as that location
is changed, and then let the program run!

```
(gdb) watch -location r[2]
Hardware watchpoint 2: -location r[2]
(gdb) continue
Continuing.

Hardware watchpoint 2: -location r[2]

Old value = 97
New value = 20
boom::merge (recv=&mut [i32](size=5) = {...}, a=&[i32](size=2) = {...}, b=&[i32](size=3) = {...}) at boom.rs:29
29	                b_cur = bi.next();
(gdb) 
```

We are now within the body of `fn merge` instead of `fn main` (as you might have
expected), immediately after the mutation occured:

```rust
            (None, Some(y)) => {
                recv[i] = *y;
                b_cur = bi.next();
            }
```

And indeed, if we print out `recv` at this point, we can see that the
array has been "corrupted", since we now see the two copies of 20 in the
output:

```
(gdb) p recv
$3 = &mut [i32](size=5) = {10, 20, 20, 96, 95}
(gdb) ```
```

I am running with the `rust-gdb` command here, which has extensions for
printing slices nicely. If you were doing this under just normal `gdb`, then
you would see something like this instead.

```
(gdb) p recv
$3 = &mut [i32] {data_ptr: 0x7fffffffe238, length: 5}
```

This kind of break-on-write is most useful for *unsafe* languages, when you have
data being corrupted due to out of bounds accesses. One certainly can encode
examples of that in unsafe Rust, but we also do not expect them to arise as
frequently.

But *honestly* (a ha): Is this actually revealing anything about the core problem
here? The logic at *this* `match`-arm is entirely sound: if we have `(None,
Some(y))` for our two cursors traversing the slices, then we should pass along
`y` and keep traversing down the right cursor. Whatever the bug is, it is
arising somewhere else, before we even get here.

## Reversible Debugging

If I were restricting my attention to `gdb`/`rust-gdb` alone, this would be a
point where I would start talking about re-running the program and stepping
through it carefully, or trying to hypothesize points of interest based on
analyzing the source code and setting breakpoints at those points. But if we
were going to do that, then we might as well invest our time adding some of
those logging statements discussed above, because I think those would show the
problem here just as fast, if not faster!

My goal is to show you an *alternative* strategy, that works for code bases that
you are not that familiar with.

Lets continue looking at the merge example, but let us now run it under [`rr.`][`rr`]

First we record a run of interest:

```
% rr record ./boom
rr: Saving execution to trace directory `/home/pnkfelix/.local/share/rr/boom-2'.
[boom.rs:4] r = [
    10,
    20,
    20,
    40,
    60,
]
% 
```

With that recording in place, we can replay the run under `rr`, which gives an
interface similar to `gdb`. (And we will tell `rr` that we want it to use
`rust-gdb` as our debugger to get those extensions for rendering Rust
collections.)

```
% rr replay -d ~/.cargo/bin/rust-gdb
```

Our initial interaction with `rr` will be much like how we interacted with `gdb`
above: we'll start the program, set a hardware watchpoint for the third array
element, and let it run forward.


```
(rr) break main
Breakpoint 1 at 0x55e4f162f437: main. (2 locations)
(rr) c
Continuing.

Breakpoint 1, 0x000055e4f162fae0 in main ()
(rr) next
Single stepping until exit from function main,
which has no line number information.

Breakpoint 1, boom::main () at boom.rs:2
2	    let mut r = [99, 98, 97, 96, 95];
(rr) 
3	    merge(&mut r, &[10, 30], &[20, 40, 60]);
(rr) watch -location r[2]
Hardware watchpoint 2: -location r[2]
(rr) c
Continuing.

Hardware watchpoint 2: -location r[2]

Old value = 97
New value = 20
boom::merge (recv=&mut [i32](size=5) = {...}, a=&[i32](size=2) = {...}, b=&[i32](size=3) = {...}) at boom.rs:29
29	                b_cur = bi.next();
(rr) 
```

But now that we have reached this point in *replay*, we can rewind time.
We can ask questions like "how did `a_cur` (the left-hand side of the tuple) become `None` already, without us ever emitting 30 to the `recv` array?
Well, the question doesn't *quite* look like that. But we can ask "When was `a_cur` last written to before this point?", which ends up effectively the same question.

```
(rr) watch a_cur
Hardware watchpoint 3: a_cur
(rr) 
```
Now we tell the system to run backwards.

```
(rr) reverse-continue
Continuing.

Hardware watchpoint 2: -location r[2]

Old value = 20
New value = 97
0x000055e4f162fa95 in boom::merge (recv=&mut [i32](size=5) = {...}, a=&[i32](size=2) = {...}, b=&[i32](size=3) = {...}) at boom.rs:28
28	                recv[i] = *y;
(rr)
```

{% marginblock %}
The `gdb` output is potentially confusing because the notions of "Old" and "New"
are inverted here. To dive deeper into this topic, you might consider the
watching the films Primer or TENET.
{% endmarginblock %}

We still have our first watch point in place for `r[2]`, so it halted almost
immediately. (When we were running forward, a watchpoint makes us stop right
after the write occurs. When we run backwards, a watchpoint makes us stop right
before the write occurs.)

Let us keep going backwards.

```
(rr) reverse-continue
Continuing.

Hardware watchpoint 3: a_cur

Old value = core::option::Option<&i32>::None
New value = core::option::Option<&i32>::Some(0x55e4f1664004)
boom::merge (recv=&mut [i32](size=5) = {...}, a=&[i32](size=2) = {...}, b=&[i32](size=3) = {...}) at boom.rs:20
20	                    a_cur = ai.next();
(rr) 
```

And now we see where `a_cur` is being assigned `None`: line 20:

```rust
        match (a_cur, b_cur) {
            (Some(x), Some(y)) => {
                if *x <= *y {
                    recv[i] = *x;
                    a_cur = ai.next();
                } else {
                    recv[i] = *y;
                    a_cur = ai.next(); // here's the assignment
                }
            }
            ...
        }
```

And if you look at this, you might see the problem: in that arm of the `if`, we
are copying over the right-hand tuple value (`*y`, from `b_cur`), but we are
advancing the *left-hand* cursor, `a_cur`.

With that insight, we can make an easy fix: replace line 20 with

```rust
                    b_cur = bi.next(); // was `a_cur = ai.next();`
```

With that change in place, the code works:

```
[boom.rs:4] r = [
    10,
    20,
    30,
    40,
    60,
]
```

{% marginblock %}
*This* is a case where the ability to explore memory
and add printouts from the debugger is crucial: Because now we are exploring the
behavior of one specific trace, and we do not always have the opportunity to
try to recreate the trace on a new version of the program.
{% endmarginblock %}

There are other advantages provided by `rr`. For example, record/replay means
that you can investigate a trace repeatedly. Each time you replay the trace,
you'll observe the same execution; and you can add breakpoints (and print
statements on those breakpoints) on that exact trace as it replays. This is
invaluable for tracking down bugs that are intermittent or that depend on subtle
event orderings.

## Closing Thoughts

I started writing this post thinking "I'm excited about debuggers! Why is no one
in the Rust community using them?" As I dug into the topic, I kept asking myself
"what feature is provided here that people cannot more readily get via their
logging instrumentation? What value *am* I getting from this?"

As you might infer from my "Honestly" notes above, I had to admit that
the advantages of a debugger when compared to logging instrumentation
may be limited in scope. At least, that's how I see things with typical
debugging workflows.

Tools like `rr` open up *new* debugging workflows, one of which I delved into
above. I plan to write more in the future about other such new workflows, such
as ones provided by services like [pernos.co][]. More generally, I want to spend
some time thinking about the nature of debugging: how it links in with how we
develop features, how we communicate our ideas, and how we validate our models.
