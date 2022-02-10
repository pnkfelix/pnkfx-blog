---
layout: post
title: "What is Rust's Hole Purpose?"
date: 2022-02-09 13:00:00 -0500
comments: false
categories: rust proglang programming
---
There is an adage in the business world that goes something like this:

{% marginblock %}
This quote is attributed to Theodore Levitt, but there is [significant evidence][quote-investigator] that the
adage predated his use of it.
{% endmarginblock %}

> People don't want to buy a quarter-inch drill, they want a quarter-inch hole.

[quote-investigator]: https://quoteinvestigator.com/2019/03/23/drill/

It is a good line.

{% marginblock %}
Some people I talk to make the leap from safety to another property: *security*.
The two topics are related, since safety issues can sometimes be exploited and thus
yield security issues. But you can theoretically achieve security atop an unsafe language; and safety alone does not ensure security. So it is a messy relationship at best.
{% endmarginblock %}

The adage came to my mind recently in a discussion of Rust's selling points.
An oft-cited selling point, at least for a Programming Language enthusiast like myself, is that Rust offers *safety*.

<!-- more -->

I was not familiar with the adage before last year. I first heard it used as the
punchline to a significantly longer parable, set at the keynote for a sales convention for a drill company.
The lesson of the adage is that to be customer-focused, you
must keep in mind that drills are only a *means to an end* for your customer.

Safety is important: it eliminates a significant class of bugs that
plague software written in low-level systems languages.

But, thinking of the drill/hole adage, I stopped and asked myself: "Who *wants* safety? Is it an end in itself? If it is merely a means to an end, then to *what end*?"

At this point, while reflecting on that topic, I decided to look more into the origins of the drill/hole adage; that diversion led me to a lovely [post critiquing the adage][hole-doesnt-go-far-enough], saying it "doesn't go far enough." The heart of the argument there is that people don't want *holes* either. The hole is itself another means to an end, such as hooks or shelving.
But do people want hooks and shelving? No, they want to store objects on the wall.
Why?
{% marginblock %}
A shelf of objects can also serve as decor; my favorite walls are filled with books.
{% endmarginblock %}
Because they want room to store *more things*, or they want to be able to fetch and replace things more efficiently, yielding *more time*.

[hole-doesnt-go-far-enough]: https://www.websearchsocial.com/why-that-whole-people-dont-want-a-drill-they-want-a-hole-thing-doesnt-go-far-enough/

{% marginblock %}
The [post][hole-doesnt-go-far-enough] includes the caveat that intermediate results
like the hole are not irrelevant. Its just important to keep the end-goal somewhere
in your mind, if only in the back of your mind, as you identify your more immediate (and usually intermediate) goals.
{% endmarginblock %}

That is what people want: Extra capacity for "stuff", or to reduce the time they
spend searching through clutter.

That is such an important point, and I think it provides the right perspective that we will need to answer the question of "What is Rust's hole purpose?" (Or, if you prefer a pun-free version: "What is Rust's value proposition?")

<!-- I do not think this section serves the overall document. Its just filler. So I'm killing it.

## The Pillars

Under "Why Rust?", the language website offers three pillars:

* Performance
* Reliability
* Productivity

We can already see a shift in perspective: "safety" is listed *under* "reliabilty", as a component of that feature.

There is a reason the team is showing all three pillars at once. The underlying
point is that you have to weigh tradeoffs.

Historically, maximizing performance meant that you had to work in a language
like C or C++. That risks sacrificing reliabilty; if you wanted to recover
reliability, you had to spend more time architecting and validating your system
to avoid the safety pitfalls suffered by those languages.

As just-in-time compiler and memory-management technology has improved, managed
languages like Java are narrowing the performance gap when compared with C, *if*
you are willing to accept the garbage-collection overhead (either in time or in
memory usage), and you also need to know how to write code in a manner that
meshes well with your runtime environment so that it will optimize well.

So in either case, if you want to deliver performance, then either reliability
or productivity will suffer.

Rust's claim is that you can get all three. But how?

-->

## The Promise(s) of Rust

{% marginblock %}
I am using "sub-" and "super-" here in a mathematical sense, as in "every safe Rust program is also an Unsafe Rust program, but there are some Unsafe Rust programs that are not part of safe Rust." There is no value judgement being made as to one being "superior" to the other.
{% endmarginblock %}

Rust provides two distinct languages: the safe Rust (sub)language, and the Unsafe Rust (super)language. You can read the [Rustonomicon](https://doc.rust-lang.org/nomicon/meet-safe-and-unsafe.html) for more discussion of this distinction.

An over-simplified way of describing the benefits of the safe Rust language is
this "promise": You will get *predictable* behavior from your program. If you
write a program in safe Rust, it will not surprise you.

{% marginblock %}
After all, if a program were incapable of surprising anyone, then would it be worth
executing in the first place?
{% endmarginblock %}
But, that promise is a lie: There's plenty of ways to observe unpredictable
outcomes, in most any programming language of interest. (Consider for example [psuedorandom number generation][PRNG].)

[PRNG]: https://en.wikipedia.org/wiki/Pseudorandom_number_generator

Here is a somewhat improved promise: You will never get [Undefined Behavior][UB]
from a safe Rust program. It won't ever access memory after its been freed, and
it won't ever have two threads racing to read and write the same location in
memory.

[UB]: https://en.wikipedia.org/wiki/Undefined_behavior

But in fact, this is still somewhat of a lie!

For one thing, we allow crates written in safe Rust to link to crates that are
written in Unsafe Rust. Should that be considered an instance of a "safe Rust"
program? If it is, then its easy to see a counter-example to the improved
promise: just have safe crate that calls out to a helper function `oops` in an
unsafe crate, where `oops` demonstrates undefined behavior.

Even if you say "no, I do not consider such a program to be an instance of safe
Rust. You need *all* of your crates to be written in safe Rust to make me
happy", you will still run afoul of problems. (1.) Rust's standard library is
using unsafe code that might not always maintain global safety invariants. Even
worse, (2.) the Rust compiler itself may generate incorrect output.

So, your program might *still* exhibit Undefined Behavior even if you restrict
yourself to crate graphs where all crates are written in safe Rust.

Such an outcome is not what anyone *desires*; but it is a reality that we have
to deal with, at least with the state of the art today.

<!-- Do I need to say this? Lets see how it looks without this.

Software development is a process of continual refinement, striving towards
idealized goals. Those idealized goals themselves may change over time. Part of
that process is that expecting mistakes to happen, and having plans in place to
correct errors in a timely fashion after they have been detected.
-->

{% marginblock %}
This title implicitly references the work of [Findler and Felleisen (2002)][FF02] on
using contracts to assign proper *blame* when one is dealing with higher-order functions.

[FF02]: https://www2.ccs.neu.edu/racket/pubs/icfp2002-ff.pdf
{% endmarginblock %}

## Blame Assignment

Here is my favorite statement of Rust's promise: Undefined Behavior is never a
bug in your (safe) code.

Now *this* is a promise I can get behind.

This is important, because it clearly delineates the
["Trusted Computing Base"](https://en.wikipedia.org/wiki/Trusted_computing_base)
for your program.

{% marginblock %}
In other words, The TCB is TCB: The Trusted Computing Base is Taking Care of Business.
{% endmarginblock %}

In safe Rust, you are trusting the compiler and standard library to get the
safety conditions right. You expect that any crates you link to will properly
ensure any preconditions necessary for *their* `unsafe` blocks, if any.

Why does this matter: Compare against other environments, like C projects, where
people can debate for ages about whether a given example is violating the rules
of an API or of the language itself, and that leads to issues lying unaddressed,
because its unclear *who is responsible* for the issue.

In Rust, this occurs far less frequently. If someone finds unsound behavior via
a program example that has no occurrences of `unsafe { ... }`, then the Rust
community tends to immediately say "that must be a bug!", and the only task then
is to identify whether it is from some `unsafe { ... }` code elsewhere, or if
the Rust compiler itself has an issue.

Of course, Rust is not alone in enjoying this property. *Any* safe language
worth its salt can make the same statement. But Rust is different, in that we
make it really easy for people who *want* to drop into the Unsafe Rust
superlanguage to do so.
{% marginblock %}
Again, no value judgement is being made here. Really! Safe and unsafe Rust are mathematically incomparable when it comes to inherent value!
{% endmarginblock %}
That lets developers get their work done faster,
because they can hyper-optimize their generated machine code. Or they can
easily call out to a foreign library and avoid implementing a tricky algorithm in the first place!

I claim: Compared to developers using unsafe languages, safe Rust developers
spend less time debating about who is responsible when soundness issues arise.
Compared to developers using safe languages, Unsafe Rust developers spend less
time figuring out how to deliver a performant solution.

The end value provided by Rust to its developers (and thus to *their* customers
and employers) is *less time* arguing linguistic minutia and *less time*
wrestling with a managed language environment, and *more time* focused on what
actually matters to each of those developers (be it business logic, or family
time).

But, notice that I used two distinct categories for the subjects of the two
claims. Am I comfortable strengthening my claim? Can I just say "Rust developers
spend less time debating about who is responsible when issues arise, and they
spend less time figuring out how to get a performant solution into shape"?

## The Holes in the Argument

I would like to make that stronger claim. I think we have some evidence to support
it. But we are not all the way there yet.

### From safe to unsafe

To me, the biggest open problem is: How does one make the transition from safe
Rust developer to unsafe Rust developer? How do you prove to yourself that your
`unsafe { ... }` code is not introducing an soundness issue?

We have some ongoing work in this space, but it is not a solved problem.

### How to stay safe

The other obvious open problem is: When is safe Rust not performant enough, and
why?

There are some inherent overheads that safe Rust is simply going to pay (e.g.
bounds checking on array accesses). Some of those cases can be side-stepped in
many cases by better understanding of what the language offers (e.g. iterating
rather than indexing will skip the bounds checks), which is arguably an instance
of "wrestling with your environment" to reap more performance.

Then there are overheads that are incidental rather than inherent. The Rust
developers have tried to leave many doors open for future improvements to our
output code quality. (Some of these cases overlap with our need for better ways
to catch bugs in `unsafe` code. Others are a matter of "just" putting in the
work.)

## Conclusion

If you are interested in helping solve any of these problems, please reach
out!
