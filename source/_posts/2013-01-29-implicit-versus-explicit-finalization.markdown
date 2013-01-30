---
layout: post
title: "Implicit versus Explicit Finalization"
date: 2013-01-29 22:47
comments: true
categories: rust, gc, finalizers
---

(This post is largely a response to Niko Matsakis's blog post
[Destructors and Finalizers in Rust] [1].  I started composing it as a comment there,
but eventually realized I needed more breathing room.)

I agree wholeheartedly with Niko's statement that the Boehm paper
["Destructors, Finalizers, and Synchronization"] [2] is a really nice
discussion of this topic.
However, a significant portion of the destructor/finalizer design space is
only briefly discussed in that paper, and I worry that Niko's post overlooks
it.

There are designs that do not run finalizers directly from the garbage
collection process (whether that collector be a coroutine with the mutator, or
a concurrently running thread), and instead run finalization code
at explicit points in the code of the mutator (or mutator threads).

To me, such an approach seems appropriate for systems-level programming;
it seems to align with the spirit of giving the programmer access to the set of
knobs they need for explicit sequencing and resource management (or rope
to hang themselves with).

Explicit Cleanup
----------------

In the [guardian] [3] and [reference-queue] [4] family of systems (see
also [wills and executors] [5]), resource cleanup code is no longer asynchronously
run by the GC.  Instead, cleanup is the obligation of the
mutator.  The only asynchronous cleanup action of the garbage collector
is to add entries to the appropriate guardians/reference queues (that is, the
queues associated with an object that has been appropriately registered and
subsequently discovered to be unreachable).

I have included a more explicit description of what a Guardian API is like in
the appendix at the end of this blog post, in case the previous paragraph was
too terse.

With such an API in hand, developers can build [libraries] [6] that one can
plausibly reason about, in a single- or multi-threaded context.

To be fair: Boehm *does* address these approaches briefly; see the beginning
of section 3.5.1 of his paper, "Explicit Finalizer Invocation".  However,
he also mixes them in with Java's `System.runFinalization()` and the motivation
for that method.  Thus one must take care to distinguish which of Boehm's complaints
apply to all explicit finalization systems, and which apply solely to systems 
such as Java with `runFinalization` that provide a mix of explicit and implicit finalization.

Passing the Buck
----------------

One important characteristic (some might say "drawback") of explicit
finalization approaches is that the mutator does need to periodically process
its associated guardians/reference queues.  After all, with the transfer of
responsibility for cleanup from the collector to the mutator, the mutator must
meet this obligation eventually.

One standard way to do this is to sprinkle cleanup code (i.e. "check if
guardian has entries; if so, process some of them") at points in the control
flow of library routines using that guardian.  (In other words, in languages
with guardians, library designers can hopefully isolate the cleanup code behind the interface of the library that
requires such cleanup.)

In fact, since there are distinct guardians/reference-queues, one can
prioritize the scheduling (i.e. frequency and incremental-cost) of the
cleanups according to the characteristics of individual libraries, within the
mutator.  Contrast this against relying on the scheduling of the garbage collector with its
single finalization queue for the whole heap.

The two main problems with explicit cleanup from my point of view are:

  1. ensuring that the necessary processing does *eventually* happen
  (i.e. prevent resource leaks when control does not often (or ever) flow back into the
  developer's library utilizing the guardian), and

  2. inserting finalizer invocations across the code of a library detracts
  from its readability; they are a clear example of a cross-cutting concern that
  should be factored into its own "aspect."

Boehm essentially covers both of the points above (though on first reading I
only interpreted him as describing the second point):
> This appears to be the safest way to handle finalization in single-threaded
> code, although it has some clear disadvantages in certain contexts:
> Potentially large amounts of code, especially long running routines, must be
> explicitly sprinkled with finalizer invocations.

In the very worst case, in a "normal" imperative language with concurrent
threads, these last two drawbacks could be addressed by the client-developer:
the client-developer can set their associated cleanup code to run on a
distinct thread (that the *mutator* manages, not the garbage collector).  This
avoids the ugly sprinkling/cross-cutting of concerns, and should ensure that
cleanup does eventually occur (assuming correctly-written multi-threaded code,
"ha ha").

Of course, that last suggestion is essentially reimplementing the GC's
finalizer thread as a mutator process.  But nonetheless, in principle this
could be implemented as a library, as long as the appropriate primitives are
available underneath.

By making explicit finalization expressible as a library,
the developer community would be free to propose competing API's
and implementations.  That seems like a good thing for a young
experimental language.

(One *might* even be able to develop finalization
libraries that are "composable", in the sense that different finalization
libraries could be used by distinct libraries in the same overall program.
One could coin the term
["composing decomposers"](http://en.wikipedia.org/wiki/Decomposing_Composers)
for such things.)

Wait, what was that about "normal" languages?
------------------------------------------------------

I wrote the end of the previous section intending for it to apply to Rust.
Then I realized that I do not have enough experience writing Rust programs to
be certain it is reasonable to develop a mutator-based cleanup process
that runs on its own thread.

I am only discussing cleanup for structures held in managed-boxes (not in
owned-boxes or stack-allocated).  It might be feasible to express cleanup
routines for managed boxes with relatively trivial types in Rust, at least for
some libraries.

But it might be that in practice, managed boxes would generally have
types that would make it impossible for a separate thread to extract them from
a guardian and be able to access their contents.

There is also the serious problem that such a cleanup thread seems likely to require
the use of mutex locks in order to coordinate with the "real" mutator; this in
particular seems antithetical to the ideals of Rust.

(Hopefully I will soon acquire sufficient experience with Rust itself to be
able to address this issue more coherently.)


<!---

----

Another idiosyncrasy with a lot of the guardian-like systems is that they
enqueue the original object for processing, which is quite counter-intuitive
when one considers the effort that the GC went through to determine that the
object was otherwise unreachable.

This perhaps could be addressed by using something like the model described at
the start of section 3.1 of Boehm's paper, "Alternatives"; namely, instead of
enqueuing the unreachable object on a reference queue, the GC could instead
enqueue an associated distinct cleanup object. Boehm states that such a
cleanup object falls victim to all the same problems, but as I understand it,
that is only true if it is invoked by the GC in the same way that Java-style
finalizers are invoked, as described in his paper.

It would be interesting to know whether any managed runtimes offer only
guardians/reference-queues with this style of distinct associated cleanup
objects; I am not aware of any myself.  I might attempt to hack one up in the
future.

--->

Conclusion
----------

I am not saying this is easy, and I am not saying that the systems I am
referencing get everything right either.  But implicit finalizers, even those invoked
from a system-managed asynchronous thread, are not the only answer.

Perhaps my pithy summary is that: Implicit finalizers are indeed inherently
asynchronous routines; but with explicit finalization, one has more options
(yet still may be able to fall back on asynchrony if necessary).

So, what does all of this mean for Rust?  Well, Niko already suggested
limiting destructors solely to types that contain only "owned data."

I have no problem with that, since it clearly deals with all of the issues
that Boehm described.

But my suspicion is that Rust developers will soon discover that one really
does need the GC to feed information forward about the managed boxes that it
is collecting.  And so we will then be at a crossroads: Put in Java-style
finalizers, or adopt another approach?

My goal is to make sure we remember to consider those other approaches when we
hit that crossroads.

References
-----------------------------------------

 1. ["Destructors and Finalizers in Rust"] [1]
    <br/>Niko Matsakis
    <br/>Blog post, 2013
 2. ["Destructors, Finalizers, and Synchronization"] [2]
    <br/>Hans Boehm
    <br/>HP Tech Report; POPL 2003
 3. ["Guardians in a Generation-Based Garbage Collector"] [3]
    <br/>Dybvig, Bruggeman, and Eby
    <br/>PLDI, 1993
 4. ["How to Handle Java Finalization's Memory-Retention Issues"] [4]
    <br/>Tony Printezis
    <br/>Tech Report, 2007
 5. ["Wills and Executors"] [5]
    <br/>PLT Racket Reference Documentation
 6. ["Implementing User-level Value-weak Hashtables"] [6]
    <br/>Aaron Hsu
    <br/>Scheme Workshop 2010
    <br/>(I only properly appreciated the value (and difficulties) of using guardians for such purposes after I read this paper)

[1]: http://smallcultfollowing.com/babysteps/blog/2013/01/17/destructors-and-finalizers-in-rust/

[2]: http://www.hpl.hp.com/techreports/2002/HPL-2002-335.html

[3]: http://www.cs.indiana.edu/~dyb/pubs/guardians-pldi93.pdf

[4]: http://www.oracle.com/technetwork/articles/javase/finalization-137655.html

[5]: http://docs.racket-lang.org/reference/willexecutor.html

[6]: http://repository.readscheme.org/ftp/papers/sw2010/02-hsu.pdf


Appendix A. What are Guardians?
-----------------------------------------

I here outline a concrete example of how this works in the case of Guardians
(adapted from [Hsu] [6]).  (To aid readability, I have replaced the use of
procedure-arity-based-dispatch, as written by Hsu to match the Chez Scheme
API, and I am rewriting his example with a new distinct named procedures .)

One can construct as many guardian instances (or simply "guardians") as one wants.
Each guardian is associated with two things:

  1. `reclaimable` : a set of objects previously determined to be reclaimable by the garbage collector, and
  2. `registry` : a set of objects scheduled to be eventually enqueued in the first set.

Both of these sets are initially empty when the guardian is constructed.

    > (define g (make-guardian))
    > (define v "VAL")
    > (define p (weak-cons v '())
    > (guardian-register! g v)

    > (set! v #f)                    ;; Now `"VAL"` is unreachable via strong-refs ...
    > (guardian-fetch! g)            ;; ... but GC has not discovered that yet.
    #f

    > (collect-garbage)              ;; `"VAL"` is still unreachable ...
    > p                              ;; ... though one can access via weak paths ...
    ("VAL")
    > (define x (guardian-fetch! g)) ;; ... and the guardian held it in `reclaimable`
    > x
    "VAL"

                                     ;; At this point, "VAL" is again strongly reachable
                                     ;; (via `x`).  However, it is *not* in either of the
                                     ;; sets for the guardian `g`, and thus is not scheduled
                                     ;; to be enqueued in the `reclaimable` set.

    > (set! x #f)                    ;; Now "VAL" is no longer strongly reachable...
    > (collect-garbage)
    > p                              ;; ... and thus can be reclaimed.
    (#!blank-weak-pointer)

So, what does this have to do with finalization?

With the guardian API in hand, one could take care of closing individual
file descriptors associated with heap-allocated file objects, by
a protocol such as:

1. Create one guardian G for the file library, whose sole purpose is closing
file descriptors.

2. In the constructor for file objects, register each constructed file object
with G.

3. In a periodic process, dequeue objects from G and close their associated
file descriptors.


Appendix B. Another peeve about guardians
-----------------------------------------

One idiosyncrasy about a lot of the guardian-like systems is that they enqueue
the original object for processing, which is quite counter-intuitive to me
given the effort that the GC went through to determine that the object was
otherwise unreachable.

Perhaps this could be addressed by using something like the model described at
the start of section 3.1 of Boehm's paper, "Alternatives"; namely, instead of
enqueuing the unreachable object on a reference queue, the GC could instead
enqueue an associated distinct cleanup object, and reclaim the originally
registered object itself. (Boehm states that such a cleanup object falls
victim to all the same problems, but as I understand it, that is only true if
it is invoked by the GC in the same way that Java-style finalizers are
invoked, as described in his paper.)

The associated cleanup object would need to carry any state necessary for the
cleanup action (e.g., the file descriptor itself).  So that seems like a
potential for ugly redundancy and wastage (in either time or space, depending
on whether uses a level of indirection or simply redundantly stores all the
necessary state in the object and in its associated cleanup object.).

But nonetheless, a decoupled system like this may be easier to reason about,
especially when one has scenarios where an object is registered with multiple
guardians/reference-queues.

As far as I understand, the reference queue system in Java, as described by
[Printezis] [4], is sufficient to express a structure like this.  Still, it
would be interesting to know whether any managed runtimes offer *only*
guardians/reference-queues with this style of distinct associated cleanup
objects; I am not aware of any that are so conservative.  I might attempt to
hack one up in the future.
