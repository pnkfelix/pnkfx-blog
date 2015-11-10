---
layout: post
title: "GC and Rust Part 1: Specifying the Problem"
date: 2015-11-10 17:45
comments: true
categories: gc rust
---

This is the first in a series of posts will discuss why garbage
collection is hard, especially for Rust, and brainstorm about
solutions to the problems we face.

The relationship between garbage collection (GC) and the Rust
programming language has been an interesting one.

GC was originally deeply integrated into the language, complete with
dedicated syntax (good old `@T` ...). Over time the team found ways to
lessen the dependency on GC, and then finally remove it from the
language entirely.

However, we still want to provide support for garbage collection.

To explain why, I need to define the actual problems we seek to solve.
So let us explore the problem space.

<!-- more -->

# ... [now you have][nyh] two problems

[nyh]: http://regex.info/blog/2006-09-15/247

(The body of this post makes heavy use of client-side rendering,
because of author idiosyncrasies.  You may need to wait a moment while
the supporting Javascript loads.)

<script src="http://blog.pnkfx.org/javascripts/viz.js" charset="utf-8"></script>
<script src="http://blog.pnkfx.org/javascripts/js_to_dot.js" charset="utf-8"></script>
<script src="http://blog.pnkfx.org/javascripts/gc_rendering.js" charset="utf-8"></script>

## The Problem Space
[problem-space]: #the-problem-space

Now that we have [reviewed][part0] what GC is and how it works, let us
discuss what GC could mean to Rust.

[part0]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/
[conservative-gc]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#conservative-gc
[pinning-support]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#pinning-support

I have identified two distinct kinds of support that we could provide:
"GC" could describe a feature for pure Rust programs, or "GC" could mean a
3rd-party runtime interoperation feature. Let us discuss each in turn.

### One GC shared by every crate

We could add a smart-pointer to `libstd`, e.g. a `Gc<T>` type, that
arbitrary library crates could use as they create or receive instances
of `T`. The intention here would be similar to how `Rc<T>` is used:
One does not want to track ownership precisely, but rather treat
ownership as shared amongst all users of a value, and let the runtime
system handle reclaiming the value.

So for example, we might want to write code that looks like this:

```rust
use std::gc::Gc;

struct Cons<T> {
    head: T,
    tail: Cell<Option<Gc<Self>>>,
}

impl<T> Cons<T> {
    fn new(head: T, tail: Option<Gc<Self>>) -> Self {
        Cons { head: head, tail: Cell::new(tail) }
    }
    fn head(&self) -> &T { &self.head }
    fn tail(&self) -> Option<Gc<Self>> { self.tail.get() }
}

#[test]
fn demo() {
    let a;
    let f: Gc<_>;
    {
            a = box Cons::new(1, None);
        let b = box Cons::new(2, Some(a));
        let c = box Cons::new(3, Some(a));
        let d = box Cons::new(4, Some(b));
        let e: Gc<_>;
            e = box Cons::new(a, Some(b));
            f = box Cons::new(c, Some(d));

        let mut g = box Cons::new(10, None);
        let     h = box Cons::new(20, Some(g));
        g.tail.set(Some(h));
    }
    // here, locals `a` and `f` are the roots
}
```

(The above snippet assumes we have extended `box EXPR` to an
overloaded operator in the manner similar to that described in
[RFC 809][], so that `let g: Gc<_> = box EXPR;` works, and that
the type inference figures out that all the locals need to be
in `Gc<_>`.)

[RFC 809]: https://github.com/rust-lang/rfcs/blob/master/text/0809-box-and-in-for-stdlib.md


This results in a stack and heap modelled by this picture.

<p id="target_anchor_gc_demo_1"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };
var a = object_record("A", "<head> head: 1 | <tail> tail: None");
var b = object_record("B", "<head> head: 2 | <tail> tail: Some(A)");
var c = object_record("C", "<head> head: 3 | <tail> tail: Some(A)");
var d = object_record("D", "<head> head: 4 | <tail> tail: Some(B)");
var e = object_record("E", "<head> head: A | <tail> tail: Some(B)");
var f = object_record("F", "<head> head: C | <tail> tail: Some(D)");
var g = object_record("G", "<head> head: 10 | <tail> tail: Some(H)");
var h = object_record("H", "<head> head: 20 | <tail> tail: Some(G)");

var local_a = { id: "local_a", label: "a", shape: "record" };
var local_f = { id: "local_f", label: "f", shape: "record" };

stack[1] = local_a;
stack[2] = local_f;

b.tail = edge_from_port(":tail", a);
c.tail = edge_from_port(":tail", a);
d.tail = edge_from_to_ports(":tail", ":id", b);
e.head = edge_from_port(":head", a);
e.tail = edge_from_to_ports(":tail", ":id", b);
f.head = edge_from_to_ports(":head", ":id", c);
f.tail = edge_from_to_ports(":tail", ":id", d);
g.tail = edge_from_to_ports(":tail", ":id", h);
h.tail = edge_from_to_ports(":tail", ":id", g);

local_a.ref = a;
local_f.ref = edge_to_port(":id", f);

gc_heap[0] = a;
gc_heap[1] = b;
gc_heap[2] = c;
gc_heap[3] = d;
gc_heap[4] = e;
gc_heap[5] = f;
gc_heap[6] = g;
gc_heap[7] = h;

var objects = [stack, gc_heap];
post_objects("target_anchor_gc_demo_1", objects, { rankdir:"LR", nodesep:0.2 });
</script>

The GC would be allowed to collect the objects labelled "E", "G", and
"H" in the picture, since it is not reachable from the roots.
(However, the GC is not obligated to reclaim "E" at any particular
time. Usually GC's provide little guarantees about how soon objects
will be reclaimed.)

This kind of feature could be useful in any Rust library.

#### Advantages of Gc<T> over Rc<T>

The main hypothesized advantages over `Gc<T>` over `Rc<T>` are:

  * `Gc<T>` is `Copy`, which makes it possible to construct types like
    `Cell<Gc<T>>`.

    (It also has nicer programmer ergonomics in some cases; e.g. some
    programmers dislike having to write `x.clone()` every time they
    want to make a copy of ref-counted `x`.)

  * `Gc<T>` allows cyclic structure to be reclaimed (e.g. the objects
     "G" and "H" in the picture above.

  * Using `Gc<T>` *might* have less overhead than `Rc<T>`: every time
    you clone an `Rc<T>` it incurs reference-count overhead, while
    `Gc<T>` just copies the reference.

    (However, this stated advantage must be tempered by the
    realization that GC incurs its own separate overheads, as
    discussed in the [background post][post0].

[post0]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/

#### Drawbacks of one GC for everyone

There are two immediate drawbacks with this kind of collector
support.

First, adding it would require that the standard library either
provide a garbage collector (that all clients of `Gc<T>` would have to
link in), or at least standardize a fixed API that third-party
collector implementations would have to satisfy to support `Gc<T>`.

Second, it is difficult to provide the ergonomics that one expects
from a smart-pointer<sup>[1](#footnote1)</sup> type analogous to
`Rc<T>`.

Okay, so that's the outline of the tradeoffs of providing
a "GC for everyone" in `libstd`.  What about a more limited
GC feature, where the audience is not "every Rust crate", but instead
just the crates linking to a managed runtime.

### GC as Interoperation Feature

GC as an interoperation feature means that Rust would provide
introspective hooks to improve integration with application frameworks
that are using their own garbage collector. One example of this is
Servo's use of the SpiderMonkey Virtual Machine for its Javascript
support.

Servo is relying on SpiderMonkey's garbage collection for memory
management, not only for Javascript values, but even for
[native-code DOM objects][servo post].

[servo post]: https://blog.mozilla.org/research/2014/08/26/javascript-servos-only-garbage-collector/

That post describes (unchecked) scenarios where one can end up with
dangling pointers -- that is, they invite unsoundness.  Proper support
for GC-interoperation in Rust could address this; I will discuss this
further down in this post.

Critically, GC-interoperation does not require the same level of
ergonomics that `Rc<T>` provides. For example, in this context it is
acceptable for `Gc<T>` to not support [`Deref`][Deref trait].

(Furthermore, in this context, it may even be acceptable to require
unchecked constraints like "the programmer must ensure the collector
is not invoked during this extent", or perhaps "the programmer must
periodically invoke a call that tells the GC that this is an
acceptable time to do collection work that could move objects.")

[Deref trait]: https://doc.rust-lang.org/std/ops/trait.Deref.html

Without a `Deref` trait and with such unchecked requriements, such
interoperation might end up looking something like this:

```rust
fn double_last(x: Gc<Vec<i32>>) {
    unsafe {
        let ptr: *mut Vec<i32> = x.get_ptr();

        // during the extent of this block, it is the responsibility
        // of the double_last author to ensure the GC never gets
        // invoked (i.e., do not do any allocations to the GC'ed heap
        // during this unsafe-block).

        if Some(i) = (*ptr).last_mut() {
            *i = *i * 2;
        }
    }
}
```

In this context, interoperation still requires defining a standard
interface that the third-party collector implementation has to conform
with.

In a simple world (e.g., a conservative collector designed to
interoperate with C/C++, such as [boehm-demers-weiser][BDW] (BDW)), this
standard interface could be nothing more than just "swap in a
different [#[allocator] crate][custom_alloc] that your GC provides."

[BDW]: http://www.hboehm.info/gc/

[custom_alloc]: https://doc.rust-lang.org/nightly/book/custom-allocators.html

(The actual interface is unlikely to be so
simple<sup>[2](#footnote2)</sup>, but the point is, there is a wide
design space to be explored here.)

#### Interoperation with a "black box" GC

One way to look at the difference between "GC for pure Rust programs"
versus "GC for interoperation" is that in the former case, the GC
feels deeply integrated with the language and standard library, while
in the latter case, the GC is clearly the concern of some entity
outside the language (and we are just trying to accommodate it as best
we can).

An extreme instance of a GC that is definitely an entity outside the
language is a case where the whole GC heap is treated like a black
box, and the objects inside the heap are never directly exposed to the
application code outside the box.

For example, one can imagine a virtual machine (VM) interface where
the code outside the VM is never given addresses of objects on the
heap. Instead, such foreign code only has *handles* that indirectly
point to those objects.

<p id="target_anchor_black_box_gc_1"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };
var handles = object_record("handles", "<h2> Y | <h1> X | <h3> X");

var c = object_record("C", "<f0> Gc(X) | <f1> Box(O)");
c.style = "rounded";
var o = object_record("O", "<f0> Handle(2)");
var x = object_record("X", "<f0> 'a' | <f1> next");
var y = object_record("Y", "<f0> 'b' | <f1> next");
var z = object_record("Z", "<f0> 'c' | <f1> (next)");
x.style = "rounded";
y.style = "rounded";
z.style = "rounded";
var local_x = { id: "local_x", label: "handle_x", shape: "record" };
var local_y = { id: "local_y", label: "handle_y", shape: "record" };
var local_o = { id: "local_o", label: "boxed_o", shape: "record" };

x.next = edge_from_to_ports(":f1", ":id:sw", y);
y.next = edge_from_to_ports(":f1", ":id", z);

o.f0 = edge_from_to_ports(":f0", ":h3", handles);

c.f0 = edge_from_to_ports(":f0", ":id", x);
c.f1 = edge_from_to_ports(":f1", ":id", o);

stack[1] = local_x;
stack[2] = local_y;
stack[3] = local_o;

rust_heap[0] = o;
gc_heap[0] = handles;
handles.x1 = edge_from_to_ports(":h1", ":id", x);
handles.y2 = edge_from_to_ports(":h2", ":id", y);
handles.x3 = edge_from_to_ports(":h3", ":id:sw", x);
local_x.handle = edge_to_port(":h1", handles);
local_y.handle = edge_to_port(":h2", handles);
local_o.box = edge_to_port(":id", o);
gc_heap[2] = x;
gc_heap[3] = y;
gc_heap[4] = z;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_black_box_gc_1", objects, { rankdir:"LR", nodesep:0.2 });
</script>

In this setting, direct references to objects *never* escape the black
box. Instead, by setting up a level of indirection, the management of
the objects within the GC heap is completely abstracted away.

In a black box GC setting, one would not expose the data structure of
the objects (since they can never be directly addressed
anyway). Instead, one would define functions on handles that extract
the fields and maps them to handles when necessary:

```rust
extern fn handle_data(a: Handle) -> char;
extern fn handle_next(a: Handle) -> Option<Handle>;
extern fn handle_set_next(a: Handle, b: Option<Handle>);

// sample code interacting with the black box GC

// all of these predicates hold of the above heap diagram
assert_eq!(handle_data(handle_x), 'a');
assert_eq!(handle_data(handle_next(handle_y).unwrap()), 'c');
assert!(handle_next(handle_next(handle_y).unwrap()).is_none());

// this changes the heap to match the diagram below.
handle_set_next(handle_x, handle_next(handle_y));
```

<p id="target_anchor_black_box_gc_2"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };
// var handles = object_record("handles", "<h2> Y | <h1> X | <h3> X | <h4> (temp for Z)");
var handles = object_record("handles", "<h2> Y | <h1> X | <h3> X");

var c = object_record("C", "<f0> Gc(X) | <f1> Box(O)");
c.style = "rounded";
var o = object_record("O", "<f0> Handle(2)");
var x = object_record("X", "<f0> 'a' | <f1> next");
var y = object_record("Y", "<f0> 'b' | <f1> next");
var z = object_record("Z", "<f0> 'c' | <f1> (next)");
x.style = "rounded";
y.style = "rounded";
z.style = "rounded";
var local_x = { id: "local_x", label: "handle_x", shape: "record" };
var local_y = { id: "local_y", label: "handle_y", shape: "record" };
var local_o = { id: "local_o", label: "boxed_o", shape: "record" };

x.next = edge_from_to_ports(":f1", ":id:w", z);
y.next = edge_from_to_ports(":f1", ":id:n", z);

o.f0 = edge_from_to_ports(":f0", ":h3", handles);

c.f0 = edge_from_to_ports(":f0", ":id", x);
c.f1 = edge_from_to_ports(":f1", ":id", o);

stack[1] = local_x;
stack[2] = local_y;
stack[3] = local_o;

rust_heap[0] = o;
gc_heap[0] = handles;
handles.x1 = edge_from_to_ports(":h1", ":id", x);
handles.y2 = edge_from_to_ports(":h2", ":id", y);
handles.x3 = edge_from_to_ports(":h3", ":id:sw", x);
// handles.z4 = edge_from_to_ports(":h4", ":id:sw", z);
local_x.handle = edge_to_port(":h1", handles);
local_y.handle = edge_to_port(":h2", handles);
local_o.box = edge_to_port(":id", o);
gc_heap[2] = x;
gc_heap[3] = y;
gc_heap[4] = z;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_black_box_gc_2", objects, { rankdir:"LR", nodesep:0.2 });
</script>

In case it isn't clear, supporting interoperation with this kind of
"black box" GC requires very little from the Rust side; potentially
nothing at all. The object addresses are hidden, so the GC could move
an object and update its address in the handle
array.<sup>[3](#footnote3)</sup>

However, this so-called interoperation is also quite limited in
expressiveness. The defining property of the "black box" GC, the fact
that it does not expose the addresses of the objects held within, also
means that we cannot expose `&`-references to the objects or the state
within them, which means we cannot use these objects with the large
number of Rust functions that operate on `&`-references and slices.

Also, the hidden object addresses may complicate client code trying to
instantiate GC objects with its own types.<sup>[4](#footnote4)</sup>

In any case, interoperation with a blackbox GC is not a primary goal,
since the level of indirection and handles array maintainence is not
ideal.

## Objectives and Requirements (oh no, now five problems)

The two (or perhaps three) kinds of support described above are
distinct features; there is overlap between them, but trying to find a
single solution that solves both problems completely may not be
possible, and in any case we do not want to wait for that single
solution to be discovered.

Since `Rc<T>` is already a workable solution for many (though not all)
use cases of `Gc<T>`, the above idealized "one GC shared by every
crate" is not a main priority right now (and may never be added to the
Rust language).

Let us focus on GC as an interop feature, and dive into what we would
want to get out of it.

There are a number of objectives for Rust/GC integration that are
worth noting, which I will list here and then define and discuss
below.

  1. [Safe][safety]
  2. [Modular][modularity]
  3. [Zero-Cost][zero-cost]
  4. [Compositional][compositionality]
  5. [Precise (Space-Efficient)][precision]

### <span id="safety">Safety with respect to GC</span>
[safety]: #safety

If a Rust crate does not use `unsafe` constructs (`unsafe` blocks,
attributes or types with "unsafe" in their name, etc.), then linking
it with a sound set of crates that use GC must maintain soundness.

In other words, linking in a crate that uses no `unsafe` construct
should not inject any dereferences of dangling pointers, nor any data
races.

By the way, we absolutely do need to provide criteria that says what
`unsafe` code *is* allowed to do when linked with a crate that uses
GC. I am going to assume for these initial posts that we will solve
that problem eventually, but not attempt to address it at the outset.

### <span id="modularity">Modularity with respect to GC</span>
[modularity]: #modularity

A Rust program that uses GC should be able to link to a crate whose
source code was authored without knowledge of GC.

For example, if I make a parsing library today that works on string
slices `&str`, you should be able to link that parsing library into a
program that uses GC, without having to worry about whether the
parsing library carries hidden requirements that invalidate
assumptions made by the GC.

Note: A crate being "authored without knowledge of GC" is a
property of the source code, not the generated object code. Given
such a crate, the Rust compiler may itself inject metadata
related to GC, such as descriptions of object layout, or
automatically-generated code that dictate how objects should
traced by the collector.

Note: A crate being "authored without knowledge of GC" is entirely
distinct a crate not supporting GC. That is, we may add well a way for
a crate to declare that it is not compatible with GC. (This would
count as having knowledge of GC; in fact, enough knowledge to know, or
at least guess, that its presence would cause the GC to break, or vice
versa.)

If we cannot satisfy this requirement, then the addition of GC
will, at best, split the growing space of library crates (such as
those available on [crates.io][]) into two disjoint
sub-communities: crates that support GC, and those that do not
(since the latter were written without accounting for the
potential presence of a GC).

An aside: I would really like to find a way to combine the
descriptions of "modularity" and "safety", since they seem to be
attempted to express similar or related objectives.

A final note: There are some features available to crates, such as
requiring a specific low-level allocator, that are likely to be
incompatible with a program that uses GC. We need to define these
caveats and incorporate them into the above definition of
"modularity", without weakening it to the point of uselessness.
(However, I will not attempt to tackle that here.)

### <span id="zero-cost">Zero-Cost GC</span>
[zero-cost]: #zero-cost

If you don't use the GC feature (in whatever form it takes), your code
should not pay for it.

This applies to the quality of the generated code (in execution
time and code size), and also to the source code, with respect to
difficulty in writing a program or library.

There are two forms of the zero-cost property relevant here:

  1. Strongly zero-cost: A unit of code generation that does not use
     GC should not pay for it.

     For example, in the above example of the string parsing module,
     ideally the code generated for parsing `&str` values should have
     the same performance characteristics, regardless of whether it is
     linked into a program that uses GC or not.

  2. Weakly zero-cost: A program that does not use GC should not pay
     for it.

     (At worst, one can imagine ensuring this property by compiling
     two different versions of each code unit, and then linking to the
     appropriate one. Hopefully we will not need to resort to that.)

Strongly zero-cost implies weakly zero-cost, but not vice-versa.

### <span id="compositionality">Compositional GC</span>
[compositionality]: #compositionality

One can use a reference to a gc-allocated object (call it a `GcRef`)
as the field type in a `struct`, store it into a `Vec<GcRef>`, and
in general do anything with it that one can do with a normal Rust value.

Furthermore, one should be able to describe, via a Rust type
definition, the layout of a value allocated on the GC heap, allocate
such values there, and acquire a suitable `GcRef` to the allocated
object.

To be concrete about this, consider the following program,
which uses a hypothetical `make_gc_ref` function to move
values into a newly-allocated spot on the GC heap, and returns
a reference to that spot. (In the future one will probably use
the `box` syntax for this, and rely on type-context to inform
box that this is a GC-allocation.)

```rust
fn demo() {
    let gc_v = {
        let ref_x1 = make_gc_ref("data_one");
        let ref_x2 = make_gc_ref("data_two");
        let v = vec![x1, x1, x2];
        make_gc_ref(v)
    };
    ...
}
```

This results in the following diagram:

<p id="target_anchor_demo_composition_1"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var x1 = object_record("X1", "<f0> 'data_one'");
var x2 = object_record("X2", "<f0> 'data_two'");

x1.style = "rounded";
x2.style = "rounded";

var gc_v = { id: "gc_v", label: "Gc(V)", shape: "record" };

var v = object_record("V", "<f0> len: 3 | cap: 4 | <f2> ptr: Arr");
v.style = "rounded";
var arr = object_record("Arr", "<f0> Gc(X1) | <f1> Gc(X1) | <f2> Gc(X2)");
arr.color = "blue";

v.f2 = edge_from_to_ports(":f2", ":id", arr);
gc_v.f0 = edge_to_port(":id", v);

arr.f0 = edge_from_to_ports(":f0", ":id", x1);
arr.f1 = edge_from_to_ports(":f1", ":id", x1);
arr.f2 = edge_from_to_ports(":f2", ":id", x2);

stack[0] = gc_v;
rust_heap[0] = arr;
gc_heap[0] = v;
gc_heap[1] = x1;
gc_heap[2] = x2;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_demo_composition_1", objects, { rankdir:"LR", nodesep:0.2 });
</script>

Here, I have made explicit the heap-allocated backing store `Arr` (in
blue) for the vector that holds the references to `x1` and `x2`.

This shows that if we want GC to reasonably usable (i.e., allow GC
references to be used like other Rust values), we need to support
references out of the GC heap and into the Rust heap, and likewise
references out of the Rust heap and into the GC heap.

It can sometimes be simpler (without necessarily eliminating the
fundamental problem) to just a `Box` rather than a `Vec`:

<p id="target_anchor_demo_composition_2"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };
var c = object_record("C", "<f0> Gc(X) | <f1> Box(O)");
c.style = "rounded";
var o = object_record("O", "<f0> Gc(X)");
var x = object_record("X", "<f0> 'data'");
x.style = "rounded";
var gc_a = { id: "gc_c", label: "Gc(C)", shape: "record" };

gc_a.f0 = edge_to_port(":id", c);

o.f0 = edge_from_to_ports(":f0", ":id", x);

c.f0 = edge_from_to_ports(":f0", ":id", x);
c.f1 = edge_from_to_ports(":f1", ":id", o);

stack[0] = gc_a;
rust_heap[0] = o;
gc_heap[0] = c;
gc_heap[1] = x;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_demo_composition_2", objects, { rankdir:"LR", nodesep:0.2 });
</script>


The program to construct the above picture might look like
this:

```rust
fn demo() {
    struct C(Gc<str>, Box<Gc<str>>);
    let gc_c = {
        let ref_x = make_gc_ref("data");
        let box_o = Box::new(ref_x);
        make_gc_ref(C(ref_x, box_o))
    };
    ...
}
```

(The types in the demo program above assume certain features like
allowing `Gc<T>` for `T: ?Sized`, which may or may not be reasonable.)

The compositionality constraint may seem obvious (especially if one
starts by assuming that references to gc-allocated objects will be
values of type `Gc<T>` for arbtrary `T`).

But using "black box" GC interop (as described above) would likely
*defeat* compositionality.  That is why I point out this objective
explicitly.

[crates.io]: https://crates.io/

### <a id="precision">Precision (Space-Efficiency)</a>
[precision]: #precision

A 100% precise GC is one that knows the type of every object and field
that it encounters, in terms of being able to classify a word of
memory as an integer or a pointer, and also classify whether a given
word of memory is actually usable according to the type of the value
the word is embedded within.

A space-efficient GC, in essence, is one that is eventually able to
reclaim all garbage, without being subverted by particular details of
the host program or the system state.

(Calling a language implementation space-efficient is a reference to
the [asymptotic space complexity] of a language implementation.  I am
employing the term here because the objective I want to capture is
more general than just precision.)

[asymptotic space complexity]: http://www.cesura17.net/~will/professional/research/papers/tail.pdf

A [conservative GC][conservative-gc] lacks precision. In other words,
a precise GC is more space-efficient than a conservative GC: There
exists a program that will exhibit worse (asymptotic) space
performance atop a conservative GC than it would atop a precise GC.

We would like Rust to be able to interoperate with 100% precise
collectors.

Ideally, we would also like to be able to interoperate with collectors
that do not support [pinning][pinning-support].

Finally, we would like to ensure that the heap patterns associated
with [Compositionality][] do not cause garbage to go unreclaimed.

 * Note that a precise GC that treats *all* objects on the "Rust Heap"
   as roots is not very space-efficient: it will fail to collect
   cyclic garbage structure like the below.

<p id="target_anchor_demo_garbage_cycle_thru_rust_heap"></p>
<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var local_a = { id: "local_a", label: "a", shape: "record" };

var a = object_record("A", "<f0> Some(Gc(B)) | <f1> None");
a.style = "rounded";
var b = object_record("B", "<f0> None | <f1> None");
b.style = "rounded";
var c = object_record("C", "<f0> Some(Gc(B)) | <f1> Some(Box(O))");
c.style = "rounded";
var o = object_record("O", "<f0> Gc(C)");

a.f0 = edge_from_to_ports(":f0", ":id:n", b);

c.f0 = edge_from_to_ports(":f0", ":id", b);
c.f1 = edge_from_to_ports(":f1:s", ":id", o);
o.f0 = edge_from_to_ports(":f0", ":id", c);

stack[1] = local_a;
local_a.ref = edge_to_port(":id", a);

gc_heap[0] = a;
gc_heap[1] = b;
gc_heap[2] = c;
rust_heap[0] = o;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_demo_garbage_cycle_thru_rust_heap", objects, { rankdir:"LR", nodesep:0.2 });
</script>

In the above diagram, "C" and "O" are unreachable by the program
itself ("O" is owned by the gc-allocated "C"), but if you treat all
objects in the Rust Heap as roots, then it will classify "O" as a
root, and "C" will never be reclaimed.

This is why compositionality can interact with space-efficiency.
Allowing gc-allocated objects to own data allocated on the Rust heap,
while also allowing references to gc-allocated objects to be stored in
values on the Rust heap, then you will encounter cyclic structure like
this. (This was the design bug that led me to withdraw my "Take II"
[allocator RFC][allocators take II].)

[allocators take II]: https://github.com/rust-lang/rfcs/pull/244

## Conclusion

This post was dedicated to identifying criteria that we would
like GC-integration with Rust to satisfy.

Next up: Why is it hard to satisfy the above criteria simultaneously?

## Footnotes

<a name="footnote1">1.</a> In particular, smart-pointers in Rust
require at *least* support for the [`Deref` trait][Deref trait], so
that dereferencing expressions like `gc_ref.field` and
`gc_ref.method()` are compiled into code that resolves the `gc_ref` to
a`&T` reference (and then the subsequent field or method lookup is
performed with respect to that `&T` reference).

As a reminder, the signature of the `deref` method, before lifetime
elision, is `fn deref<'a>(&'a self) -> &'a Self::Target` (and the
associated `Target` type for `Gc<T>` would be `T`).  Thus, the
compiler will ensure that the reference `&'a T` we extract from the
`gc_ref` outlive the `gc_ref` itself; this means that the `gc_ref`
will be one (of potentially many) root keeping the object from being
reclaimed for the entirety of the lifetime `'a`, and thus supporting
the `Deref` trait design on a `Gc<T>` could work seamlessly on an
entirely non-moving GC.

However, moving objects complicate `Deref` support; now one needs to
ensure not only that the object remains alive, but also that the
reference `&'a T` consistently points to the same object that the
original `Gc<T>` pointed to, and that references to substructure
within the object (e.g. a `&Left` within a `Gc<(Left, Right)>` that
has been deref'ed to `&(Left, Right)`) also retain a consistent view
of the heap structure. Doing this at an acceptable cost is difficult;
I may discuss this more in a future post.

<a name="footnote2">2.</a> In truth, even for a conservative collector
like [BDW][], one must do more than just "swap in a new
`#[allocator]`" to actually integrate it properly; the current Rust
standard library does not provide a way to intercept thread spawns and
register the new stack associated with each new thread. I only
realized this only
[recently](https://github.com/swgillespie/boehm_gc_allocator/issues/2).

<a name="footnote3">3.</a> If the GC Heap is exposed to multiple
threads, then there are complications even with the seemingly simple
task of updating the handles array, since one must ensure that if two
threads have consistent views of the heap object graph.

<a name="footnote4">4.</a> It is not clear whether client code hooking
into the "black box" GC would be able to instantiate the GC objects
with its own types.

For example, one might think that the objects in the GC heap could be
defined via type parameterization: `fn bbox_gc_alloc<T>(t: T) -> Handle;`
would create an object on the heap, copy `t` into it, and return a
handle to that object.

For this to work, the layout of the list cells in the GC heap above
would need to look something like this:

```rust
struct Cons<T> {
    data: T,
    next: Option<GcPtr<Cons<T>>>,
}
```

Then constructing a list like the "X, Y, Z" in the heap diagrams
above would look like:

```rust
let handle_z = bbox_gc_alloc(Cons { data: 'c', next: None });
let handle_y = bbox_gc_alloc(Cons { data: 'b', next: None });
let handle_x = bbox_gc_alloc(Cons { data: 'a', next: None });
handle_set_next(handle_y, handle_z);
handle_set_next(handle_x, handle_y);
```

But there are two (related) problems:

  1. How does one instantiate values that *unconditionally* hold
     pointers to GC objects.  (For example, how do we allocate an
     instance of `Cons<GcPtr<Cons<char>>>`?)

     We have already established that the address in the GC Heap are
     not exposed outside of the heap, so the approach of passing in an
     `T` value that we used with `bbox_gc_alloc` above will not work,
     because we cannot put our hands on a `GcPtr` to use for the
     `data` field.

  2. How do we get from the `struct` definition for `Cons` to
     the family of methods defined in terms of `Handle`?

     Every occurrence of `GcPtr` used for the struct (as seen from
     the point of view of the GC Heap) needs to be mapped to
     a `Handle` in the functions exposed to the functions outside
     of GC Heap.

It could be that there is a solution to the problem lurking here; but
as stated in the text, interoperation with a fully "black box" GC is
not a primary goal.

