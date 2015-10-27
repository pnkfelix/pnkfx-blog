---
layout: post
title: "GC and Rust Part 1: The Roots of Integration"
date: 2015-10-28 15:24
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

<!-- more -->

(The body of this post makes heavy use of client-side rendering,
because of author idiosyncrasies.  You may need to wait a moment while
the supporting Javascript loads.)

<script src="/javascripts/viz.js" charset="utf-8"></script>
<script src="/javascripts/js_to_dot.js" charset="utf-8"></script>
<script src="/javascripts/gc_rendering.js" charset="utf-8"></script>

## The Problem Space
[problem-space]: #the-problem-space

Now that we have [reviewed][part0] what GC is and how it works, let us
discuss what GC could mean to Rust.

[part0]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/
[conservative-gc]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#conservative-gc
[pinning-support]: /blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#pinning-support

I have identified two distinct kinds of support that we could provide:
a feature for pure Rust programs, versus an 3rd-party runtime
interoperation feature.

### GC for pure Rust programs

We could add a smart-pointer interface, e.g. a `Gc<T>` type, that
arbitrary library crates could use as they create or receive instances
of `T`. The intention here would be similar to how `Rc<T>` is used:
One does not want to track ownership precisely, but rather treat
ownership as shared amongst all users of a value, and let the runtime
system handle reclaiming the value.

This kind of feature could be useful in any Rust library.

There are two easy-to-identify drawbacks with this kind of collector
support.

First, adding it would require that the standard library either
provide a garbage collector (that all clients of `Gc<T>` would have to
link in), or at least standardize a fixed API that third-party
collector implementations would have to satisfy to support `Gc<T>`.

Second, it is difficult to provide the ergonomics (such as
[`Deref`][Deref trait] support) that one expects from a smart-pointer
type analogous to `Rc<T>`.

Since `Rc<T>` is already a workable solution for many (though not all)
use cases of `Gc<T>`, it is not a main priority right now.

### GC as Interoperation Feature

GC as an interoperation feature means that Rust would provide
introspective hooks to improve integration with application frameworks
that are using their own garbage collector. An obvious example of this
is Servo's use of the SpiderMonkey Virtual Machine for its Javascript
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
programmer ergonomics that `Rc<T>` provides. For example, it is
acceptable to not support [`Deref`][Deref trait].

[Deref trait]: https://doc.rust-lang.org/std/ops/trait.Deref.html

However, it still requires defining a standard interface that the
third-party collector implementation has to conform with.

In a simple world (e.g., a conservative collector designed to
interoperate with C/C++, such as [boehm-demers-weiser][BDW]), this
standard interface could be nothing more than just "swap in a
different [`#[allocator]` crate][custom_alloc] that your GC provides."

[BDW]: http://www.hboehm.info/gc/

[custom_alloc]: https://doc.rust-lang.org/nightly/book/custom-allocators.html

(The actual interface is unlikely to be so simple, but the point is,
there is a wide design space to be explored here.)

### Objectives and Requirements

The two kinds of support described above are two distinct features;
there is overlap between them, but trying to find a single solution
that solves both problems completely may not be possible, and in any
case we do not want to wait for it to be discovered.

There are a number of other objectives for Rust/GC integration that
are worth noting, which I will list here and then define and discuss
below.

  1. [Modularity][modularity]
  2. [Safety][safety]
  3. [Zero-Cost][zero-cost]
  4. [Compositionality][compositionality]
  5. [Precision][precision]

### <span id="modularity">Modularity with respect to GC</span>
[modularity]: #modularity

A Rust program that uses GC should be able to link with a crate whose
source code was authored without knowledge of GC.

For example, if I make a parsing library today that works on string
slices, you should be able to link that parsing library into a program
that uses GC, without having to worry about whether the parsing
library carries hidden requirements that invalidate linking its crate
together with yours.



Note: A crate being "authored without knowledge of GC" is a
property of the source code, not the generated object code. Given
such a crate, the Rust compiler may itself inject metadata
related to GC, such as descriptiohs of object layout, or
automatically-generated code that dictate how objects should
traced by the collector.

Note: A crate being "authored without knowledge of GC" is not
quite the same as that crate not supporting GC. That is, we may
add well a way for a crate to declare that it is not compatible
with GC. (This would count as having knowledge of GC; in fact,
enough knowledge to know, or at least guess, that the presence of
a GC would cause the crate to break.)

If we cannot satisfy this requirement, then the addition of GC
will, at best, split the growing space of library crates (such as
those available on [crates.io][]) into two disjoint
sub-communities: crates that support GC, and those that do not
(since the latter were written without accounting for the
potential presence of a GC).

A final note: There are some features available to crates, such as
requiring a specific low-level allocator, that are likely to be
incompatible with a program that uses GC. I need to find a way to
incorporate these caveats into the above definition of "modularity",
without weakening it to the point of uselessness.

### <span id="safety">Safety with respect to GC</span>
[safety]: #safety

If a Rust crate does not use `unsafe` constructs
(`unsafe` blocks, attributes or types with "unsafe" in their
name, etc.), then linking it with another crate that uses GC
should maintain soundness.

In other words, linking in a crate C that uses no `unsafe`
construct should not inject any dereferences of dangling
pointers, nor any data races.

This objective arguably should be lumped in with modularity.

I list it as a separate item, because (I claim) it is a non-goal
for the Rust compiler to ensure safety if I start with a sound
program, and then I swap one of its sub-crates with some
arbitrary other crate, `C`, that uses `unsafe { ... }`.

It is simply too easy for the crate `C` to perform operations in
the `unsafe`-block that invalidate the assumptions of the
original program. (But it should still be feasible to compile and
run the new version linked with `C`, even if it is no longer
guaranteed to be safe.)

### <span id="zero-cost">Zero-Cost GC</span>
[zero-cost]: #zero-cost

If you don't use the GC feature (in whatever form it
takes), your code should not pay for it.

This applies to the quality of the generated code (in execution
time and code size), and also to the source code, with respect to
difficulty in writing a program or library.

### <span id="compositionality">Compositional GC</span>
[compositionality]: #compositionality

One can use a reference to a gc-allocated object, call it a `GcRef`,
as the field type in a `struct`, store it into a `Vec<GcRef>`, and
generally anything else one can do with a Rust value.

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

The compositionality constraint may seem obvious (especially if one starts by
assuming that references to gc-allocated objects will be values
of type `Gc<T>` for arbtrary `T`).

But a hypothetical library for implementing a particular language
could completely insulate the representations of garbage-collected
values, offering only accessor methods for manipulating their
internals. Such a constrained setting makes the GC implementors job
much easier, but at the cost of more difficult interoperation (since,
for example, in such a setting one cannot extract a reference to the
internal structure of a garbage-collected value as a `&T`).

[crates.io]: https://crates.io/

### <a id="precision">Precision</a>
[precision]: #precision

A precise GC is one that is eventually able to reclaim all garbage,
without being subverted by particular details of the host program or
the system state.

Obviously a [conservative GC][conservative-gc] lacks precision.

We would like to be able to interoperate with 100% precise collectors.

Ideally, we would also like to be able to interoperate with collectors
that do not support [pinning][pinning-support].

## Rust complicates GC
[rust-complicates-gc]: #rust-complicates-gc

Let us assume that for the short term we are more interested in
providing GC-interoperation and are willing to forego GC as a pure
Rust programming feature.

Even with that assumption, there are still serious obstacles.

### Identifying the Root Set

<script>
function root_set_objects() {
    var gc_a = { id: "gc_a", label: "Gc(A)", shape: "record" };
    var box_b = { id: "box_b", label: "Box(B)", shape: "record" };

    var a = object_record("A", "<f0> Gc(C)");
    a.style = "rounded";
    var b = object_record("B", "<f0> Gc(C)");

    var c = object_record("C", "<f0> Gc(X) | <f1> Box(O)");
    c.style = "rounded";
    var g = object_record("G", "<f0> Vec(V)");
    g.style = "rounded";
    var v = object_record("V", "");
    var o = object_record("O", "<f0> Gc(X)");
    var x = object_record("X", "<f0> 'data'");
    x.style = "rounded";

    gc_a.val = edge_to_port(":id", a);
    box_b.val = edge_to_port(":id", b);
    a.f0 = edge_from_to_ports(":f0", ":id", c);
    b.f0 = edge_from_to_ports(":f0", ":id", c);
    c.f0 = edge_from_port(":f0", x);
    c.f1 = edge_from_port(":f1", o);
    g.f0 = edge_from_to_ports(":f0", ":id", v);
    o.f0 = edge_from_to_ports(":f0", ":id", x);

    // You need the .id of a subgraph to start with "cluster" if you want
    // it to show up with the outline (and label, if present).
    var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true, color: "blue" };
    var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
    var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

    stack.rank = "same";
    stack[1] = gc_a;
    stack[0] = box_b;

    rust_heap.rank = "same";
    rust_heap[0] = o;
    rust_heap[1] = box_b;
    rust_heap[2] = b;
    rust_heap[3] = v;

    gc_heap.rank = "same";
    gc_heap[0] = a;
    gc_heap[1] = c;
    gc_heap[2] = x;
    gc_heap[3] = g;

    var objects = [stack, gc_heap, rust_heap];
    return objects;
}
</script>

<p id="target_anchor_identifying_the_root_set"></p>
<script>
var objects = root_set_objects();
post_objects("target_anchor_identifying_the_root_set", objects, { rankdir:"RL", with_code: true, nodesep:0.2 });
</script>
