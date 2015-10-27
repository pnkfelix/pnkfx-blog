---
layout: post
title: "GC and Rust Part 2: The Roots of Integration"
date: 2015-10-29 15:00
comments: true
categories: gc rust
---

This is the second in a series of posts will discuss why garbage
collection is hard, especially for Rust, and brainstorm about
solutions to the problems we face.

The [previous post][part1] wrote down some criteria for integration.
Now I want to delve into why satisfying those criteria is hard,
at least in Rust as it stands today.

[part1]: /blog/2015/10/27/gc-and-rust-part-1-specing-the-problem/

<!-- more -->

(The body of this post makes heavy use of client-side rendering,
because of author idiosyncrasies.  You may need to wait a moment while
the supporting Javascript loads.)

<script src="/javascripts/viz.js" charset="utf-8"></script>
<script src="/javascripts/js_to_dot.js" charset="utf-8"></script>
<script src="/javascripts/gc_rendering.js" charset="utf-8"></script>

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
