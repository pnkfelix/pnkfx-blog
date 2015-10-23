---
layout: post
title: "GC and Rust Part 1: The Roots of Integration"
date: 2015-10-16 15:24
comments: true
categories:
---

<script>
function digraph(content) {
    return 'digraph { bgcolor="transparent"; ' +
      ' overlap="false"; ' +  // if left out, nodes may overlap
      ' start=0; ' +          // seed the RNG (for consistency)
      '\n' + content  + '}';
}

function lr_digraph(content) {
    return digraph('rankdir="LR"; '+content);
}

function make_regfile(rf_id) {
    var rf = {
        id: rf_id,
        label: "<id>" + rf_id + " | <r0>r0 | <r1>r1 | <r2>r2 | <r3>r3",
        shape: "record"
    };

    rf.link = function(source, target) {
        if (!(0 <= source <= 3)) {
            console.error("unknown regfile source: "+source);
        }
        this['r'+source] = {
            source_port: ':r'+source+':e',
            target: target,
            is_edge: true,
        };
    }

    return rf;
}

function simple_gc_structure() {
    var rf = make_regfile("RF");
    var a = { id: "A" };
    var b = { id: "B" };
    var c = { id: "C" };
    var d = { id: "D" };
    var e = { id: "E" };
    var f = { id: "F" };
    b.f0 = c;
    d.f0 = a;
    d.f1 = e;
    e.f0 = f;
    f.f0 = e;

    rf.link(0, a);
    rf.link(1, b);
    rf.link(3, c);

    return [rf, d];
}

function copied_gc_structure() {
    var rf = make_regfile("RF");
    var a = { id: "A" };
    var b = { id: "B" };
    var c = { id: "C" };
    var d = { id: "D" };
    var e = { id: "E" };
    var f = { id: "F" };

    var a2 = { id: "A2", label: "A'" };
    var b2 = { id: "B2", label: "B'" };
    var c2 = { id: "C2", label: "C'" };

    b.f0 = c;
    d.f0 = a;
    d.f1 = e;
    e.f0 = f;
    f.f0 = e;

    b2.f0 = c2;

    a.fwd = a2;
    b.fwd = b2;
    c.fwd = c2;

    rf.link(0, a2);
    rf.link(1, b2);
    rf.link(3, c2);

    return [rf, d, a, b, c];
}

function simple_gc2() {
    var [rf, d] = simple_gc_structure();
    // for_each_reachable([d], hide, hide);
    // for_each_reachable([rf], unhide);
    var content = render_objects([rf, d]);
    return lr_digraph(content);
}

// Warning: I have not yet managed to get `options` to work.
function post_graph(target, g, options) {
    var elem = document.getElementById(target)
    // elem.innerHTML += "<code>" + g + "</code>"
    if (options) {
        elem.innerHTML += Viz(g, options);
    } else {
        elem.innerHTML += Viz(g, "svg");
    }
}

function post_objects(target, objects) {
    var g = lr_digraph(render_objects(objects));
    var elem = document.getElementById(target)
    // elem.innerHTML += "<code>" + g + "</code>"
    elem.innerHTML += Viz(g, "svg")
}
</script>

<script src="/javascripts/viz.js" charset="utf-8"></script>
<script src="/javascripts/js_to_dot.js" charset="utf-8"></script>

<p id="target_anchor1"></p>
<script>
var [rf, d] = simple_gc_structure();
var content = render_objects([rf]);
post_graph("target_anchor1", lr_digraph(content));
</script>

## Post really starts here!

This is the first in a series of posts will discuss why garbage
collection is hard, especially for Rust, and brainstorm about
solutions to the problems we face.

The relationship between garbage collection (GC) and the Rust
programming language has been an interesting one.

GC was originally deeply integrated into the language, complete with
dedicated syntax; then over time we found ways to lessen the
dependency on GC, and then finally remove it from the language
entirely.

However, we still want to provide support for garbage collection.

To explain why, I need to define the actual problems we seek to solve.

<!-- more -->

(The body of this post makes heavy use of client-side rendering.  You
may need to wait a moment while the supporting Javascript loads.)

<script src="/javascripts/viz.js" charset="utf-8"></script>

## What is Garbage Collection

A garbage collector is a component in the runtime for a programming
language that periodically attempts to reclaim memory (without
requiring explicit calls to memory-freeing routines in the programning
language). To do this soundly, the collector must identify blocks of
memory that cannot possibly be used in the future by the program
(i.e., "dead objects").

Discussions of garbage collection often equate the notion of "dead
object" with "unreachable object": If no chain of references exists
that could lead the program to an object, then that object cannot be
reached (and therefore cannot be used in the future).<sup>[1](#footnote1)</sup>

When one says "garbage collector", one usually means a "*tracing*
garbage collector": a collector that works by identifying the
reachable objects by computing the connected
components that include the "roots" of the object graph, i.e. the starting points from
which any chain of references originates.

So, for example, we might have the following set of
gc-managed objects (labelled "A" through "F" below),
along with a register file labelled "RF".

<p id="target_anchor2"></p>
<script>
post_objects("target_anchor2", simple_gc_structure());
</script>

In the simple model above, the roots *are* the processor
registers. Such a model is applicable to a language runtime where all
memory (*including* the stack frames) is managed by the garbage
collector. (In other words, we are not talking about Rust yet.)

The reachable objects, as stated above, are the connected
components of the graph that includes the roots, highlighted
in red below.

<p id="target_anchor3"></p>
<script>
var objects = simple_gc_structure();
for_each_reachable([objects[0]], highlight, highlight);
post_objects("target_anchor3", objects);
</script>

A garbage collector would determine that the objects
labelled "D", "E", and "F" are unreachable, and thus
their storage can be reclaimed.

----

<a name="footnote1">1.</a> Researchers have explored methods exist to
identify objects as dead even when reachable, such as using
["parametricity"][collecting-more-garbage]; but I am not aware of any
such method being used outside of a research setting.

[collecting-more-garbage]: http://pop-art.inrialpes.fr/~fradet/PDFs/LISP94.pdf

## How Garbage Collection works

A Garbage collector is often presented as a *coroutine* that is linked
in with the main program, which itself is often referred to as a
"mutator", since it is the entity that *mutates* the object graph.
(The collector does not modify the abstract object graph, but rather
the *representation* of the object graph in memory.)

Garbage collectors are often divided into two categories: Copying
collectors, and Mark-Sweep collectors. Both collectors accomplish the
goal of identifying the reachable objects and reclaiming the
remainder, but they do it in different ways.

It is worthwhile to remember at this point that even though our object
graphs are drawn as abstract circles and arrows, the objects are
actually occupy space in memory and have a representation there.

For example, here is one potential representation for the above object
graph, where ` - ` denotes some value that the GC knows is not a
memory reference. (Assume for this example that every GC allocated
object is made from four consecutive words in memory.)

<script>
function make_memory_label(count, name_callback, val_callback) {
    var addresses = "ADDRESS";
    var contents = "CONTENT";
    var saw_one = true;
    for (i = 0; i < count; i++) {
        if (saw_one) { addresses += " | "; contents += " | ";}
        var name;
        if (name_callback) {
            name = name_callback(i);
        } else {
            name = "0x1";
            name += ("0000" + (i * 8).toString(16)).slice(-4);
        }
        addresses += name;
        if (val_callback) { contents += val_callback(i, name); }
        saw_one = true;
    }
    var label = "{ { " + addresses + " } | { " + contents + " } }";
    return label;
}

function make_memory_addr_val(state) {
var marks = state.marked;
var swept = state.swept;

var addr1=[
          "<nd> 0x10000 (D) \\l",
          "0x10004 \\l",
          "0x10008 \\l",
          "0x1000c \\l",
          "<na> 0x10010 (A) \\l",
          "0x10014 \\l",
          "0x10018 \\l",
          "0x1001c \\l",
          "<ne> 0x10020 (E) \\l",
          "0x10024 \\l",
          "0x10028 \\l",
          "0x1002c \\l",
          "<nb> 0x10030 (B) \\l",
          "0x10034 \\l",
          "0x10038 \\l",
          "0x1003c \\l",
         ];
var addr2 = [
          "<nc> 0x10040 (C) \\l",
          "0x10044 \\l",
          "0x10048 \\l",
          "0x1004c \\l",
          "<ny> 0x10050 \\l",
          "0x10054 \\l",
          "0x10058 \\l",
          "0x1005c \\l",
          "<nf> 0x10060 (F) \\l",
          "0x10064 \\l",
          "0x10068 \\l",
          "0x1006c \\l",
          "<nz> 0x10070 \\l",
          "0x10074 \\l",
          "0x10078 \\l",
          "0x1007c \\l",
         ];
var val1 = [
           (swept ? "<vd> 0x10020" : "<vd> (header)"), // D
           (swept ? " - "          : "<dpa> 0x10010"),
           (swept ? " - "          : "<dpe> 0x10020"),
           " - ",

           (marks ? "<va> (marked)" : "<va> (header)"), // A
           " - ",
           " - ",
           " - ",

           (swept ? "<ve> 0x10050" : "<ve> (header)"), // (E)
           (swept ? " - "          : "<epf> 0x10060"),
           " - ",
           " - ",

           (marks ? "<vb> (marked)" : "<vb> (header)"), // (B)
           "<bpc> 0x10040",
           " - ",
           " - ",
          ];
var val2 = [
           (marks ? "<vc> (marked)" : "<vc> (header)"), // (C)
           " - ",
           " - ",
           " - ",

           (swept ? "<vy> 0x10060" : "<vy> (header)"), // unused
           " - ",
           " - ",
           " - ",

           (swept ? "<vf> 0x10070" : "<vf> (header)"), // (F)
           " - ",
           (swept ? " - "          : "<fpe> 0x10020"),
           " - ",

           (swept ? "<vz> null" : "<vz> (header) "), // unused
           " - ",
           " - ",
           " - ",
          ];

    return [addr1, val1, addr2, val2];
}
</script>

<script>
function make_graph_in_memory(options) {
    var original = options.original;
    var marking  = options.marking;
    var marked = options.marked;
    var swept  = options.swept;
    var free_list = options.free_list;

    var rf = make_regfile("RF");
    var addrval = make_memory_addr_val(options);
    var addr1 = addrval[0];
    var val1 = addrval[1];
    var addr2 = addrval[2];
    var val2 = addrval[3];

    rf.label = "<id>RF | { { <r0>r0 | <r1>r1 | <r2>r2 | <r3>r3 } | " +
        "{ <r0v>0x10010 | <r1v>0x10030 | <r2v> | <r3v>0x10040 } }";
    rf.pos = "0,500!";
    var graph_in_memory = ['digraph { node [fontsize=8]; ',
        'bgcolor="transparent";',
        'layout="neato"; inputscale=72;',
        // 'overlap="false";',
        // 'node [ pin=true ];',
        'rankdir="LR";',
        'nodesep=1.2;',
        // 'rank="same";',
        'splines="curved";',
        // 'node [font = "10px Monospace"];',
        render_node(rf),
        'mem1 [shape="record",',
        'pos="120,500!",',
        'label=\"'+make_memory_label(16,
            function (i) { return addr1[i]; },
            function (i, addr) { return val1[i]; })+'\",',
        '];',
        'hidden_ra  [ pos="-50,550!", shape="point", label="", width=0 ];',
        'hidden_rb  [ pos="-50,450!", shape="point", label="", width=0 ];',
        'hidden_rc  [ pos="-30,660!", shape="point", label="", width=0 ];',
        'hidden_da  [ pos="200,590!", shape="point", label="", width=0 ];',
        'hidden_de  [ pos="200,540!", shape="point", label="", width=0 ];',
        'hidden_bc  [ pos="220,600!", shape="point", label="", width=0 ];',
        'hidden_fe1 [ pos="375,350!", shape="point", label="", width=0 ];',
        'hidden_fe2 [ pos="25,350!", shape="point", label="", width=0 ];',
        'hidden_yf [ pos="375,550!", shape="point", label="", width=0 ];',
        'hidden_fz [ pos="375,450!", shape="point", label="", width=0 ];',
        free_list ? 'free_list [ pos="0,620!", shape="rectangle", label="free-list" ];' : '',
        'mem2 [shape="record",',
        'pos="300,500!",',
        'label=\"'+make_memory_label(16,
            function (i) { return addr2[i]; },
            function (i, addr) { return val2[i]; })+'\",',
        '];',
        // !marking ? 'RF:r0:w -> hidden_ra [arrowhead="none"];' : 'RF:r0:w -> hidden_ra [arrowhead="none",penwidth=3.0];',
        // !marking ? 'hidden_ra -> mem1:na:w;' : 'hidden_ra -> mem1:na:w [label="1", penwidth=3.0];',
        !marking ? 'RF:r0v:e -> mem1:na:w;' : 'RF:r0v:e -> mem1:na:w [label="1", penwidth=3.0];',
        !marking ? 'RF:r1:w -> hidden_rb [arrowhead="none"];' : 'RF:r1:w -> hidden_rb [arrowhead="none",penwidth=3.0];',
        !marking ? 'hidden_rb -> mem1:nb:w;' : 'hidden_rb -> mem1:nb:w [label="2", penwidth=3.0,penwidth=3.0];',
        !marking ? 'RF:r3:w -> hidden_rc [arrowhead="none"];' : 'RF:r3:w -> hidden_rc [arrowhead="none",penwidth=3.0];',
        !marking ? 'hidden_rc -> mem2:nc:w;' : 'hidden_rc -> mem2:nc:w [label="4", penwidth=3.0];',
        original ? 'mem1:dpa:e -> hidden_da [arrowhead="none"];' : '',
        original ? 'hidden_da -> mem1:va:e;' : '',
        original ? 'mem1:dpe:e -> hidden_de [arrowhead="none"];' : '',
        original ? 'hidden_de -> mem1:ve:e;' : '',
        original ? 'mem1:epf:e -> mem2:nf:w;' : '',
        !marking ? 'mem1:bpc:e -> hidden_bc [arrowhead="none"];' :
                 'mem1:bpc:e -> hidden_bc [arrowhead="none", label="3", penwidth=3.0];',
        !marking ? 'hidden_bc -> mem2:nc:w;' : 'hidden_bc -> mem2:nc:w [penwidth=3.0];',
        original? 'mem2:fpe:e -> hidden_fe1 [arrowhead="none"];' : '',
        original ? 'hidden_fe1 -> hidden_fe2 [arrowhead="none"];' : '',
        original ? 'hidden_fe2 -> mem1:ne:w' : '',
        free_list ? 'free_list -> mem1:nd:w [style="dashed"]' : '',
        free_list ? 'mem1:vd:e -> hidden_de [style="dashed", arrowhead="none"]' : '',
        free_list ? 'hidden_de -> mem1:ve:e [style="dashed"]' : '',
        free_list ? 'mem1:ve:e -> mem2:ny:w [style="dashed"]' : '',
        free_list ? 'mem2:vy:e -> hidden_yf [style="dashed", arrowhead="none"]' : '',
        free_list ? 'hidden_yf -> mem2:vf:e [style="dashed"]' : '',
        free_list ? 'mem2:vf:e -> hidden_fz [style="dashed", arrowhead="none"]' : '',
        free_list ? 'hidden_fz -> mem2:vz:e [style="dashed"]' : '',

        '}'].join('\n');

    return graph_in_memory;
}
</script>

<p id="target_anchor4"></p>
<script>
var graph_in_memory = make_graph_in_memory({original:true});
post_graph("target_anchor4", graph_in_memory);
</script>

### Mark-Sweep Collection

A Mark-Sweep collector works by first doing a traversal of the
reachable memory, *marking* each object it finds (e.g. by setting a
bit reserved in the object header, or in separate mark bitmap if there
is no such bit reserved). This traversal requires some amount of extra
memory in reserve to track remaining work for the trace (e.g. a "mark
stack" of objects we are in the midst of traversing, and/or a queue of
objects scheduled for future traversal).

<p id="target_anchor5"></p>
<script>
var graph_in_memory = make_graph_in_memory({marking:true, marked:true});
post_graph("target_anchor5", graph_in_memory);
</script>

(The numbers on the arcs above are meant to correspond to a
hypothetical traversal order as the GC marks the memory; particular
tracing strategies may yield different orders. Also, I have left the
memory for the mark-stack out of the picture; in this case the
mark-stack would not grow very large, but in general one must
anticipate the mark-stack growing as large as the longest path through
the object graph.)

A Mark-Sweep collector does not move objects, so it must resort to
metadata such as a free-list to track reclaimed memory.  So, after the
marking is finished, the GC then *sweeps* over the memory and builds
up a free-list of blocks that were not marked during the traversal.

<p id="target_anchor6"></p>
<script>
var graph_in_memory = make_graph_in_memory({marked:true, swept: true, free_list: true});
post_graph("target_anchor6", graph_in_memory);
</script>

(The arcs that make up the free-list above are dashed, to distinguish
them from the "real" references that make up the object graph.)

With that, the GC is done; the mutator (i.e. main program) is now free
to take blocks off of the free-list to satisfy memory requests.

### Copying Collection

A Copying collector moves objects from one location to another as part
of its tracing process, and updates the references in reachable
objects as it goes.


<p id="target_anchor7"></p>
<script>
var objects = copied_gc_structure();
post_objects("target_anchor7", objects);
</script>


## The Problem Space

I have identified two distinct kinds of support that we could provide:

  1. A pure Rust programming feature: We could add a collector
     interface, e.g. a `Gc<T>` type, that arbitrary library crates
     could use as they create or receive instances of `T`. The
     intention here would be similar to how `Rc<T>` is used: One does
     not want to track ownership precisely, but rather treat ownership
     as shared amongst all users of a value, and let the runtime
     system handle reclaiming the value.

     This kind of feature could be useful in any Rust library.

     However, it is also very difficult to add (at least with the
     ergonomics that one expects from a smart-pointer type like
     `Rc<T>`), and since `Rc<T>` is already a workable solution for
     many (though not all) use cases of `Gc<T>`, it is not the main
     priority right now.

  2. An interoperation feature: Provide introspective hooks to improve
     integration with application frameworks that are using their own
     garbage collector. An obvious example of this is Servo's use of
     the SpiderMonkey Virtual Machine for its Javascript support.

     Servo is relying on SpiderMonkey's garbage collection for memory
     management, not only for Javascript values, but even for
     [native-code DOM objects][servo post].

[servo post]: https://blog.mozilla.org/research/2014/08/26/javascript-servos-only-garbage-collector/

     That post describes (unchecked) scenarios where one can end up
     with dangling pointers -- that is, they invite unsoundness.
     Proper support for GC-interoperation in Rust could address this;
     I will discuss this further down in this post.

     Critically, GC-interoperation does not require the same level of
     programmer ergonomics that `Rc<T>` provides. For example, it is
     acceptable to not support [`Deref`][Deref trait].

[Deref trait]: https://doc.rust-lang.org/std/ops/trait.Deref.html

These are two distinct features; there is overlap between them, but
trying to find a single solution that solves both problems completely
may not be possible, and in any case we do not want to wait for it to
be discovered.

Let us assume that for the short term we are more interested in
providing GC-interoperation and are willing to forego GC as a pure
Rust programming feature.

<script src="/javascripts/viz.js" charset="utf-8"></script>

