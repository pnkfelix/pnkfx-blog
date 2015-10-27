---
layout: post
title: "GC and Rust Part 0: Garbage Collection Background"
date: 2015-10-27 14:09
comments: true
categories: gc
published: true
---

This post is a prequel to a series of posts discussing why garbage
collection is hard, especially for Rust, and brainstorming about
solutions to the problems we face.

The goal of this post is to provide the background foundational material
about Garbage Collection that the other posts will then build upon.

<!-- more -->

You can skip ahead to the follow-up posts (once they are published) if
you feel you are already well-versed in the low-level mechanics of
garbage collection.

I may add more material to this post in the future if I discover a
need to provide more detail on a particular subtopic, such as "write
barriers".

(The body of this post makes heavy use of client-side rendering,
because of author idiosyncrasies.  You may need to wait a moment while
the supporting Javascript loads.)

<script src="/javascripts/viz.js" charset="utf-8"></script>
<script src="/javascripts/js_to_dot.js" charset="utf-8"></script>
<script src="/javascripts/gc_rendering.js" charset="utf-8"></script>
<script>
function simple_gc_structure() {
    var rf = make_regfile("RF");
    var a = { id: "A" };
    var b = { id: "B" };
    var c = { id: "C" };
    var d = { id: "D" };
    var e = { id: "E" };
    var f = { id: "F" };
    var g = { id: "G" };
    b.f0 = c;
    d.f0 = a;
    d.f1 = e;
    e.f0 = f;
    f.f0 = e;
    c.f0 = g;

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
    var g = { id: "G" };

    var a2 = { id: "A2", label: "A'" };
    var b2 = { id: "B2", label: "B'" };
    var c2 = { id: "C2", label: "C'" };
    var g2 = { id: "G2", label: "G'" };

    b.f0 = c;
    c.f0 = g;
    d.f0 = a;
    d.f1 = e;
    e.f0 = f;
    f.f0 = e;

    b2.f0 = c2;
    c2.f0 = g2;

    a.fwd = dashed_edge(a2);
    b.fwd = dashed_edge(b2);
    c.fwd = dashed_edge(c2);
    g.fwd = dashed_edge(g2);

    rf.link(0, a2);
    rf.link(1, b2);
    rf.link(3, c2);

    return [rf, d, a, b, c];
}

function simple_gc2() {
    var [rf, d] = simple_gc_structure();
    // for_each_reachable([d], hide, { on_edge: hide });
    // for_each_reachable([rf], unhide);
    var content = render_objects([rf, d]);
    return digraph(content, { rankdir:"LR" });
}
</script>

<p id="target_anchor1"></p>
<script>
var [rf, d] = simple_gc_structure();
var content = render_objects([rf]);
post_graph("target_anchor1", digraph(content, { rankdir:"LR" }));
</script>

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
reachable objects by computing the connected components that include
the "roots" of the object graph. (The "roots" are the starting points
from which any chain of references must originate in the source
program.)

So, for example, we might have the following set of
gc-managed objects (labelled "A" through "F" below),
along with a register file labelled "RF".

<p id="target_anchor2"></p>
<script>
post_objects("target_anchor2", simple_gc_structure(), { rankdir:"LR" });
</script>

In the simple model above, the roots *are* the processor
registers. Such a model is applicable to a language runtime where all
memory blocks (*including* the stack frames) are managed by the garbage
collector. (In other words, we are not talking about Rust yet.)

The reachable objects, as stated above, are the connected
components of the graph that includes the roots, highlighted
below.

<p id="target_anchor3"></p>
<script>
// Overriding the `highlight` I put into js_to_dot.js
function highlight(object) {
    object.penwidth = "3.0";
    return object;
}

var objects = simple_gc_structure();
for_each_reachable([objects[0]], { on_node: highlight, on_edge: highlight });
post_objects("target_anchor3", objects, { rankdir:"LR" });
</script>

A garbage collector would determine that the objects
labelled "D", "E", and "F" are unreachable, and thus
their storage can be reclaimed.

#### Footnotes I

<a name="footnote1">1.</a> Researchers have explored methods exist to
identify objects as dead even when reachable, such as using
["parametricity"][collecting-more-garbage]; but I am not aware of any
such method being used outside of a research setting.

[collecting-more-garbage]: http://pop-art.inrialpes.fr/~fradet/PDFs/LISP94.pdf

## <span id="how-gc-works">How Garbage Collection works</span>
[how-gc-works]: #how-gc-works

A garbage collector is often presented as a
*coroutine*<sup>[2](#footnote2)</sup> that is linked in with the main
program. The main program itself is often referred to as a "mutator",
since it is the entity that *mutates* the object graph.  (The
collector does not modify the abstract object graph, but rather the
*representation* of the object graph in memory.)

The mutator requests memory from some allocation service (usually
deeply integrated with the garbage collector for reasons we will see).
If there is a memory block immediately available to satisfy the
request, then the allocator hands that over. If there is not
sufficient free memory, then the mutator's allocation attempt invokes
the garbage collector coroutine.

Garbage collectors are also often divided into two categories: Copying
collectors, and Mark-Sweep collectors. Both collectors accomplish the
goal of identifying the reachable objects and reclaiming the
remainder, but they do it in different ways.

It is worthwhile to remember at this point that even though our object
graphs above are drawn as abstract circles and arrows, the objects are
represented somehow in memory.

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
var a_copied = state.a_copied;
var b_copied = state.b_copied;
var c_copied = state.c_copied;
var g_copied = state.g_copied;
var a_scanned = state.a_scanned;
var b_scanned = state.b_scanned;
var c_scanned = state.c_scanned;
var g_scanned = state.g_scanned;

var addr1=[
          "<nd> 0x10000 "+(swept?"":"(D) ")+"\\l",
          "0x10004 "+(swept?" (next)":"")+"\\l",
          "0x10008 \\l",
          "0x1000c \\l",
          "<na> 0x10010 (A) \\l",
          "0x10014 \\l",
          "0x10018 \\l",
          "0x1001c \\l",
          "<ne> 0x10020 "+(swept?"":"(E) ")+"\\l",
          "0x10024 "+(swept?" (next)":"")+"\\l",
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
          "0x10054 "+(swept?" (next)":"")+"\\l",
          "0x10058 \\l",
          "0x1005c \\l",
          "<nf> 0x10060 "+(swept?"":"(F) ")+"\\l",
          "0x10064 "+(swept?" (next)":"")+"\\l",
          "0x10068 \\l",
          "0x1006c \\l",
          "<ng> 0x10070 (G) \\l",
          "0x10074 \\l",
          "0x10078 \\l",
          "0x1007c \\l",
         ];
var addr3 = [
          "<na> 0x20000 "+(a_copied?"(A') ":"")+"\\l",
          "0x20004 \\l",
          "0x20008 \\l",
          "0x2000c \\l",
          "<nb> 0x20010 "+(b_copied?"(B') ":"")+"\\l",
          "0x20014 \\l",
          "0x20018 \\l",
          "0x2001c \\l",
          "<nc> 0x20020 "+(c_copied?"(C') ":"")+"\\l",
          "0x20024 \\l",
          "0x20028 \\l",
          "0x2002c \\l",
          "<ng> 0x20030 "+(g_copied?"(G') ":"")+"\\l",
          "0x20034 \\l",
          "0x20038 \\l",
          "<lastg> 0x2003c \\l",
];

var val1 = [
           (swept ? "<vd> (free) " : "<vd> (header)"), // (D)
           (swept ? "<f0> 0x10020" : "<dpa> 0x10010"),
           (swept ? " - "          : "<dpe> 0x10020"),
           " - ",

           (marks ? "<va> (marked)" : a_copied ? "<va> (fwd)" : "<va> (header)"), // (A)
           (a_copied ? "<afwd> 0x20000" : " - "),
           " - ",
           " - ",

           (swept ? "<ve> (free) " : "<ve> (header)"), // (E)
           (swept ? "<f1> 0x10050" : "<epf> 0x10060"),
           " - ",
           " - ",

            // (B)
           (marks ? "<vb> (marked)" : b_copied ? "<vb> (fwd)" : "<vb> (header)"),
           (b_copied ? "<bfwd> 0x20010" : "<bpc> 0x10040"),
           " - ",
           " - ",
          ];

var val2 = [
           (marks ? "<vc> (marked)" : c_copied ? "<vc> (fwd)" : "<vc> (header)"), // (C)
           (c_copied ? "<cfwd> 0x20020" : "<cpg> 0x10070"),
           " - ",
           " - ",

           (swept ? "<vy> (free) " : "<vy> (header)"), // unused
           (swept ? "<f2> 0x10060" : " - "),
           " - ",
           " - ",

           (swept ? "<vf> (free) " : "<vf> (header)"), // (F)
           (swept ? "<f3> null " : " - "),
           (swept ? " - "          : "<fpe> 0x10020"),
           " - ",

           (g_copied ? "<vg> (fwd)" : marks ? "<vg> (marked)" : "<vg> (header) "), // (G)
           (g_copied ? "<gfwd> 0x20030" : " - "),
           " - ",
           " - ",
          ];

var val3 = [
           ((a_scanned || a_copied) ? "<va> (header)" : " - "), // (A')
           " - ",
           " - ",
           " - ",

           // (B')
           (b_scanned ? "<vb> (header) " : b_copied ? "<vb> (header)" : " - "),
           (b_scanned ? "<bpc2> 0x20020 " : b_copied ? "<bpc> 0x10040" : " - "),
           " - ",
           " - ",

           // (C')
           (c_scanned ? "<vc> (header) " : c_copied ? "<vc> (header)" : " - "),
           (c_scanned ? "<cpg2> 0x20030 " : c_copied ? "<cpg> 0x10070" : " - "),
           " - ",
           " - ",

            // (G')
           (g_scanned ? "<vg> (header) " : g_copied ? "<vg> (header)" : " - "),
           " - ",
           " - ",
           " - ",
          ];

    return [addr1, val1, addr2, val2, addr3, val3];
}
</script>

<script>
function make_graph_in_memory(options) {
    var original = options.original;
    var marking  = options.marking;
    var marked = options.marked;
    var swept  = options.swept;
    var free_list = options.free_list;
    var conservative_r2 = options.conservative_r2;
    var avail = options.avail;

    var rf = make_regfile("RF");
    var addrval = make_memory_addr_val(options);
    var addr1 = addrval[0];
    var val1 = addrval[1];
    var addr2 = addrval[2];
    var val2 = addrval[3];
    var addr3 = addrval[4];
    var val3 = addrval[5];

    var a_copied = options.a_copied;
    var b_copied = options.b_copied;
    var c_copied = options.c_copied;
    var g_copied = options.g_copied;
    var a_scanned = options.a_scanned;
    var b_scanned = options.b_scanned;
    var c_scanned = options.c_scanned;
    var g_scanned = options.g_scanned;

    var highlight_bpc2 = options.highlight_bpc2;
    var highlight_rc2 = options.highlight_rc2;
    var highlight_cfwd = options.highlight_cfwd;

    rf.label = "<id>RF | { { <r0>r0 | <r1>r1 | <r2>r2 | <r3>r3 } |" +
        " { <r0v>"+(a_copied?"0x20000":"0x10010")+
        " | <r1v>"+(b_copied?"0x20010":"0x10030")+
        " | <r2v>"+(conservative_r2?"0x10000":"-")+
        " | <r3v>"+(c_copied?"0x20020":"0x10040")+
        " } }";
    rf.pos = "0,500!";

    var from_space = options.from_space;
    var two_space = options.two_space;
    var two_space_content = !two_space ? "" : [
        'mem3 [shape="record",',
        'pos="-180,500!",',
        'label=\"'+make_memory_label(16,
                function (i) { return addr3[i]; },
                function (i, addr) { return val3[i]; })+'\",',
        '];',
        (!from_space ? "" : a_copied ? 'mem1:afwd:w -> mem3:va:e [style="dashed"];' : ""),
        (!from_space ? "" : b_copied ? 'mem1:bfwd:w -> mem3:vb:e [style="dashed"];' : ""),
        (!from_space ? "" : c_copied ? 'mem2:cfwd:w -> mem3:vc:e ['+(highlight_cfwd ? 'penwidth="3.0",' : '')+'style="dashed"];' : ""),
        (!from_space ? "" : g_copied ? 'mem2:gfwd:w -> mem3:vg:e [style="dashed"];' : ""),
        (b_scanned ? 'mem3:bpc2:e -> hidden_bc2 ['+(highlight_bpc2 ? 'penwidth="3.0",' : '')+'arrowhead="none"];' : b_copied ? 'mem3:bpc:e -> hidden_rc [arrowhead="none"];' : ""),
        (b_scanned ? 'hidden_bc2 -> mem3:vc:e' + (highlight_bpc2 ? '[penwidth="3.0"]':';') : ""),
        (c_scanned ? 'mem3:cpg2:e -> hidden_cg2 [arrowhead="none"];' : c_copied ? 'mem3:cpg:e -> mem2:ng:w;' : ""),
        (c_scanned ? 'hidden_cg2 -> mem3:vg:e;' : ""),
        (avail ? 'avail[pos="-90,375!"]; ' : ''),
        ((avail && avail.trim() != "") ? 'avail -> ' + avail + ';': ''),
    ].join('\n');

    var from_space_content = [
        'mem1 [shape="record",',
        !from_space ? 'style="invis",' : "",
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
        'hidden_bc  [ pos="220,590!", shape="point", label="", width=0 ];',
        'hidden_bc2  [ pos="-100,510!", shape="point", label="", width=0 ];',
        'hidden_cg  [ pos="375,590!", shape="point", label="", width=0 ];',
        'hidden_cg2  [ pos="-100,430!", shape="point", label="", width=0 ];',
        'hidden_fe1 [ pos="375,350!", shape="point", label="", width=0 ];',
        'hidden_fe2 [ pos="25,350!", shape="point", label="", width=0 ];',
        'hidden_yf [ pos="375,500!", shape="point", label="", width=0 ];',
        'hidden_fz [ pos="375,450!", shape="point", label="", width=0 ];',
        free_list ? 'free_list [ pos="0,620!", shape="rectangle", label="free-list" ];' : '',
        'mem2 [shape="record",',
        !from_space ? 'style="invis",' : "",
        'pos="300,500!",',
        'label=\"'+make_memory_label(16,
            function (i) { return addr2[i]; },
            function (i, addr) { return val2[i]; })+'\",',
        '];',
    ].join('\n');

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
        from_space_content,
        // !marking ? 'RF:r0:w -> hidden_ra [arrowhead="none"];' : 'RF:r0:w -> hidden_ra [arrowhead="none",penwidth=3.0];',
        // !marking ? 'hidden_ra -> mem1:na:w;' : 'hidden_ra -> mem1:na:w [label="1", penwidth=3.0];',
        a_copied ? 'RF:r0:w -> mem3:va:e;' : !marking ? 'RF:r0v:e -> mem1:na:w;' : 'RF:r0v:e -> mem1:na:w [label="1", penwidth=3.0];',
        b_copied ? 'RF:r1:w -> mem3:vb:e;' : !marking ? 'RF:r1:w -> hidden_rb [arrowhead="none"];' : 'RF:r1:w -> hidden_rb [arrowhead="none",penwidth=3.0];',
        b_copied ? "" : !marking ? 'hidden_rb -> mem1:nb:w;' : 'hidden_rb -> mem1:nb:w [label="2", penwidth=3.0,penwidth=3.0];',
        c_copied ? 'RF:r3:w -> mem3:vc:e'+(highlight_rc2 ? "[penwidth=3.0]" : "") + ';' : !marking ? 'RF:r3:w -> hidden_rc [arrowhead="none"];' : 'RF:r3:w -> hidden_rc [arrowhead="none",penwidth=3.0];',
        conservative_r2 ? 'RF:r2v:e -> mem1:nd:w [penwidth=3.0];' : '',
        b_scanned ? '' : !marking ? 'hidden_rc -> mem2:nc:w;' : 'hidden_rc -> mem2:nc:w [label="5", penwidth=3.0];',
        two_space ? '' : original ? 'mem1:dpa:e -> hidden_da [arrowhead="none"];' : '',
        two_space ? '' : original ? 'hidden_da -> mem1:va:e;' : '',
        two_space ? '' : original ? 'mem1:dpe:e -> hidden_de [arrowhead="none"];' : '',
        two_space ? '' : original ? 'hidden_de -> mem1:ve:e;' : '',
        two_space ? '' : original ? 'mem1:epf:e -> mem2:nf:w;' : '',
        b_copied ? '' : !marking ? 'mem1:bpc:e -> hidden_bc [arrowhead="none"];' :
                 'mem1:bpc:e -> hidden_bc [arrowhead="none", label="3", penwidth=3.0];',
        b_copied ? '' : !marking ? 'hidden_bc -> mem2:nc:w;' : 'hidden_bc -> mem2:nc:w [penwidth=3.0];',
        c_copied ? '' : !marking ? 'mem2:cpg:e -> hidden_cg [arrowhead="none"];' :
                 'mem2:cpg:e -> hidden_cg [arrowhead="none", label="4", penwidth=3.0];',
        c_copied ? '' : !marking ? 'hidden_cg -> mem2:vg:e;' : 'hidden_cg -> mem2:vg:e [penwidth=3.0];',
        two_space ? '' : original ? 'mem2:fpe:e -> hidden_fe1 [arrowhead="none"];' : '',
        two_space ? '' : original ? 'hidden_fe1 -> hidden_fe2 [arrowhead="none"];' : '',
        two_space ? '' : original ? 'hidden_fe2 -> mem1:ne:w' : '',
        free_list ? 'free_list -> mem1:nd:w [style="dashed",penwidth=3.0]' : '',
        free_list ? 'mem1:f0:e -> hidden_de [style="dashed",penwidth=3.0, arrowhead="none"]' : '',
        free_list ? 'hidden_de -> mem1:ve:e [style="dashed",penwidth=3.0]' : '',
        free_list ? 'mem1:f1:e -> mem2:ny:w [style="dashed",penwidth=3.0]' : '',
        free_list ? 'mem2:f2:e -> hidden_yf [style="dashed",penwidth=3.0, arrowhead="none"]' : '',
        free_list ? 'hidden_yf -> mem2:vf:e [style="dashed",penwidth=3.0]' : '',
        two_space_content,
        '}'].join('\n');

    return graph_in_memory;
}
</script>

<p id="target_anchor4"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true});
post_graph("target_anchor4", graph_in_memory);
</script>

In these pictures, there is no difference between an arrow pointing to
the left- verus right-side of a memory cell; so the occurrence of the
pointer to A (`0x10010`) in `r0` is no different than the occurrence
of that same value in memory cell `0x10004` (the first non-header word
of `D`), even though the arc for the former is pointing at the left
side of the first memory cell of `A`, and the arc for the latter is
pointing at the right side of that memory cell.

### Mark-Sweep Collection

A Mark-Sweep collector works by first doing a traversal of the
reachable memory, *marking* each object it finds (e.g. by setting a
bit reserved in the object header, or in separate mark bitmap if there
is no such bit reserved). This traversal requires some amount of extra
memory in reserve to track remaining work for the trace (e.g. a "mark
stack" of objects we are in the midst of traversing, and/or a queue of
objects scheduled for future traversal).

#### The "Mark" phase

Here is a sketch of the traversals that the garbage collector
makes in order to mark each reachable object.

<p id="target_anchor5"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,marking:true, marked:true});
post_graph("target_anchor5", graph_in_memory);
</script>

As reflected in the diagram above, each object that the GC reaches has
its mark bit set to "marked" in its header word.

The numbers on the arcs above are meant to correspond to a
hypothetical traversal order as the GC marks the memory; particular
tracing strategies may yield different orders. (No matter what, we
will not trace object "G" until after we have seen "C" via some
route.)

Also, I have left the memory for the mark-stack out of the picture; in
this case the mark-stack would not grow very large, but in general one
must anticipate the mark-stack growing as large as the longest path
through the reachable object graph. (The longest path in this case is
three objects long.)

#### The "Sweep" phase

A Mark-Sweep collector does not move objects, so it must resort to
metadata such as a free-list to track reclaimed memory.  So, after the
marking is finished, the GC then *sweeps* over the memory: it walks
over the GC-managed address space<sup>[3](#footnote3)</sup> and
builds up a free-list of blocks that were not marked during the traversal.

<p id="target_anchor6"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,marked:true, swept: true, free_list: true});
post_graph("target_anchor6", graph_in_memory);
</script>

(The arcs that make up the free-list above are dashed, to distinguish
them from the "real" references that make up the object graph. In the
above scheme, the pointer to the next element in the free list is held
in the second word of each free block.<sup>[4](#footnote4)</sup>.)

With that, the GC is done; the mutator (i.e. main program) is now free
to take blocks off of the free-list to satisfy memory requests.

#### Footnotes II

<a name="footnote2">2.</a> Coroutines are much like subroutines,
except that instead of having a parent-child relationship (where the
child subroutine "returns" to the parent caller), a call from
coroutine A to coroutine B: saves the current context of where A
currently is, transfers control to B, and the next time B calls A,
resumes the context that was saved at the outset.

In other words, once the linkage has been established between A and B,
then A's calls to B look like *returns* from the viewpoint of B, (and
B's calls to A look like returns from the viewpoint of A).

<a name="footnote3">3.</a> Note that collectors often require that the
GC-managed memory be formatted in a way such that the collector can
"parse" it when doing a scan over its address space.

For example, in the Mark-Sweep collector, the mark bit for each object
needs to be located at predictable location.

Similarly, the GC needs to be able to derive the size of each object
it looks at, as part of this "parsing". (The address-space could be
partitioned into size classes, as we did above; or if objects in a
block are to be variable-sized, the size could be recorded in object
headers. There are clever representation techniques that avoid using
header words for small objects like pairs without requiring size-class
partitioning; but I digress.)

<a name="footnote4">4.</a> Putting the `next`-pointers for the
free-list into the second word of each four-word block is one way of
satisfying the aforementioned requirement that the GC-managed memory
be parseable.

### <span id="conservative-gc">Conservative Collection</span>
[conservative-gc]: #conservative-gc

A conservative collector is a particular kind of Mark-Sweep collector
where it is not provided enough information to know whether one or
more words of reachable data that it encounters should be interpreted
as a reference or not. Such a collector is forced to assume
(conservatively) that if the encountered datum is an allocated address
on the GC-managed heap, then that could be its purpose from the
perspective of the mutator, and therefore that datum must be treated
as a reference to memory.

In the diagrams above, I said that ` - ` denotes some value that the
GC knows is not a memory reference. In a conservative collector
without any type information, the only kind of value that can be
interpreted that way is one that lies outside the addreses space of
the GC-heap.

So for example, if we had the same picture as above, but the register
`r2` happen to hold an integer with value `0x10000`, and the
conservative collector is not told "`r2` holds a non-reference at this
point in the execution", then this diagram would result:

<p id="target_anchor_conservative_gc"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true,conservative_r2:true});
post_graph("target_anchor_conservative_gc", graph_in_memory);
</script>

That is, even though in the program itself, the value `0x10000` is not
meant to be interpreted as a memory address, `D` (and `E` and `F`) are
all conservatively classified as live objects.

### Copying Collection

A Copying collector moves objects from one location to another as part
of its tracing process, and updates the references in reachable
objects as it goes.

I will first illustrate this using our low-level memory graph,
but I will not draw the edges for the dead objects anymore,
as they add significant clutter to the picture.

#### The Reserved "To-space"

First, we need to have some reserved memory to target as we copy
objects.  I have put this target memory (the so-called "to-space") on
the left-hand side of the picture; the nearby "avail" circle is a
local variable in the GC that indicates the starting address that we
can use to copy objects into; so it starts off at the first address,
`0x20000`.

<p id="target_anchor7"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true, two_space:true, avail: "mem3:na"});
post_graph("target_anchor7", graph_in_memory);
</script>

#### Copying from the Roots

First we walk over the roots (in our simplified model, the registers),
and copy over all of the objects we see. So the below results after
we scan just the first two registers, copying the objects `A` and `B`
into new locations, respectively labelled `A'` and `B'`, and updating
`avail` accordingly.

<a id="memory_post_copy_a_and_b"><p id="target_anchor8a"></p></a>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true, two_space:true,
    a_copied:true, b_copied:true, avail: "mem3:nc"});
post_graph("target_anchor8a", graph_in_memory);
</script>

Note that as we copy objects from the source memory
(the so-called "from-space"), we must maintain a map from the
original object to its newly allocated copy. In this model,
this is accomplished by imperatively overwriting the original object
with `fwd` header marking it as "forwarded" as well as a "forwarding
pointer" (the dashed arcs) that points to the new location.

The copies themselves just get the original memory contents, so they
may have pointers to the old objects in the source memory (such as the
`B' -> C` arc in the picture). Those will get fixed up later.

We still need to scan the rest of the registers, which copies `C` as
shown below.

<p id="target_anchor8b"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true, two_space:true,
    highlight_rc2: true, highlight_cfwd: true,
    a_copied:true, b_copied:true, c_copied:true, avail: "mem3:ng"});
post_graph("target_anchor8b", graph_in_memory);
</script>

#### Scan the "To-space"

Now that we have finished scanning the roots, we start the fixup
process of scanning over the "to-space." Every time we encounter a
pointer into the "from-space", there are two cases to consider: Either
it is an already copied object (in which case there will be a
forwarding pointer installed), or it is an object that is not yet
copied.

If its a forwarded object, then we fixup our reference so that it
points to the new copy in the "to-space". We see this when the fixup
scan is scanning over `B'` and sees the `B' -> C` reference, which it
then rewrites to a `B' -> C'` reference, highlighted below.

<p id="target_anchor9"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true, two_space:true,
    highlight_bpc2:true,
    a_copied:true, b_copied:true, c_copied:true,
    a_scanned:true, b_scanned:true, avail: "mem3:ng"
    });
post_graph("target_anchor9", graph_in_memory);
</script>

The fixup scan is not yet complete; the next object it encounters,
`C'`, illustrates the other case of a reference to an object (`G`
here) that has not yet been copied. In this case, it just copies it,
in the same manner that we did when we were scanning the roots. (This
adds the forwarded object to the set of objects enqueued for fixup
scanning.)

<p id="target_anchor10"></p>
<script>
var graph_in_memory = make_graph_in_memory({from_space:true,original:true, two_space:true,
    a_copied:true, b_copied:true, c_copied:true, g_copied:true,
    a_scanned:true, b_scanned:true, c_scanned:true, avail: "mem3:lastg:s",
    });
post_graph("target_anchor10", graph_in_memory);
</script>

#### Reclaim the "From-space"

Eventually the fixup scan will finish processing all of the "to-space"
(since there are only a finite number of objects that could be
enqueued). At this point, there will be no more reachable objects in
any part of the from-space, and thus those memory blocks can be
reclaimed in their entirety.

<p id="target_anchor11"></p>
<script>
var graph_in_memory = make_graph_in_memory({two_space:true,
    a_copied:true, b_copied:true, c_copied:true, g_copied:true,
    a_scanned:true, b_scanned:true, c_scanned:true, g_scanned:true, avail: "mem3:lastg:s"
    });
post_graph("target_anchor11", graph_in_memory);
</script>

Woo, done!

### A Spectrum of Collectors

The mark-sweep and copying collection methods illustrated above
actually form two extreme points on a spectrum of implementation
techhniques.

In practice, many collectors are neither entirely mark-sweep nor
copying, but rather employ a hybrid strategy, where some memory
regions are reclaimed via a copying-collection, while others are
managed via a mark-sweep method. For example, some generational
collectors work by promoting objects from a young space into an older
space via copying collection, but then the last space (with the eldest
objects) can be managed via mark-sweep.

As another example of a hybrid strategy, there exist conservative
collectors (such as "mostly copying" collectors) where exact type
information is known for the heap-allocated objects, but the types are
not known for the roots (i.e. the registers and values embedded in the
stack). In such systems, it is not safe to move objects that are
referenced via conservatively-scanned words. Such objects are "pinned"
in place (which means that it cannot be moved by the collector) for
the duration of this collection, and thus space in their memory blocks
can only be reclaimed with via a mark-sweep collection.  However,
objects reachable solely via precisely-scanned words *can* be moved,
and memory blocks made up solely of such objects can be reclaimed via
a copying-collection strategy.

### <span id="pinning-support">Pinning Support</span>

In our discussion to follow, rather than attempt to characterize a
collector as "mark-sweep" or "copying", it will be more useful to
distinguish collectors in terms of whether or not they support
"pinning". In a language runtime that supports pinning, a mutator
(i.e. the main program linked with the collector coroutine) can tag
any live object as "pinned".

(In the example of "mostly copying" above, such pinning is
accomplished by putting a reference into a conservatively-scanned
root. However, some language runtimes provide first class support for
pinning; for example, the Microsoft CLR once offerred a
[`pin_ptr`][pin_ptr] smart-pointer for Managed C++ that would prevent
a referenced object from moving.)

[pin_ptr]: https://msdn.microsoft.com/en-us/library/1dz8byfh.aspx

In other words, in a runtime with pinning, the mutator dictates which
objects can be managed via copying collection. If the runtime does not
support pinning, then it is the *collector* that dictates which
objects are managed via copying; the mutator cannot rely on the
collector allowing arbitrary objects to be pinned.

### Simplifying our diagrams

While I am sure it was fun to decode the above renderings of memory
banks, now that we have seen how collectors work at a low-level, I am
going to revert to the earlier high-level object notation.  It should
be easier for you to read, and (just as important) for me to write.

<p id="target_anchor12"></p>
<script>
var [rf, d] = simple_gc_structure();

var a = rf.r0.target;
var b = rf.r1.target;
var c = rf.r3.target;

a_and_b = [a,b];
a_and_b.is_subgraph = true;
a_and_b.id = "cluster_a_and_b";
a_and_b.rank = "same";
a_and_b.style = "invis";

var gc_heap = [];
gc_heap.push(a_and_b);
for_each_reachable([c,d], function (o) { if (o !== rf) { gc_heap.push(o); } })

gc_heap.is_subgraph = true;
gc_heap.style = "invis";
gc_heap.id = "cluster_gc_heap";
var content = render_objects([rf, a_and_b, gc_heap, d]);
post_graph("target_anchor12", digraph(content, {rankdir:"LR"}));
</script>

To show a copying collector's intermediate state in a high-level
picture, I will show newly copied objects with prime marks (and, if
possible, in a separately delineated to-space), and a dashed-line for
forwarding pointers.

Here is an example of this style of rendering, using the earlier
example at [the point where](#memory_post_copy_a_and_b) a copying
collector had scanned just registers `r0` and `r1` (but had not yet
copied `C` from register `r3`), highlighting the copied objects and
the newly written references (including the dashed forwarding
pointers).

<p id="target_anchor_simplified_copying"></p>
<script>
var rf = make_regfile("RF");
// rf.label = "{" + rf.label + "}";
var a = { id: "A" };
var b = { id: "B" };
var c = { id: "C" };
var d = { id: "D" };
var e = { id: "E" };
var f = { id: "F" };
var g = { id: "G" };
var a2 = { id: "A2", label: "A'", penwidth: "3.0" };
var b2 = { id: "B2", label: "B'", penwidth: "3.0" };
b.f0 = c;
c.f0 = g;
d.f0 = a;
d.f1 = e;
e.f0 = f;
f.f0 = e;
b2.f0 = c;
a.fwd = highlight(dashed_edge(a2));
b.fwd = highlight(dashed_edge(b2));
rf.link(0, a2, {penwidth: "3.0"});
rf.link(1, b2, {penwidth: "3.0"});
rf.link(3, c);

var a_and_b = [a, b];
a_and_b.is_subgraph = true;
a_and_b.rank="same";
var gc_heap1 = [a_and_b, c, d, e, f, g];
gc_heap1.is_subgraph = true;
gc_heap1.label = "from space";
gc_heap1.id = "cluster_gc_heap1";
var gc_heap2 = [a2, b2];
gc_heap2.is_subgraph = true;
gc_heap2.id = "cluster_gc_heap2";
gc_heap2.label = "to space";
gc_heap2.rank = "same";
gc_heaps = [gc_heap1, gc_heap2];
gc_heaps.is_subgraph = true;
// gc_heaps.id = "cluster_all_gc_heaps";
// gc_heaps.rankdir = "TD";
var objects = [rf, gc_heaps];
post_objects("target_anchor_simplified_copying", objects, {rankdir:"LR"});
</script>

(This rendering is arguably just as clear (or unclear) as our earlier
[memory diagram](#memory_post_copy_a_and_b) was, apart from some
annoying edge-crossings due to the graphviz layout engine.)

## Conclusion

Well, I don't know if you learned anything about GC from this post.
I certainly learned a lot about techniques for wrestling with
graphviz.  :)

There will be a followup post soon-ish that will bring Rust into the
picture, discussing what GC and Rust integration even *means*, and
the host of problems that crop up.
