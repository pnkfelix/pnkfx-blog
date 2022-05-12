---
layout: post
title: "GC and Rust Part 2: The Roots of the Problem"
date: 2015-12-31 24:00
comments: true
categories: gc rust
---

This is the second in a series of posts will discuss why garbage
collection is hard, especially for Rust, and brainstorm about
solutions to the problems we face.

The [previous post][part1] wrote down some criteria for integration.
Now I want to delve into why satisfying those criteria is hard,
at least in Rust as it stands today.

[part1]: /blog/2015/11/10/gc-and-rust-part-1-specing-the-problem/

<!-- more -->

<script>
// See https://github.com/imathis/octopress/issues/424
$(document).ready(function(){
    $('body').addClass('collapse-sidebar');
});
</script>

(The body of this post makes heavy use of client-side rendering,
because of author idiosyncrasies.  You may need to wait a moment while
the supporting Javascript loads.)

<script src="/javascripts/viz.js" charset="utf-8"></script>
<script src="/javascripts/js_to_dot.js" charset="utf-8"></script>
<script src="/javascripts/gc_rendering.js" charset="utf-8"></script>

## Simplifying Assumptions
[simplifying-assumptions]: #Simplifying.Assumptions

Let us make some assumptions, so that we can focus on why
this problem is still hard even *after* being somewhat simplified.

As previously discussed in [the proloque][part0], one can think of the
main program and the GC as *coroutines*. We will continue with that
mind set.

Let us assume (for now) that the main program will not be running
concurrently with the GC;{% sidenote 'no-concurrent-mutation' 'This is certainly a significant assumption; in practice, to enforce this we would probably need to employ some sort of rendezvous protocol with gc-safepoints on every thread that might hold roots for a given GC Heap. (Of course, one might also satisfy this by directly adopting the coroutine model and also disallowing sharing of references into any one GC heap across multiple threads.)' %} or more specifically, the main program
thread(s) will not read or write any GC roots nor GC Heap-allocated
objects concurrently with the GC thread.

In addition, let us assume that in the context of the GC coroutine, no
mutator roots are live values in CPU registers;{% sidenote 'no-register-roots' 'This assumption is mostly a convenience for the text below, so that instead of saying "in live callee-save registers or on the stack", I can just say "on the stack." Also, in practice, I do not know if many GCs actually handle this in a more "clever" fashion than just forcing such values to live on the stack during a root scan (though certainly *some* do support roots in registers).' %}
 in other words, all mutator register values that the GC might care about
will have been saved somewhere in a mutator stack frame
(and will be reloaded from those saved stack slots
before subsequent use by the mutator).

Again, these assumptions are *not* meant to be interpreted as specific
requirements of Rust's final solution for GC; instead, they describe a
simplified version of "the GC integration problem" that
I claim is *still* hard to solve for Rust in general.

Throughout most of this post, I will be discussing various data
structures to support GC activity. When providing concrete examples of
the runtime state, the goal will usually be to represent something analogous
to the following
fragment of an object graph (or some small variant thereof).

<p id="running_example_graph"></p>

<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var o = object_record("O: StructZ", "<f0> field z (root)");
o.id = "O";
var x = object_record("X1", "...");
var y = object_record("Y", "...");
var x2 = object_record("X2", "...");
x.style = "rounded";
y.style = "rounded";
x2.style = "rounded";

var local_x = { id: "local_x", label: "local x: Gc&lt;X&gt; (root)", shape: "record" };
var local_y = object_record("StructY", "<f0> field y (root)");
var local_o = { id: "local_o", label: "local o = Box(O)", shape: "record" };

stack[1] = local_x;
stack[2] = local_y;
stack[3] = local_o;
local_x.ref = edge_from_to_ports(":e", ":id:nw", x);
local_y.ref = edge_from_to_ports(":f0", ":id", y);
local_o.box = edge_to_port(":id", o);

o.f0 = edge_from_to_ports(":f0", ":id:w", x);
rust_heap[0] = o;

gc_heap[2] = x;
gc_heap[3] = y;
gc_heap[4] = x2;

var objects = [stack, gc_heap, rust_heap];

stack.rank = "same";

post_objects("running_example_graph", objects, { rankdir:"LR", nodesep:0.2, no_dims: true, with_code: false });
</script>

Instances of structured data are
shown with a label{% sidenote 'labels-for-presentation' 'These labels are often a presentation artifact: they do not necessarily denote a header word in the memory itself.' %}
(`StructY`, `O: StructZ`, `X`, `Y`) that identifies the data and usually includes its type.

Often I will omit the type of a local variable or member (such as with
`o`, `y`, and `z` above). If I want to specify the type, I will do so via
type-ascription
syntax (e.g. `x: Gc<X>` above), and if I want to specify the particular
value given to a variable, I will use assignment syntax (e.g. `o = Box(O)` above).
(Note that the assigned value should always be redundant, since there
should also be an arrow linking to the assigned value in these diagrams.)

Values of type `Gc<T>` hold references to objects allocated on the GC Heap.
Every object on the GC Heap will have a label that is derived from the
type `T` and, if necessary, a numeric suffix to disambiguate between
multiple instances of `T` on the GC Heap.

I have denoted the *contents* of the objects on the GC Heap by
ellipses, because I am focusing in this post
solely on problems related to finding the roots;
the contents of the objects referenced by the roots, and the remaining 
transitively reachable objects beyond them, are not important to us today.

Objects allocated on the Rust Heap will tend to be boxes owned by
values of type `Box<T>`; they will have an identifying label and the
type of the contents (e.g. `O: StructZ` above).

I will tend to present examples of structured data with trivial
structs that have one field; e.g. `StructY` has a single field `y`,
and likewise `StructZ` has just the field `z`. (Of course in real
programs there will be structs and arrays with multiple members, but single field
structs simplifies the diagrams here.)

## Rust complicates Root Identification
[rust-complicates-root-id]: #Rust.complicates.Root.Identification

At some point, probably in response to a memory allocation request,
the GC is going to initiate a collection.

That requires traversing the root set of the main program, since those
roots will be the kernel that the GC uses to identify the reachable
objects.

[part0]: http://blog.pnkfx.org/blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#how-gc-works

What are the options for traversing the root-set?

### Do we need to solve this problem?

One approach is to "define away the problem"; one version of this I
[previously described][part1-black-box-interop] is to
hide the root-set itself inside the black-box abstraction
that we are interoperating with, and expose only handles that
point to the roots.

[part1-black-box-interop]: #Interoperation.with.a..black.box..GC

<p id="target_anchor_black_box_gc_1" class="fullwidth"></p>

The key principle in this picture is that the GC is meant to be
completely isolated from the state of the main program; when it does a
collection, the GC just starts from the root-set hidden within the
`handles` in the GC-heap. It does not inspect any state in the boxes
labelled "Stack" nor "Rust Heap."

But a big problem with this, that I failed to stress in my earlier
post, is that you now need to *manage* the hidden-root set stored in
the `handles` array.

In particular, in the above picture, every entry in `handles` maps to
exactly one `Handle` value on the "Stack" or "Rust Heap." This leads
to some troubling questions.

 * What happens when you clone the box referenced by the local variable `o`: does that need to create a new entry in the hidden `handles` array?

 * How about if you instead dropped `o` -- does that clear the `handles` entry at index 2?

   * If not, when/how will the root set be updated appropriately?

   * If so, are previously cleared entries reused? If so, how do you determine whether an entry is available for reuse -- do you keep a mark-bit on each handle?

 * This handle array maintenance sounds expensive, maybe
   we should instead periodically scan the stack to look for pointers to handles ...

{% marginblock %}
Just to be clear: the joke here is that we are basically
suggesting layering our own semi-automated memory management system
on top of a third-party automated memory management system. We should be striving to
*reduce* our problems to smaller subproblems, not *reproducing* them.
{% endmarginblock %}
... maybe we should rethink our overall approach here.

<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };
var handles = object_record("handles", "<h2> Y | <h1> X | <h3> X");

var c = object_record("C", "<f0> Gc(X) | <f1> Box(O)");
c.style = "rounded";
var o = object_record("O: StructZ", "<f0> field z = Handle(2)");
o.id = "O";
var x = object_record("X", "...");
var y = object_record("Y", "...");
x.style = "rounded";
y.style = "rounded";
var local_x = { id: "local_x", label: "local x = Handle(1)", shape: "record" };
var local_y = { id: "local_y", label: "StructY | <f0> field y = Handle(0)", shape: "record" };
var local_o = { id: "local_o", label: "local o = Box(O)", shape: "record" };

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
local_y.handle = edge_from_to_ports(":f0", ":h2", handles);
local_o.box = edge_to_port(":id", o);
gc_heap[2] = x;
gc_heap[3] = y;

var objects = [stack, gc_heap, rust_heap];
post_objects("target_anchor_black_box_gc_1", objects, { rankdir:"LR", nodesep:0.2, no_dims: true });
</script>

### Scanning the Mutator State

So let's assume we are *not* dealing with a complete black box;
instead, the main program (aka "the mutator") and the GC are going
to collaborate in some more fundamental way.

In other words, let's assume that roots are allowed to leak outside of
the GC Heap and into the mutator; no more black-box.

Once we have roots floating around under the control of the mutator,
we need to talk about identifying those roots by inspecting/querying the mutator
state.

Some relevant issues to consider on this topic:

 * Are all roots *precisely* identified as roots?

 * Where can the roots reside in the mutator? (Frames on the stack? Boxes on the Rust Heap?)

 * How is the GC informed about the location of the roots in the mutator?

 * How does the mutator itself access the roots?

 * What information might the mutator maintain on the behalf of the GC?

 * Might a given root's value (i.e. the address of the referenced
   object on the GC Heap) be changed by the GC during the collection
   (in other words, does the GC rule out [pinning][pinning])?

[pinning]: http://blog.pnkfx.org/blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/#pinning-support

Let's explore some of the problems associated with these
questions, especially how it relates to Rust.

## Are roots precisely identified?

The roots are somewhere in mutator-managed memory.
The GC will need to know the values held in those roots,
and possibly will need to update those values if the referenced
objects are moved in memory.

There are two basic options for root scanning: conservative or precise.

A *conservative* scan is needed when some of the values
might hold an actual root, but might also hold a false-positive.

This arises when, for example, there is not enough type information
available{% sidenote 'avail-types' '"Not available" can mean that that the information is absent; but it can also mean that it is *untrusted*. I discuss this further below.' %}
to know that a scanned word is meant to be interpreted by the mutator as an object
reference.

If there are any conservatively-scanned roots, the GC needs to
validate their values (e.g. by checking if it lies within one of the
ranges of addresses used for the objects allocated on the GC Heap),
and trace any object potentially referenced by such values.

An earlier discussion on "[pinning][]" established that any object
referenced by a conservatively scanned root
cannot be moved by the GC.
Therefore, integrating with a GC that does not support object pinning
(such as a fully-copying collector)
will require we scan the roots precisely, not conservatively.

{% marginblock %}
One problem with ensuring that a word on the stack is precisely identified
is that it requires close cooperation with the compiler backend.
E.g. if the backend (LLVM in the case of `rustc`) is permitted to reuse a stack
slot for two values of different types (and disjoint extents) then
we need to take care that the GC knows whether the current value in that slot
is or is not a GC reference.
(LLVM is a very expressive backend, so it provides mechanisms to account for this scenario, but it is not automatic.)
{% endmarginblock %}
A given word in memory can be *precisely* scanned
if we ensure that the root's location in memory is
unambiguously interpreted by the mutator as an object reference.
I will say that such a root can be
"unambiguously classified" only if such assurance is established.

Often the ability to classify a root unambiguously is derived from
static types, runtime type information, and global system invariants.

Where the roots might reside influences the design space for
unambiguous classification quite a bit.
For example, if all roots are members of heap-allocated objects, then
the allocator might embed a type-tag in the header of such an object,
or segregate objects into disjoint regions of memory based on that
type.

Therefore, we will explore the question of where roots reside next.

## Where are the roots?

There are two components to the question "where are the roots?":

  * Where can roots *possibly* reside?

  * Where do the roots *actually* reside?

The first bullet is a question about the system as a whole; it is a
question that we must answer as designers.

The second bullet is about encoding how the GC will look up the memory
addresses of the roots (potentially with the mutator's help) every
time it wants to initiate a root scan.

The two parts interact with each other, so we will address them both
somewhat simultaneously.

{% marginblock %}
This list is leaving out some other options,
such as *completely unconstrained*, where roots might live in memory
unknown to the both the GC and Rust runtime (I do not see a way this first option could work without
requiring programmers to instrument foreign code with calls to root registration
and deregistration functions),
or keeping the roots solely on a [shadow stack][henderson]
with structure isomorphic to the actual stack, but not vulnerable
to disruption due to compiler code transformations (I am omitting
this second option since it is known to carry a significant performance penalty).
{% endmarginblock %}

Consider these options for locations where roots can reside:

  1. *Constrained To Boxed Values*:
     Solely found embedded in boxed values on the Rust Heap.

  2. *Constrained To Stack*:
     Stored solely on the program stack, and

  3. *Rust-Managed But Otherwise Unconstrained*:
     Stored either on the stack or embedded in boxed values on Rust Heap.

### Roots Constrained To Boxed Values (Option 1)

If roots are *solely* stored in members of boxed values, then we might
store runtime-type metadata in an allocator-injected header.

This option is seductive: Adding a header via the runtime system's
`#[allocator]` crate could sidestep a significant amount of compiler
integration (maybe even all of it).

There are some interesting ideas to explore from that starting point,
such as collecting all such boxed values together in a linked list
whose head is known to the GC (and thus the answer to "how does the
GC scan the roots?" is just "it walks the list"{% sidenote 'list-maintenance' 'Do not be fooled; it would not be that easy. In particular, properly maintaining such a list could complicate the interface between the mutator and the values holding the roots.' %}).

However,
constraining roots to solely live in members of boxed
values may not be feasible in Rust as it stands today.
For example, one is always free to move the instance of `T` out of a
`Box<T>`, deallocating the backing storage but moving the `T` into
another location, such as a stack-allocated local variable.

Let's look at the remaining two
approaches.

### Roots Constrained To Stack (Option 2)

If roots can be stored directly on the stack (i.e. options 2 or 3 above),
then when the GC initiates a root scan, it will need to find those
roots.

This search of the stack can be guided by
"stack maps":{% sidenote 'stack maps' 'For details, see [Compiler Support for Garbage Collection in a Statically Typed Language][diwan-moss-hudson], Diwan Moss and Hudson (1992).' %}
compiler-generated metadata providing a mapping from a
code address{% sidenote 'code-address' 'This mapping need not have an entry for *every* address from the program instruction stream; we can make do with just the addresses of *call-sites* into runtime routines that could cause a GC.' %}
to the set of stack slots{% sidenote 'stack-map-slot' 'More specifically, the offset in a stack frame of a slot, and any relevant type information needed by the GC the compiler opted to include.' %}
that hold values of interest.

However, restricting the roots to live solely on the stack may be
problematic for much the same reason that plagues the earlier idea of
restricting roots to boxed values: in Rust today, one is always free
to move instances of `T` from a stack-local slot into a member of a
boxed value.

In some circumstances, we might be able to counteract these
"freedom of movement" issues in a backwards-compatible manner with a compiler
plugin (lint) that analyzes the source and trys to flag any code might move a root
into an illegal location. ([Servo][] already uses [a lint like this][servo-gc-lint]
for its integration with the Spidermonkey GC.)

[Servo]: https://servo.org/

[servo-gc-lint]: https://blog.mozilla.org/research/2014/08/26/javascript-servos-only-garbage-collector/#custom-static-analysis

Or, if we are willing to extend the language itself,
we might add marker trait `Immobile` that indicates
that values of such type *cannot* be
moved.{% sidenote 'hunh-moved' 'Proper integration of `trait Immobile` would probably require a way type for type parameters to opt-out of the capability to be moved, e.g. via a `T: ?Moved` anti-bound, analogous to the `?Sized` anti-bound.<br></br>Yes, I just made up the term "anti-bound."' %}

But either of those options are just ways of enforcing a restriction,
and it will outlaw certain kinds of program composition.{% sidenote 'vec-composition' 'An easy example of this: If you want to be able to put a root as part of the element type in a `Vec<T>`, then that `T` has to be able to be moved (since expanding the capacity of a vec will require moving its contents from one backing buffer to another).' %}

In practice, we simply may be better off lifting such restrictions
entirely. So, let us now consider our remaining option:
allowing roots to be embedded in values on the stack or boxed on the Rust Heap.

## Roots are Rust-Managed, But Otherwise Unconstrained (Option 3)

Now we come to what is probably the most realistic option for Rust/GC integration:
allowing roots to reside anywhere that the Rust compiler or runtime knows about.

Arguably, I might well have *started* this discussion with this
approach, since it is by definition the most general of the three, and
thus if we *do* have a solution for it, then why not jump to it?

The reason I left it for last is that I suspect any design we adopt
for GC integration in Rust 1.x is going to require a hybrid of the
approaches described in the prior two sections (allocator-injected
metadata *and* stack maps), and therefore I wanted to outline them in
isolation, before I started mixing them together.

## GC: "Where are the roots?", Mutator: "..."

If we assume that roots can be embedded in values either on the stack
or in boxes on the Rust Heap, then how will the GC find the roots when
it needs to scan them?

The support for the GC's root scanning capability can be seen as having three parts:

 1. What work does the GC itself do, on the fly, to determine the roots
    when it needs them,

 2. What work does the mutator do (if any) as it executes the
    program{% sidenote 'mutator-work' '"Mutator work" here includes code hidden in library functions the mutator calls, such as `#[allocator]` subroutines, or code automatically injected by the compiler, such read- or write-barriers.' %}
    to support a potential root scan by the GC in a future, and,

 3. What meta-data must be gathered and emitted by the compiler
    to support root-scanning?

One idea for enabling easy GC root traversal was mentioned earlier:
why not collect the roots together in a linked list structure?
Towards this goal, we might
consider maintaining an intrusive
 links forming a list of all roots.
 
<p id="intrusive_list_of_roots"></p>

<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var o = object_record("O: StructZ", "<f0> field z (root) | <next> next_root = null");
o.id = "O";
var x = object_record("X", "...");
var y = object_record("Y", "...");
x.style = "rounded";
y.style = "rounded";

var local_x = object_record("local_x", "<f0> (root) | <next> next_root");
var local_y = object_record("StructY", "<f0> field y (root) | <next> next_root");
var local_o = { id: "local_o", label: "local o = Box(O)", shape: "record" };

stack[1] = local_x;
stack[2] = local_y;
stack[3] = local_o;

local_x.ref = edge_from_to_ports(":f0", ":id", x);
local_y.ref = edge_from_to_ports(":f0", ":id", y);
local_o.box = edge_to_port(":id", o);

o.f0 = edge_from_to_ports(":f0", ":id:sw", x);
rust_heap[0] = o;

gc_heap[2] = x;
gc_heap[3] = y;

root_head = object_record("roots", "<next> next_root");
var anonymous_xy = {id: "anonymous_xy", style:"invis", shape: "point"};

// gc_heap[0] = root_head;

root_head.next = edge_from_to_ports(":next", ":f0", local_x);
local_x.next = edge_from_to_ports(":next", ":f0", anonymous_xy);
local_x.next.arrowhead = "none";

anonymous_xy.ref = edge_to_port(":f0:w", local_y);
local_y.next = edge_from_to_ports(":next", ":f0", o);

// anonymous_yx.ref.constraint = "false";
// anonymous_yx.ref.arrowhead = "none";

stack.rank = "same";

// root_head.presentation = invisible_edge(anonymous_yx);
// local_y.presentation = invisible_edge(local_x);
// local_x.presentation = invisible_edge(anonymous_xy);
// stack[4] = anonymous_yx;

var objects = [stack, gc_heap, rust_heap, root_head];
post_objects("intrusive_list_of_roots", objects, { rankdir:"LR", nodesep:0.2, no_dims: true, with_code: false });
</script>

This is an *intrusive* list because the pointers in the list are
pointing into the interiors of objects. This allows traversing the
list to be completely uniform (from the viewpoint of the GC,
it looks like nothing more than a linked list of pairs).
In this scenario, the GC does essentially *zero* work on-the-fly
to find the locations of the roots;
maintaining the list would become the reponsibility of the mutator as
it creates and moves values with embedded roots.

However, Rust today does not have good support for intrusive data structures ([RFC Issue 417][]).
The already-mentioned capability to move values freely, as well as the
capability to swap a pre-existing `T` with the value held beind a
`&mut T` reference, are among the reasons that intrusive structures
are difficult today, since it changes the addresses associated
with objects, and thus requires updating of the interior links.

[RFC Issue 417]: https://github.com/rust-lang/rfcs/issues/417

So, what other options do we have?

Having the GC traverse the memory of the call-stack, even with the assistance of a stack map
to provide precise type information, will not give us the locations of all the roots,
since some are located on the Rust Heap. A stack map cannot hold the addresses of the blocks
of memory dynamically allocated for a box on the heap.

However, the stack map *can* hold the type information for the local
variables, and that sounds promising: If we record that a local
variable `o` has type `Box<Struct>`, then treat the contents of the
box on the heap as owned by the stack, so that when we encounter `o`
during the stack scan, we can recursively scan the memory of the box,
using the type `Struct` to inform the scan about how to treat each of
the embedded members.

{% marginblock %}
I have slightly modified the running example to show two instances
of the local `x` on the call-stack in separate frames, each corresponding
to a distinct (recursive) invocation of the function `fn f`.
<br></br>
This is just to bring home the point that the stack map info encodes
static information about the frame for a function (at a particular call-site),
and thus recursive invocations of the same function can potentially
reuse the same entry in the stack map.
{% endmarginblock %}

<p id="stack_map_boxes"></p>

<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var frame0 = { id: "cluster_frame0", label: "frame0: fn f()", is_subgraph: true };
var frame1 = { id: "cluster_frame1", label: "frame1: fn g()", is_subgraph: true };
var frame2 = { id: "cluster_frame2", label: "frame2: fn h()", is_subgraph: true };
var frame3 = { id: "cluster_frame3", label: "frame3: fn f()", is_subgraph: true };

var o = object_record("O: StructZ", "<f0> field z (root) = Gc(X1)");
o.id = "O";
var x  = object_record("X1", "...");
var x2 = object_record("X2", "...");
var y = object_record("Y", "...");
x.style = "rounded";
x2.style = "rounded";
y.style = "rounded";

var local_x = { id: "local_x1", label: "local x (root) = Gc(X1)", shape: "record" };
var local_x2 = { id: "local_x2", label: "local x (root) = Gc(X2)", shape: "record" };

var local_y = object_record("StructY", "<f0> field y (root) = Gc(Y)");
var local_o = { id: "local_o", label: "local o = Box(O)", shape: "record" };

frame0[0] = local_x;
frame1[0] = local_y;
frame2[0] = local_o;
frame3[0] = local_x2;

// frame3[0].ref = { is_edge: true, target: frame2[0], ltail: frame3.id, lhead: frame2.id, constraint: false };
// frame2[0].ref = { is_edge: true, target: frame1[0], ltail: frame2.id, lhead: frame1.id, constraint: false };
// frame1[0].ref = { is_edge: true, target: frame0[0], ltail: frame1.id, lhead: frame0.id, constraint: false };

stack[0] = frame0;
stack[1] = frame1;
stack[2] = frame2;
stack[3] = frame3;

local_x.ref = edge_to_port(":id:nw", x);
local_x2.ref = edge_to_port(":id:sw", x2);
local_y.ref = edge_from_to_ports(":f0", ":id", y);
local_o.box = edge_to_port(":id", o);

o.f0 = edge_from_to_ports(":f0", ":id:w", x);
rust_heap[0] = o;

gc_heap[2] = x;
gc_heap[3] = y;
gc_heap[4] = x2;

var objects = [stack, gc_heap, rust_heap];

stack.rank = "same";

post_objects("stack_map_boxes", objects, { rankdir:"LR", nodesep:0.2, compound: true, no_dims: true, with_code: false });
</script>

The principle is that when control shifts to the GC coroutine,
it walks through the stack backtrace, consulting the stack
map for each callsite.

```
stack_map_info for callsite 0x0010_ABBA in fn f:
  local x:
    offset: [...]
    type: Gc<X>

stack_map_info for callsite 0x0020_BACA in fn g:
  local _:
    offset: [...]
    type: StructY

stack_map_info for callsite 0x0030_C0C0 in fn h:
  local o:
    offset: [...]
    type: Box<StructZ>
```

From the stack map, it finds the offsets of relevant local variables
within that stack frame, and the type information for those locals, so
that it knows when it needs to dereference an pointer to inspect a
block on the Rust Heap (such as the `Box(O)` in our running example).

The GC will need separate meta-data describing the layout of each
type, with the offset and type of each field of interest:

```
type_map_info for type StructY:
  field y:
    offset: [...]
    type: Gc<Y>

type_map_info for type Box<StructZ>:
  field 0:
    offset: [...]
    type: StructZ

type_map_info for type StructZ:
  field z:
    offset: [...]
    type: Gc<X>
```

The boxed objects may themselves own other root-holding objects on the Rust Heap,
like so:

<p id="nested_stack_map_boxes" class="fullwidth"></p>

<script>
var stack = { id: "cluster_stack", label: "Stack", is_subgraph: true };
var rust_heap = { rankdir:"LR", id: "cluster_rust_heap", label: "Rust Heap", is_subgraph: true };
var gc_heap = { id: "cluster_gc_heap", label: "GC Heap", is_subgraph: true, style: "rounded" };

var frame0 = { id: "cluster_frame0", label: "frame0: fn f()", is_subgraph: true };
var frame1 = { id: "cluster_frame1", label: "frame1: fn g()", is_subgraph: true };
var frame2 = { id: "cluster_frame2", label: "frame2: fn h()", is_subgraph: true };
var frame3 = { id: "cluster_frame3", label: "frame3: fn f()", is_subgraph: true };

var o = object_record("O: StructB", "<f0> field b (root) = Box(Z)");
o.id = "O";
var b = object_record("Z: StructZ", "<f0> field z (root) = Gc(X1)");
b.id = "B";
var x  = object_record("X1", "...");
var x2 = object_record("X2", "...");
var y = object_record("Y", "...");
x.style = "rounded";
x2.style = "rounded";
y.style = "rounded";

var local_x = { id: "local_x1", label: "local x (root) = Gc(X1)", shape: "record" };
var local_x2 = { id: "local_x2", label: "local x (root) = Gc(X2)", shape: "record" };

var local_y = object_record("StructY", "<f0> field y (root) = Gc(Y)");
var local_o = { id: "local_o", label: "local o = Box(O)", shape: "record" };

frame0[0] = local_x;
frame1[0] = local_y;
frame2[0] = local_o;
frame3[0] = local_x2;

// frame3[0].ref = { is_edge: true, target: frame2[0], ltail: frame3.id, lhead: frame2.id, constraint: false };
// frame2[0].ref = { is_edge: true, target: frame1[0], ltail: frame2.id, lhead: frame1.id, constraint: false };
// frame1[0].ref = { is_edge: true, target: frame0[0], ltail: frame1.id, lhead: frame0.id, constraint: false };

stack[0] = frame0;
stack[1] = frame1;
stack[2] = frame2;
stack[3] = frame3;

local_x.ref = edge_to_port(":id:w", x);
local_x2.ref = edge_to_port(":id:sw", x2);
local_y.ref = edge_from_to_ports(":f0", ":id", y);
local_o.box = edge_to_port(":id", o);

o.f0 = edge_from_to_ports(":f0", ":id:w", b);
b.f0 = edge_from_to_ports(":f0", ":id:nw", x);
rust_heap[0] = o;
rust_heap[1] = b;

gc_heap[2] = x;
gc_heap[3] = y;
gc_heap[4] = x2;

var objects = [stack, gc_heap, rust_heap];

stack.rank = "same";

post_objects("nested_stack_map_boxes", objects, { rankdir:"LR", nodesep:0.2, compound: true, no_dims: true, with_code: false });
</script>

To find all the roots starting from the stack
in the presence of such ownership chains
(which may go through other types like `Vec`),
the GC will need to recursively traverse the boxes,
or otherwise enqueue them onto a worklist structure.
In principle, if we can prove that certain types never
transitively own roots, then the GC should be able to skip traversing
boxed data for such types.

Using the stack map and type map data to find all roots transitively
owned by the stack is a promising approach. What is the catch, if any?

## Unsafe Pointers

{% marginblock %}
The `from_raw` method that converts a `*mut T` to `Box<T>`
is unsafe, but `into_raw` is a safe method. Safe code
can always convert a `Box<T>` to a `*mut T`, and clients
expect it to also be reasonable to round-trip via `from_raw`.
{% endmarginblock %}
What should we do about unsafe pointers `*mut T` and `*const T`.
For example, it is not uncommon for library code to convert
boxed data `Box<T>` to a `*mut T` or vice versa;
that is an ownership transfer.

I used `local o = Box(O)` above (where `o: Box<StructB>`),
but it is entirely possible that `o` has type `*mut StructB`.

Here are some options for how to handle unsafe pointers:

 * Skip unsafe pointers during root scanning.

   This seems almost certain to cause unsound behavior; as noted above,
   transmuting `Box<T>` to `*mut T` is an ownership transfer, and if
   `T` holds a root, then later code might access it. This means that
   the roots owned by `T` need to be scanned, to keep their associated
   objects on the GC Heap alive.

 * Punt the question: if a program uses GC, any use of unsafe pointers
   (as local variables or as members of structures) needs some sort of
   attribute or annotation that tells the GC how to interpret the value
   held in the unsafe pointer.

   This would be quite difficult to put into practice. [Part 1][part1]
   included a ["Modularity" goal][part1-modularity]:

   > A Rust program that uses GC should be able to link to a crate
   > whose source code was authored without knowledge of GC.

   Requiring annotations on
   every use of unsafe pointers means sacrificing this goal.

 * Treat unsafe pointers as potential root owners: Traverse them
   and use their type as the basis for the scan.

   This seems like the most reasonable option. But, can the types
   of unsafe pointers be trusted?

[part1-modularity]: /blog/2015/11/10/gc-and-rust-part-1-specing-the-problem/#modularity

## Is the meta-data trustworthy?

We assumed the existence of stack and type maps.
But where do they come from?

These maps need to be generated by the `rustc` compiler; after all,
they rely on low-level details of the generated code, such as the
offsets of fields within types, the offsets of local variables in a
stack frame, or the addresses of function call-sites.

The `rustc` compiler, in turn, is going to generate the information
based on the source code text. So far, so good.

Here's the rub: we assumed that the stack map will tell us the types
we need for all local variables of interest for all frames on the call
stack.

But in practice, a value can be *cast* to a different type.

In particular, in today's Rust 1.x, it is considered *safe* to cast
between `*mut T` and `*mut U` for any `T` and `U`:

```rust
fn main() {
    let b = Box::new("peanut butter"); // (imagine if this held rooted data)
    let mut p = Box::into_raw(b);
    let pb = p as *mut String; // bogus type, but safe
    p = Box::into_raw(Box::new("jelly"));
    // this is where a potential GC would be worrisome
    println!("p: {:?} p2: {:?}", p, pb);

    // (just demonstrating recovery of original value via unsafe code)
    let pb = pb as *mut &'static str;
    let recover = unsafe { Box::from_raw(pb) };
    println!("recovered: {:?}", recover);
}
```

This is a real problem, in terms of the goals we have set up for ourselves.
100% modular GC requires that we be able to link with code that does things
like the above with the owners of its roots, and that includes when the roots
are indirectly held in boxes on the Rust Heap.

We may be able to add constraints on the `Gc<T>` type to prevent such things
from occurring when the types are apparent (e.g. when one is working with a
local variable of type `Gc<T>`). But in general, the types will
not be apparent to the code performing the cast; in particular,
we would still need to address type-parametric code that performs
such casts of unsafe pointers.

## Solutions

What can we do about these problems?

One obvious response to the untrustworthy meta-data problem would be to change the language and make casts from `*T` to `*U`
*unsafe*.{% sidenote 'unsafe-casts' 'Indeed, we may make such casts unsafe anyway; nmatsakis has said during informal conversation that he is not sure why we classified such casts as safe in the first place.' %}
 This would deal with the problem at a surface level, in the sense that
we would be able to at least allow a program using GC to link to a
GC-oblivious crate if the latter crate did not use any `unsafe`
blocks.
But it would not be terribly satisfactory; we want Rust's
solution for GC to be able to link to as many crates as possible, and
ruling out all code that does any cast of an unsafe pointer seems quite limiting.

We could also revise the set of goals, e.g. scale back our ambitions
with respect to compositionality, and return to ideas like having the
roots constrained to stack, as [discussed above](#Roots.Constrained.To.Stack..Option.2.).

An alternative solution I have been considering is to try to adopt a
hybrid approach for root scanning: use stack maps for the local
variables, but also have the allocator inject tracing meta-data onto
the objects allocated on the Rust Heap, and do a kind of conservative
scanning, but *solely* for finding roots embeded in objects on the
Rust Heap. This way, unsafe casts might become irrelevant: when
encountering *any* native pointer (e.g. `*mut u8`), we would ignore
the referenced type and instead look up whether it is an object on the
Rust Heap, and if so, extract the allocator-injected tracing
information.

I plan to dive more deeply into discussing solutions in a follow-up post.
This post is already quite long, but more importantly, I want to get some
concrete data first on the overheads imposed by the metadata injected during
allocation in state of the art conservative GC's like [BDW][].

----

Oh, and finally (but completely unrelated): Happy 2016 to all the hackers out there!
Hope that you copied over all your live objects from 2015!

<script>
// ## References
//
// * [On LLVM’s GC Infrastructure][on-llvms-gc]
//
// * [Compiler Support for Garbage Collection in a Statically Typed Language][diwan-moss-hudson]
</script>

[BDW]: http://www.hboehm.info/gc/

[on-llvms-gc]: https://eschew.wordpress.com/2013/10/28/on-llvms-gc-infrastructure/

[diwan-moss-hudson]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.1641

[henderson]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.5570