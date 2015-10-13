---
layout: post
title: "Vis-a-vis, part 1: Visualizing Graphs via viz.js"
date: 2015-10-12 14:10
comments: true
categories:
published: false
---

This is a post discussing a couple different ways to draw pictures,
mostly those of graphs (nodes and edges, i.e. circles or boxes, and
lines between them).

The technology covered by this post is [viz.js][], a Javascript
library for rendering graphs specified via the [DOT] language.

<!-- more -->

Its meant both as a reference for me to use when I want to remind
myself of my options here, and as a demo of some interesting web
technologies.

I hope in later posts to cover SVG ("Scalable Vector Graphics")
itself, and perhaps higher level Javascript libraries such as [D3][].

I will focus in these posts on client-side rendering technologies.
Another entirely reasonable option is to render the content to an
image (or SVG, or a JS script that renders into a canvas, et cetera).
However, one of my goals with my documents is to embed all of the
source text into the markdown content; a separate rendering tool would
require some sort of pre-process step, and I am loathe to try to
incoporate that into the `Rakefile` that Octopress uses.


[viz.js]: https://github.com/mdaines/viz.js/
[D3]: http://d3js.org/

## viz.js
The first item we will cover, since it amongst the simplest to adopt,
is `vis.js`, a javascript library that provides
[graphviz-style][graphviz] rendering in the browser client.

[graphviz]: http://www.graphviz.org/
[DOT]: http://www.graphviz.org/content/dot-language

([Graphviz][graphviz] is a tool for automatically laying out and
rendering graphs; it is coupled with [DOT][], a simple domain-specific
language for describing graph structures by defining the nodes, edges,
and various attributes attached to them, like labels.)

### Hooking up the JS source

Since this is using (heavy duty) javascript, you are not likely to
want to put the supporting source code inline in your web page.
Instead, you will need to load it up, either:

 1. from the original [source site](src="https://github.com/mdaines/viz.js/releases/download/0.0.3/viz.js"), or

 2. in a server-side local copy of the file that you deploy alongside
    your content
    (e.g. put it into the `_source/javascripts/` directory,
    if you are using Octopress for a blog like this one).

I recommend the latter route, since the linked github repository is
outside of your control, and if it dissapears, you lose the rendering
and your page is broken.

After you have selected your source for the code, you need to
make your page load it up, via a `script` tag invocation,
like: `<script src="/javascripts/viz.js" charset="utf-8"></script>;`

This script can take a while to load. You may want to insert a warning,
like this one, before the `script` invocation.

<script src="/javascripts/viz.js" charset="utf-8"></script>

### Injecting generated SVG into the document

Once the script is loaded, you can start using inline Javascript to
render graphviz-style descriptions of directed graphs to SVG within
the page.

The smallest example of this given in the `viz.js` documentation
does this by dynamically adding to the `innerHTML` property:

```html
  <script>
    document.body.innerHTML += "<p>Sample addition.</p>";
    document.body.innerHTML += Viz("digraph { a -> b; }", "svg");
  </script>
```

The above "works" (even in the context of an Octopress blog post), but
it is a rather brute-force approach. Plus, the resulting composition
of the blog text with the graphics added at the end is not likely to
please you nor your audience.

We can do better: Add a content-less `div` tag (or `p` tag, et
cetera), and then search for that element in a `script` block that
will add our picture to the inner HTML for that tag.

Here is a concrete illustration of the idea (that does not use
Graphviz or `viz.js`):


```html
<div id="target_anchor0"></div>
<script>
    var elem = document.getElementById("target_anchor0");
    elem.innerHTML = "This text was injected.";
</script>
```

<div id="target_anchor0"></div>
<script>
    var elem = document.getElementById("target_anchor0");
    elem.innerHTML = "This text was injected.";
</script>

So, now that we know how to insert HTML into our document (even in the
context of the markdown source for a blog post), let us dive into how
to combine that with Graphviz, via `viz.js`.

### `dot_source` holds DOT source

```html
<div id="target_anchor1"></div>

<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c; }';
    var elem = document.getElementById("target_anchor1");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

The contents of `dot_source` make up a DOT program:

  * `digraph` means we are making a directed graph,

  * `rankdir="LR"` means we want the nodes to prefer horizontal left-to-right layout (the default is to have them vertically stacked top-to-bottom),

  * `bgcolor="transparent"` means we want a transparent background (the default is white, which is fine but a little offputting for this site),

  * `a -> b` means "I want an edge coming out of `a` and into `b`"; you can chain them together as shown in `a -> b -> c`.

Here is how that ends up rendering:

<div id="target_anchor1"></div>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c; }';
    var elem = document.getElementById("target_anchor1");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

It turns out that the generated SVG is actually pretty readable.  That
is, I am able to use "Inspect Element" in my web browser, and I see
there that the SVG element is actually made up of many `g` elements
that have each had their `class` attribute set according to their
role: the whole graph has `class="graph"`, and then each node and edge
is assigned `"node1"`, `"node2"`, `"edge1"`, `"node3"`, `"edge2"`.
(There are also comments embedded in the generated SVG above each
element, so that one can map the element back to the node or edge
in the original `dot_source` text.)

That is how to use `viz.js` to embed graphs described via [DOT][].

This can be especially useful if you are making a dynamic page where
the user can inject their own graph descriptions, and you want `viz.js`
to do the heavy lifting of deciding how to lay out the nodes.

### Controlling node layout

But what if we want more control over the layout of the picture
elements?

For example, one might want more control over layout to
ensure that there is a easy-to-see correspondence between a series of
pictures, or if there is a structural symmetry that is easier to see
if the layout is also symmetrical.

Consider the following example graph:

```html
<p id="target_anchor2">Symmetric structure but asymmetric layout: </p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c -> e; b -> d -> e -> a; }';
    var elem = document.getElementById("target_anchor2");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

<p id="target_anchor2">Symmetric structure but asymmetric layout: </p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c -> e; b -> d -> e -> a; }';
    var elem = document.getElementById("target_anchor2");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

There are a number of options at this point. We could abandon Graphviz (and `viz.js`),
and do our diagrams directly in SVG. But, assuming we want to stay within the confines
of `viz.js`, there are a few things we *can* do.

First, we can specify initial node positions by adding a `pos` attribute to
each node that needs it. The default layout engine (called `dot`) just
ignores such attributes, but other engines, such as the `neato` layout,
will incorporate such information.

One can switch the layout engine via an argument to the `Viz` function, but I will instead do it
by setting an attribute in the graph itself.

```html
<p id="target_anchor3"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    // Start of the layout selection code
    dot_source += 'layout="neato"; inputscale=72;' // specifying "neato" layout to allow init position specification
    dot_source += 'overlap="false";' // if you leave this out, the engine may put nodes in overlapping spots
    dot_source += 'start=0;' // seed the RNG (to ensure consistent results)
    // End of the layout selection code
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a;';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor3");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

<p id="target_anchor3"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    // Start of the layout selection code
    dot_source += 'layout="neato"; inputscale=72;' // specifying "neato" layout to allow init position specification
    dot_source += 'overlap="false";' // if you leave this out, the engine may put nodes in overlapping spots
    dot_source += 'start=0;' // seed the RNG (to ensure consistent results)
    // End of the layout selection code
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a;';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor3");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

As you can see, we definitely have a different node layout. It is not
yet directly illustrating the mirror symmetry of the graph structure,
though. To achieve that, we could put the `a` node inside of a
diamond shape formed by { `b`, `c`, `d`, `e` }.

We can specify node positions by adding separate declarations for
each of the individual nodes, and then adding `pos` attributes
for each.

```
    dot_source += 'b [pos="0,40"]; a [pos="100,40"]; c [pos="160,80"];'
    dot_source += 'd [pos="160,0"]; e [pos="220,40"];'
```

<p id="target_anchor4"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    // Start of the layout selection code
    dot_source += 'layout="neato"; inputscale=72;' // specifying "neato" layout to allow init position specification
    dot_source += 'overlap="false";' // if you leave this out, the engine may put nodes in overlapping spots
    dot_source += 'start=0;' // seed the RNG (to ensure consistent results)
    // End of the layout selection code
    // Start of the node positioning code
    dot_source += 'b [pos="0,40"]; a [pos="100,40"]; c [pos="160,80"];'
    dot_source += 'd [pos="160,0"]; e [pos="220,40"];'
    // End of the node positioning code
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a;';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor4");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

*That* shows the mirror symmetry of the graph.

The careful reader may have noticed an oddity in the above rendering: I had specified
that `a` have an x-coordinate of 100, while the corresponding x-coordinate for `c` and `d` is 160; but
in the rendering, all three of the nodes fall on a vertical line sharing the same x-coordinate.
What happened?

The answer is that the input node positions are by default only as
*initial* values; the layout engine may still choose to adjust them in
order to "improve" the layout according to its internal heuristics.

We can override this by *pinning* the nodes in place. An individual node can be pinned by
putting an exclamation point after the coordinate in its `pos` attribute, or equivalently by
setting the nodes `pin` attribute to `true`.

```
    dot_source += 'b [pos="0,40"]; a [pos="100,40!"]; c [pos="160,80!"];'
    dot_source += 'd [pos="160,0!"]; e [pos="220,40"];'
```

<p id="target_anchor5"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    // Start of the layout selection code
    dot_source += 'layout="neato"; inputscale=72;' // specifying "neato" layout to allow init position specification
    dot_source += 'overlap="false";' // if you leave this out, the engine may put nodes in overlapping spots
    dot_source += 'start=0;' // seed the RNG (to ensure consistent results)
    // End of the layout selection code
    // Start of the node positioning code
    dot_source += 'b [pos="0,40"]; a [pos="100,40!"]; c [pos="160,80!"];'
    dot_source += 'd [pos="160,0!"]; e [pos="220,40"];'
    // End of the node positioning code
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a;';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor5");
    elem.innerHTML = Viz(dot_source,"svg");
</script>

Now we can see that `a`, `c`, and `d` are all pinned in place, and the resulting layout
is perhaps not as nice.

In particular, the `a -> b` edge does not even have enough room
for its arrow head. We could fix this by pinning the `b` node in place as well,
but an alternative is to encourage graphviz to put more space between the nodes
via the `sep` attribute.

```
    dot_source += 'sep=0.2;' // treat each node as 1.2 times larger than it is
```

<p id="target_anchor6"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    // Start of the layout selection code
    dot_source += 'layout="neato"; inputscale=72;' // specifying "neato" layout to allow init position specification
    dot_source += 'overlap="false";' // if you leave this out, the engine may put nodes in overlapping spots
    dot_source += 'sep=0.2;' // treat each node as 1.2 times larger than it is
    dot_source += 'start=0;' // seed the RNG (to ensure consistent results)
    // End of the layout selection code
    // Start of the node positioning code
    dot_source += 'b [pos="0,40"]; a [pos="100,40!"]; c [pos="160,80!"];'
    dot_source += 'd [pos="160,0!"]; e [pos="220,40"];'
    // End of the node positioning code
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a;';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor6");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

That is a pretty legible graph. Of course, the code to describe it is
quite a bit more complex than our [original code](#target_anchor2);
hopefully I will not feel the need to specify node placement too
often.

### Edge attributes

Above we saw examples of node attributes, which can be used to adjust the
node placement and rendering. (The [family of attributes][] is much larger,
and includes ways to specify shapes, color, label text, et cetera.)

[family of attributes]: http://www.graphviz.org/doc/info/attrs.html

Graphviz also offers the ability to customize attributes for each
*edge*.  This can also be useful for influencing layout.
In particular, the `len` attribute can specify a preferred edge length
(ignored by the `dot` layout), and the `weight` attribute can be
increased to encourage the edge to be shorter (or, when `len` is
specified and relevant, to encourage that edge length to more closely
approximate `len`).

```html
<p id="target_anchor7a"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += 'edge [weight=2];'
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a [weight=1];';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor7a");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

Here we have used `edge [weight=2]` to specify a default weight of 2 for all edges,
and then we override that weight for the `e -> a` edge (and just that edge).
(Note: The `dot` layout requires integral values for `weight`.)

Here is the effect of this:

<p id="target_anchor7a"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += 'edge [weight=2];'
    dot_source += 'a -> b -> c -> e; b -> d -> e -> a [weight=1];';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor7a");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

Look at that! We can see the mirror symmetry of the graph (depending on
how much we are willing to squint with regards to that `e -> a` edge),
but we did not have to do any layout hacking.

Unsurprisingly, a different weight-assignment may yield a different layout.

```
    dot_source += '   { edge [weight=10]; a -> b; e -> a; }'
    dot_source += '   { edge [weight=1]; b -> c -> e; b -> d -> e; }'
```

Here we have moved a portion of the edges into a subgraph, so that we
can override their default `weight` as a group.

<p id="target_anchor7b"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += '   { edge [weight=10]; a -> b; e -> a; }'
    dot_source += '   { edge [weight=1]; b -> c -> e; b -> d -> e; }'
    dot_source += ' }';
    var elem = document.getElementById("target_anchor7b");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

Perhaps more surprising, changing node introduction order in the
source can also affect the layout.

```
    dot_source += '   b -> a [dir="back"]; a -> e [dir="back"];'
    dot_source += '   b -> c -> e; b -> d -> e;'
```

Here, we introduce the nodes `b` then `a` then `e` to encourage them
to be laid out horizontally in that order, then apply the `dir`
attribute so that the edge between them has its direction reversed.

<p id="target_anchor7c"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += '   b -> a [dir="back"]; a -> e [dir="back"];'
    dot_source += '   b -> c -> e; b -> d -> e;'
    dot_source += ' }';
    var elem = document.getElementById("target_anchor7c");
    elem.innerHTML = Viz(dot_source, "svg");
</script>


Finally, if you want to describe a path through the graph, you can
highlight the edges of the path by overriding their `color` and
`penwidth` attribute.

```html
<p id="target_anchor8"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += 'edge [weight=2];'
    dot_source += 'a -> b -> d;';
    dot_source += '{ edge [color="red",penwidth="3.0"]; b -> c -> e -> a [weight=1] }';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor8");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

Here again we have used a subgraph to reduce the annotation burden.

<p id="target_anchor8"></p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent";'
    dot_source += 'edge [weight=2];'
    dot_source += 'a -> b -> d -> e;';
    dot_source += '{ edge [color="red",penwidth="3.0"]; b -> c -> e -> a [weight=1] }';
    dot_source += ' }';
    var elem = document.getElementById("target_anchor8");
    elem.innerHTML = Viz(dot_source, "svg");
</script>

(One can use subgraphs for a number of other tricks, such as forcing all
nodes in the subgraph to fall into the same `rank`, which can be another
useful technique for encouraging particular layouts.)
