---
layout: post
title: "Drawing pictures of mountain tops"
date: 2015-10-12 14:10
comments: true
categories:
published: false
---

This is a post discussing a couple different ways to draw pictures,
mostly those of graphs (nodes and edges, i.e. circles or boxes, and
lines between them).

Its meant both as a reference for me to use when I want to remind
myself of my options here, and as a demo of some interesting web
technologies.

The three technologies covered here are [viz.js][], [SVG][], and [d3.js][].

[viz.js]: #toc_0
[SVG]: #toc_1
[d3.js]: #toc_2

I have focused in this post on client-side rendering technologies.
Another entirely reasonable option is to render the content to an
image (or SVG, or a JS script that renders into a canvas, et cetera).
However, one of my goals with my documents is to embed all of the
source text into the markdown content; a separate rendering tool would
require some sort of pre-process step, and I am loathe to try to
incoporate that into the `Rakefile` that Octopress uses.

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
make your page load it up, via a `<script ...>` invocation,
like: `<script src="/javascripts/viz.js" charset="utf-8"></script>`

This script can take a while to load. You may want to insert a warning,
like this one, before the `<script ...>` invocation.

<script src="/javascripts/viz.js" charset="utf-8"></script>

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

The above "works", even in the context of an Octopress blog post, but
it is a rather brute-force approach, and the resulting composition of
the blog text with the graphs added at the end is not likely to
please.

Instead, let us see if we can dynamically-extend more specific content within
the post. We can inject a content-less `div` tag, and then search for that
element in a `<script>` block that will add our picture to the tag.

Here is a concrete illustration of the idea:

```html
<div id="target_anchor1"></div>

<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c; }';
    var elem = document.getElementById("target_anchor1");
    elem.innerHTML = Viz(dot_source, "svg");
</script>
```

The contents of `dot_source` make up a DOT program:

  * `digraph` means we're making a directed graph,
  * `rankdir="LR"` means we want the nodes to prefer horizontal left-to-right layout (the default is to have them vertically stacked top-to-bottom),
  * `bgcolor="transparent"` means we want a transparent background (the default is white, which is fine but a little offputting for this site),
  * `a -> b` means "I want an edge coming out of `a` and into `b`"; you can chain them together as shown in `a -> b -> c`.

Here is how that ends up rendering:

<div id="target_anchor1"></div>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c; }';
    var elem = document.getElementById("target_anchor1");
    var rendered = Viz(dot_source, "svg");
    elem.innerHTML = rendered;
</script>

It turns out that the generated SVG is actually pretty readable.  That
is, I am able to use "Inspect Element" in my web browser, and I see
there that the SVG element is actually made up of many `<g>` elements
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

But what if we want more control over the layout of the picture
elements?

For example, one might want more control over layout to
ensure that there is a easy-to-see correspondence between a series of
pictures, or if there is a structural symmetry that is easier to see
if the layout is also symmetrical.

<p id="target_anchor2">Symmetric structure but asymmetric layout: </p>
<script>
    var dot_source = 'digraph { rankdir="LR"; bgcolor="transparent"; a -> b -> c -> e; b -> d -> e -> a; }';
    var elem = document.getElementById("target_anchor2");
    var rendered = Viz(dot_source, "svg");
    elem.innerHTML += rendered;
</script>

For that, we will need to resort to more complex tools.

## SVG

Our next tool will be SVG. It is a natural next step after `viz.js`,
because that tool is already generating SVG *for us* in the
document. So we can just cut-and-paste the SVG content generated by
`viz.js` and then modify it as needed.



## d3.js

Our last tool will be `d3.js`, which is a higher level tool than SVG
but offers many more bells and whistles than `viz.js`

