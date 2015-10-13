---
layout: post
title: "Drawing Circles and Arrows 2"
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

The three technologies covered here are [viz.js][], [SVG][], and [D3][].

[viz.js]: #toc_0
[SVG]: #toc_1
[D3]: #toc_2

<!-- more -->

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
make your page load it up, via a `script` tag invocation,
like: `<script src="/javascripts/viz.js" charset="utf-8"></script>;`

This script can take a while to load. You may want to insert a warning,
like this one, before the `script` invocation.

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

The above "works" (even in the context of an Octopress blog post), but
it is a rather brute-force approach. Plus, the resulting composition
of the blog text with the graphics added at the end is not likely to
please you nor your audience.

Instead, let us see if we can dynamically-extend more specific content within
the post. We can inject a content-less `div` tag, and then search for that
element in a `script` block that will add our picture to the tag.

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

Our next tool is SVG. It is a natural next step after `viz.js`,
because that tool is already generating SVG *for us* in the
document. So we can just cut-and-paste the SVG content generated by
`viz.js` (by doing "Inspect Element" and then "Copy Outer HTML" --
view source won't help us get the intermediate structure we seek
here), and then modify it as needed.

In the case of our asymmetric layout above, what we want is to
nudge the `a` node up so that it is inline with the `b` and `e`
nodes, and nudge the `c` and `d` nodes down so that they are
each equidistant from the line that goes through `b` and `e`.

After skimming the graphic elements of the generated SVG
(this is where the inline comments labelling each element
come in handy),
I see some clues as to what could accomplish this:

  * The `rx` and `ry` attributes for *all* of the nodes are the same (`rx="27"` and `ry="18"`, respectively).
    This is unsurprising, once we learn that `rx` and `ry` stand for the *radius values* of each ellipse.

  * The `cy` attributes for `b`, `d`, and `e` are all the same (`cy="-41"`), while the `cx` attributes vary.
    This is a strong hint that the `cy="-41"` is the line that we seek.

  * The `cy` attribute for `a` is `cy="-18"`, while `c` has `cy="-95"`. This is reasonable: note that the
     `a` node is only laid out slightly lower than the line at `cy="-41"`, while `c` is far above it.

So, we may be able to get a long ways towards our goal by putting `cy="-41"` in for `a`, and adjusting
the `cy` for `c` and `d` so that they are each 23 units away from `cy="-41".

After making that change (you can see it via View Source, I'm not going to copy the full source
inline for any of these SVG examples), you get this:

<svg width="332pt" height="121pt" viewBox="0.00 0.00 332.00 121.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 117)">
<title>%3</title>
<!-- a -->
<g id="node1" class="node"><title>a</title>
<ellipse fill="none" stroke="black" cx="27" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="27" y="-13.8" font-family="Times,serif" font-size="14.00">a</text>
</g>
<!-- b -->
<g id="node2" class="node"><title>b</title>
<ellipse fill="none" stroke="black" cx="117" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="117" y="-36.8" font-family="Times,serif" font-size="14.00">b</text>
</g>
<!-- a&#45;&gt;b -->
<g id="edge1" class="edge"><title>a-&gt;b</title>
<path fill="none" stroke="black" d="M52.5129,-24.4068C61.5717,-26.7744 72.0115,-29.503 81.775,-32.0548"></path>
<polygon fill="black" stroke="black" points="81.0585,-35.4851 91.6186,-34.6276 82.8286,-28.7126 81.0585,-35.4851"></polygon>
</g>
<!-- c -->
<g id="node3" class="node"><title>c</title>
<ellipse fill="none" stroke="black" cx="207" cy="-64" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="207" y="-90.8" font-family="Times,serif" font-size="14.00">c</text>
</g>
<!-- b&#45;&gt;c -->
<g id="edge2" class="edge"><title>b-&gt;c</title>
<path fill="none" stroke="black" d="M137.53,-52.9844C149.379,-60.2552 164.699,-69.6562 177.842,-77.7215"></path>
<polygon fill="black" stroke="black" points="176.171,-80.8021 186.525,-83.0492 179.832,-74.8358 176.171,-80.8021"></polygon>
</g>
<!-- d -->
<g id="node5" class="node"><title>d</title>
<ellipse fill="none" stroke="black" cx="207" cy="-18" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="207" y="-36.8" font-family="Times,serif" font-size="14.00">d</text>
</g>
<!-- b&#45;&gt;d -->
<g id="edge4" class="edge"><title>b-&gt;d</title>
<path fill="none" stroke="black" d="M144.403,-41C152.393,-41 161.311,-41 169.824,-41"></path>
<polygon fill="black" stroke="black" points="169.919,-44.5001 179.919,-41 169.919,-37.5001 169.919,-44.5001"></polygon>
</g>
<!-- e -->
<g id="node4" class="node"><title>e</title>
<ellipse fill="none" stroke="black" cx="297" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="297" y="-36.8" font-family="Times,serif" font-size="14.00">e</text>
</g>
<!-- c&#45;&gt;e -->
<g id="edge3" class="edge"><title>c-&gt;e</title>
<path fill="none" stroke="black" d="M227.53,-83.0156C239.379,-75.7448 254.699,-66.3438 267.842,-58.2785"></path>
<polygon fill="black" stroke="black" points="269.832,-61.1642 276.525,-52.9508 266.171,-55.1979 269.832,-61.1642"></polygon>
</g>
<!-- e&#45;&gt;a -->
<g id="edge6" class="edge"><title>e-&gt;a</title>
<path fill="none" stroke="black" d="M275.756,-29.6579C263.97,-23.7091 248.597,-17.0404 234,-14 175.208,-1.75394 104.932,-7.29022 63.5195,-12.5522"></path>
<polygon fill="black" stroke="black" points="62.8348,-9.11249 53.3875,-13.9081 63.7634,-16.0506 62.8348,-9.11249"></polygon>
</g>
<!-- d&#45;&gt;e -->
<g id="edge5" class="edge"><title>d-&gt;e</title>
<path fill="none" stroke="black" d="M234.403,-41C242.393,-41 251.311,-41 259.824,-41"></path>
<polygon fill="black" stroke="black" points="259.919,-44.5001 269.919,-41 259.919,-37.5001 259.919,-44.5001"></polygon>
</g>
</g>
</svg>

Oops!

  1. We need to adjust the labels for each of the ellipses that we moved.

  2. Furthermore, we need to adjust the edges that connect the ellipses that we moved!

Adjusting the labels' y-positions is pretty simple (just calculate the
difference in the ellipse center and apply it to the text center).


The edges are harder, because they are made via a curve that is going through a series
of points, so we need to adjust potentially *all* of the the points on the curve
to keep it from going through one of the nodes we have moved.

(The story is not so bad for the `a -> b` edge, which we can largely adopt from
the pre-existing horizontal edges from the original `b -> d` and `d -> e`.
Likewise we can probably adapt the original diagonal `a -> b` to work
for `b -> c`, `b -> d`, `c -> e`, and `d -> e`. Its really the `e -> a` edge
that looks like the most difficult to handle.
Nonetheless, our life would probably be easier still if we threw away the given coordinates
and used new ones that we select.)

Anyway, the edges are all described via SVG `path` elements. The `d`
attribute for a `path` contains an embedded domain specific language
(DSL) for describing its traversal through the points on the
plane. Some terms of this DSL are borrowed from [Turtle Graphics][],
where one positions a cursor on the plane and then manipulates it.

[Turtle Graphics]: https://en.wikipedia.org/wiki/Turtle_graphics

  * The `M x y` subcommand *moves* the pen to the point (x,y). (Here and
    in all of the following bullets, `x`, `y`, `x1`, `y1`, ... always
    stand for concrete numbers in the `d` attribute's text.)

  * The `L x y` subcommand draws a *line* from the current point to (x,y),

  * One can *omit* the command letter if the same command is
    used multiple times in a row.

  * So `M x1 y1 L x2 y2 x3 y3` draws two lines: (x1,y1) -- (x2,y2) and (x2,y2) -- (x3,y3).

  * The `Q x1 y1 x2 y2` subcommand ("*Quadratic* Bézier curve") draws
    a curve that heads towards the *control point* (x1,y2), but not
    through it, and *ends at* (x2,y2).

  * The `C x1 y1 x2 y2 x3 y3` subcommand ("*Cubic* Bézier curve")
    draws a curve that ends at (x3,y3), but with *two* control points.
    The first control point is the point we want the curve to heading *towards* as
    it leaves the origin, and the second control point is where we want it to
    seem like it is heading *from* as it approaches the target.

The main reason I listed all the above commands out is that the
`viz.js`-generated SVG uses cubic bezier curves even when a simple
line would serve; thus by understanding what each is doing, we can try
to simplify the SVG by replacing complex curves with simpler ones when
possible.


In particular, we know the center of `a` is (27,-41) and that it has
an x-radius of `27`; thus we know that our `a -> b` edge should start
at (54,-41), because 27 + 27 = 54.  A similar bit of arithmetic based
on the center of `b` tells us that the edge should end at (90,41).

And we can use similar arithmetic to fix the arrowhead as well, which
is specified as a `polygon` with four points (I have copied `viz.js`
and repeated one of the points, though if I understand correctly, this
is not necessary for the `polygon` element in SVG).
This yields the following code:

```html
<!-- a&#45;&gt;b -->
<g id="edge1" class="edge"><title>a-&gt;b</title>
<path fill="none" stroke="black" d="M54,-41 L90,-41"></path>
<polygon fill="black" stroke="black" points="90,-41 80,-45 80,-37, 90,-41"></polygon>
</g>
```


<svg width="332pt" height="121pt" viewBox="0.00 0.00 332.00 121.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 117)">
<title>%3</title>
<!-- a -->
<g id="node1" class="node"><title>a</title>
<ellipse fill="none" stroke="black" cx="27" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="27" y="-36.8" font-family="Times,serif" font-size="14.00">a</text>
</g>
<!-- b -->
<g id="node2" class="node"><title>b</title>
<ellipse fill="none" stroke="black" cx="117" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="117" y="-36.8" font-family="Times,serif" font-size="14.00">b</text>
</g>
<!-- a&#45;&gt;b -->
<g id="edge1" class="edge"><title>a-&gt;b</title>
<path fill="none" stroke="black" d="M54,-41 L90,-41"></path>
<polygon fill="black" stroke="black" points="90,-41 80,-45 80,-37, 90,-41"></polygon>
</g>
<!-- c -->
<g id="node3" class="node"><title>c</title>
<ellipse fill="none" stroke="black" cx="207" cy="-64" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="207" y="-59.8" font-family="Times,serif" font-size="14.00">c</text>
</g>
<!-- b&#45;&gt;c -->
<g id="edge2" class="edge"><title>b-&gt;c</title>
<path fill="none" stroke="black" d="M137.53,-52.9844C149.379,-60.2552 164.699,-69.6562 177.842,-77.7215"></path>
<polygon fill="black" stroke="black" points="176.171,-80.8021 186.525,-83.0492 179.832,-74.8358 176.171,-80.8021"></polygon>
</g>
<!-- d -->
<g id="node5" class="node"><title>d</title>
<ellipse fill="none" stroke="black" cx="207" cy="-18" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="207" y="-13.8" font-family="Times,serif" font-size="14.00">d</text>
</g>
<!-- b&#45;&gt;d -->
<g id="edge4" class="edge"><title>b-&gt;d</title>
<path fill="none" stroke="black" d="M144.403,-41C152.393,-41 161.311,-41 169.824,-41"></path>
<polygon fill="black" stroke="black" points="169.919,-44.5001 179.919,-41 169.919,-37.5001 169.919,-44.5001"></polygon>
</g>
<!-- e -->
<g id="node4" class="node"><title>e</title>
<ellipse fill="none" stroke="black" cx="297" cy="-41" rx="27" ry="18"></ellipse>
<text text-anchor="middle" x="297" y="-36.8" font-family="Times,serif" font-size="14.00">e</text>
</g>
<!-- c&#45;&gt;e -->
<g id="edge3" class="edge"><title>c-&gt;e</title>
<path fill="none" stroke="black" d="M227.53,-83.0156C239.379,-75.7448 254.699,-66.3438 267.842,-58.2785"></path>
<polygon fill="black" stroke="black" points="269.832,-61.1642 276.525,-52.9508 266.171,-55.1979 269.832,-61.1642"></polygon>
</g>
<!-- e&#45;&gt;a -->
<g id="edge6" class="edge"><title>e-&gt;a</title>
<path fill="none" stroke="black" d="M275.756,-29.6579C263.97,-23.7091 248.597,-17.0404 234,-14 175.208,-1.75394 104.932,-7.29022 63.5195,-12.5522"></path>
<polygon fill="black" stroke="black" points="62.8348,-9.11249 53.3875,-13.9081 63.7634,-16.0506 62.8348,-9.11249"></polygon>
</g>
<!-- d&#45;&gt;e -->
<g id="edge5" class="edge"><title>d-&gt;e</title>
<path fill="none" stroke="black" d="M234.403,-41C242.393,-41 251.311,-41 259.824,-41"></path>
<polygon fill="black" stroke="black" points="259.919,-44.5001 269.919,-41 259.919,-37.5001 259.919,-44.5001"></polygon>
</g>
</g>
</svg>


I have left out much of the details, but: That was a lot of work, and
we have only fixed the `a -> b` edge.
There are still the edges `b -> c`, `b -> d`, `c -> e`, `d -> e`, and
`e -> a` remaining!  (Plus, the end-points of these edges will be a
bit harder to calculate, since they are not right on the x- or y-axes
of our ellipses.)

To me, the real lesson here though is that while mucking with the
`viz.js`-generated SVG *sounds* like a plausible way to move forward,
it probably is more trouble than it is worth, except *perhaps* as a
learning exercise for how to write your own SVG from scratch, which
may be far more readable depending on what tricks you employ.

(Maybe you will write your own layout engine in JS; we will see an
example of this in the discussion of [D3][] below.)

If we are willing to write our own SVG from scratch, then we can
certainly construct simpler looking code, such as:


```html
<svg width="720" height="80">
  <circle stroke="blue" fill-opacity="0.8" cx="40" cy="50" r="10"></circle> <!-- a -->
  <circle stroke="blue" fill-opacity="0.8" cx="80" cy="50" r="10"></circle> <!-- b -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="35" r="10"></circle> <!-- c -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="65" r="10"></circle> <!-- d -->
  <circle stroke="blue" fill-opacity="0.8" cx="160" cy="50" r="10"></circle> <!-- e -->
</svg>
```

which renders as:

<svg width="720" height="80">
  <circle stroke="blue" fill-opacity="0.8" cx="40" cy="50" r="10"></circle> <!-- a -->
  <circle stroke="blue" fill-opacity="0.8" cx="80" cy="50" r="10"></circle> <!-- b -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="35" r="10"></circle> <!-- c -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="65" r="10"></circle> <!-- d -->
  <circle stroke="blue" fill-opacity="0.8" cx="160" cy="50" r="10"></circle> <!-- e -->
</svg>

But, we would still need to put in our arrows for the edges by hand;
the calculations will be easier to do in our head since we're using
circles and the positions are round numbers.

```html
<svg width="720" height="80">
  <path fill="none" stroke="black" d="M40,50 L80,50"></path>
  <path fill="none" stroke="black" d="M80,50 L120,35"></path>
  <path fill="none" stroke="black" d="M80,50 L120,65"></path>
  <path fill="none" stroke="black" d="M120,35 L160,50"></path>
  <path fill="none" stroke="black" d="M120,65 L160,50"></path>
  <circle stroke="blue" fill-opacity="0.8" cx="40" cy="50" r="10"></circle> <!-- a -->
  <circle stroke="blue" fill-opacity="0.8" cx="80" cy="50" r="10"></circle> <!-- b -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="35" r="10"></circle> <!-- c -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="65" r="10"></circle> <!-- d -->
  <circle stroke="blue" fill-opacity="0.8" cx="160" cy="50" r="10"></circle> <!-- e -->
</svg>
```

<svg width="720" height="80">
  <path fill="none" stroke="black" d="M40,50 L80,50"></path>
  <path fill="none" stroke="black" d="M80,50 L120,35"></path>
  <path fill="none" stroke="black" d="M80,50 L120,65"></path>
  <path fill="none" stroke="black" d="M120,35 L160,50"></path>
  <path fill="none" stroke="black" d="M120,65 L160,50"></path>
  <circle stroke="blue" fill-opacity="0.8" cx="40" cy="50" r="10"></circle> <!-- a -->
  <circle stroke="blue" fill-opacity="0.8" cx="80" cy="50" r="10"></circle> <!-- b -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="35" r="10"></circle> <!-- c -->
  <circle stroke="blue" fill-opacity="0.8" cx="120" cy="65" r="10"></circle> <!-- d -->
  <circle stroke="blue" fill-opacity="0.8" cx="160" cy="50" r="10"></circle> <!-- e -->
</svg>

The reason that the above is so simple is that the lines are going all
the way to the center of the circles.  This last picture is still
missing the arrow-heads, and to put those in, we would need to do
calculations that again I do not feel like spending my time on while
composing a document.

SVG *does* have some facilities for avoiding such repetition. In particular:

  1. One can *group* graphical elements together in a `g` element.

  2. One can *reuse* graphical elements with a `use` element, adjusting attributes locally as needed.
     (To avoid rendering the initial definition, put it in a `defs` block.)

  3. One can specify *relative* positions in the path description language (the `d` attribute) by
     using lower-case letters instead of uppercase ones.

  4. One can also specify *transformations* on a graphical element via the `transform` attribute.

<svg width="720" height="80">
  <defs>
  <circle id="def_node" stroke="blue" fill-opacity="0.8" r="10"></circle>
  <path id="def_edge" fill="none" stroke="blue" d="M 0,0 m 5,0 l 30,0 l -3,-3 m 3,3 l -3,3"></path>
  </defs>
  <!-- <path fill="none" stroke="black" d="M80,50 L120,35"></path> -->
  <!-- <path fill="none" stroke="black" d="M80,50 L120,65"></path> -->
  <!-- <path fill="none" stroke="black" d="M120,35 L160,50"></path> -->
  <!-- <path fill="none" stroke="black" d="M120,65 L160,50"></path> -->
  <use xlink:href="#def_node" transform="translate(40,40)"></use>  <!-- a -->
  <use xlink:href="#def_node" transform="translate(80,40)"></use>  <!-- b -->
  <use xlink:href="#def_edge" transform="translate(40,40)"></use>
  <use xlink:href="#def_edge" transform="translate(80,40) rotate(-45)"></use>
  <use xlink:href="#def_edge" transform="translate(80,40) rotate(45)"></use>
  <use xlink:href="#def_node" transform="translate(40,0) rotate(-45) translate(80,40)"></use> <!-- c -->
  <use xlink:href="#def_node" transform="translate(40,0) rotate( 45) translate(80,40)"></use> <!-- d -->
  <use xlink:href="#def_edge" transform="translate(120,35) rotate( 45)"></use>
  <use xlink:href="#def_edge" transform="translate(120,55) rotate(-45)"></use>
  <use xlink:href="#def_node" transform="translate(160,40)"></use> <!-- e -->
</svg>

Nonetheless, I would prefer to spend more of my time on higher level
descriptions of things (and have less redundancy in the source text as
well).

Let us move on to a another tool that may help us make our
desired picture quickly.

## D3

Our last tool is D3, which is a higher level tool than SVG
but offers many more bells and whistles than `viz.js`.

<script src="/javascripts/d3/d3.min.js" charset="utf-8"></script>

Actually, D3 might be called a "translucent layer" atop SVG:
it provides a high-level API, but can also *interoperate* with
graphical elements specified via SVG.

D3 is designed to be used for (dynamic) data-driven visualization.
Its API is centered around a separation between the data you are
trying to interpret and the visualization that should be generated
from that data.

It does this via the `data` method, which establishes the on-going
connection between each element of the input-array and the selected set
of visualizing nodes.

In general, there may be a mismatch: for example, there may be more
entries in the input-array than there are nodes in the current
visualization.  (D3 is designed to deal with this directly, by offering
the `enter` and `exit` methods, which respectively provides the nodes
that should be added or removed in order to re-establish the
correspondence between the number of data entries and the nodes in the
visualization.)

<svg id="target_svg3" width="720" height="100"></svg>

<script>
var w = 720;
var h = 100;

var vis = d3.select("#target_svg3");
var nodes = [{name: "a", x: 40, y: 60},
             {name: "b", x: 80, y: 60},
             {name: "c", x: 120, y: 75},
             {name: "d", x: 120, y: 45},
             {name: "e", x: 160, y: 60},
             ];

var links = [{source: nodes[0], target: nodes[1]},
             {source: nodes[1], target: nodes[2]},
             {source: nodes[1], target: nodes[3]},
             {source: nodes[2], target: nodes[4]},
             {source: nodes[3], target: nodes[4]},
             {source: nodes[4], target: nodes[0]}];

var link = vis.selectAll(".link")
        .data(links)
        .enter().append("line")
        .attr("class", "link")
        .attr("stroke", "#CCC")
        .attr("fill", "none");

var node = vis.selectAll("circle.node")
        .data(nodes)
        .enter().append("g")
        .attr("class", "node");

var force =
    d3.layout.force().nodes(nodes).links([]).gravity(0.1).charge(-1000).size([w, h]);

vis.selectAll("circle.nodes")
   .data(nodes)
   .enter()
   .append("svg:circle")
   .attr("cx", function (d) { return d.x; })
   .attr("cy", function (d) { return d.y; })
   .attr("r", "10px")
   .attr("stroke", "blue")
   .attr("fill-opacity", "0.2")
</script>

Or here is another demo, adapted from a code pen [by Patrick Mulder](http://codepen.io/mulderp/pen/KGFvx):

<svg id="target_svg4" width="720" height="100"></svg>

<script>
var w = 900,
    h = 400;

var circleWidth = 5;

var nodes = [
    {"name": "a" },
    {"name": "b" },
    {"name": "c" },
    {"name": "d" },
    {"name": "e" },
  ]

var links = [
   {source: nodes[0], target: nodes[1]},
   {source: nodes[1], target: nodes[2]},
   {source: nodes[0], target: nodes[3]},
   {source: nodes[4], target: nodes[2]},
   {source: nodes[2], target: nodes[3]},
  ]

var vis = d3.select("svg#target_svg4")
      .attr("class", "stage")
      .attr("width", w)
      .attr("height", h);

var force = d3.layout.force()
    .nodes(nodes)
    .links(links)
    .gravity(0.1)
    .charge(-1000)
    .size([w, h]);

 var link = vis.selectAll(".link")
        .data(links)
        .enter().append("line")
          .attr("class", "link")
          .attr("stroke", "black")
          .attr("fill", "none");

 var node = vis.selectAll("circle.node")
      .data(nodes)
      .enter().append("g")
      .attr("class", "node")
      .call(force.drag);

    node.append("svg:circle")
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .attr("r", circleWidth)
      .attr("fill", "black")

    node.append("text")
      .text(function(d, i) { return d.name; })
      .attr("x", function(d, i) { return circleWidth + 5; })
      .attr("y", 8)
      .attr("font-family",  "Serif")
      .attr("fill",         "black")
      .attr("font-size",    function(d, i) {  return  "1em"; })
      .attr("text-anchor",  function(d, i) { if (i>0) { return  "beginning"; }      else { return "end" } })



force.on("tick", function(e) {
  node.attr("transform", function(d, i) {
        return "translate(" + d.x + "," + d.y + ")"; 
    });

   link.attr("x1", function(d)   { return d.source.x; })
       .attr("y1", function(d)   { return d.source.y; })
       .attr("x2", function(d)   { return d.target.x; })
       .attr("y2", function(d)   { return d.target.y; })
});

force.start();
</script>
