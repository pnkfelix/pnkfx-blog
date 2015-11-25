---
layout: post
title: "Cherry-picking from the shoulders of giants"
date: 2015-11-24 18:00
comments: true
categories: octopress css
published: true
---

This post: I describe some changes to my blog's presentation
format, and how I hacked them in by cherry-picking from the shoulders
of giants.

<!-- more -->

I'm a big fan of Edward Tufte's work on presentation of
information. He mostly focuses on graphical presentations, especially
of statistical data. But really one can learn much from his overall
presentation style, e.g. from how he formats his text.

Of course I am not the first person to recognize this. :)

Lots of people have been adapting Tufte's style to formats compatible
with standard electronic authoring environments.

 * [tufte-latex][] is pretty cool, if you're working in the realm of
   LaTeX. (For example, [aturon's thesis](http://www.mpi-sws.org/~turon/turon-thesis.pdf)
   uses this style.)

 * [tufte-css][] is pretty cool, if you're working in the realm of
   writing HTML and want to just plug in a style sheet and go.

(I'm sure the hardcore Tufte fans in the audience are now saying "How
can you claim to want to emulate Tufte's work in a post that uses a
bulleted list!?!" I think there remains a time and place for a
bulleted list; this digression continues [below][on-bullets].)

 *  What if you're trying to work with a blog system like
    Octopress?

    Well, you're actually in luck; other people have already done the
    work of adapting the style rules in [tufte-css][] to Jekyll (the
    infrastructure that Octopress is built upon), yielding
    [tufte-jekyll][].

After skimming over the presentation over at [tufte-jekyll][] and
suffering with recurrent Octopress issues, I briefly considered
<span style="white-space: nowrap">`rm -rf`'ing</span> Octopress from my blog system and adopting Jekyll directly.
(Which isn't as big a deal as it sounds, since Octopress is built on
top of Jekyll and so in principle I would get to reuse much of my
previous customizations.)

But the reality is that I do not really want to spend that much time on this;
having a presentation that matches every aspect of [tufte-jekyll] is not
currently that important to me. (At least, not yet.)

What I really wanted was to cherry-pick certain aspects that I find
important.

In particular, I *really* want sidenotes (and margin notes/figures),
and an option for full width figures. I'll discuss each in turn.

Luckily for me, the presentation of [tufte-jekyll][] is itself
somewhat dedicated to spelling out how each feature was added.  In
other words, it seems *perfect* to use as the basis for such
cherry-picking.

Pretty much all of the things I wanted were implemented in
[tufte-jekyll][] via Jekyll Plugins. Let's go through them one by one,
largely just to prove to myself that it is all working as I expect,

(In actual practice, what this actually meant is that iteratively
fixed my CSS and/or made my own variants of the plugins until this
text rendered as a postable demonstration).

(The presentation below makes claims about how each construct is
written in the source markdown; however, each case is assuming that
one has installed the appropriate Jekyll plugins and SCSS support
files)

## Side Notes

My previous blog posts{% sidenote 'previous-blog-posts' 'Hey, a sidenote! One post that can/will benefit from these is my earlier [GC and Rust Part 0][gc-part-0].' %}
sometimes hacked in ad-hoc footnotes. But footnotes on the web are
super awkward because I am never quite sure how far down into the post
to collect the footnotes together.  Furthermore, I dislike how my
ad-hoc footnotes would disrupt the flow of the text if they were not
placed at the bottom of the (potentially very long) post; but if they
were all collected at the bottom, then navigating the page becomes
unwieldy at best.

[gc-part-0]: blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/

Side notes do not suffer from either of these problems.

Side note support (and margin content in general) *does* come at the
cost of potentially introducing a large unused space on the right-hand
side of the presentation.

But the CSS magic here is smart enough to only try to grab the space
if it can afford to do so; if you are viewing this on a narrow
device,{% sidenote 'thin-browser' 'Or make your browser window sufficiently thin relative to its font size.'  %}
the superscript indicating the presence of an associated side note
turns into a toggle switch for the associated content (now presented *inline*
with the text when toggled on).

One writes a sidenote with the following liquid tag syntax:
{% raw %}
```
main content{% sidenote 'unique-id' 'side content' %}
```
{% endraw %}
which renders as:
main content{% sidenote 'unique-id' 'side content' %}

## Margin Notes

Sometimes even the superscripted number that a sidenote carries is
disruptive, and unnecessary if:

  1. the content is lined up nicely, and

  2. the presentation width for the main column is sufficiently wide.

In other words: Why not drop the superscript?
{% marginnote 'inherited-from-foonotes' 'This is a margin note *without* a superscript. The superscripts on sidenotes are a holdover from the world of footnotes.' %}

That's what marginnotes are for.

But again, one wants to handle the case where the rendering area is
too narrow to set aside a margin. If your make the window narrow, you
will see that a margin note is replaced with an
inline symbol{% sidenote 'checkplus' 'Since there is no number associated with a margin note, the symbol is a plus sign with a circle around it; I think it is this: "&#x2295;" U+2295 (aka "CIRCLED PLUS")' %}
that will provide the reader with a way to toggle the presentation
of the elided content on and off, the same way that the superscript
acted as a toggle for a sidenote above.

Anyway, the code for `marginnote` is entirely analogous to that for `sidenote`.
One writes a margin note with the following liquid tag syntax:
{% raw %}
```
main content{% marginnote 'uniq-id' 'margin content' %}
```
{% endraw %}
which renders as:
main content{% marginnote 'uniq-id' 'margin content' %}

## Margin Figures

Another topic I have touched on (and plan to do more with soon) is
drawing diagrams of graph structures. This usually involves SVG in
some manner.

Here is a relatively simple bit of SVG:

<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_main" height="50" width="100%">
<defs>
<pattern id="grid_cell" width=10 height=10 patternUnits="userSpaceOnUse">
<path d="M 0,0 v10 h10" fill="none" stroke="#aaa" stroke-width=1/>
</pattern>
<pattern id="grid" width=100 height=100 patternUnits="userSpaceOnUse">
<path d="M 0,0 v100 h100 v-100 z" stroke="#555" stroke-width=1 fill="url(#grid_cell)"/>
</pattern>
</defs>
<rect x=0 y=0 width="100%" height="100%" fill="url(#grid)"/>
<circle cx=20 cy=20 r=10 stroke="green" fill="none"/>
<path d="M 80,30 C 90,20 30,20 10,40" stroke="blue" fill="none"/>
</svg>

In any case, I want to be able to present such diagrams
in the same manner that `tufte-css` and `tufte-jekyll` presents margin
figures, full-width figures, and column width figures.

However, `tufte-jekyll` assumes in its plugin for these features that
whatever figure you want to present is held in a separate file.

{% marginblock 'svg-margin-figure' %}
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_margin" height="50" width="100%">
<defs>
<pattern id="grid_cell" width=10 height=10 patternUnits="userSpaceOnUse">
<path d="M 0,0 v10 h10" fill="none" stroke="#aaa" stroke-width=1/>
</pattern>
<pattern id="grid" width=100 height=100 patternUnits="userSpaceOnUse">
<path d="M 0,0 v100 h100 v-100 z" stroke="#555" stroke-width=1 fill="url(#grid_cell)"/>
</pattern>
</defs>
<rect x=0 y=0 width="100%" height="100%" fill="url(#grid)"/>
<circle cx=20 cy=20 r=10 stroke="green" fill="none"/>
<path d="M 80,30 C 90,20 30,20 10,40" stroke="blue" fill="none"/>
</svg>
Here is a margin figure; it shows a circle and a cubic bezier curve
(along with helpful grid lines, which I need to figure out how to
adjust the coordinates in the source SVG).
{% endmarginblock %}

I resolved this by hacking the `margin_figure.rb` plugin so that it
extends from `Liquid::Block` instead of `Liquid::Tag`, and then
hacking the implementation of the plugin until it started producing
the output I expect. By the time I finished, I had something
with a totally different feature-set and interface; so I renamed
this version of the plugin to `marginblock`.

{% marginblock %}
Actually, I think all of the side- and margin-content extensions
discussed here have the same limitation that they only work with
inline elements.
{% endmarginblock %}

Important Caveat: this hacked `marginblock` plugin can only handle
inline elements.

{% marginblock 'demo-code-in-margin' %}
`<code>blocks</code>` do work (via HTML or Markdown backticks), but
fenced code blocks do not, because they generate `figure` and `table`
elements.
{% endmarginblock %}

  1. HTML code for certain elements (like
     <nobr>`figure`,</nobr> <nobr>`p`,</nobr> <nobr>`pre`,</nobr> or
     `table` elements) will fragment the content, with some initial
     portion in the margin, and the remainder injected into the main
     text.

  2. Markdown syntax that generates block-level elements has the
     same problem as HTML block-level elements.

     For example, content after an empty line will be converted into
     a `p` element (and those will end up injected back into the main
     text).

In other words: any markdown syntax inside a `marginblock`, including just an empty line
separating two blocks of text, will just confuse things.

So, don't do that.

To write a margin block, use the following syntax:
{% raw %}
```
{% marginblock %}
Margin block content, with no line breaks (see caveat above).
{% endmarginblock %}
```
{% endraw %}

## Full Width Figures

You might have noticed that in the SVG diagram <a
href="#demo_svg_main">above</a>, the diagram only spans the width of
the "main text", while the code block spans the full extent of the
page, flowing into the margin area.

I have hacked the CSS so that all of the figures for code are given
the full width of the page (otherwise I almost overflowed the
available space, yielding unfortunate scroll bars).

Still, it might well be that some SVG diagrams will likewise need the
full width of the page.
`tufte-jekyll` provides a plugin for this (called `fullwidth`), but
much like with its `marginfigure` plugin,
the `fullwidth` plugin assumes that
the content to be rendered lives in a separate file. Analogously to
how I dealt with that by making my own version of plugin,
`margin_block`, I have here made my own `fullwidth_block` plugin.

{% fullwidthblock %}
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_main" height="50" width="100%">
<defs>
<pattern id="grid_cell" width=10 height=10 patternUnits="userSpaceOnUse">
<path d="M 0,0 v10 h10" fill="none" stroke="#aaa" stroke-width=1/>
</pattern>
<pattern id="grid" width=100 height=100 patternUnits="userSpaceOnUse">
<path d="M 0,0 v100 h100 v-100 z" stroke="#555" stroke-width=1 fill="url(#grid_cell)"/>
</pattern>
</defs>
<rect x=0 y=0 width="100%" height="100%" fill="url(#grid)"/>
<circle cx=20 cy=20 r=10 stroke="green" fill="none"/>
<path d="M 80,30 C 90,20 30,20 10,40" stroke="blue" fill="none"/>
</svg>
{% endfullwidthblock %}

So, all you need to do is write:
{% raw %}
```
{% fullwidthblock %}
The lines of full Width Block content will extend across the full
width of the page before line wrapping. (Some block elements will end
up being mishandled, so don't use elements like `p` or `ul`, et
cetera, or Markdown that generates them.)
{% endfullwidthblock %}
```
{% endraw %}
which renders as:
{% fullwidthblock %}
The lines of full Width Block content will extend across the full
width of the page before line wrapping. (Some block elements will end
up being mishandled, so don't use elements like `p` or `ul`, et
cetera, or Markdown that generates them.)
{% endfullwidthblock %}

## Drawbacks for Authorship

Originally I had a whole section here decrying the fact that I could
not put *any* line breaks into the source for a `sidenote` or
`marginnote`, since the Liquid tag parser requires the entirety of a
tag be contained on one line.

But then I hacked up `marginblock` (described above), and my problem
just goes away
{% marginblock %}
Well, it goes away for superscript-free margin notes, at least.
If I ever feel the need to write a long side note, then I guess
I'll make a `sideblock` plugin at that time.
{% endmarginblock %}

Besides, maybe discouraging me from writing
long side notes{% sidenote 'dfw' 'I am pretty sure [Infinite Jest][IJ] would not work so well as a blog post.' %}
is not such a bad thing.

 * Oh, by the way, it looks like you cannot throw side notes just
   *anywhere*{% sidenote 'bullet-test' 'Like, say, a bulleted list; further discussion in the [On Bulleted Lists][on-bullets] section.' %}
   and expect them to play nice with the other marked up text.

   For example, it may appear at a inconsistently inset margin... or worse...

 * Markup in a surrounding context is handled *somewhat{% sidenote 'emph-test' 'This is relatively fine, even though it was not written with emphasis.' %} poorly*,
   in that it may inherit the style of the surrounding markup it was embedded within

 * And some contexts, like code markup,{% marginnote 'code-test' 'Yeah, that raw HTML you see there is due to some bad interaction between the Jekyll sidenote plugin code and the markdown pre-processor.' %}
   are handled *especially poorly*.
   ```
   As in `not{% sidenote 'code-test' 'This is not fine' %} good.
   ```

[DFW]: https://en.wikipedia.org/wiki/David_Foster_Wallace

[IJ]: https://en.wikipedia.org/wiki/Infinite_Jest

## <a id="on-bulleted-lists">On Bulleted Lists</a>
[on-bullets]: #on-bulleted-lists

Speaking of bulleted lists: I implied up above that Tufte has a
problem with the use of bulleted lists, and that this is why the
standard [tufte-css][] does away with
them.{% sidenote 'is-ul-really-gone' 'Or rather, it does away with the bullet sigils, not unordered lists themselves.' %}

If you are curious about Tufte's argument against the use of bulleted
lists, I highly recommend you pick up his essay
["The Cognitive Style of PowerPoint"][books_pp],{% marginnote 'the-be-book' 'I suspect your local library is more likely to carry "Beautiful Evidence" than the PowerPoint essay (which one might deem a pamphlet).'%}
(which you can acquire on its own, or can be found as a chapter of his book
["Beautiful Evidence"][books_be]).

[books_pp]: http://www.edwardtufte.com/tufte/books_pp
[books_be]: http://www.edwardtufte.com/tufte/books_be

For me, when writing markdown, the visible bullets serve a purpose.

{% marginblock %}
According to some conventions, my argument here is a strawman,
because I have already failed by putting more than six words on a
single bullet ("any more words per bullet, and you don't have a
bullet."). I do not really have an argument against that.
{% endmarginblock %}

 * In particular, I sometimes attach more than one paragraph of text
   to an item in an unordered list.

 * But if I associate more than one paragraph of text with an item on
   such a list, then without the visible bullet, one cannot readily
   tell whether the new paragraph has started a new item, ...

   ... or if it is a continuation of the previous item.

(Then again, since the standard Octopress format does not indent
unordered lists, the same problem arises, unless one has customized its
SCSS in the same manner that I now have done.)

I *am* trying to learn how to make my blog posts more stream of thought
(for quick generation and publication), rather than carefully crafted
pieces of art. The bulleted list approach certainly provides a
quick-and-dirty way to do that.{% sidenote 'long-text' 'Speaking of quick-and-dirty, here is some really long text because I want to see what happens to the horizontal rule that divides the two parts of the main text below. From what I have observed, some (but not all) browsers stop the rendering of the horizontal rule from crossing into the text here.' %}

----

In any case, there is a technical problem that arises when mixing
lists with side or margin notes.

I already mentioned above that the margin inset for a sidenote may get
a little screwy.

For some reason, it gets particularly bad when I try to use sidenotes
with direct `<ul>` element written in raw HTML.

Check this out; I have preserved access to the original `tufte-css` style for `ul`,
by putting it under a class named `"tufte"`. Compare:

<ul>
 <li> a "normal" (class-less){% sidenote 'normal-bullet' 'kind of odd; "far-out!"' %}</li>
 <li> unordered (aka bulleted)</li>
 <li> list </li>
</ul>

to:

<ul class="tufte">
 <li> Tufte-style (class="tufte"){% sidenote 'tufte-bullet' 'also kind of odd; "fall-in!"' %}</li>
 <li> unordered (and unbulleted)</li>
 <li> list </li>
</ul>

The original [tufte-jekyll][] code has workarounds in its CSS for the
latter case above. I have not yet attempted to isolate those workarounds
and apply them to my own code here.{% marginnote 'my-own-code' "At this point I have diverged in many ways from the original <code>tufte-jekyll</code> code; for example, I want to devote more than 50% of the page to my main text; the margin notes don't need to be given half of the browser's screen real estate." %}

Either way, it is super weird that the inset goes in different
directions for the two cases. (It is especially unfortunate for the
"far-out!" case above; if you collapse the sidebar for the page, the
"far-out!" can easily fall outside of the visible portion of the
presentation.

[tufte-latex]: https://tufte-latex.github.io/tufte-latex/

[tufte-css]: https://edwardtufte.github.io/tufte-css/

[tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
