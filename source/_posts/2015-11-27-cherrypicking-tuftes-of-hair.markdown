---
layout: post
title: "Cherry-picking from the shoulders of giants"
date: 2015-11-27 14:00
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
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_defs" height="0" viewBox="0 0 32 32">
<defs>
<pattern id="grid_cell" width=10 height=10 patternUnits="userSpaceOnUse">
<path d="M 0,0 v10 h10" fill="none" stroke="#aaa" stroke-width=1/>
</pattern>
<pattern id="grid" width=100 height=100 patternUnits="userSpaceOnUse">
<path d="M 0,0 v100 h100 v-100 z" stroke="#555" stroke-width=1 fill="url(#grid_cell)"/>
</pattern>
<g id="the_pic">
<rect x=0 y=0 width="100%" height="100%" fill="url(#grid)"/>
<circle cx=20 cy=20 r=10 stroke="green" fill="none"/>
<path d="M 80,30 C 90,20 30,20 10,40" stroke="blue" fill="none"/>
</g>
</defs>
</svg>

Here is a relatively simple bit of SVG:

<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_main" height="50" width="100%">
<use xlink:href="#the_pic"/>
</svg>
<!-- If you're looking at this source code, then you can see that the
     actual content definitions are above, in the SVG group (`g`)
     element identified by `the_pic` -->

I want to be able to present such diagrams in the same manner that
`tufte-css` and `tufte-jekyll` presents margin figures, full-width
figures, and column width figures.

However, `tufte-jekyll` assumes in its plugin for these features that
whatever figure you want to present is held in a separate file.

{% marginblock 'svg-margin-figure' %}
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_margin" height="50" width="100%">
<use xlink:href="#the_pic"/>
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
`<code>styling</code>` does work (via HTML or Markdown backticks), but
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
the full width of the page (otherwise, they always overflowed the
available space, yielding unfortunate scroll bars).

Still, it might well be that some SVG diagrams will likewise need the
full width of the page.
`tufte-jekyll` provides a plugin for this (called `fullwidth`), but
much like with its `marginfigure` plugin,
the `fullwidth` plugin assumes that
the content to be rendered lives in a separate file. Analogously to
how I dealt with that by making my own version of plugin,
`margin_block`, I have here made my own `fullwidth_figure` plugin.

Here is that circle and bezier curve again, this time spanning
the full width of the page:

{% fullwidthfigure %}
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     id="demo_svg_main" height="50" width="100%">
<use xlink:href="#the_pic"/>
</svg>
{% endfullwidthfigure %}

So, all you need to do is write:
{% raw %}
```
{% fullwidthfigure %}
The lines of fullwidth figure content will extend across the full
width of the page before line wrapping. (Some block elements will end
up being mishandled, so don't use elements like `p` or `ul`, et
cetera, or Markdown that generates them.)
As the name implies, this is nested within a `figure` element;
that means, for example, you can use `figcaption` to add a caption:
<figcaption>Caption for `fullwidthfigure` demo</figcaption>
{% endfullwidthfigure %}
```
{% endraw %}
which renders as:
{% fullwidthfigure %}
The lines of fullwidth figure content will extend across the full
width of the page before line wrapping. (Some block elements will end
up being mishandled, so don't use elements like `p` or `ul`, et
cetera, or Markdown that generates them.)
As the name implies, this is nested within a `figure` element;
that means, for example, you can use `figcaption` to add a caption:
<figcaption>Caption for `fullwidthfigure` demo</figcaption>
{% endfullwidthfigure %}

## Drawbacks, Rendering Bugs and/or Gotcha's

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

## <a id="on-bulleted-lists"></a>On Bulleted Lists
[on-bullets]: #on-bulleted-lists

Speaking of bulleted lists: I implied up above that Tufte has a
problem with the use of bulleted lists, and that this is why the
standard [tufte-css][] does away with
them.{% sidenote 'is-ul-really-gone' 'Or rather, it does away with the bullet sigils, not unordered lists themselves.' %}

If you are curious about Tufte's argument against the use of bulleted
lists, I highly recommend you pick up his essay
["The Cognitive Style of PowerPoint"][books_pp],{% marginblock %}
Your local library is more likely to carry "Beautiful Evidence"
than the PowerPoint essay on its own (which one might deem a pamphlet).
Alternatively, you can also 
<a href="http://www.edwardtufte.com/tufte/ebooks">purchase</a>
the Powerpoint essay as a PDF-format e-book on Tufte's website.
{% endmarginblock %}
(which you can acquire on its own, or can be found as a chapter of his book
["Beautiful Evidence"][books_be]).

[books_pp]: http://www.edwardtufte.com/tufte/books_pp
[books_be]: http://www.edwardtufte.com/tufte/books_be

For me, when writing markdown, the visible bullets serve a purpose.

{% marginblock %}
According to some conventions, my argument here is a strawman,
because I have already failed by putting more than six words on a
single bullet ("any more words per bullet, and you don't have a
bullet."). I do not really have an argument against that,
(except that Tufte himself seems to lambast such rules of thumb).
{% endmarginblock %}

 * In particular, I sometimes attach more than one paragraph of text
   to an item in an unordered list.

 * But if I associate more than one paragraph of text with an item on
   such a list, then without the visible bullet, one cannot readily
   tell whether the new paragraph has started a new item, ...

   ... or if it is a continuation of the previous item.

 * Then again, since the standard Octopress format does not indent
   unordered lists, the same problem arises unless one
   customizes{% sidenote 'scss-indented-list' 'See the `$indented-lists` variable in `_layout.scss`' %}
   its SCSS to turn it on. (I tried this, but it ends up being more
   trouble than its worth in terms of it breaking other things in the
   CSS implementing this main-and-margin presentation.)

 * Have you noticed that the indentation of the margin notes
   that are attached to list-elements are a little screwy?

    * This is because of how the CSS is implemented; the main content elements
      (like `p`, `ul`, et cetera) are all assigned `width: 60%`,
      so that the remaining 40% is available for the margin content,
      which is then assigned `margin-right: -40%` in the CSS.

   * The problem, as far as I can tell, is that the `-40%` is computed
     relative to the width of the parent element.{% sidenote 'relative-margins-and-list-indents' 'When list content gets indented, the width of a list element is less than that of paragraphs in the main text, and so its computed `margin-right: -40%` is a bit smaller than that for other margin content, causing a slight (but annoyingly noticeable) difference.' %}

   * I have spent some time trying to puzzle this out, but at this point
     I am willing to just say "avoid attaching margin content to list
     elements."

I *am* trying to learn how to make my blog posts more stream of thought
(for quick generation and publication), rather than carefully crafted
pieces of art. The bulleted list approach certainly provides a
quick-and-dirty way to do that.{% sidenote 'long-text' 'Speaking of quick-and-dirty, here is some really long text because I want to see what happens to the horizontal rule that divides the two parts of the main text below. At first different browsers handled the rendering in different ways; but now I have customized my SCSS so that `hr` has the same `width` constraint as a `p` element.' %}


## How'd He Do Dat?

So how did I do all this?

I started by copying the [tufte-jekyll][] plugins and SCSS support
files into my blog source tree. Since I use Octopress, I had to move
things around a bit:

 1. The `tufte.scss` that you find in tufte-jekyll's `css/`
    needs to go somewhere; I put a copy of it into `sass/custom`.

    Then I made a copy of the copy and named it `_fsk_tufte.scss`{% marginnote 'on-renaming' 'In truth, the renamed copy came later, but it is good practice and simplifies the current discussion to assume I did it at the outset.' %}

 2. After putting the `.scss` files into place, I had to actually load the
    main one into the Jekyll's page generation system. That is accomplished
    via an `@import` in `sass/custom/_styles.scss`:

    ```
    @import "fsk_tufte";
    ```

 3. Running the `rake generate` task to generate the CSS and the pages showed that
    there were references to undefined variables like `$constrast-color`.
    I skimmed through `fsk_tufte.scss` and either removed such references
    or figured out where those variables were defined in the original
    tufte-jekyll source repository, and ported them over accordingly.
    For example, `$contrast-color` was defined in `_sass/_settings.scss`;
    I cut-and-pasted it into `sass/custom/_color.scss`.

 4. I initially copied all of the plugins from tufte-jekyll's
    `_plugins/` into my `plugins/`.

 5. Even after doing this, a lot of things were broken. I spent a *long*
    time with a web browser's "Inspect Element" tool, comparing how
    the tufte-jekyll post was being rendered to how my own draft blog
    post was rendered.

    Over the course of doing this, I found it necessary to revise my
    `_fsk_tufte.scss` in myriad ways.

### Revisions to _fsk_tufte.scss

 1. I removed any content that I hypothesized was not related to my
    margin-rendering related goals. For example, `tufte.scss`
    automatically increases the font size as the browser window
    width increases. I find this distracting as a programmer (and
    annoying as a reader), so I removed it.

 2. I changed the footnote font-size selection strategy.

    * Tufte-jekyll's strategy is to use `1.0rem`{% marginblock %}
      "rem" stands for "root em" which in turn means "unit equal to the
      height of a capital M in the root (i.e. `html`) element of the document"
      {% endmarginblock %}
      as the `font-size` for the margin text and then scaling up the font-size
      for the main content to be `1.4rem` (so that the main text ends up 1.4x
      the size of the margin text).

    * I instead used `font-size: 0.8rem;` for margin text, and leave the
      font-size for the main text unchanged (so that in principle it will
      remain at `1rem`).

    * My reasoning here is that the majority of the text should be
      occurring in the main content area, and we should be respecting
      the font settings chosen by the user in their browser for such
      text. If the user has selected a font size of 14, then that is
      what we should use for the main text; we should not be using 14
      for the margin notes and scale the main text up to 1.4*14 (=
      19.6).

 3. After I noticed that my main-and-margin presentation style was
    begin applied even to things like the *sidebar* of the blog,
    I added guards to all of the relevant parts of `_fsk_tufte.scss`
    so that the main-and-margin styling *only* applies to elements
    that occur within an `article` element (since the main content
    blocks always fall under an article).

    Those were rules like this:

    ```
    // If a `ul`/`ol` occurs *immediately* (>) under an `article`
    article > ul, article > ol {
        width: 60%
    }
    ```

    I also added customization to try to avoid double-applications of
    the percentage-based width restrictions; for example, I added
    a rule like:

    ```
    p, ul, ol { p, ul, ol: width: 100 ]
    ```

    (In hindsight, perhaps this last rule would be redundant if I were
    to generalize the previous rule in a suitable fashion...)

Anyway, those are the main customizations of the SCSS that I can think
of off-hand. You can of course just go peek at its source code
if you want to see what it currently looks like (and perhaps offer
tips on how I might revise it).

[tufte-latex]: https://tufte-latex.github.io/tufte-latex/

[tufte-css]: https://edwardtufte.github.io/tufte-css/

[tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
