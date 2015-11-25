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
   uses this style.

 * [tufte-css][] is pretty cool, if you're working in the realm of
   writing HTML and want to just plug in a style sheet and go.

(I'm sure the hardcore Tufte fans in the audience are now saying "How
can you claim to want to emulate Tufte's work in a post that uses a
bulleted list!?!" I think there remains a time and place for a
[bulleted list][on-bullets]; but I digress.)

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
currently that important to me. (At least, not yet.) Instead, what I really
want is to cherry-pick certain aspects that I find important.

In particular, I *really* want sidenotes (and margin notes/figures),
and the option for full width figures. Below, I'll discuss the
motivation for each in turn.

Luckily for me, the presentation of [tufte-jekyll][] is itself
dedicated to spelling out how each feature was added. In other words,
it is *perfect* to use as the basis for cherry-picking.

Pretty much all of the things I wanted were implemented in
[tufte-jekyll][] via Jekyll Plugins. Lets go through them one by one,
largely just to prove to myself that it is all working as I expect,
(or in actual practice, to iteratively fix each one until this text is
a postable demo).

## Side Notes

My previous blog posts{% sidenote 'previous-blog-posts' 'Hey, a sidenote! One post that can/will benefit from these is my earlier [GC and Rust Part 0][gc-part-0].' %} often hack in footnotes, but its always super awkward because I am
never quite sure how far down into the post to collect the footnotes,
and I dislike how the footnotes disrupt the flow of the text if they
are not placed at the bottom of the (potentially very long) post.

[gc-part-0]: blog/2015/10/27/gc-and-rust-part-0-how-does-gc-work/

Side notes do not suffer from either of these problems.

Side note support (and margin content in general) *does* come at the
cost of potentially introducing a large unused space on the right-hand
side of the presentation.

But the CSS magic here is smart enough to only try to grab the space
if it can afford to do so; if you are viewing this on a narrow
device,{% sidenote 'thin-browser' '(or if you make your browser window sufficiently thin relative to your current font size)'  %}
the superscript indicating the presence of an associated side note
turns into a toggle switch for the associated content (now presented *inline*
with the text when toggled on).

## Margin Notes

Sometimes even
{% marginnote 'inherited-from-foonotes' 'This is a margin note *without* a superscript. The superscripts on sidenotes are a holdover from the world of footnotes.' %}
the superscripted number that a sidenote carries is disruptive, and
unnecessary if:

  1. the content is lined up nicely, and

  2. the presentation width for the main column is sufficiently wide.

But then again, if you are viewing this on a narrow device (or if you make
your browser window sufficiently thin), you will see that a margin note is replaced
with a
symbol{% sidenote 'checkplus' 'For me, the symbol is a plus sign with a circle around it; I think it is this: "&#x2295;" U+2295 (aka "CIRCLED PLUS")' %}
that will provide the reader with a way to toggle the presentation
of the elided content on and off.

## Margin Figures

TODO

## Drawbacks for Authorship

Unfortunately, since Markdown does not have a standard footnote
syntax, I am forced to resort to a Jekyll plugin to render these side
notes and margin notes, which means that every such note forces me to
write source text analogous to this into my (otherwise readable)
markdown:

Pretend that my editor window is forty characters wide:

    0000000001111111111222222222233333333334
    1234567890123456789012345678901234567890

Then the source text ends up looking like this{% comment %} 'meta' "Isn't this sad?" {% endcomment %}
in my editor (where "`\`" represents the continuation of a single line in the editor's wrapping presentation{% sidenote 'huge-scrollbar' 'Depending on your choice of editor, you may be more familiar with a scroll bar coming up in this context; in which case, imagine a scrollbar that pans over a space 20 times the width of your window. Annoying, right? <br/><br/> That is what it feels like for me writing side notes like this one.' %}):

    Here is an
    artificially
    narrow column{% raw %}{% sidenote 'unique-id-for\
    -note' "this content all must stay on o\
    ne line to placate Jekyll's regexp base\
    d parsing. Why bother with matching ope\
    n- and close-delimiters if you are not \
    going to use them to allow things like \
    line breaks in the input? :(" %}
    {% endraw %}to emphasize
    my point.

    <!--
    0000000001111111111222222222233333333334
    1234567890123456789012345678901234567890
    -->

Part of the whole reason I author blog posts using a Markdown syntax
is so that my source text tends to be readable as-is; the "no line
breaks" limitation in the Jekyll plugin for side and margin notes
sort of breaks that readability property.

Well, maybe discouraging me from writing
long side notes{% sidenote 'dfw' 'I am pretty sure [Infinite Jest][IJ] would not work so well as a blog post.' %}
is not such a bad thing.

 * Oh, by the way, it looks like you cannot throw side notes just
   *anywhere*{% sidenote 'bullet-test' 'Like, say, a bulleted list; further discussion in [On Bulleted Lists][on-bullets] section' %}
   and expect them to play nice with the other marked up text.

   For example, it may appear at a inconsistently inset margin... or worse...

 * Some markup is handled *somewhat{% sidenote 'emph-test' 'This is relatively fine.' %} poorly*,
   in that it may inherit the style of the surrounding markup it was embedded within

 * And some, like code markup,{% marginnote 'code-test' 'Yeah, that raw HTML you see there is due to some bad interaction between the Jekyll sidenote plugin code and the markdown pre-processor.' %}
   is handled *especially poorly*.
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
them.{% marginnote 'is-ul-really-gone' 'Or rather, it does away with the bullet sigils, not unordered lists themselves.' %}

If you are curious about Tufte's argument against the use of bulleted
lists, I highly recommend you pick up his essay
["The Cognitive Style of PowerPoint"][books_pp],
(which you can acquire on its own, or can be found as a chapter of his book
["Beautiful Evidence"][books_be]{% marginnote 'the-be-book' 'I suspect your local library is more likely to carry "Beautiful Evidence" than the PowerPoint essay (which one might deem a pamphlet).'%}).

[books_pp]: http://www.edwardtufte.com/tufte/books_pp
[books_be]: http://www.edwardtufte.com/tufte/books_be

For me, when writing markdown, the visible bullets serve a purpose.

 * In particular, I sometimes attach more than one paragraph of text
   to an item in an unordered list.

 * But if I associate more than one paragraph of text with an item on
   such a list, then without the visible bullet, one cannot readily
   tell whether the new paragraph has started a new item, ...

   ... or if it is a continuation of the previous item.

(Then again, since the standard Octopress format does not indent
unordered lists, the same problem arises, unless one has customized its
SCSS in the same manner that I now have done.)

I suspect the response here goes something like:

> If you are attaching multiple paragraphs to a single item in an
> unordered list, then your method of presentation is flawed.

I do not really have an argument against that, though I do find it
funny that one of the primary arguments against bulleted lists is that
one tends to not provide detailed discussion with them. Seems like a
case of "damned if you do, damned if you don't."

I am trying to learn how to make my blog posts more stream of thought
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


## Full Width Figures

## Column Width Figures

[tufte-latex]: https://tufte-latex.github.io/tufte-latex/

[tufte-css]: https://edwardtufte.github.io/tufte-css/

[tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog

TODO: full width figures

TODO: margin figures