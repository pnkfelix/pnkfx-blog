---
layout: post
title: "Fixing Octopress Table Rendering"
date: 2015-12-18 22:00
comments: true
categories: octopress css github
published: true
---

Learn how I hacked the SCSS for my blog to get tables to look right,
without breaking everything else (I hope).

<!-- more -->

Here is a sample table.

key | value1 | value2
----|--------|----------
a   | apple  | aardvark
b   | banana | bonobo
c   | clementine | cat

It is written in my blog source via this source text:

```
key | value1 | value2
----|--------|----------
a   | apple  | aardvark
b   | banana | bonobo
c   | clementine | cat
```

The default octopress presentation of such a table just smushes the
text into a grid, but does not render the division of the header,
rows, columns, nor cells in any way (no borders, no colors, et
cetera).

After hearing a colleague complain about how bad that default
presentation looked compared to what Github does with such tables, I
decided to try to figure out how to fix it.

First, I opened up gist.github.com and put my table into a gist there
(with a `.md`) extension, so I could see what github does to render
such tables.

By hitting "Inpsect Element" and looking at the
cascading style sheet (CSS) settings for the `table`, `tr`, `th`, and
`td` elements,{% marginnote 'settings' 'Namely, the `border`, `padding`, `font-weight`, `background-color`, and `margin-bottom` settings.' %}
I identified the things that needed to change.

{% marginblock %}
The one trick I *will* note, since its a pretty cute hack on the
part of CSS, is the way it uses the `:nth-child` selector to
differentiate the even rows from the odd rows when deciding on the
background color.
{% endmarginblock %}
You can see the specific changes, with comments, in the `.scss`
file transcribed below; I will not describe the effect of each one
here.

My usual tactic when doing this sort of interactive exploration of CSS
is to toggle each such setting on and off in the reference document
(the rendered gist, in this case), to see the effect of the setting on
the overall document, and then manually enter the setting into a
similarly inspected element in the target document where I am trying
to recreate the effect.

To limit the effect of the styling to just the tables that appear in a
blog post, I made sure that each CSS customization was prefixed by
`.entry-content`; I had determined via inspection that each block
entry is surrounded by a `<div class="entry-content">`.

So, I added the customizations to my `sass/custom/_styles.scss`,
regenerated my site, and looked at the result. It was quite promising,
except for one big problem: I had changed the formatting for *all*
tables, including the ones that are used to render code snippets!

{% marginblock %}
I will admit that I do not actually know the semantics of an
"inherit" setting. I assume it means something like "inherit the
value from your parent element, based on the context of where you
appear in the document."
{% endmarginblock %}
So I further revised the CSS so that, when you are in the context of a
`class="code"` underneath a `class="entry-content"`, *then* you should
just inherit the setting, rather than using the values specified here.

Anyway, *that* seemed to work great!

So here's my customized `_better_tables.scss` file (which is
imported into the aformentioned `_styles.scss` file via
`@import "better_tables";`)

```scss
// make the text for the header row bold and centered.
// (I have not been able to figure out where jekyll/octopress are overriding
//  the text-align and setting it to `left`)
.entry-content th { font-weight: bold; text-align: center }

// typography for p/blockquote/ul/ol puts a 1.5em margin below those elements,
// so do the same for our tables ...
.entry-content table { margin-bottom: 1.5em }

// ... but undo that for tables for pygments-generated code
.entry-content .code table { margin-bottom: inherit }


// add a border around each cell and padding around its content ...
.entry-content th, .entry-content td {
  border: 1px solid #ddd;
  padding: 6px 13px;
}

// ... but undo that for tables for pygments-generated code
.entry-content .code th, .entry-content .code td {
  border: inherit; padding: inherit;
}


// zebra-stripe the rows (N.B. `nth-child(2n)` works too) ...
.entry-content tr                 { background-color: #FFFFFF }
.entry-content tr:nth-child(even) { background-color: #F8F8F8 }

// ... but undo that for tables for pygments-generated code
.entry-content .code tr                 { background-color: inherit }
.entry-content .code tr:nth-child(even) { background-color: inherit }
```
