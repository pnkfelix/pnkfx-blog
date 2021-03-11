---
layout: post
title: "Mentoring rust diagnostics issue 81658"
date: 2021-03-11 14:01:33 -0500
comments: true
categories: 
---

{% marginblock %}
This was originally a [github comment][] that, due to my writing style (and
desire to teach), spun a bit out of control (or at least, didn't match the
overall style we try to follow for comments on [rust-lang/rust repository][]).

[github comment]: https://github.com/rust-lang/rust/issues/81658#issuecomment-795714938

[rust-lang/rust repository]: https://github.com/rust-lang/rust/
{% endmarginblock %}

{% marginblock %}
So now, this is a blog post instead, where I get to do fun things like have
better control over formatting and whatnot. (Based on what feedback I get on the
post, maybe I will next work to promote it to a chapter in the [rustc dev guide][].)

[rustc dev guide]: https://rustc-dev-guide.rust-lang.org/
{% endmarginblock %}

[Issue #81658][], "Invalid \`field is never read: \` lint warning", is a
bug filed against rustc.

These are mentorship instructions for how to go about fixing the bug.
They are also an overview of some steps I use to get started looking at a
bug like this.

<!-- more -->

## Context

[Issue #81658][], "Invalid \`field is never read: \` lint warning", is a
bug filed against rustc. It was filed at the beginning of February 2021.

The heart of the problem is that a diagnostic lint was firing, saying a
particular field was never read, and mentioning the diagnostic resulted due to a
`warn(dead_code)` lint setting.
It was quickly prioritized as a medium priority issue (as lints often are).

The issue reporter was quite [concerned][] that the issue reached the `rustc`
beta channel without being addressed, because they felt that the diagnostic text
could be misinterpreted as saying that the field in question *is* 100% dead
code, which would imply that it could be removed with no effect to the semantics
of the program.

[concerned]: https://github.com/rust-lang/rust/issues/81658#issuecomment-778067614

[Issue #81658]: https://github.com/rust-lang/rust/issues/81658

The issue reporter asked for the lint to be narrowed in scope to not fire in
these cases, as they are false positives, and the `rustc` compiler has a
principle of eschewing lints that signal false positives.

The observable behavior of lints is under the purview of the Rust language
design team. the issue was nominated for discussion at one of the team's weekly
triage meetings.

After some discussion, the conclusion of the language design team was that we
were not going to revise the lint itself to be more restrictive in when it fires.
{% marginblock %}
Of course the discussion was a bit more nuanced than what I've written here.
There was a bit more discussion about what the lint would do in an ideal world,
in terms of firing only on "truly dead" code,
but the realities of systems programming (as well as the halting problem) mean
we do not live in that ideal world.
{% endmarginblock %}
We saw the example code, and the collective reaction was
"the lint is fine here. The right thing for the developer to do in such cases is
to prepend an underscore to their field or variable name".

But when I was reviewing the issue comment thread after the meeting, something
resonated with me: The issue reporter pointed out another principle of `rustc`:

> If rustc warns about something, it should mention a clear way how to fix the warning.

I certainly agree with that. And, importantly: The end consensus of the language
team ("prepend an underscore") is *not* reflected **anywhere** in the diagnostic text issued
by `rustc`.

So, I posted a note saying we should fix the latter aspect of the diagnostic.
{% marginblock %}
It is almost always easier to fine-tune the english text of a diagnostic rather than
revise when the diagnostic fires. (Or, at least, it is less risky to our users,
in terms of what impact a mistake might have in each of the two cases.)
{% endmarginblock %}
It will be an easy bug to fix.
Its a good way for new contributors to get involved, and I'll mentor it.

Thus, these are my mentorship instructions.

# Mentorship Instructions

Our goal is to improve the diagnostic in this case to inform the user of their
options: If the field is truly unused, they can remove it. If the field is used
in some implicit way, then they can silence the lint itself, or they can prepend
its name with an underscore (which serves as a signal that the field has some
purpose, despite it not being read by expressions visible in the control flow of
the program).

## Test driven development

There are a number of example usages of these "unread fields" given in the
comment thread.

It would be a good idea to start by writing simple test cases based on those
examples. The main ones I see discussed in this thread and thus might deserve
specific focus (both in test cases and in potentially variations in the
diagnostic output) are these two:

 1. Values whose types implement `Drop`, and where that drop has some
     side-effect beyond the value itself (e.g. mutexes that unlock on drop).

 2. Unsafe code accesses, including (as a very important case), fields that end
    up being referenced by foreign code that we reach via the foreign function
    interface (FFI).

## Hacking on the diagnostic

Now that you have made tests, you should be able to run the compiler on them and
observe the current (sub-par) diagnostic output, which will say something like:

```
warning: field is never read: `s`
```

and

```
= note: `#[warn(dead_code)]` on by default
```

There are different strategies for tracking down where to look in the rustc
source for where these diagnostics are emitted.

{% marginblock %}
I do hold a special spot in my own heart for running `rustc`
under the [`rr` reversible debugger](https://rr-project.org/), a spot that is probably meme-worthy at
this point...
{% endmarginblock %}
Some bad initial ideas include:

 1. fire up `rustc` under a debugger and step through its behavior, or
 2. make a local build of `rustc` with debug logging [turned on][debug logging] and then use
    `RUSTC_LOG=debug` to observe *all* of that logging output.

[debug logging]: https://github.com/rust-lang/rust/blob/a4d9624242df6bfe6c0a298867dd2bd527263424/config.toml.example#L406

The two options above are both bad ideas because they will take forever to wade
through.

Here's a better idea, though primitive: search the source code for the
diagnostic.
{% marginblock %}
preferably with a fast tool like [`rg`](https://blog.burntsushi.net/ripgrep/)
that has better defaults, like automatically recursing through subdirectories of the source tree
without needing to pass an extra option asking for that behavior.
{% endmarginblock %}
Two immediate guesses for
patterns we can derive from the diagnostic are "dead_code" (the name of the
lint) and the text "field is never read:"

You'll quickly discover that grepping for the string "dead_code" yields a lot of
false-positives (because the lint is `#![allow]`'ed heavily in the Rust source
tree). You can filter many of those out with a regexp pattern like:
`[^\(]dead_code`. But even then, its a lot to sift through.

{% marginblock %}
At least, I saw
something on the order of 56 matches, and I think we can do better.
{% endmarginblock %}

We could try to refine the pattern further, but before we try that, lets try the
other pattern that we saw.


The second pattern, "field is never read", has far fewer false-positives:

```
rg "field is never read"

Grep finished with no matches found at Wed Mar 10 11:34:47
```

The problem now is that we don't have *any* positives. What happened?

Well, the diagnostic strings are constructed dynamically (so that they can
include information about the source code, like field names). And as part of
that construction, they are often abstracted in other ways, e.g. to treat the
word "field"/"variable" as just another parameter.

So, in this scenario, I recommend making the search pattern less specific: drop
the word "field":

```
rg "is never read"
rustc_passes/src/liveness.rs
1451:                                lint.build(&format!("value captured by `{}` is never read", name))
1480:                        format!("value passed to `{}` is never read", name)
1619:                format!("value assigned to `{}` is never read", name)

rustc_resolve/src/late.rs
2261:                    // Since this res is a label, it is never read.

Grep finished with matches found at Wed Mar 10 11:38:59
```

More promising, but if you look carefully, none of these strings could actually
yield the diagnostic we're seeking. So our pattern is *still* too specific.

Searching for "is never" will work, but it ends up yielding 53 matches (and 53&asymp;56, which I
rejected above as being too many to wade through).

So here's the trick: Include the Rust formatting curly braces in the search string:

```
rg "\} is never \{"
rustc_passes/src/dead.rs
580:                lint.build(&format!("{} is never {}: `{}`", descr, participle, name)).emit()

Grep finished with matches found at Wed Mar 10 11:41:54
```

Now we have found what is almost certainly the exact place where we need to edit
the code to improve the diagnostic in question.


## What kind of code to write

Here is my short-term advice:

It would be good to focus first on a simple patch that is easy to backport to
beta.
{% marginblock %}
It [turns out](https://zulip-archive.rust-lang.org/238009tcompilermeetings/27332weeklymeeting2021031154818.html#229869685)
that we are almost certainly going to revert [PR #81473](https://github.com/rust-lang/rust/pull/81473) rather than backport other fixes to beta, but the advice about making the first version here simple still holds.
{% endmarginblock %}
I.e. all the bells and whistles about inspecting `Drop` do not need to be in beta.

A simple general change to the diagnostic text is all I would want (and I would
expect the team to balk at approving a larger beta backport).

## After the code is written

You need to run your tests. You'll probably discover that even if the tests you
added pass (which is pretty unlikely, unless you are *very* good at predicting
how the diagnostic you added will actually get emitted), a whole bunch of other
tests are going to fail.

Review them. Most of them are going to be failing because the diagnostic output you added is not
expected by the test.

You could try to update all those tests by hand. But that is not a good use of developer effort.
Instead, use [`x.py test --bless`][] to automatically generate the new expected output, and then
manually review the output of `git diff` to double-check that the changes to the expected output 
all make sense.

[`x.py test --bless`]: https://rustc-dev-guide.rust-lang.org/tests/running.html#editing-and-updating-the-reference-files

## What next?

Ping me on Zulip if you want to help with this, and I'll be happy to provide
guidance (either here on github, or directly via text chat).
