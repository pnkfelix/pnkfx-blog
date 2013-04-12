---
layout: post
title: "Better command completion in bash on OS X"
subtitle: "(aka resolving light zsh envy)"
date: 2013-04-12 07:57
comments: true
categories: osx bash git super-user zsh-envy
---

(This post started as a personal e-mail to Niko, and then I figured it
was blog worthy.)

There's plenty of problems when working atop "OS X";
but no need to be jealous of Shu's zsh setup
(at least not for its tab-completion on git stuff),
at least not if you are already using Homebrew.
Just install
[brew's bash completion package][4]!

Executive summary: 

```bash
% brew install bash-completion

% echo >> ~/.bash_profile <<END
  if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
  fi
END
```

And voilà!

See also potentially related topics from [super user][1], [stack overflow][2], [milkbox blog][3].

----

Note also that I did burn myself by trying to get too smart: In
particular, after reading [stack overflow][2], I over-eagerly
attempted to address a purported problem by installing the homebrew
newer git instead of the default (older) built-in git installed by
Apple.

This was a little more painful than I expected, because there were a
bunch of git-related commands already in my `/usr/local/bin`, probably
I had likely already copied git to there once before by hand, and so
brew kept aborting the installation because it did not want to
overwrite the binaries that it was not already managing.  I think brew
was aborting the install in a sound transactional manner, but I am not
100% sure of that, because at least one point the command completion
stopped working and at that point I just

  1. gave up on understanding where everything came from,
  2. moved the non-brew git-related material
in `/usr/local/bin/` out of the way, and
  3. redid the `brew install git`

----

Anyway, I should also give a shout-out to Axel Hecht; his post
on Mozilla's Yammer instance is what got me to the point of even
attempting to install this piece of marvelousness.

(Also, further posts on yammer are lightly pushing for readers to consider
zsh as an alternative to bash.  I do not think I am ready to switch to zsh,
but I can at least link to the [blog post arguing for zsh][5].)



 [1]: http://superuser.com/questions/288438/bash-completion-for-commands-in-mac-os

 [2]: http://stackoverflow.com/questions/14970728/homebrews-git-not-using-completion

 [3]: http://milkbox.net/note/brace-completion-in-snow-leopard-upgrading-bash/

 [4]: http://blog.jeffterrace.com/2012/09/bash-completion-for-mac-os-x.html

 [5]: http://friedcpu.wordpress.com/2007/07/24/zsh-the-last-shell-youll-ever-need/
