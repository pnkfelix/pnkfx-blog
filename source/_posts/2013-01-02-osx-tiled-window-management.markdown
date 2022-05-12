---
layout: post
title: "OS X tiled window management with Slate"
date: 2013-01-02 22:34
comments: false
categories: sysadmin macosx wm
---

I recently discovered [Slate] [1], an
open-source window-management tool for Mac OS X.

It is very cool, mainly in that it is very configurable (but with a
reasonably readable syntax for its configuration file).

<!-- more -->

Perhaps the most important thing to say is: Do not judge it solely
based upon its default configuration file, which is very bare bones
and does not illustrate anything near the full feature set that it
offers.

Instead, I recommend one at least read over Tristan Hume'e [blog post] [2],
which advertises Slate much more effectively than the project's
github page.  The blog post describes some of the crucial features that are not
exposed in the default configuration.  In particular, the
window-switcher shortcut, which overlays each window with a
letter to give that window focus, is much more tile-friendly
if you also turn on 

    config windowHintsIgnoreHiddenWindows false
    config windowHintsShowIcons true

You can see this and other custimzations I have made for myself
in my own `.slate` file, which I keep with my other dotfiles in
[my public repository] [3].

[1]: https://github.com/jigish/slate   "Slate: github repository"

[2]: http://thume.ca/howto/2012/11/19/using-slate/ "Tristan Hume: Using Slate"

[3]: https://github.com/pnkfelix/DotFiles/blob/master/slate "Felix's DotFiles github repo"
