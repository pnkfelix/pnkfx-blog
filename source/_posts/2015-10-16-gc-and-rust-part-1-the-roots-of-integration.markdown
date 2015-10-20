---
layout: post
title: "GC and Rust Part 1: The Roots of Integration"
date: 2015-10-16 15:24
comments: true
categories:
published: false
---

This is the first in a series of posts will discuss why garbage
collection is hard, especially for Rust, and brainstorm about
solutions to the problems we face.

The relationship between garbage collection (GC) and the Rust
programming language has been an interesting one.

GC was originally deeply integrated into the language, complete with
dedicated syntax; then over time we found ways to lessen the
dependency on GC, and then finally remove it from the language
entirely.

However, we still want to provide support for garbage collection.

To explain why, I need to define the actual problems we seek to solve.

<!-- more -->

## What is Garbage Collection



## The Problem Space

I have identified two distinct kinds of support that we could provide:

  1. A pure Rust programming feature: We could add a collector
     interface, e.g. a `Gc<T>` type, that arbitrary library crates
     could use as they create or receive instances of `T`. The
     intention here would be similar to how `Rc<T>` is used: One does
     not want to track ownership precisely, but rather treat ownership
     as shared amongst all users of a value, and let the runtime
     system handle reclaiming the value.

     This kind of feature could be useful in any Rust library.

     However, it is also very difficult to add (at least with the
     ergonomics that one expects from a smart-pointer type like
     `Rc<T>`), and since `Rc<T>` is already a workable solution for
     many (though not all) use cases of `Gc<T>`, it is not the main
     priority right now.

  2. An interoperation feature: Provide introspective hooks to improve
     integration with application frameworks that are using their own
     garbage collector. An obvious example of this is Servo's use of
     the SpiderMonkey Virtual Machine for its Javascript support.

     Servo is relying on SpiderMonkey's garbage collection for memory
     management, not only for Javascript values, but even for
     [native-code DOM objects][servo post].

[servo post]: https://blog.mozilla.org/research/2014/08/26/javascript-servos-only-garbage-collector/

     That post describes (unchecked) scenarios where one can end up
     with dangling pointers -- that is, they invite unsoundness.
     Proper support for GC-interoperation in Rust could address this;
     I will discuss this further down in this post.

     Critically, GC-interoperation does not require the same level of
     programmer ergonomics that `Rc<T>` provides. For example, it is
     acceptable to not support [`Deref`][Deref trait].

[Deref trait]: https://doc.rust-lang.org/std/ops/trait.Deref.html

These are two distinct features; there is overlap between them, but
trying to find a single solution that solves both problems completely
may not be possible, and in any case we do not want to wait for it to
be discovered.

Let us assume that for the short term we are more interested in
providing GC-interoperation and are willing to forego GC as a pure
Rust programming feature.

<script src="/javascripts/viz.js" charset="utf-8"></script>

