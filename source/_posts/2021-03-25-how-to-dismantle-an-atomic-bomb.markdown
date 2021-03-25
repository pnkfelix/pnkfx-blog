---
layout: post
title: "How to dismantle an Atomic bomb"
date: 2021-03-25 15:22:22 -0400
comments: true
categories: rust
---

# The problem

Driving deallocation of state via ref-counting under our current atomic API means that you cannot avoid having a `&AtomicUsize` variable at the same time that you are deallocating the block of memory that contains the referenced atomic usize state.


# The proposal

The idea is simple.

Experienced Rustaceans know that in the general case, `&T` doesn't actually mean immutable; it merely signals that unsynchronized mutation through aliases is disallowed. Interior mutability is still an option, via `Cell` or `AtomicUsize`.

All such sources of interior mutability *are* tracked in the type system. Namely, they all have an `UnsafeCell<U>` somewhere in the structure layout that tells the compiler that such mutation is permitted on the `U` inside the unsafe cell. Thus the compiler knows it cannot make optimizations that rely on that `U` remaining invariant.

So, here's the suggested change: if code needs to allow for concurrent actors (or even aliasing accesses on the current thread) to mutate some state, that same code must allow that those accesses might *deallocate* that state.

<!-- more -->

{% marginblock %}
That is, "may be modified or deallocated" unless (of course) the compiler/author has some extra proof or established invariant that the memory M cannot be deallocated. Such invariants are the very basis of ref-counting, for example.
{% endmarginblock %}
In other words: in addition to saying that the content of an `&UnsafeCell<U>` might be modified via an alias, we also say that the memory holding the unsafe cell may even be outright *deallocated*, despite any extant `&`-references to that unsafe cell.

These statements about "what may happen" are talking about what reasoning you can employ locally, based on the type declarations.

# (Background) The Issue: Side-effects of atomic operations

The status quo: I will present a strawman atomic-reference counted type (analogous to `std::rc::Arc`).

It needs a heap-allocated payload, along with the reference count.

As an added part of this strawman presentation, I'm going to have the ref-counted object also carry a single-character label. The label will be used to illustrate potential program transformations (i.e. compiler optimizations) as we progress. (Furthermore, I will try to catch
use-after-free bugs resulting from these transformations by reallocating memory and scribbling on it before the label accesses.)

```rust
use std::sync::atomic::{self, AtomicUsize, Ordering};

type Data = [u8; 128];

struct LabelledPayload {
    label: char,
    #[allow(dead_code)]
    data: Data,
}

struct Inner {
    ref_count: AtomicUsize,
    payload: LabelledPayload,
}
```

`fn dealloc` is a small shim around Box-based deallocation to keep the examples cleaner; `fn scribble` is a routine that tries to catch us if we have a use-after-free.  by making a fresh allocation and overwriting its state.

```rust
unsafe fn dealloc(ptr: *const Inner) {
    drop(Box::from_raw(ptr as *mut Inner));
}

fn scribble() {
    Box::new(Inner { ref_count: AtomicUsize::new(0), payload: LabelledPayload { label: 'X', data: [0; 128], } });
}
macro_rules! scribble_then_println {
    ($template: expr, $label:expr) => {
        scribble();
        println!($template, $label);
    }
}
```

(In reality the `Payload` type would usually a generic type `T`, but this is detail we can side-step for this discussion.  The same issues with atomics still arise, and side-stepping generics simplifies the presentation in various ways with respect to issues of variance, `PhantomData` and `?Sized`.)

You will also need the handles that have (fractional) ownership of each heap-allocated `Inner`:


```rust
struct Handle {
    ptr: *const Inner,
}
```

(In practice the type of the `ptr` field would actually be `core::ptr::NonNull<Inner>`, but that is a niche optimization we can sidestep in this presentation.)

The allocation code and ref-count maintenance code would look
something like this

```rust
impl Handle {
    pub fn new(label: char, data: Data) -> Handle {
        let x = Box::new(Inner {
            ref_count: atomic::AtomicUsize::new(1),
            payload: LabelledPayload { label, data },
        });
        Self { ptr: Box::leak(x) as *const Inner }
    }
}

impl Clone for Handle {
    fn clone(&self) -> Handle {
        unsafe { (*self.ptr).ref_count.fetch_add(1, Ordering::Relaxed); }
        // (we'll skip preventing overflow; see Arc code for those details.)
        Handle { ptr: self.ptr }
    }
}
```

The troubles arrive with the destructor code that decrements the ref count and deallocates the heap-allocated value if the resulting ref-count is zero.

## Version 1: "inner-ref handled inline"

One "obvious way" to do it is with something like this:

```rust
#[cfg(feature = "version1")]
impl Drop for Handle {
    fn drop(&mut self) {
        let inner: &Inner = unsafe { &(*self.ptr) };
        let label: char = inner.payload.label;

        // reminder: concurrent actors may also be running drop, decrementing the ref-count...
        let pre_decr = inner.ref_count.fetch_sub(1, Ordering::Release);

        // ... so at this point in control flow, if `pre_decr > 1`,
        // then someone else might be freeing `*inner` (or have
        // already freed it). Now the only time it is safe to
        // access `*inner` is if we can prove we are sole remaining
        // owner (i.e. that `pre_decr == 1`).

        if pre_decr == 1 {
            unsafe { dealloc(inner as *const Inner); }
        }

        scribble_then_println!("version1 dropped handle to {}", label);
    }
}
```

This code is designed to deallocate the heap-allocated `Inner` on some paths. But at the same it does that, the local variable `inner: &Inner` is still in (lexical) scope.

Is *this* okay?

In general within this document, when I write "is *this* okay", it is meant as a rough short-hand for the following more elaborate text:

Does allowing a deallocation like *this* break any of:

{% marginblock %}
(Of course items (b.) and (c.) should in principle be derived from (a.); but we do not have a specification on hand, while we do have concrete examples of compiler optimizations and unsafe code that people write in practice, so that is why I try to spell them out as explicit concerns to keep in mind.)
{% endmarginblock %}
(a.) the memory model we hope to have for the language itself,
(b.) compiler optimizations today or tomorrow, or
(c.) `unsafe` code people write in practice today or tomorrow?


What is an example of a code transformation one might consider (for either compiler optimization or unsafe code authors)? Well: in version 1, we have code that looks like:
```

{
  let inner: &Inner = [...];
  let label: char = inner.payload.label;
  [...]
  println!("dropped handle to {}", label);
}
```

A reasonable person might say "clearly `*inner` has to be valid and immutable for its whole scope, and that `label` has only one use. So we should be able to transform it to this:

```
{
  let inner: &Inner = [...];                                
  [...] 
  println!("dropped handle to {}", inner.payload.label); 
}
```

{% marginblock %}
I tried to use a semi-novel name, specific to the example, because I don't want to get bogged down debating what optimization we're discussing, where it lives, and how it justifies its actions. So instead I'm choosing something relatively self-contained, easy-to-explain, and potentially incorrect.
{% endmarginblock %}
Let's call this the "Label Code Motion" transformation.

If version 1 of the `impl Drop for Handle` code is okay, then Label Code Motion cannot be applied here. Is *that* expected?

(Perhaps so; perhaps such code motion must be justified with deeper analysis of the other code I've elided here, rather than be justifed
by the types alone.)

(As you might have guessed, the whole reason I put in the `scribble_then_println!` macro was to illustrate that things *do* go
wrong if you apply the Label Code Motion transformation to version 1.
So *something* is wrong; later we will try to establish whether version 1 itself is to blame or the Label Code Motion transformation.)

Nonetheless, let us assume that version 1 is okay, at least going into the
discussion of version 2.

(We will revisit the question of the correctness of each variant at
the end of the document.)

## Version 2: "inner-ref handled out-of-line"

Consider this different factoring of the code:

```rust
#[cfg(feature = "version2")]
unsafe fn may_drop_inner<'a>(inner: &'a Inner) {
    let pre_decr = inner.ref_count.fetch_sub(1, Ordering::Release);
    // (at this point, `pre_decr > 1` ==> someone else might be freeing `*inner`)
    if pre_decr == 1 {
        unsafe { dealloc(inner as *const Inner); }
    }
}

#[cfg(feature = "version2")]
impl Drop for Handle {
    fn drop(&mut self) {
        unsafe {
            let inner: &Inner = &*self.ptr;
            let label: char = inner.payload.label;
            may_drop_inner(inner);
            scribble_then_println!("version2 dropped handle to {}", label);
        }
    }
}
```

Now we have `fn may_drop_inner` which takes a `&'a Inner` as a function parameter.

My own intuition about function arguments in such cases is that "obviously"
the value `*inner` has to live as long as the lifetime `'a`, and that lifetime "obviously" has to be longer than the invocation of `may_drop_inner(inner)`, right?

The intuition in the previous paragraph implies that Version 2 ("inner-ref handled out-of-line") is *not* correct.

So: Is my intuition wrong? (Maybe; we'll revisit that in a moment.)

If my intuition is right (and thus version 2 is not okay), then: Is the refactoring transformation from version 1 to version 2 wrong? Or does non-okay-ness for version 2 imply that version 1 is also not okay?

## Version 3: "atomic-ref handled inline"

Let us consider this variant on version 1.

```rust
#[cfg(feature = "version3")]
impl Drop for Handle {
    fn drop(&mut self) {
        let ref_count: &AtomicUsize = unsafe { &(*self.ptr).ref_count };
        let label: char = unsafe { (*self.ptr).payload.label };
        let pre_decr = ref_count.fetch_sub(1, Ordering::Release);
        // (at this point, `pre_decr > 1` ==> someone else might be freeing `*self.ptr`)
        if pre_decr == 1 {
            unsafe { dealloc(self.ptr); }
        }
        scribble_then_println!("version3 dropped handle to {}", label);
    }
}
```

Now, instead of holding a whole `&Inner` reference, we are holding "just" an `&AtomicUsize` reference. It still is a pointer to storage that is deallocated on some control-flow paths, but we again are careful to never dereference it after the decrement (because after the decrement, concurrent actors could deallocate `*self.ptr`).

Clearly if version 1 is okay, then version 3 should be too, right?

But what about the other direction: can version 3 be okay even if version 1 is not okay?  Well, notably, now one cannot apply the Label Code Motion optimization: at the point where `println!` is called, there is no `&Inner` in lexical scope to pull the label out of. So that may be evidence in favor of permitting version 3 while disallowing versions 1 and 2.

We have one more variant to consider in our matrix.

## Version 4: "atomic-ref handled out-of-line"

Now let us take the refactoring we saw when we made version 2, and apply that refactoring to version 3.

```rust
#[cfg(feature = "version4")]
unsafe fn may_drop_owner<'a>(ref_count: &'a AtomicUsize) -> usize {
    let pre_decr = ref_count.fetch_sub(1, Ordering::Release);
    // (at this point, `pre_decr > 1` ==> someone else might be freeing owner of `*ref_count`)
    return pre_decr;
}

#[cfg(feature = "version4")]
impl Drop for Handle {
    fn drop(&mut self) {
        let label: char;
        unsafe {
            let ref_count: &AtomicUsize = &(*self.ptr).ref_count;
            label = (*self.ptr).payload.label;
            let pre_decr = may_drop_owner(ref_count);
            if pre_decr == 1 {
                dealloc(self.ptr);
            }
        }
        scribble_then_println!("version4 dropped handle to {}", label);
    }
}
```

In short, we took the ref-count decrement and moved it into its own local function.

And we might take the same intuition about function arguments from version 2 and apply it to `fn may_drop_owner` in version 4: "doesn't `*ref_count` have to outlive the lifetime `'a`? How can we allow a concurrent actor to free `*ref_count` here?"

An important insight about version 4: the `fn may_drop_owner` is nearly a trivial wrapper around `AtomicUsize::fetch_sub`. So if `fn may_drop_owner` ends up classified as "not okay", then under what mental model is `AtomicUsize::fetch_sub` "okay"? (In other words, how can one write any helper functions if something this simple is not okay.)

## Version 5: "out-of-band ref-count"

In the discussion on [UCG#252][], Ralf pointed out other corner cases
one might consider. In particular, so far the cases I've shown here
have ref-count on the allocated memory block itself. But one can also imagine
scenarios where the ref-count is stored elsewhere.

However: I argue that an out-of-band ref-count does not hit the particular
problem I am concerned with. Since the ref-count is not stored with the heap-allocated
block, we are simply not going to encouter the issue described here.

(Such a system may encounter similar problems when it designs its
system for deallocating the out-of-band ref-counts themselves, but
without more concrete details about the proposal, it is hard to make
suggestions about how to handle it.)

See also: https://github.com/rust-lang/unsafe-code-guidelines/issues/88


## Summary of versions

Here's a matrix summarizing the first four variants I posted above.

|             |   in-line   | out-of-line |
| ----------- |   -------   | ----------- |
|  `&Inner`   | (version 1) | (version 2) |
| `&AtomicX`  | (version 3) | (version 4) |


## What options might we consider 

### Option 1. Outlaw them all

Preface: This space held my preferred solution when I first started seriously looking at this months ago (but don't worry, I end up arguing against these "outlaw them all" variants).

When I first looked at this problem, I thought the right answer would be to outlaw all four versions. That is, I thought my intuition about function arguments should extend to all lexical scopes for all `&T`, and therefore it was simply always wrong to let the ref-count decrement on something else in a `&T` (whether `T` is `Inner` or `AtomicUsize`) drive the deallocations shown above.

At that time, the "clear" answer to my point of view was that all four cases should instead have been manipulating either a `*const Inner` or a `*const AtomicUsize` at all times. (This would trivially disallow the Label Code Motion transformation, since no `&Inner` would be in scope at the point where `label` is printed.)

A critical problem with this point of view is that we don't offer any way for someone to decrement an atomic direectly on a `*const AtomicX` pointer.  Our atomics API for e.g. `AtomicUsize::fetch_sub` *forces* the developer to pass in a `&AtomicX`.

So we have to refine this option in some way. Here are two variants I considered.

#### 1a. Outlaw them all and stick with current atomic API

Since the API forces the developer to pass an `&AtomicUsize` to `fetch_sub`: Maybe the developer handles `*const AtomicX` in their code, but casts it via `as` back to `&AtomicX` at the `fetch_sub` calls? But then can the developer similarly cast the `*const Inner` to a `&Inner` and trust that the lifetimes will work out in their favor? (No, that doesn't work; one ends up in the same morass again.)

In short, I don't think this option is reasonable. Even if we could come up with a semantics for what people are supposed to do, I think it will be too confusing for people to understand what the ref-to-ptr and ptr-to-ref casts in each spot convey.

#### 1b. Outlaw them all and put in alternative atomic API

Why not have variants of `AtomicUsize::fetch_sub` and similar atomic methods that take a `*const AtomicUsize` as their formal parameter? If the standard library provided that, then the developer could be advised to only manipulate `*const Inner` or `*const AtomicUsize` in code like this, and they would be able to pass `*const AtomicUsize` to the relevant methods.

There's a host of answers as to why we wouldn't want to do this:

 * `*const`-pointers are hard to work with (e.g. we don't support `*const
self`-methods);
 * it would force us to duplicate a large portion of the API surface and maintain it,
 * we would have to bikeshed whether these methods take `*const` or `*mut`, and
 * perhaps most importantly, it would be a ongoing source for  confusion with users as to when they should use the `&AtomicX` methods versus when they should use the `*const AtomicX` methods,   (note in particular that we almost certainly wouldn't want to deprecate the `&AtomicX` variants in favor of the `*const AtomicX` variants).

So: Let us at least consider other options.

### Option 2. Allow them all

I wasn't going to talk about this option when I started writing, but after taking the time to develop the matrix of variants, I figure its worth at least talking about it.

If we were to allow all four variants, then the my own mental model about `&T` would be incorrect.  The Label Code Motion optimization could not be applied via type-based reasoning; thus, people (or compilers) who used such reasoning to justify such transformations would be performing an incorrect transformation.

When I first started writing this section, I wanted to include this assertion:

> I suspect very little Rust code used in production today would actually fall into that category of "unsound because incorrect reasoning was used to justify this code"

But I don't know if I believe that assertion anymore. Use-after-free bugs happen.  Code like the below (the result from applying Label Code Motion) is going to happen (and, as you might guess, you get a scribbled label due to the use-after-free here). But there's nothing in the *type structure* of how `Inner` holds its label field that would give you a hint that `inner.payload.label` could be invalid there.

```rust
#[cfg(feature = "locomotion")]
impl Drop for Handle {
    fn drop(&mut self) {
        let inner: &Inner = unsafe { &(*self.ptr) };
        let pre_decr = inner.ref_count.fetch_sub(1, Ordering::Release);
        if pre_decr == 1 {
            unsafe { dealloc(inner as *const Inner); }
        }
        scribble_then_println!("locomotion dropped handle to {}", inner.payload.label);
    }
}
```

I do assert that the mental model for what `&T` **means** becomes "quite subtle" (at best) if we allow all four variants.

I would like to do better.

### Option 3. Function boundaries are special

I put this in here because I figured there was some reason that [UCG#252][] was focused on function arguments in particular. That is also why I have the in-line vs out-of-line distiction.

[UCG#252]: https://github.com/rust-lang/unsafe-code-guidelines/issues/252

I think the heart of this would be something saying that `&'a T` where `'a` is a lifetime parameter to the function (or any lifetime that outlives the function, really) gets treated specially: The compiler gets to assume that in such cases, the `T` will not be deallocated via an alias.


For the matrix, here's the outcome of this option

|             |   in-line          | out-of-line        |
| ----------- |   -------          | -----------        |
|  `&Inner`   | version1: okay     | version2: not okay |
| `&AtomicX`  | version3: okay     | version4: not okay |

This isn't really satisfying, because it isn't actually coherent.

As discussed in the text below the presentation of version 4, we need to have some way to *at least* designate version4 as "okay", even if its via some opt-in attribute or special casing in the compiler. If we cannot do that, then we might as well go back to option 1 (outlawing them all).


In short: There's a reason [UCG#252][] is not trivial to resolve: in practice, we just cannot rule out all of the out-of-line column. 


### Option 4. Put deallocation on same footing as mutation

This option comes from a [UCG#252 comment][]. As RalfJung put it:

> this is the key new property that unsafe code would get with your
> proposal: if you are allowed to use a pointer for writes to some
> memory, you are also allowed to deallocate that memory.

[UCG#252 comment]: https://github.com/rust-lang/unsafe-code-guidelines/issues/252#issuecomment-709035408

This is the proposal I put at the outset of this document.

But what does it imply for our examples?

#### interpretation 4A: focus on Atomics alone

The proposal might be interpreted to mean:

> if you hold a `&A`, where `A` is `UnsafeCell<U>` or some simple (e.g. `#[repr(transparent)]`) wrapper
> around `UnsafeCell<U>` that merely adds alignment constraints (like `AtomicUsize`) then you must assume
> that aliasing accesses could deallocate the `UnsafeCell<U>`.

The main goal of this interpretation is to allow people to usually use their existing mental model of `&T`: if you have a `&T`, then in almost all cases you know its dereferencable for the entirety of its lexical scope. The only exception is when `T` itself is, in spirit, itself an `UnsafeCell`. Such cases are already a warning flag for developers: "here be dragons", and we would just be adding a new wrinkle to the dragon's scaly skin.

For the matrix, here's the outcome of this interpetation

|             |   in-line          | out-of-line        |
| ----------- |   -------          | -----------        |
|  `&Inner`   | version1: not okay | version2: not okay |
| `&AtomicX`  | version3: okay     | version4: okay     |


(again, "not okay" means it is unsound to write deallocation code in this manner.)

#### interpretation 4B: Blast radius includes Buddy fields

The proposal might be interpreted to mean:

> if you hold an `&T` where `T` has *any* `UnsafeCell<U>` within it,
> then you must assume that aliasing accesses could deallocate the
> whole `T`.

In other words, sibling fields like `.label` in our example above would be deemed as potential casualties of a potential deallocation, and thus the Label Code Motion optimization

I think this ends up meaning the same thing as "Option 2: Allow them all", except that we could at least say that my naive mental model about `&T` is applicable when `T` holds no `UnsafeCode<U>`

For the matrix, here's the outcome of this interpetation

|             |   in-line      | out-of-line    |
| ----------- |   -------      | -----------    |
|  `&Inner`   | version1: okay | version2: okay |
| `&AtomicX`  | version3: okay | version4: okay |


(This wouldn't be the end of the world, and in fact, it might be our only real choice, depending on how much code in the wild we might expect to be broken by interpretation A ("focus on Atomics"). In particular, Interpretation 4B would classify the `Arc` code in the standard library as "okay".)

### Option 5. Add marker to indicate that struct may be deallocated in a volatile manner.

This is a refinement of option 4.

Essentially: If there is too much fallout from following option 4 (variants 4A or 4B, either applies), then we could narrow the focus of the change by having type definitions *declare* that they have this behavior.

Under this option, a developer of our ref-counted type would be expected to state that this type is deallocated via a protocol that can fire at times unpredictable to the compiler.

This opt-in could take the form of an attribute on the `struct Inner` definition. Or perhaps it would be a trait that `Inner` implements.
Or maybe its a wrapper type, similar to `UnsafeCell<T>`, that would wrap around the `AtomicUsize`.

In any case, that would then be used as a marker telling the compiler whether the "Label Code Motion" optimization applies.

(Another way of seeing this: instead of inferring from the presence of `UnsafeCell` that such deallocation might happen, we would be adding an alternative marker that developers would have to know about and use correctly if they are implementing a memory-management scheme like this.)


### Conclusion

This is all a bunch of notes I wrote down to prepare for a language team meeting.
And at some point I wanted them to have executable code. (I use a literate programming system, tango, to write blog posts like this.)

And at some point after that, I realized this document was too long for the language team meeting.

So it became a blog post.

### Appendix


Below is some code to just try driving the above things, to test them out.


```rust
fn main() {
    let mut payload = [0u8; 128];
    let source = "Hello World".as_bytes();
    (&mut payload[..source.len()]).copy_from_slice(source);
    let h1 = Handle::new('h', payload);
    let _h2 = h1.clone();
    scribble_then_println!("h1 ref_count: {}", unsafe { (*h1.ptr).ref_count.load(Ordering::SeqCst) });
}
```

<!--

FYI here's how to run the thing on my local box:

```
(source ~/.cargo/env  && cd .. && for F in version{1,2,3,4,5} ; do cargo +nightly run --features $F  ; done )
```

-->
