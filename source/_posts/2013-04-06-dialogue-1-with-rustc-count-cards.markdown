---
layout: post
title: "Dialogue 1 with rustc: count-cards"
date: 2013-04-06 12:41
comments: true
published: false
categories: rust
---

I know I promised a follow-up post to my previous post with [a small
Rust puzzle](2013/03/26/rusty-chain-puzzle-1/).  And hopefully I will
provide that within the next day or two (because if I don't do it by
then, it won't happen for probably another week).

But before I write up that post, I wanted to transcribe a series of
interactions I had with the Rust compiler, `rustc`, from immediately
*before* I encountered that puzzle..

<!-- more -->

My goal is largely to document
my own experience as a semi-infomed Rust newbie who has not yet
internalized all of the in's and out's of programming with owned
pointers.  (I think I am addressing this post to myself, the me from a
year or more in the future, when I have become more competent in the
language.  I want a reminder of what it was like when I was starting
out, because I have discovered over time that I forget what it was
like e.g. to be frustrated by Lisp/Scheme syntax, or by dynamic
dispatch, or a number of other semi-interesting language features that
I have now stashed away in my mental utility belt.)

The goal: make a procedure that counts the number of cards in a linked
list of playing cards.

Here is relevant portion of the module holding the data defintion for
cards:

```rust
#[ link(name = "cards", vers = "0.2" ) ];
pub enum card_tag { facedown, faceup }
pub enum card_suit { clubs, diamonds, hearts, spades }

pub struct card {
    tag: card_tag,
    suit: card_suit,
    rank: u8, // 1..13
    next: Option<~card>
}
```

(I have now opted to put this code into its own file; Rust does not
require one provide the linkage specification for a module, but if you
do not, `rustc` issues warning messages indicating the defaults it has
chosen, and I'd just as soon write out the linkage specification
as have to skim through the extra output from `rustc`.)

So, how about counting cards.  To put things in perspective, here are
two ways one might choose to write analogous code in Scheme: ```scheme
(require 'srfi-9)

;; A Tag is one of 'facedown or 'faceup
;; A Suit is one of 'clubs 'diamonds 'hearts or 'spades
;; A Rank is a number in [1,13]
;; A [Maybe X] is one of '() or X
;; A Card is a (make-card Tag Suit Rank [Maybe Card])
(define-record-type card
  (make-card tag suit rank next)
  card?
  (tag card-tag) (suit card-suit) (rank card-rank) (next card-next))

;; count-cards : [Maybe Card] -> Number
(define (count-cards pile)
  ;; Tail-recursive accumulator-based applicative implementation.
  (let loop ((pile pile) (accum 0))
    (cond ((card? pile) (loop (card-next pile) (+ accum 1)))
          ((null? pile) accum))))

;; count-cards : [Maybe Card] -> Number
(define (count-cards.v2 pile)
  ;; Imperative implementation.
  (let ((c 0) (pile pile))
    (let loop ()
      (cond ((card? pile)
             (set! c (+ c 1))
             (set! pile (card-next pile))
             (loop))))
    c))
```

Now, the above is pretty easy to translate into Rust if one is not
attempting to use owned-pointers (which was largely the point of the
exercise for me).  In particular, here is a translation of
`count-cards.v2` into Rust code using a card data definition with
shared-pointers instead of owned-pointers.

TODO

Here was my first draft in Rust; it looks somewhat like `count-cards.v2` above.

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 1:
fn count_cards(pile: Option<~card>) -> uint {
    let mut c = 0;
    let mut pile = pile;
    loop {
        match pile {
            Some(card) => { c += 1; pile = card.next },
            None => break
        }
    }
    return c;
}
```

Here's what `rustc` told me when I tried to compile the above:
```
count-cards.rs:10:43: 10:52 error: moving out of immutable field
count-cards.rs:10             Some(card) => { c += 1; pile = card.next },
                                                             ^~~~~~~~~
```

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 2:
fn count_cards(pile: Option<~card>) -> uint {
    let mut c = 0;
    let mut pile = pile;
    loop {
        match pile {
            Some(card) => { c += 1; pile = &card.next },
            None => break
        }
    }
    return c;
}
```

```
count-cards.rs:10:43: 10:53 error: mismatched types: expected `core::option::Option<~cards::card>` but found `&core::option::Option<~cards::card>` (expected enum core::option::Option but found &-ptr)
count-cards.rs:10             Some(card) => { c += 1; pile = &card.next },
                                                             ^~~~~~~~~~
```

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 3:
fn count_cards(pile_orig: Option<~card>) -> uint {
    let mut c = 0;
    // (alpha-rename for exposition; not required by rustc)
    let mut pile = &pile_orig;
    loop {
        match pile {
            Some(card) => { c += 1; pile = &card.next },
            None => break
        }
    }
    return c;
}
```

```
count-cards.rs:11:12: 11:25 error: mismatched types: expected `&core::option::Option<~cards::card>` but found enum or structure
count-cards.rs:11             Some(card) => { c += 1; pile = &card.next },
                              ^~~~~~~~~~~~~
```

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 4:
fn count_cards(pile_orig: Option<~card>) -> uint {
    let mut c = 0;
    // (alpha-rename for exposition; not required by rustc)
    let mut pile = &pile_orig;
    loop {
        match *pile {
            Some(card) => { c += 1; pile = &card.next },
            None => break
        }
    }
    return c;
}
```

```
count-cards.rs:11:44: 11:48 error: illegal borrow: borrowed value does not live long enough
count-cards.rs:11             Some(card) => { c += 1; pile = &card.next },
                                                              ^~~~
count-cards.rs:5:49: 16:1 note: borrowed pointer must be valid for the block at 5:49...
count-cards.rs:5 fn count_cards(pile_orig: Option<~card>) -> uint {
count-cards.rs:6     let mut c = 0;
count-cards.rs:7     // (alpha-rename for exposition; not required by rustc)
count-cards.rs:8     let mut pile = &pile_orig;
count-cards.rs:9     loop {
count-cards.rs:10         match *pile {
                  ...
count-cards.rs:10:8: 13:9 note: ...but borrowed value is only valid for the match at 10:8
count-cards.rs:10         match *pile {
count-cards.rs:11             Some(card) => { c += 1; pile = &card.next },
count-cards.rs:12             None => break
count-cards.rs:13         }
count-cards.rs:10:14: 10:19 error: moving out of dereference of immutable & pointer
count-cards.rs:10         match *pile {
                                ^~~~~
```

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 5 (restarting from version 3):
fn count_cards(pile_orig: Option<~card>) -> uint {
    let mut c = 0;
    // (alpha-rename for exposition; not required by rustc)
    let mut pile = &pile_orig;
    loop {
        match pile {
            &Some(card) => { c += 1; pile = &card.next },
            &None => break
        }
    }
    return c;
}
```

```
count-cards.rs:11:18: 11:23 error: by-move pattern bindings may not occur behind @ or & bindings
count-cards.rs:11             &Some(card) => { c += 1; pile = &card.next },
                                    ^~~~~
```

```rust
extern mod cards (vers = "0.2");
use cards::*;

// version 6:
fn count_cards(pile_orig: Option<~card>) -> uint {
    let mut c = 0;
    // (alpha-rename for exposition; not required by rustc)
    let mut pile = &pile_orig;
    loop {
        match pile {
            &Some(ref card) => { c += 1; pile = &card.next },
            &None => break
        }
    }
    return c;
}
```
