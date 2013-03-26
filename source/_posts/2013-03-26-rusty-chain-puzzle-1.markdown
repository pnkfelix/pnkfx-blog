---
layout: post
title: "Rusty Chain Puzzle 1."
date: 2013-03-26 10:28
comments: true
categories: rust
---

I have been trying to get my feet wet programming in
[Rust](http://www.rust-lang.org/).

A month and a half ago, I thought "Maybe I will hack up an Earley
parser in a variety of languages, including Rust."  That sent me down
a long path of learning about how Earley parsing works; I have not yet
written up my results from that investigation, and I still have not
written the Rust version of the code.

Last weekend, I sat down and said, "Let's try a simpler goal: A couple
simple exercies, maybe taken from Knuth's
[TAOCP](http://www-cs-faculty.stanford.edu/~uno/taocp.html)"
This was indeed a much simpler goal, but it was more difficult than
I had expected.

So, here is a Rust hacking puzzle that I struggled with.

I am representing piles of playing cards via linked structures.
Here are the data structure declarations:

```rust
enum card_suit { clubs, diamonds, hearts, spades }
struct card { suit: card_suit,
              rank: u8, // 1..13
              next: Option<~card> }
```

Note that the `next` field is an (optional) *owned* pointer to the
next card in the pile.  `Option<~card>` will be generally used to
represent a potentially empty pile (or "stack", "deck" or "hand", as
the context dictates), while `~card` is a non-empty pile (or, when its
`next` is `None`, a single card, again as context dictates)

The goal
--------
I want to write four functions: `place_top`, `place_bot`, `pop_top`,
and `pop_bot`, which respectively:

* `place_top(stack, c)` pushes a card `c` onto the top of the stack,
  represented by return the new top of the stack.

* `place_bot(stack, c)` places a card beneath the stack.  For an empty
  stack, the placed card is returned as the top of the newly formed
  stack; otherwise, the old stack top is returned (since the stack is
  imperatively modified).

* `pop_top(stack)` pops the top of the stack, returning a tuple of the
  popped card and the remaining, potentially empty stack.

* `pop_bot(stack)` removes the bottom of the stack (i.e. "deals from
  the bottom of the deck"), returning a tuple of the removed card and
  the new, potentially empty stack.

In code, here are the signatures for the desired functions,
as well as one-line reminders of the behavior for each.

```rust
// [c1, ..., cN], cX -> [cX, c1, ..., cN]
fn place_top(pile: Option<~card>, newcard: ~card) -> ~card;

// [c1, ..., cN], cX -> [c1, ..., cN, cX]
fn place_bot(pile: Option<~card>, newcard: ~card) -> ~card;

// [c1, c2, ..., cN] -> (c1, [c2, ..., cN])
fn pop_top(pile: ~card) -> (~card, Option<~card>);

// [c1, ..., cN-1, cN] -> (Some(cN), [c1, ..., cN-1])
fn pop_bot(pile: ~card) -> (~card, Option<~card>);
```

(Some non-critical helper infrastructure follows, showing off Rust as language)
-------------------------------------------------------------------------------

Here is some example code that puts together a hand and does
a few manipulations using the above operations (as well as
some printing routines to make looking at these cards nicer
in the terminal output)

```rust
fn make_hand() -> ~card {
    let hand = ~card { suit: clubs, rank: 10, next: None };
    let hand = ~card { suit: spades, rank: 3, next: Some(hand) };
    let hand = ~card { suit: diamonds, rank: 2, next: Some(hand) };
    hand
}

fn main() {
    let hand : ~card = make_hand();
    hand.report(~"initial hand: ");
    let AceD = ~card{ suit: diamonds, rank: 1, next: None };
    AceD.report(~"place top: ");
    let hand = place_top(Some(hand), AceD);
    hand.report(~"new hand: ");
    let SixD = ~card{ suit: diamonds, rank: 6, next: None };
    SixD.report(~"place bot: ");
    let hand = place_bot(Some(hand), SixD);
    hand.report(~"new hand: ");
    let (top, rest) = pop_top(hand);
    top.report(~"popped top: ");
    let hand = rest.unwrap();
    hand.report(~"new hand: ");
    let (bot, rest) = pop_bot(hand);
    bot.report(~"popped bot: ");
    let hand = rest.unwrap();
    hand.report(~"new hand: ");
}

// Below are "just" some notation niceties that should not effect
// the semantics of the code + algorithms above.

impl ToStr for card_suit {
    fn to_str(&self) -> ~str {
        match self { &spades   => ~"\u2664", &hearts   => ~"\u2665",
                     &diamonds => ~"\u2666", &clubs    => ~"\u2667" } }
}

fn rank_to_str(r:u8) -> ~str {
    match r {
        1     => ~"A",
        2..10 => r.to_str(),
        11    => ~"J",
        12    => ~"Q",
        13    => ~"K",
        _     => fail!()
    }
}

impl card {
    fn rank_to_str(&self) -> ~str { rank_to_str(self.rank) }
    fn report(&self, prefix: ~str) { io::println(prefix + self.to_str()); }
}

impl ToStr for card {
    fn to_str(&self) -> ~str {
        let mut ret = self.rank_to_str() + self.suit.to_str();
        match &self.next {
            &None => (),
            &Some(ref n) => ret = ret + "," + n.to_str()
        }
        ret
    }
}
```

In my terminal, I get the following output from the above `main`
function:

```
initial hand: 2♦,3♤,10♧
place top: A♦
new hand: A♦,2♦,3♤,10♧
place bot: 6♦
new hand: A♦,2♦,3♤,10♧,6♦
popped top: A♦
new hand: 2♦,3♤,10♧,6♦
popped bot: 6♦
new hand: 2♦,3♤,10♧
```


(I will post my initial "solution" to the puzzle in a follow-up post;
 I wanted to share this first because I know my current solution
 is non-optimal and wanted to see what others had to offer for how
 to solve this first.)
