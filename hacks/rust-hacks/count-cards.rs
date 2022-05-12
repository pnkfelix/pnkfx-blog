//extern mod cards (vers = "0.2");
//use cards::*;

    pub enum card_tag { facedown, faceup }
    pub enum card_suit { clubs, diamonds, hearts, spades }

    pub struct card {
        tag: card_tag,
        suit: card_suit,
        rank: u8, // 1..13
        next: Option<@card>
    }

// version 0:
fn count_cards(pile: Option<@card>) -> uint {
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

/*
// version 1:
count-cards.rs:13:43: 13:52 error: moving out of immutable field
count-cards.rs:13             Some(card) => { c += 1; pile = card.next },
                                                             ^~~~~~~~~
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
*/

/*
// version 2:
count-cards.rs:32:43: 32:53 error: mismatched types: expected `core::option::Option<~cards::card>` but found `&core::option::Option<~cards::card>` (expected enum core::option::Option but found &-ptr)
count-cards.rs:32             Some(card) => { c += 1; pile = &card.next },
                                                             ^~~~~~~~~~
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
*/

/*
// version 3:
count-cards.rs:51:12: 51:25 error: mismatched types: expected enum or structure but found `&core::option::Option<~cards::card>`
count-cards.rs:51             Some(card) => { c += 1; pile = &card.next },
                              ^~~~~~~~~~~~~
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
*/

// Two ways to go from here: either change the patterns, or change the
// match input.  For my original go-round, I changed the match input.

/*
// version 4:
count-cards.rs:89:44: 89:48 error: illegal borrow: borrowed value does not live long enough
count-cards.rs:89             Some(card) => { c += 1; pile = &card.next },
                                                              ^~~~
count-cards.rs:83:49: 94:1 note: borrowed pointer must be valid for the block at 83:49...
count-cards.rs:83 fn count_cards(pile_orig: Option<~card>) -> uint {
count-cards.rs:84     let mut c = 0;
count-cards.rs:85     // (alpha-rename for exposition; not required by rustc)
count-cards.rs:86     let mut pile = &pile_orig;
count-cards.rs:87     loop {
count-cards.rs:88         match *pile {
                  ...
count-cards.rs:88:8: 91:9 note: ...but borrowed value is only valid for the match at 88:8
count-cards.rs:88         match *pile {
count-cards.rs:89             Some(card) => { c += 1; pile = &card.next },
count-cards.rs:90             None => break
count-cards.rs:91         }
count-cards.rs:88:14: 88:19 error: moving out of dereference of immutable & pointer
count-cards.rs:88         match *pile {
                                ^~~~~
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
*/

// And *then* I decided to try changing the pattern forms, after seeing how
// many issues came up with the deref on match input.

/*
// version 5 (restarting from version 3):
count-cards.rs:111:18: 111:23 error: by-move pattern bindings may not occur behind @ or & bindings
count-cards.rs:111             &Some(card) => { c += 1; pile = &card.next },
                                     ^~~~~
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
*/

/*
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
*/

fn make_hand() -> @card {
    let hand = @card { tag: facedown, suit: clubs, rank: 10, next: None };
    let hand = @card { tag: faceup, suit: spades, rank: 3, next: Some(hand) };
    let hand = @card { tag: faceup, suit: diamonds, rank: 2, next: Some(hand) };
    hand
}

fn main() {
    let hand = make_hand();
    io::println(fmt!("hand: %?", hand));
    let count = count_cards(Some(hand));
    io::println(fmt!("count: %?", count));
}