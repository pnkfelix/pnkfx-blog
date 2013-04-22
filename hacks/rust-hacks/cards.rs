#[ link(name = "cards", vers = "0.2" ) ];
pub enum card_tag { facedown, faceup }
pub enum card_suit { clubs, diamonds, hearts, spades }

pub struct card {
    tag: card_tag,
    suit: card_suit,
    rank: u8, // 1..13
    next: Option<~card>
}

impl card {
    fn rank_to_str(&self) -> ~str { rank_to_str(self.rank) }
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

impl ToStr for card_suit {
    fn to_str(&self) -> ~str {
        match self {
            // U+2660–2667
            &clubs    => ~"\u2663",
            &diamonds => ~"\u2662",
            &hearts   => ~"\u2661",
            &spades   => ~"\u2660"
        }
    }
}

impl ToStr for card {
    fn to_str(&self) -> ~str {
        let content = self.rank_to_str() + self.suit.to_str();
        match self.tag {
            faceup => content,
            facedown => ~"(" + content + ~")"
        }
    }
}
