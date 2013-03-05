mod gram {
    type nonterm = ~str;
    enum sym {
        nt (nonterm),
        term (~str)
    }
    struct rule(nonterm, ~[sym]);
    type grammar = ~[rule];
}

fn main() {
    use gram::*;
    let AE : grammar = ~[rule(~"E", ~[term(~"T")]),
                         rule(~"E", ~[nt(~"E"),term(~"+"),nt(~"T")]),
                         rule(~"T", ~[nt(~"P")]),
                         rule(~"T", ~[nt(~"T"),term(~"*"),nt(~"P")]),
                         rule(~"P", ~[term(~"a")])
                        ];
    io::println(fmt!("Hello world %?", AE));
}