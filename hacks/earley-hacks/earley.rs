mod pp {
    pub struct args<'self> {
        tok_join: &'self str,
        rule_join: &'self str
    }
    pub trait pretty {
        pub fn pp(&self, args:&args) -> ~str;
    }
}

mod gram {
    use pp::args;
    use pp::pretty;
    type nonterm = ~str;
    pub enum sym {
        nt (nonterm),
        term (~str)
    }
    pub struct rule(nonterm, ~[sym]);
    pub type grammar = ~[rule];

    impl pretty for grammar {
        pub fn pp(&self, args:&args) -> ~str {
            let mut ret = ~"";
            let mut seen = false;
            for self.each |a_rule| {
                match a_rule {
                    &rule(ref head, ref syms) => {
                        if seen { ret += args.rule_join }
                        let rhs = do syms.foldr(~"") |s, acc| {
                            match s {
                                &nt(ref name) => name + args.tok_join + acc,
                                &term(ref tok) => tok + args.tok_join + acc
                            }
                        };
                        let right_arrow = "\u2192";
                        ret += fmt!("%s %s %s", *head, right_arrow, rhs);
                        seen = true;
                    }
                }
            }
            ret
        }
    }
}

fn main() {
    use gram::*;
    use pp::pretty;
    use pp::args;
    let AE : grammar = ~[rule(~"E", ~[term(~"T")]),
                         rule(~"E", ~[nt(~"E"),term(~"+"),nt(~"T")]),
                         rule(~"T", ~[nt(~"P")]),
                         rule(~"T", ~[nt(~"T"),term(~"*"),nt(~"P")]),
                         rule(~"P", ~[term(~"a")])
                        ];
    let pp_args: args = args{tok_join: " ", rule_join: "\n"};
    io::println(fmt!("%s", AE.pp(&pp_args)));
}
