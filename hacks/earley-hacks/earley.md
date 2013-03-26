SPPF Parsing from Cubic Earley Recognizer
=========================================

Elizabeth Scott 2008
--------------------

* A _context free grammar_ consists of a set *N* of non-terminal
  symbols, a set *T* of terminal symbols, an element S ∈ *N* called
  the start symbol, and a set P of numbered grammar rules of the form
  "A ⩴ α" where A ∈ *N* and α is a (possibly empty) string of
  terminals and non-terminals.  The symbol ε denotes the empty string.

* A _derivation step_ is an element of the form γAβ ⇒ γαβ where γ and
  β are strings of terminals and non-terminals and A ⩴ α is a
  grammar rule.  A _derivation_ of τ from σ is a sequence of
  derivation steps σ ⇒ β_{1} ⇒ ... ⇒ B_{n-1} ⇒ τ.  We may
  also write σ ⇒* τ or σ ⇒ⁿ in this case.

* A _sentential form_ is any string α such that S ⇒* α, and a
  _sentence_ is a sentential form which contains only elements of *T*.
  The set, L(Γ), of sentences which can be derived from the start
  symbol of a grammar Γ, is defined to be the _language_ generated
  by Γ.

* A _derivation tree_ is an ordered tree whose root is labelled with
  the start symbol, leaf nodes are labelled with a terminal or ε and
  interior nodes are labelled with a non-terminal, A say, and have a
  sequence of children corresponding to the symbols on the right hand
  side of a rule for A.

* A _shared packed parse forest_ (SPPF) is a representation designed
  to reduce the space required to represent multiple derivation trees
  for an ambiguous sentence.  In an SPPF, nodes which have the same
  tree below them are shared and nodes which correspond to different
  derivations of the same substring from the same non-terminal are
  combined by creating a packed node for each _family_ of children.

  – To make it easier to determine whether two alernates can be packed
    under a given node, SPPF nodes are labellled with a triple (x,j,i)
    where a_{j+1},...,a_i is a substring matched by x.

  – _Binarised_ SPPFs contain intermediate additional nodes but are of
    worst case cubic size.  (The SPPF is said to be binarised because
    the additional nodes ensure that nodes whose children are not
    packed nodes have out degree at most two.)

* Earley's recognition algorithm constructs, for each position i in
  the input string a_1 ... a_n, a set of _items_.  Each item
  represents a position in the grammar that a top down parser could
  be in after matching a_1 ... a_i.

  – In detail, the set *E_0* is initially set to be the items
    (S ⩴ ∘ α, 0).  For i > 0, *E_i* is initially set to be the items
    (A ⩴ α a_i ∘ β, j) such that (A ⩴ α ∘ a_i β, j) ∈ *E_i-1*.

    The sets *E_i* are constructed in order and `completed' by adding
    items as follows: for each item (B ⩴ γ ∘ D δ, k) ∈ *E_i* and
    each grammar rule D ⩴ ρ, (D ⩴ ∘ ρ, i) is added to *E_i*, and
    for each item (B ⩴ ν ∘, k) ∈ *E_i*, if (D ⩴ τ ∘ B μ, h) ∈ *E_k*
    then (D ⩴ τ B ∘ μ, h) is added to *E_i*.
    The input string is in the language of the grammar if and only if
    there is an item (S ⩴ α ∘, 0) ∈ *E_n*.

* As an example consider the grammar:
     S ⩴ S T | a
     B ⩴ ε
     T ⩴ a B | a
  and input string "aa".  The Earley sets are:
     *E_0* = {(S ⩴ ∘ S T, 0), (S ⩴ ∘ a, 0)}
     *E_1* = {(S ⩴ a ∘, 0), (S ⩴ S ∘ T, 0), (T ⩴ ∘ a B, 1), (T ⩴ ∘ a, 1)}
     *E_2* = {(T ⩴ a ∘ B, 1), (T ⩴ a ∘, 1), (B ⩴ ∘, 2), (S ⩴ S T ∘, 0),
              (T ⩴ a B ∘, 1), (S ⩴ S ∘ T, 0), (T ⩴ ∘ a B, 2), (T ⩴ ∘ a, 2)}

* Earley's proposal for parser construction: maintain pointers from
  the non-terminal instances of the right hand sides of a rule in an
  item to the item that `generated' that item.  So, if
  (D ⩴ τ ∘ B μ, h) ∈ *E_k* and (B ⩴ δ ∘, k) ∈ *E_i*
  then a pointer is assigned from the instance of B on the left
  of the dot in (D ⩴ τ B ∘ μ, h) ∈ *E_i* to the
  item (B ⩴ δ ∘, k) ∈ *E_i*.

  (Scott points out at this point that to keep the set-cardinalities
   the same between parser and recognizer, the nonterminals are
   associated with a collection of pointers to its generating items,
   rather than just one pointer, which would necessitate adding the
   "same item, modulo pointers" multiple times to a set.  Felix wonders
   which approach Earley described, if any, or intended.  It seems
   possible that this is partially addressed in the paragraph after the
   S ⩴ SS | b example; in particular, it points out that if you start
   duplicating items because of pointer mismatches, you may break the
   O(n³) bound; see more on this below.)

* Continuing the example:

     *E_0* = {(S ⩴ ∘ S T, 0), (S ⩴ ∘ a, 0)}
     *E_1* = {#1=(S ⩴ a ∘, 0), (S ⩴ S[#1#] ∘ T, 0), (T ⩴ ∘ a B, 1), (T ⩴ ∘ a, 1)}
     *E_2* = {(T ⩴ a ∘ B, 1), #2=(T ⩴ a ∘, 1), #3=(B ⩴ ∘, 2),
              #4=(S ⩴ S[#1#] T[#2#, #5#] ∘, 0),
              #5=(T ⩴ a B[#3#] ∘, 1), (S ⩴ S[#4#] ∘ T, 0),
              (T ⩴ ∘ a B, 2), (T ⩴ ∘ a, 2)}

   From this structure the SPPF below can be constructed, as follows.

   ((S,0,2):u_0
    ((S,0,1):u_1
     (a,0,1):u_2)
    ((T,1,2):u_3)
     (packed #1=(a,1,2):u_4)
     (packed #1#
             ((B,2,2):u_5
              (ε))))

   (See text for description of how the above is constructed from the
    Earley pointer-annotated items.)

* But, if we consider the grammar
    S ⩴ S S | b
  and the input string "bbb" we find the above procedure generates
  the correct derivations of "bbb" but also spurious derivations of
  the strings "bbbb" and "bb".  The problem is that the derivation
  of "bb" from the left-most S in one derivation of "bbb" becomes
  intertwined with the derivation of "bb" from the rightmost S in the
  other derivation, resulting in the creation of "bbbb".

* One might try to work around this by duplicating items when they
  have different pointer destinations; this embeds all derivation
  trees in the construction (uh, and that's bad?  maybe Felix
  misunderstands "embed" here), and the size cannot be bounded by
  O(nᵖ) for any fixed integer p.

  – For example, the input "bⁿ" to the grammar S ⩴ SSS | SS | b
    will causes the set *E_i* to contain Ω(i³) items and thus the
    complete structure contains Ω(n^4) elements; thus this is not
    a cubic parser.

  - To see the above, note the recognizer constructs
      *E_i* == U_0 ∪ U_1 ∪ U_2 ∪ U_3 ∪ U_4 ∪ U_5
    where
      U_0 = {(S ⩴ b ∘, i-1), (S ⩴ ∘ S S S, i), (S ⩴ ∘ S S, i), (S ⩴ ∘ b, i)}
      U_1 = {(S ⩴ S ∘ S S, k) | i-1 ≥ k ≥ 0 }
      U_2 = {(S ⩴ S ∘ S, k) | i-1 ≥ k ≥ 0 }
      U_2 = {(S ⩴ S S ∘, k) | i-1 ≥ k ≥ 0 }
      U_4 = {(S ⩴ S S ∘ S, k) | i-2 ≥ k ≥ 0 }
      U_5 = {(S ⩴ S S S ∘, k) | i-3 ≥ k ≥ 0 }

    If we add pointers to the above, since there are i elements
      (S ⩴ S S ∘, q) in *E_i*, 0 ≤ q ≤ i-1, and
      (S ⩴ ∘ S S S, q) ∈ *E_q*,
    we will add i elements of the form (S ⩴ S ∘ S S, q) to *E_i*

    Then *E_q* will have q elements of the form
      (S ⩴ S ∘ S S, p), 0 ≤ p ≤ q-1,
    so we will add i(i-1)/2 elements of the form (S ⩴ S S ∘ S, r)
    to E_i, 0 ≤ r ≤ i-1.

    Finally, *E_q* will have q(q-1)/2 elements of the form
    (S ⩴ S S ∘ S, p), 0 ≤ p ≤ (q-1), so we will add
    i(i-1)(i-3)/6 elements of the form (S ⩴ S S S ∘, r) to *E_i*.

* Scott's contribution: Turn Earley's algorithm into a correct parser
  by (1.) adding pointers between items rather than instances of
  non-terminals, (2.) labelling the pointers in such a way which
  allows a binarised SPPF to be constructed by walking the resulting
  structure, and (3.) the walking algorithm that reconstructs the SPPF.

  She also provides (4.) a more efficient fused recognition+parsing
  algorithm that avoids the need to attach pointers to the items;
  Felix does not yet know whether to focus on (1-3.) or on (4.);
  probably simplest to read them both, and then implement just (1-3.)
  for now.

* Adding pointers between items:

  Set *E_0* to be the items (S ⩴ ∘ α, 0).  For i > 0 initialise E_i
  by adding the item p = (A ⩴ α a_i ∘ β, j) for each
  q = (A ⩴ α ∘ a_i β, j) ∈ *E_i-1* and, if α ≠ ε, creating a
  predecssor pointer labelled i-1 from q to p.

  Before initialising *E_i+1* complete *E_i*:
    For each item (B ⩴ γ ∘ D δ, k) ∈ *E_i* and
        each rule D :: ρ,
      (D ⩴ ∘ ρ, i) is added to *E_i*
    For each item t = (B ⩴ τ ∘, k) ∈ *E_i* and
        each corresp item q = (D ⩴ τ ∘ B μ, h) ∈ *E_k*,
      if there is no item p = (D ⩴ τ B ∘ μ, h) ∈ *E_i*
         then create one.
      Add a reduction pointer labelled k from p to t and, if τ ≠ ε, a
      predecessor pointer labelled k from p to q.

* The walk (is more complicated than the original example due to the
  non-binary nature of right hand sides of parse rules).

* The walk, Data Definitions:

  An _Interior Node_, u, is one of
  – Symbol node, with label (B, j, i)
  - Intermediate node, with label (B ⩴ γ x ∘ δ, j, i)

  A _Family of Children_ (or _Family_) of u will consist of one or two nodes.
  interpretation:
  A symbol node corresponds to the grammar rule B ⩴ γ y, or B ⩴ ε
  If γ ≠ ε then the children will be labelled (B ⩴ γ ∘ y, j, l)
  and (y, l, i) for some l.  Otherwise there will be a single child
  in the family, labelled (y,j,i) or ε.
  For an additional node, the family will have a child labelled (x, l, i).
  If γ ≠ ε then the family will have a second child labelled (B ⩴ γ ∘ x δ, j, l).

  (Aside from Felix: The above is a *terrible* data definition, and that
   terribleness is inherited from the original text.  Maybe this will all
   make more sense after I walk through the algorithm itself.)

* The Walk

