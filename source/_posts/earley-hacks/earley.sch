;; Grammar AE:
;; E -> T
;; E -> E + T
;; T -> P
;; T -> T * P
;; P -> a
;; terminals: {a, +, *}
;; nonterminals: {E, T, P}
;; root: E

;; Define: alpha => beta :
;;   Exists gamma delta eta A such that:
;;     alpha = gamma A delta,
;;     beta = gamma eta delta, and
;;     A -> eta is a production.
;;
;; Define alpha =>* beta (beta is derived from alpha):
;;   Exists strings alpha_0, alpha_1, ..., alpha_m (m >= 0) such that:
;;     alpha = alpha_0 => alpha_1 => ... => alpha_m = beta
;;
;; A *sentential form* is a string alpha such that the root R =>* alpha
;;
;; A *sentence* is a sentential form consisting entirely of terminal symbols.
;;
;; A *language defined by a grammar* *L(G)* is the set of its sentences.
;;
;; Can represent any sentential form in at least one way as a *derivation tree*
;; (or *parse tree*) reflecting the steps made in deriving it (though not the
;; order of the steps).
;;
;; For example, in grammar AE, either derivation
;;   E -> E + T => T + T => T + P => T * P + P
;; or
;;   E => E + T => E + P => T + P => T * P + P
;; is represented by
;;   (E (E (T T * P)) + (T P))
;;
;; The *degree of ambiguity* of a setnence is the number of its
;; distinct derivation trees. A sentence is *unambiguous* if it has
;; degree 1 of ambiguity.  A grammar is *unambiguous* if each of its
;; sentences is unambiguous.  A grammar has *bounded ambiguity* if
;; there is a bound b on the degree of ambiguity of any sentences of
;; the grammar.  A grammar is *reduced* if every nonterminal appears
;; in some derivation of some sentence.
;;
;; A *recognizer* is an algorithm which takes as input a string and
;; either accepts it or rejects it depending on whether or not the
;; string is a sentence of the grammar.  A *parser* is a recognizer
;; which also outputs the set of all legal derivation trees for the
;; string.

;; A Terminal is a Character.

;; A GrammarTerminal is a Character other than #\nul.

(define (terminal? c) (char? c))

(define (grammar-terminal? c)
  (and (char? c) (not (char=? c #\nul))))

;; A Nonterminal is a Symbol or #t.

;; A GrammarNonterminal is a Symbol

;; A GSym is one of Nonterminal or GrammarTerminal.

;; A ESym is one of Nonterminal or Terminal

(define (nonterminal? c)
  (or (symbol? c) (eq? c #t)))

(define (grammar-nonterminal? c)
  (symbol? c))

;; A ProdElem is one of: Symbol Character.
;; A ProdSeq is a [Listof [Oneof String ProdElem]]
;; A TermSeq is a [Listof GrammarTerminal]

;; ProdSeq -> ProdElem
(define (prodseq-first ps)
  (let ((s (car ps)))
    (cond ((string? s)
           (cond ((zero? (string-length s))
                  (prodseq-first (cdr ps)))
                 (else
                  (string-ref s 0))))
          (else
           s))))

;; ProdSeq -> ProdSeq
(define (prodseq-rest ps)
  (let ((s (car ps)))
    (cond ((string? s)
           (cond ((zero? (string-length s))
                  (prodseq-rest (cdr ps)))
                 (else
                  (cons (substring s 1 (string-length s)) (cdr ps)))))
          (else
           (cdr ps)))))

;; A Production is a (cons Nonterminal (cons '-> ProdSeq))

;; production-lhs : Production -> Nonterminal
(define (production-lhs p)
  (car p))

;; production-rhs : Production -> ProdSeq
(define (production-rhs p)
  (cddr p))

;; A Grammar is a [Listof Production]
;; interpretation: The left-hand side of the first production in the
;;     list is the implicit root of the grammar.

(define AE
  '((E -> T)
    (E -> E "+" T)
    (T -> P)
    (T -> T "*" P)
    (P -> "a")))

;; grammar-root : Grammar -> Nonterminal
(define (grammar-root g)
  (let ((prod (car g)))
    (car prod)))

;; A PTerm is a Character
;; interpretation: non #\nul is string content;
;; #\nul marks end-of-string.
;;
;; A PProdElem is one of: Symbol Character.
;; A PProdSeq is a [Listof [Oneof PProdElem String]]
;; A Lookahead is a String

;; lookahead-matches? : Lookahead String Nat -> Boolean
(define (lookahead-matches? alpha input i)
  (let* ((first-nul (let loop ((i 0)) (cond ((= i (string-length alpha))
                                             #f)
                                            ((char=? #\nul (string-ref alpha i))
                                             i)
                                            (else
                                             (loop (+ i 1))))))
         (alpha (cond (first-nul (substring alpha 0 first-nul)) (else alpha)))
         (k (string-length alpha))
         (l (string-length input)))
    (and (<= (+ i k) l)
         (string=? alpha (substring input i (+ i k))))))

;; A ProductionCursor is a
;;   (list [Oneof Nonterminal #t] PProdSeq PProdSeq)

;; production->cursor : Production -> ProductionCursor
(define (production->cursor p)
  (list (production-lhs p)
        '()
        (production-rhs p)))

;; root->cursor : Nonterminal -> ProductionCursor
(define (root->cursor r)
  (list #t '() (list r #\nul)))

(define (cursor-nonterm c) (list-ref c 0))
(define (cursor-prev c) (list-ref c 1))
(define (cursor-post c) (list-ref c 2))

;; cursor-next : ProductionCursor -> ESym
(define (cursor-next c)
  (let ((p (cursor-post c)))
    (prodseq-first p)))

;; cursor-shift : ProductionCursor -> ProductionCursor
(define (cursor-shift c)
  (let* ((p (cursor-post c))
         (s (prodseq-first p))
         (r (prodseq-rest p)))
    (list (cursor-nonterm c) (cons s (cursor-prev c)) r)))

;; A Position is a Nat
;; (may consider making it a (cons String Nat), but for now lets get by with Nat

;; A State is a (list ProductionCursor Position Lookahead)

(define (state cursor pos termseq)
  (list cursor pos termseq))
(define (state-cursor s) (list-ref s 0))
(define (state-input-position s) (list-ref s 1))
(define (state-successor s) (list-ref s 2))

(define (state-final? s)
  (null? (cursor-post (state-cursor s))))
(define (state-nonfinal? s)
  (not (state-final? s)))

(define (state-position-index s)
  (list-ref s 1))

(define (state-cursor-shift/! s)
  (state (cursor-shift (state-cursor s))
         (state-input-position s)
         (state-successor s)))

;; A [Worksetof X] is a (cons [Listof X] [Listof X])
;; interpretation: A workset (todo . done) represents the set (todo U
;; done), where the elements of done have already been processed.

;; workset: [Listof X] -> [Worksetof X]
;; workset: [Listof X] [Listof X] -> [Worksetof X]
(define (workset todo . args)
  (let ((done (if (null? args) '() (car args))))
    (cons todo done)))

;; workset-more-todo? : [Worksetof X] -> Boolean
(define (workset-more-todo? s) (not (null? (car x))))

;; workset-next/!: [Worksetof X] -> values: X [Worksetof X]
;; requires: (workset-more-todo? set)
;; Takes an element x off the to-do list; returns x and set with x on
;; done list.
(define (workset-next/! set)
  (let* ((todo (car set))
         (done (cdr set))
         (x (car todo)))
    (values x (workset (cdr todo) (cons x done)))))

;; workset-member? : X [Worksetof X] -> Boolean
(define (workset-member? x set)
  (or (member x (car set))
      (member x (cdr set))
      false))

;; workset-contents : [Worksetof X] -> [Listof X]
(define (workset-contents set)
  (append (car set) (cdr set)))

;; workset-add/! : [Worksetof X] X -> [Worksetof X]
(define (workset-add/! set x)
  (if (workset-member? x set)
      set
      (let ((todo (car set))
            (done (cdr set)))
        (workset (cons x todo) done))))

;; A StateSet is a [Worksetof State]

(define (state-set . states) (workset states))

;; state-set-add/! : l/StateSet State -> l/StateSet
(define (state-set-add/! set state)
  (workset-add/! set state))

;; An EarleyComputation is a (vector Grammar Nat String Nat [Vectorof StateSet])
;; interpretation: A computation (list G K STR STR-IDX STATESETS) is
;; parsing STR with repsect to G, using K characters of look-ahead.
;; It has built up STATESETSEQ, is currently looking at STR[STR-IDX],
;; and is processing workset STATESETS[STR-IDX].

(define (ec g k str idx states)
  (vector g k str idx states))
(define (ec-grammar c)          (vector-ref c 0))
(define (ec-lookahead c)        (vector-ref c 1))
(define (ec-input c)            (vector-ref c 2))
(define (ec-input-index c)      (vector-ref c 3))
(define (ec-states-vector c)    (vector-ref c 4))

(define (ec-stateset c)
  (vector-ref (ec-states-vector c) (ec-input-index c)))

;; ec-stateset-update/! : EarleyComputation Nat StateSet -> EarleyComputation
(define (ec-stateset-update/! c i states)
  (ec (ec-grammar c) (ec-lookahead c)
      (ec-input c) (ec-input-index c)
      (vector-replace/! (ec-states-vector c) i states)))

;; ec-next-token/! : EarleyComputation -> EarleyComputation
(define (ec-next-token/! c)
  (ec (ec-grammar c)
      (ec-lookahead c)
      (ec-input c)
      (+ 1 (ec-input-index c))
      (ec-states-vector c)))

;; vector-replace/! : &l/[Vectorof X] X nat -> &l/[Vectorof X]
(define (vector-replace/! vec i val)
  (let ((v (vector-copy vec)))
    (vector-set! v i val)
    v))

;; Grammar Nat String -> EarleyComputation
(define (earley g k str)
  (let ((v (make-vector (string-length str) '()))
        (root (grammar-root g)))
    (vector-set! v 0 (state-set (state (root->cursor root) 0 (make-list k #f))))
    (ec g k str 0 v)))

;; earley-step : EarleyComputation -> EarleyComputation
(define (earley-step comp)

  ;; In earley70:
  ;; - X_1...X_n is the 1-indexed input string (length n).
  ;; - G is the grammar, with productions D_p -> C_p,1 ... C_p,|p|, where 1 <= p <= d-1
  ;; - p identifies a rule of the grammar,
  ;; - j is a cursor into the p's rule,
  ;; - f is a cursor into the input string (length n),
  ;; - alpha is a k-length lookahead string,
  ;; - i is a cursor for the input string (length n) and state set S_i (indexed from 0 to n+1, inclusive)
  ;; - s is member of S_i we are currently processing

  (let ((X (ec-input comp))
        (i (ec-input-index comp))
        (S_i (ec-stateset comp)))
    (let* ((comp*
            (let-values (((s S_i) (workset-next/! S_i)))
              (let ((g (ec-grammar comp))
                    (j (state-cursor s))
                    (f (state-input-position s))
                    (alpha (state-successor s)))
                (cond
                 ;; predictor:
                 ((and (state-nonfinal? s) (nonterminal? (cursor-next (state-cursor s))))
                  (let* ((c (cursor-next (state-cursor s)))
                         (rules (filter (lambda (p) (eq? c (cursor-nonterm j))) g))
                         (suffix (cursor-post (cursor-shift (state-cursor s))))
                         (h (h-follow comp (append suffix alpha)))
                         (S_i* (foldr (lambda (beta set)
                                        (foldr (lambda (rule set)
                                                 (state-set-add/!
                                                  set
                                                  (state (production->cursor rule) i beta)))
                                               set
                                               rules))
                                      S_i
                                      h)))
                    (ec-stateset-update/! comp i S_i*)))
                 ;; completor:
                 ((and (state-final? s) (lookahead-matches? alpha X i))
                  (let* ((S_f (vector-ref (ec-states-vector c) f))
                         (D_p (cursor-nonterm (state-cursor s)))
                         (S_i* (foldr (lambda (s* set)
                                        (workset-add/! set
                                                       (state-cursor-shift/! s*)))
                                      S_i
                                      (workset-contents S_f))))
                    (ec-stateset-update/! comp i S_i*)))
                 ;; scanner:
                 ((and (state-nonfinal? s) (terminal? (cursor-next (state-cursor s))))
                  (if (terminal-matches? (cursor-next (state-cursor s)) input (+ i 1))
                      (ec-stateset-update! comp (+ i 1) (workset-add/!
                                                      (vector-ref (ec-states-vector comp) (+ i 1))
                                                      (state-cursor-shift/! s)))
                      comp)))))))
      (if (workset-more-todo? (ec-stateset comp*))
          comp*
          (ec-next-token/! comp*)))))

(display "Hello World")
(newline)
