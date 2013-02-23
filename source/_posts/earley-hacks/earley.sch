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
;; The *degree of ambiguity* of a sentence is the number of its
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

;; A Terminal is a Character or non-empty String.

;(define nul-char #\nul)
(define nul-char #\x22a3)

;; A GrammarTerminal is a Character other than nul-char.

(define (terminal? c)
  (or (char? c) (and (string? c) (not (zero? (string-length c))))))

;; Terminal Char -> Boolean
(define (terminal-matches? t c)
  (cond ((char? t) (char=? t c))
        ((string? t) (char=? (string-ref t 0) c))))

(define (grammar-terminal? c)
  (and (char? c) (not (char=? c nul-char))))

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

;; ProdSeq -> Boolean
(define (prodseq-null? ps)
  (or (null? ps)
      (let ((s (car ps)))
        (and (string? s)
             (zero? (string-length s))
             (prodseq-null? (cdr ps))))))

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

;; ProdSeq ProdSeq Nat -> Boolean
;; If the ps and ps* have equal k-prefixes, returns true.
(define (prodseq-equal-upto ps ps* k)
  (define (term-count tok)
    (cond ((string? tok) (string-length tok))
          ((char? tok)   1)
          ((symbol? tok) 1)))
  (let loop ((ps ps) (ps* ps*) (k k))
    (cond ((<= k 0)                                     #t)
          ((or (prodseq-null? ps) (prodseq-null? ps*))
           (and (prodseq-null? ps) (prodseq-null? ps*)))
          ;; Special case; short-cut when we have same token
          ((equal? (car ps) (car ps*))
           (loop (cdr ps) (cdr ps*) (- k (term-count (car ps)))))
          (else ;; Otherwise, fall through to generalized handler
           (let ((q (prodseq-first ps))
                 (qs (prodseq-rest ps))
                 (r (prodseq-first ps*))
                 (rs (prodseq-rest ps*)))
             (and (equal? q r)
                  (loop qs rs (- k 1))))))))

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

;; grammar-results-for : Grammar Nonterminal -> [Listof ProdSeq]
(define (grammar-results-for g sym)
  (map cddr (filter (lambda (rule) (eq? sym (production-lhs rule))) g)))

;; A PTerm is a Character
;; interpretation: non nul-char is string content;
;; nul-char marks end-of-string.
;;
;; A PProdElem is one of: Symbol Character.
;; A PProdSeq is a [Listof [Oneof PProdElem String]]

;; sentence? : PProdSeq -> Boolean
;; Returns true iff s is sentential; i.e. composed entirely of terminal elements.
(define (sentence? s)
  (null? (filter symbol? s)))

;; sentence->string : [Oneof String PProdSeq] -> String
;; requires: s is sentential.
(define (sentence->string s)
  (if (string? s)
      s
      (apply string-append (map (lambda (x) (if (char? x) (string x) x)) s))))

;; first-nonterm : PProdSeq -> Symbol
;; requires: s is non-sentential.
(define (first-nonterm s)
  (cond ((symbol? (car s))
         (car s))
        (else
         (first-nonterm (cdr s)))))

;; null-sentence? : PProdSeq -> Boolean
(define (null-sentence? s)
  (cond ((prodseq-null? s) #t)
        (else
         (cond ((and (string? (car s)) (zero? (string-length (car s))))
                (null-sentence? (cdr s)))
               (else
                #f)))))

;; take-prefix : [Oneof String PProdSeq] Nat -> PProdSeq
;; returns first k characters or symbols in s, or as many as s can supply.
(define (take-prefix s k)
  (cond ((string? s)
         (substring s 0 (min k (string-length s))))
        (else
         (let loop ((s s)
                    (k k) )
           (cond ((prodseq-null? s) s)
                 ((zero? k) '())
                 (else
                  (cond ((string? (car s))
                         (cond
                          ((zero? (string-length (car s))) (loop (cdr s) k))
                          (else (cons (string-ref (car s) 0)
                                      (loop (cons (substring (car s) 1 (string-length (car s))) (cdr s))
                                                   (- k 1))))))
                        (else
                         (cons (car s) (loop (cdr s) (- k 1)))))))))))

;; take-term-prefix : [Oneof String PProdSeq] -> PProdSeq
(define (take-term-prefix s)
  (cond ((string? s) s)
        (else (let loop ((s s))
                (cond ((prodseq-null? s) s)
                      (else (cond ((symbol? (car s)) '())
                                  (else (cons (car s) (loop (cdr s)))))))))))

;; substitute-first : PProdSeq Nonterminal [Oneof String PProdSeq] -> PProdSeq
;; Returns new seq r = s[nt := t].  Does not assume nt appears in s.
(define (substitute-first s nt t)
  (let loop ((s s))
    (cond ((prodseq-null? s) s)
          (else
           (cond ((eq? nt (car s))
                  (cond ((string? t)
                         (cons t (cdr s)))
                        (else
                         (append t (cdr s)))))
                 (else
                  (cons (car s) (loop (cdr s)))))))))

;; A nicely rendered -|
(define left-tack-char #\x22a3)
;; A nicely rendered ->
(define right-arrow-char #\x2192)
;; A somewhat nicely rendered \phi
(define phi-char #\x03d5)

;; A Lookahead is a String

;; lookahead-matches? : Lookahead String Nat -> Boolean
(define (lookahead-matches? alpha input i)
  (let ((len (string-length alpha)))
    (let loop ((j 0))
      (cond ((= j len)
             #t)
            (else
             (let ((c (string-ref alpha j)))
               (or (and (char=? nul-char c)
                        (= (+ i j) (string-length input)))
                   (and (< (+ i j) (string-length input))
                        (char=? c (string-ref input (+ i j)))
                        (loop (+ j 1))))))))))

(define-values (cursor cursor-nonterm cursor-prev cursor-post cursor-equal?)
  (let ()

    ;; A ProductionCursor is a
    ;;   (list [Oneof Nonterminal #t] PProdSeq PProdSeq)

    (define (cursor nonterm prev post)
      (list nonterm prev post))
    (define (cursor-nonterm c) (list-ref c 0))
    (define (cursor-prev c) (list-ref c 1))
    (define (cursor-post c) (list-ref c 2))

    (let* ((cursor-rtd (make-record-type 'cursor '(nonterm prev post)))
           (cursor (record-constructor cursor-rtd))
           (cursor-nonterm (record-accessor cursor-rtd 'nonterm))
           (cursor-prev (record-accessor cursor-rtd 'prev))
           (cursor-post (record-accessor cursor-rtd 'post))
           (_ (rtd-printer-set! cursor-rtd
                                (lambda (c port)
                                  (let* ((d (lambda (x) (display x port)))
                                         (elem (lambda (e)
                                                 (let ((e (if (eqv? e nul-char)
                                                              left-tack-char
                                                              e)))
                                                   (d e))))
                                         (nonterm (cursor-nonterm c))
                                         (nonterm (if (eqv? nonterm #t)
                                                      phi-char
                                                      nonterm)))
                                    (display "#<cursor" port)
                                    (begin (d " ")
                                           (d nonterm)
                                           (d (string #\space
                                                      right-arrow-char
                                                      #\space))
                                           (for-each elem
                                                     (reverse (cursor-prev c)))
                                           (d ".")
                                           (for-each elem (cursor-post c))
                                           (d ">")))))))

      (define (cursor-equal? a b)
        (and (equal? (cursor-nonterm a) (cursor-nonterm b))
             (equal? (cursor-prev a) (cursor-prev b))
             (equal? (cursor-post a) (cursor-post b))))

      (values cursor cursor-nonterm cursor-prev cursor-post cursor-equal?))))

;; production->cursor : Production -> ProductionCursor
(define (production->cursor p)
  (cursor (production-lhs p)
          '()
          (production-rhs p)))

;; root->cursor : Nonterminal -> ProductionCursor
(define (root->cursor r)
  (cursor #t '() (list r nul-char)))

;; cursor-next : ProductionCursor -> ESym
(define (cursor-next c)
  (let ((p (cursor-post c)))
    (prodseq-first p)))

;; cursor-shift : ProductionCursor -> ProductionCursor
(define (cursor-shift c)
  (let* ((p (cursor-post c))
         (s (prodseq-first p))
         (r (prodseq-rest p)))
    (cursor (cursor-nonterm c) (cons s (cursor-prev c)) r)))

;; (Char -> Char) String -> String
(define (string-mapchar f s)
  (let* ((len (string-length s))
         (r (make-string len)))
    (let loop ((i 0))
      (cond ((= i len) r)
            (else
             (string-set! r i (f (string-ref s i)))
             (loop (+ i 1)))))))

;; A Position is a Nat
;; (may consider making it a (cons String Nat), but for now lets get by with Nat

;; A Source is an S-exp describing when a state is added.
;; A State is a (list ProductionCursor Position Lookahead Source)

(define-values (state state-cursor state-input-position state-successor state-source
                      state-equal?)
  (let ()
    (define (state cursor pos termseq source)
      (list cursor pos termseq source))
    (define (state-cursor s) (list-ref s 0))
    (define (state-input-position s) (list-ref s 1))
    (define (state-successor s) (list-ref s 2))
    (define (state-source s) (list-ref s 3))

    (let* ((state-rtd (make-record-type 'state '(cursor pos termseq source)))
           (state (record-constructor state-rtd))
           (state-cursor (record-accessor state-rtd 'cursor))
           (state-input-position (record-accessor state-rtd 'pos))
           (state-successor (record-accessor state-rtd 'termseq))
           (state-source (record-accessor state-rtd 'source))
           (_ (rtd-printer-set! state-rtd
                                (lambda (s port)
                                  (let ((d (lambda (x) (display x port)))
                                        (w (lambda (x) (write x port))))
                                    (d "#<state")
                                    (begin
                                      (d " ")
                                      (d (state-cursor s))
                                      (d " succ:")
                                      (d (string-mapchar
                                          (lambda (c)
                                            (cond
                                             ((eqv? c nul-char) left-tack-char)
                                             (else c)))
                                          (state-successor s)))
                                      (d " at:")
                                      (d (state-input-position s))
                                      (d " source: ")
                                      (d (state-source s)))
                                    (display ">" port))))))

      (define (state-equal? a b)
        (and (cursor-equal? (state-cursor a) (state-cursor b))
             (equal? (state-input-position a) (state-input-position b))
             (equal? (state-successor a) (state-successor b))))

      (values state state-cursor state-input-position state-successor state-source
              state-equal?))))

(define (state-final? s)
  (prodseq-null? (cursor-post (state-cursor s))))
(define (state-nonfinal? s)
  (not (state-final? s)))

(define (state-position-index s)
  (list-ref s 1))

(define (state-cursor-shift/! s source)
  (state (cursor-shift (state-cursor s))
         (state-input-position s)
         (state-successor s)
         source))

;; An [EquivPred X] is a (X X -> Boolean) that is an equivalence relation.

;; workset: [Listof X] -> [Worksetof X]
;; workset: [Listof X] [Listof X] -> [Worksetof X]
;; workset-maker: [EquivPred X] -> ([Listof X] [Listof X] -> [Worksetof X])
;; workset-todo: [Worksetof X] -> [Listof X]
;; workset-done: [Worksetof X] -> [Listof X]

;; workset-more-todo? : [Worksetof X] -> Boolean

;; workset-next/! : [Worksetof X] -> values: X [Worksetof X]
;; requires: (workset-more-todo? set)
;; Takes an element x off the to-do list; returns x and set with x on
;; done list.

;; One must use the workset-maker construction when records are elements of workset.
(define-values (workset workset-maker workset-todo workset-done
                        workset-more-todo? workset-next/!
                        workset-member? workset-contents workset-add/!)
  (let ()

    (define (writln x) '(begin (write x) (newline)))

    ;; A [Worksetof X] is a (cons [Listof X] [Listof X])
    ;; interpretation: A workset (todo . done) represents the set (todo U
    ;; done), where the elements of done have already been processed.
    (define (workset todo . args)
      (let ((done (if (null? args) '() (car args))))
        (list todo done)))

    (define (workset-todo w) (car w))
    (define (workset-done w) (cadr w))

    ;; This uses Larceny's records to make results more printable at repl.
    (let* ((workset-rtd (make-record-type 'workset '(equiv todo done)))
           (ctor (record-constructor workset-rtd))
           (workset-maker (lambda (equiv?)
                            (lambda (todo . args)
                              (let ((done (if (null? args) '() (car args))))
                                (ctor equiv? todo done)))))
           (workset (workset-maker equal?))
           (workset-equiv (record-accessor workset-rtd 'equiv))
           (workset-todo (record-accessor workset-rtd 'todo))
           (workset-done (record-accessor workset-rtd 'done))

           (_ (rtd-printer-set! workset-rtd
                                (lambda (w port)
                                  (display "#<workset todo:" port)
                                  (write (workset-todo w) port)
                                  (display " done:" port)
                                  (write (workset-done w) port)
                                  (display ">" port)))))

      (define (workset-more-todo? s) (not (null? (workset-todo s))))

      (define (workset-next/! set)
        (writln `(workset-next/! set: ,set))
        (let* ((equiv (workset-equiv set))
               (todo (workset-todo set))
               (done (workset-done set))
               (x (car todo)))
          (values x (ctor equiv (cdr todo) (cons x done)))))

      ;; workset-member? : X [Worksetof X] -> Boolean
      (define (workset-member? x set)
        (let ((= (workset-equiv set)))
          (or (memp (lambda (y) (= x y)) (workset-todo set))
              (memp (lambda (y) (= x y)) (workset-done set))
              #f)))

      ;; workset-contents : [Worksetof X] -> [Listof X]
      (define (workset-contents set)
        (append (workset-todo set) (workset-done set)))

      ;; workset-add/! : [Worksetof X] X -> [Worksetof X]
      (define (workset-add/! set x)
        (if (workset-member? x set)
            set
            (let* ((equiv (workset-equiv set))
                   (todo (workset-todo set))
                   (todo* ; (cons x todo))
                    (append todo (list x)))
                   (done (workset-done set)))
              (ctor equiv todo* done))))

      (values workset workset-maker workset-todo workset-done
              workset-more-todo? workset-next/!
              workset-member? workset-contents workset-add/!
              ))))


;; workset-add-all/! [Worksetof X] [Listof X] -> [Worksetof X]
(define (workset-add-all/! set l)
  (foldr (lambda (x set) (workset-add/! set x)) set l))

;; A StateSet is a [Worksetof State]

(define state-set
  (let ((workset (workset-maker state-equal?)))
    (define (state-set . states) (workset states '()))
    state-set))

;; state-set-add/! : l/StateSet State -> l/StateSet
(define (state-set-add/! set state)
  (workset-add/! set state))

;; earleycomp-maker : Grammar Nat -> (String -> EarleyComputation)
;; ec-stateset-update/! : EarleyComputation Nat StateSet -> EarleyComputation
;; ec-next-token/! : EarleyComputation -> EarleyComputation
(define-values (earleycomp-maker
                ec-grammar ec-lookahead ec-input ec-input-index
                ec-states-vector ec-expansion-map
                ec-stateset ec-stateset-update/! ec-next-token/!)
  (let ()

    ;; An EarleyComputation is a
    ;;
    ;;     (vector grammar lookahead input index states map)
    ;;
    ;; where grammar is a Grammar
    ;;       lookahead is a Nat
    ;;       input is a String
    ;;       index is a Nat
    ;;       states is a [Vectorof StateSet]
    ;;       map is a [Maybe ExpansionMap]
    ;;
    ;; Invariant: if map non-false, then map = (build-expansion-map grammar k)
    ;;
    ;; Interpretation: A computation (vector G K STR STR-IDX STATESETS
    ;; M) is parsing STR with repsect to G, using K characters of
    ;; look-ahead.  It has built up STATESETSEQ, is currently looking
    ;; at STR[STR-IDX], and is processing workset STATESETS[STR-IDX].

    (define (ec g k str idx states mmap)
      (vector g k str idx states mmap))

    (define (ec-grammar c)          (vector-ref c 0))
    (define (ec-lookahead c)        (vector-ref c 1))
    (define (ec-input c)            (vector-ref c 2))
    (define (ec-input-index c)      (vector-ref c 3))
    (define (ec-states-vector c)    (vector-ref c 4))
    (define (ec-expansion-map c)    (vector-ref c 5))

    (let* ((ec-rtd (make-record-type 'earley-computation '(grammar
                                                           lookahead
                                                           input
                                                           input-index
                                                           states-vector
                                                           expansion-map)))
           (ec (record-constructor ec-rtd))
           (ec-grammar (record-accessor ec-rtd 'grammar))
           (ec-lookahead (record-accessor ec-rtd 'lookahead))
           (ec-input (record-accessor ec-rtd 'input))
           (ec-input-index (record-accessor ec-rtd 'input-index))
           (ec-states-vector (record-accessor ec-rtd 'states-vector))
           (ec-expansion-map (record-accessor ec-rtd 'expansion-map))
           (_ (rtd-printer-set! ec-rtd
                                (lambda (e port)
                                  (let ((d (lambda (x) (display x port)))
                                        (w (lambda (x) (write x port))))
                                    (d "#<earley-computation")
                                    (d " input:")
                                    (w (ec-input e))
                                    (d " i:")
                                    (w (ec-input-index e))
                                    (d " ")
                                    (d (ec-states-vector e))
                                    (d ">")))))
           )

      (define (ec-stateset c)
        (vector-ref (ec-states-vector c) (ec-input-index c)))

      ;; ec-stateset-update/! :
      ;;     EarleyComputation Nat StateSet -> EarleyComputation
      (define (ec-stateset-update/! c i states)
        (ec (ec-grammar c) (ec-lookahead c)
            (ec-input c) (ec-input-index c)
            (vector-replace/! (ec-states-vector c) i states)
            (ec-expansion-map c)))

      ;; ec-next-token/! : EarleyComputation -> EarleyComputation
      (define (ec-next-token/! c)
        (ec (ec-grammar c)
            (ec-lookahead c)
            (ec-input c)
            (+ 1 (ec-input-index c))
            (ec-states-vector c)
            (ec-expansion-map c)))

      (define (earley g k m str)
        ;; Not really happy about the +2 here; seems like n+1 statesets should suffice.
        (let ((v (make-vector (+ 2 (string-length str)) (state-set)))
              (root (grammar-root g)))
          (vector-set! v 0 (state-set (state (root->cursor root) 0 (make-string k nul-char) 'initial)))
          (ec g k str 0 v m)))

      (define (earleycomp-maker g k)
        (let ((m (build-expansion-map g k)))
          (lambda (str)
            (earley g k m str))))

      (values earleycomp-maker
              ec-grammar ec-lookahead ec-input ec-input-index ec-states-vector
              ec-expansion-map
              ec-stateset ec-stateset-update/! ec-next-token/!))))

;; vector-replace/! : &l/[Vectorof X] X nat -> &l/[Vectorof X]
(define (vector-replace/! vec i val)
  (let ((v (vector-copy vec)))
    (vector-set! v i val)
    v))

;; cons-union: X [Listof X] -> [Listof X]
(define (cons-union x r)
  (if (member x r)
      r
      (cons x r)))
  
;; separate-nullable-nonterminals : Grammar -> (list [Listof Nonterm] [Listof Nonterm])
;; returns ((S ...) (T ...)) where S =>* \empty and never T =>* \empty.
(define (separate-nullable-nonterminals grammar)
  (define (writln x) '(begin (write x) (newline)))

  (let ((nullable (let loop ((nullable '())
                             (g grammar))
                    (cond ((null? g) nullable)
                          (else
                           (let* ((rule (car g))
                                  (nt (production-lhs rule))
                                  (rhs (production-rhs rule)))
                             (writln `(loop rule: ,rule nt: ,nt rhs: ,rhs nullable: ,nullable))
                             (cond ((or (null-sentence? rhs)
                                        (andmap (lambda (x) (or (equal? x "") (member x nullable))) rhs))
                                    (writln `(nt: ,nt is nullable))
                                    (let ((n* (cons-union nt nullable)))
                                      (if (not (eq? n* nullable))
                                          (loop n* grammar)
                                          (loop n* (cdr g)))))
                                   (else
                                    (loop nullable (cdr g))))))))))
    (list nullable (foldr (lambda (x l) (if (member x nullable)
                                            l
                                            (cons-union x l)))
                          '()
                          (map production-lhs grammar)))))

;; ormap : (X -> Boolean) [Listof X] -> Boolean
(define (ormap f l)
  (cond ((null? l) #f)
        (else
         (or (f (car l))
             (ormap f (cdr l))))))

;; andmap : (X -> Boolean) [Listof X] -> Boolean
(define (andmap f l)
  (cond ((null? l) #t)
        (else
         (and (f (car l))
              (andmap f (cdr l))))))

;; partition : (X -> Boolean) [Listof X] -> (values [Listof X] [Listof X])
(define (partition f l)
  (let loop ((yes '()) (no '()) (l l))
    (cond ((null? l)
           (values (reverse yes) (reverse no)))
          (else
           (cond ((f (car l))
                  (loop (cons (car l) yes) no (cdr l)))
                 (else
                  (loop yes (cons (car l) no) (cdr l))))))))

;; uniq
(define (uniq l)
  (let loop ((new '())
             (l l))
    (cond ((null? l) (reverse new))
          (else
           (cond ((member (car l) new)
                  (loop new (cdr l)))
                 (else
                  (loop (cons (car l) new) (cdr l))))))))

;; grammar-rewrite-to-nonnullable : Grammar -> Grammar
;; Produces a grammar that generates the same language as input
;; grammar, but every production either rewrites to an empty right-hand-side
;; or to a string of non-nullable symbols.  (Furthermore, only the root
;; rule is allowed to have an empty right-hand-side.)
(define (grammar-rewrite-to-nonnullable grammar)
  (define (writln x) '(begin (write x) (newline)))

  (define nonterms (map symbol->string (map production-lhs grammar)))

  (define prime
     (let ((prime-suffix
            (let loop ((trial "^"))
              (if (ormap (lambda (x)
                           (let* ((len (string-length x))
                                  (suffix (substring x
                                                     (max 0 (- len (string-length trial)))
                                                     len)))
                             (string=? suffix trial)))
                         nonterms)
                  (loop (string-append trial "*"))
                  trial))))
       (lambda (sym)
         (string->symbol (string-append (symbol->string sym) prime-suffix)))))

  ;; Don't _need_ to generate fresh names for anything apart from the
  ;; root nonterminal, but doing so makes it clear when there has been
  ;; a change to the L(P) for a nonterminal P.
  '(define (prime sym)
     sym)

  (let* ((components (separate-nullable-nonterminals grammar))
         (nullable (car components))
         (nonnullable (cadr components)))

    (define (nullable? s) (member s nullable))
    (define (nonnullable? s) (member s nonnullable))


    (define (generate-all-nonnull-variants rhs)
      (let loop ((rhs rhs))
        (writln `(generate-all-nonnull-variants loop rhs: ,rhs))
        (cond ((null? rhs) (list rhs))
              (else
               (let* ((others (loop (cdr rhs)))
                      (s (car rhs)))
                 (cond ((or (terminal? s) (nonnullable? s))
                        (map (lambda (x) (cons s x)) others))
                       ((nullable? s)
                        (let ((s* (prime s)))
                          (append others (map (lambda (x) (cons s* x)) others))))))))))

    (let* ((g* (let loop ((g grammar)
                          (g* '()))
                 (writln `(loop g: ,g g*: ,g*))
                 (cond ((null? g)
                        g*)
                       (else
                        (let* ((rule (car g))
                               (nt (production-lhs rule))
                               (rhs (production-rhs rule))
                               (rhs*s (generate-all-nonnull-variants rhs)))
                          (writln `(rule: ,rule nt: ,nt rhs: ,rhs rhs*s: ,rhs*s))
                          (cond ((nullable? nt)
                                 (let ((nt* (prime nt)))
                                   (loop (cdr g)
                                         (append (map (lambda (rhs) `(,nt* -> ,@rhs)) rhs*s)
                                                 g*))))
                                ((nonnullable? nt)
                                 (loop (cdr g)
                                       (append (map (lambda (rhs) `(,nt -> ,@rhs)) rhs*s)
                                               g*)))))))))
           (g* (reverse g*))
           (orig-root (grammar-root grammar))
           (g* (filter (lambda (rule) (not (null-sentence? (production-rhs rule)))) g*))
           (g*  (cond ((nullable? orig-root)
                       (append `((,orig-root ->) (,orig-root -> ,(prime orig-root))) g*))
                      (else
                       g*))))
      g*)))

;; assq-all: [Listof (X . Y)] X -> [Listof (X . Y)]
(define (assq-all a-lst key)
  (filter (lambda (entry) (eq? (car entry) key)) a-lst))

;; A ExpansionMap is a:
;;   (list [Listof (list Nonterm Lookahead)]    ;; (D <=k-prefix-for-D)
;;         [Listof (list Nonterm Lookahead)])   ;; (D <=k-sentence-for-D)

;; cross-product: (X Y -> Z) [Listof X] [Listof Y] -> [Listof Z]
(define (cross-product op xs ys)
  (apply append (map (lambda (x) (map (lambda (y) (op x y)) ys)) xs)))

;; iterated-transform : (X -> X) Nat -> (X -> X)
(define (iterated-transform f n)
  (lambda (x)
    (let loop ((i n)
               (x x))
      (cond ((zero? i) x)
            (else (loop (- i 1) (f x)))))))

;; iterated-transform-until: (X -> X) (X -> Boolean) -> (X -> X)
(define (iterated-transform-until f p?)
  (lambda (x)
    (let loop ((x x))
      (cond ((p? x) x)
            (else (loop (f x)))))))

(define (expansion-map prefixes sentences)
  (list prefixes sentences))
(define (expansion-map-prefixes-for em nt)
  ; (write `(expansion-map-prefixes-for em: ,em nt: ,nt)) (newline)
  (map cadr (assq-all (car em) nt)))
(define (expansion-map-sentences-for em nt)
  (map cadr (assq-all (cadr em) nt)))

;; build-expansion-map : Grammar Nat -> ExpansionMap
(define (build-expansion-map grammar k)
  ;; An InterimMap is an associative list of
  ;;   (list Nonterm ProdSeq ProdSeq)
  ;;
  ;; interpretation: an imap (entry ...) represents the deduction that:
  ;; for every entry = (D alpha beta), D => beta =>* alpha.
  ;;
  ;; Possible generalization: replace the third `origin` member = beta with
  ;; the derivation beta => beta' => ... => alpha.

  (define entry-nonterm car)
  (define entry-rewrite cadr)
  (define entry-origin caddr)

  (define (writln x) '(begin (write x) (newline)))

  ;; map-entries : InterimMap Nonterminal -> [Listof ProdSeq]
  (define (map-entries imap nt)
    (map cadr (assq-all imap nt)))

  ;; map-incorporate : InterimMap Nonterm [Listof ProdSeq] ProdSeq -> [Maybe InterimMap]
  ;; If beta... has new prefixes, returns a new map with them added; otherwise #f.
  (define (map-incorporate imap nt beta... origin)
    (define (map-has-prefix-for imap A alpha)
      (ormap (lambda (entry)
               (let ((B (entry-nonterm entry)) (beta (entry-rewrite entry)))
                 (and (eq? A B) (prodseq-equal-upto alpha beta k))))
             imap))

    ;; map-incorporate-one : InterimMap Nonterm ProdSeq ProdSeq -> [Maybe InterimMap]
    (define (map-incorporate-one imap nt beta origin)
      (and (not (map-has-prefix-for imap nt beta))
           (cons (list nt beta origin) imap)))

    (let ((result (foldr (lambda (beta mmap)
                           (or (map-incorporate-one (or mmap imap) nt beta origin)
                               mmap))
                         #f
                         beta...)))

      (writln `(map-incorporate imap: ,imap nt: ,nt beta...: ,beta... result: ,result))

      result))

  (let* ((components (separate-nullable-nonterminals grammar))
         (nonnullable (cadr components))
         (nonnullable? (lambda (s) (memq s nonnullable)))

         (initial-imap  (map (lambda (rule) (list (production-lhs rule)
                                                  (production-rhs rule)
                                                  (production-rhs rule)
                                                  ))
                             grammar))

         ;; Intention: explore =>-tree to identify all intermediate
         ;; sentential k-prefixes

         (map2 (let loop ((imap initial-imap)
                          (cursor initial-imap))
                 ;; Strategy: For each entry (D alpha) in imap with
                 ;; leading nonterminal A and entries {beta ...} for A
                 ;; in imap: add {(D alpha[A := beta]) ...}  to imap.

                 ;; The "add" operation on an imap does the addition
                 ;; only if there is not already another entry with a
                 ;; matching k-prefix.

                 (cond ((null? cursor) imap)
                       (else
                        (let* ((entry (car cursor))
                               (D (entry-nonterm entry))
                               (alpha (entry-rewrite entry))
                               (origin (entry-origin entry))
                               (_ (writln `(loop imap: ,imap entry ,D =>* ,alpha)))
                               (mmap
                                (cond
                                 ((sentence? alpha) #f)
                                 (else
                                  (let* ((A (first-nonterm alpha))
                                         (beta... (map-entries imap A))
                                         (alpha*... (map (lambda (beta)
                                                           (substitute-first alpha A beta))
                                                         beta...))
                                         (dropped...
                                          (let loop2 ((alpha alpha))
                                            (cond ((sentence? alpha)
                                                   '())
                                                  ((nonnullable? (first-nonterm alpha))
                                                   '())
                                                  (else
                                                   (let ((drop (substitute-first alpha (first-nonterm alpha) '())))
                                                     (cons drop (loop2 drop))))))))

                                    (map-incorporate imap D
                                                     (append dropped... alpha*...)
                                                     origin))))))

                          (cond (mmap
                                 (writln `(mmap: ,mmap -> restart))
                                 (loop mmap mmap)) ;; restart
                                (else
                                 (writln `(mmap: ,imap -> next-rule))
                                 (loop imap (cdr cursor)))))))))
         (_ (writln `(map2: ,map2)))
         (expanded
          (map (lambda (entry)
                 (let ((D (entry-nonterm entry))
                       (alpha (entry-rewrite entry)))
                   (list D
                         (sentence->string (take-term-prefix alpha))
                         alpha
                         (entry-origin entry)
                         )))
               map2))
         (separate-entries
          (lambda ()
            (partition (lambda (expanded-entry) (sentence? (caddr expanded-entry)))
                       expanded)))
         (take-2   (lambda (x) (list (car x) (cadr x))))
         (take-3   (lambda (x) (list (car x) (cadr x) (caddr x))))
         (drop-3rd (lambda (x) (list (car x) (cadr x) (cadddr x))))
         (drop-alpha (lambda (expanded-entry) (drop-3rd expanded-entry)))
         (drop-alpha-and-origin take-2))
    (call-with-values separate-entries
      (lambda (sentences prefixes)
        (expansion-map (uniq (map drop-alpha-and-origin prefixes))
                       (uniq (map drop-alpha-and-origin sentences)))))))

;; find-prefixes : [Oneof Grammar ExpansionMap] Nat PProdSeq -> [Listof Lookahead]
;; returns list [alpha | alpha is terminal, |alpha| = k, and gamma =>* alpha ^ beta for some beta.
(define (find-prefixes g-or-em k gamma)
  (define em (if (symbol? (car (car g-or-em)))
                 (build-expansion-map g-or-em k)
                 g-or-em))

  (define (writln x) '(begin (write x) (newline)))

  (writln `(find-prefixes gamma: ,gamma k: ,k em: ,em))

  (let ((strings
         (let loop ((prefixes '(""))
                    (gamma gamma))
           (writln `(loop prefixes: ,prefixes gamma: ,gamma))
           (let* ((c (prodseq-first gamma))
                  (r (prodseq-rest gamma))
                  (c-additions
                   (cond ((char? c) (list (string c)))
                         ((symbol? c) (expansion-map-sentences-for em c)))))
             (writln `(loop prefixes: ,prefixes c: ,c r: ,r c-additions: ,c-additions))
             (cond
              ((prodseq-null? r) ;; last symbol in gamma
               (let ((c-prefixes
                      (cond ((char? c) '())
                            ((symbol? c) (expansion-map-prefixes-for em c)))))
                 (cross-product string-append
                                prefixes
                                (append c-prefixes c-additions))))
              (else
               (loop (cross-product string-append
                                    prefixes
                                    c-additions)
                     r)))))))
    (filter (lambda (x) (>= (string-length x) k))
            strings)))

(define (h-follow comp gamma)
  ;; (write `(h-follow comp: ,comp gamma: ,gamma)) (newline)
  (let* ((k (ec-lookahead comp))
         (prefixes (find-prefixes (ec-expansion-map comp)
                                  k
                                  gamma)))
    (map (lambda (str) (substring str 0 k))
         prefixes)))


;; Grammar Nat String -> EarleyComputation
(define (earley g k str)
  ((earleycomp-maker g k) str))

;; In earley70:
;; - X_1...X_n is the 1-indexed input string (length n).
;; - G is the grammar, with productions D_p -> C_p,1 ... C_p,|p|, where 1 <= p <= d-1
;; - p identifies a rule of the grammar,
;; - j is a cursor into the p's rule,
;; - f is a pointer into the input string (length n), where the search for this p began
;; - alpha is a k-length lookahead string,
;; - i is a cursor for the input string (length n) and state set S_i (indexed from 0 to n+1, inclusive)
;; - s = <p, j, f, alpha> is member of S_i we are currently processing

;; An EarleyStepResult is one of:
;; - EarleyComputation
;; - 'accept
;; - 'reject

;; earley-step : EarleyComputation -> EarleyComputation or 'accept or 'reject
(define (earley-step comp)
  (cond ((earley-acceptable? comp)
         'accept)
        ((earley-reject? comp)
         'reject)
        (else
         (let ((comp* (or (earley-predictor comp)
                          (earley-completor comp)
                          (earley-scanner comp))))
           (and comp*
                (if (workset-more-todo? (ec-stateset comp*))
                    comp*
                    (ec-next-token/! comp*)))))))

;; EarleyComputation -> Boolean
(define (earley-acceptable? comp)
  (define (writln x) (begin (write x) (newline) #t))
  (let ((X (ec-input comp))
        (i (ec-input-index comp)))
    (and (= i (string-length X))
         (let ((S_i+1 (vector-ref (ec-states-vector comp) (+ i 1))))
           (and (workset-more-todo? S_i+1)
                (let-values (((s S_i+1) (workset-next/! S_i+1)))
                  (and (not (workset-more-todo? S_i+1))
                       (eqv? #t (cursor-nonterm (state-cursor s))))))))))

(define (earley-reject? comp)
  (let ((X (ec-input comp))
        (i (ec-input-index comp))
        (S_i (ec-stateset comp)))
    (not (workset-more-todo? S_i))))

;; earley-predictor : EarleyComputation -> [Maybe EarleyComputation]
(define (earley-predictor comp)
  (define (writln x) (begin (write x) (newline)))

  (let ((X (ec-input comp))
        (i (ec-input-index comp))
        (S_i (ec-stateset comp)))
    (let-values (((s S_i) (workset-next/! S_i)))
      (let ((g (ec-grammar comp))
            (j (state-cursor s))
            (f (state-input-position s))
            (alpha (state-successor s)))
        (and (state-nonfinal? s)
             (nonterminal? (cursor-next (state-cursor s))) ;; C_p,j+1 is nonterminal
             ;; ==>
             (and (writln `(earley-predictor i: ,i c: ,(cursor-next (state-cursor s)))) #t)
             (let* ((c (cursor-next (state-cursor s)))
                    (rules (filter (lambda (p) (eq? c (production-lhs p))) g))
                    (suffix (cursor-post (cursor-shift (state-cursor s))))
                    (h (h-follow comp (append suffix (list alpha))))
                    (S_i* (foldr (lambda (beta set)
                                   (foldr (lambda (rule set)
                                            (writln `(earley-predictor add rule: ,rule i: ,i beta: ,beta))
                                            (state-set-add/!
                                             set
                                             (state (production->cursor rule) i beta 'predictor)))
                                          set
                                          rules))
                                 S_i
                                 h)))
               (ec-stateset-update/! comp i S_i*)))))))

;; earley-completor : EarleyComputation -> [Maybe EarleyComputation]
(define (earley-completor comp)
  (define (writln x) (begin (write x) (newline)))

  (let ((X (ec-input comp))
        (i (ec-input-index comp))
        (S_i (ec-stateset comp)))
    (let-values (((s S_i) (workset-next/! S_i)))
      (let ((g (ec-grammar comp))
            (j (state-cursor s))
            (f (state-input-position s))
            (alpha (state-successor s)))

        (and (state-final? s)
             ;; ==>
             (and (writln `(earley-completor i: ,i j: ,j f: ,f alpha: ,alpha)) #t)

             ;; earley70's phrasing makes it sound like lookahead
             ;; match is precondition for processing this state, but
             ;; keep in mind that even if it does not match, we should
             ;; still treat this state as processed and update cursors
             ;; accordingly (right?).

             (cond ((lookahead-matches? alpha X i)
                    (let* ((S_f (vector-ref (ec-states-vector comp) f))
                           (D_p (cursor-nonterm (state-cursor s)))
                           (S_i* (foldr (lambda (s* set)
                                          (let ((s** (state-cursor-shift/! s* 'completor)))
                                            (writln `(earley-completor consider ,s* D_p: ,D_p alpha: ,alpha for i: ,i))
                                            (cond ((and (not (prodseq-null? (cursor-post (state-cursor s*))))
                                                        (eq? D_p (prodseq-first (cursor-post (state-cursor s*)))))
                                                   (writln `(earley-completor add ,s** to i: ,i))
                                                   (state-set-add/! set s**))
                                                  (else
                                                   set))))
                                        S_i
                                        (workset-contents S_f))))
                      (ec-stateset-update/! comp i S_i*)))
                   (else
                    (ec-stateset-update/! comp i S_i))))))))

;; earley-scanner : EarleyComputation -> [Maybe EarleyComputation]
(define (earley-scanner comp)
  (define (writln x) (begin (write x) (newline)))

  (define (input-ref s i)
    (cond ((< i (string-length s))
           (string-ref s i))
          (else
           nul-char)))

  (let ((X (ec-input comp))
        (i (ec-input-index comp))
        (S_i (ec-stateset comp)))
    (let-values (((s S_i) (workset-next/! S_i)))
      (let ((g (ec-grammar comp))
            (j (state-cursor s))
            (f (state-input-position s))
            (alpha (state-successor s))
            (comp (ec-stateset-update/! comp i S_i)))


        (and (state-nonfinal? s)
             (terminal? (cursor-next (state-cursor s)))
             ;; ==>
             (and (writln `(earley-scanner i: ,i j: ,j f: ,f alpha: ,alpha)) #t)
             (if (terminal-matches? (cursor-next (state-cursor s))
                                    ;; earley70 uses X_{i+1} here, but it also uses
                                    ;; 1-indexed strings; so we use (string-ref X i).
                                    (input-ref X i))
                 (begin
                   (writln `(earley-scanner add ,(state-cursor-shift/! s 'scanner) to: ,(+ i 1)))
                   (ec-stateset-update/! comp (+ i 1) (state-set-add/!
                                                       (vector-ref (ec-states-vector comp) (+ i 1))
                                                       (state-cursor-shift/! s 'scanner))))
                 comp))))))

(display "Hello World")
(newline)

'(begin
  (display "Computing a*a 11 steps") (newline)
  (define a*a-11  ((iterated-transform earley-step 11) (earley AE 1 "a*a")))
  (display "Computing a*a 14 steps") (newline)
  (define a*a-14  ((iterated-transform earley-step 14) (earley AE 1 "a*a")))
  (display "Computing a*a 15 steps") (newline)
  (define a*a-15  ((iterated-transform earley-step 15) (earley AE 1 "a*a")))
  (display "Computing a*a 40 steps") (newline)
  (define a*a-40  ((iterated-transform earley-step 40) (earley AE 1 "a*a")))
  )

(define (earley-recognize grammar k str)
  ((iterated-transform-until earley-step symbol?)
   (earley grammar k str)))

(define (earley-compute grammar k str)
  (let* ((s earley-step)
         (s-pair (lambda (p) (cons (cdr p) (earley-step (cdr p))))))
    (car ((iterated-transform-until s-pair (lambda (p) (symbol? (cdr p))))
          (cons #f (earley grammar k str))))))

;; summarize-stateset : [Listof State] -> [Listof (list ProductionCursor [Listof Lookahead] Position)]
;; Collects together the states that have matching cursors and positions.
(define (summarize-stateset elems)
  (define (writln x) '(begin (write x) (newline)))
  (let* (;; PC = (list Position ProductionCursor)
         (pc-equal? (lambda (pc pc*)
                      (and (= (car pc) (car pc*))
                           (cursor-equal? (cadr pc) (cadr pc*)))))
         (pcmap (let loop ((elems elems)
                           (pcmap '())) ;; [Listof (cons PC [Listof Lookahead])]
                  (cond ((null? elems)
                         pcmap)
                        (else
                         (writln `(elem: ,(car elems) pcmap: ,pcmap))
                         (loop
                          (cdr elems)
                          (let* ((state (car elems))
                                 (cur (state-cursor state))
                                 (pos (state-input-position state))
                                 (suc (state-successor state))
                                 (pc (list pos cur)))
                            (writln `(cur: ,cur pos: ,pos suc: ,suc pc: ,pc))
                            (let loop* ((pcmap pcmap))
                              (cond ((null? pcmap)
                                     (list (list pc suc)))
                                    (else
                                     (let ((entry (car pcmap)))
                                       (cond ((pc-equal? pc (car entry))
                                              (cons
                                               (cons pc (cons suc (cdr entry)))
                                               (cdr pcmap)))
                                             (else
                                              (cons entry
                                                    (loop* (cdr pcmap))))))))))))))))
    (map (lambda (pc*l) (let ((p (caar pc*l)) (c (cadar pc*l)) (l (cdr pc*l)))
                          (list c l p)))
         pcmap)))
