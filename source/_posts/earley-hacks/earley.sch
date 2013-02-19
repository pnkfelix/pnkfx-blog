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

;; A GrammarTerminal is a Character other than #\nul.

(define (terminal? c)
  (or (char? c) (and (string? c) (not (zero? (string-length c))))))

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

;; ProdSeq ProdSeq Nat -> Boolean
;; If the ps and ps* have equal k-prefixes, returns true.
(define (prodseq-equal-upto ps ps* k)
  (define (term-count tok)
    (cond ((string? tok) (string-length tok))
          ((char? tok)   1)
          ((symbol? tok) 1)))
  (let loop ((ps ps) (ps* ps*) (k k))
    (cond ((<= k 0)                                     #t)
          ((or (null? ps) (null? ps*))                  (and (null? ps) (null? ps*)))
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
;; interpretation: non #\nul is string content;
;; #\nul marks end-of-string.
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
  (cond ((null? s) #t)
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
           (cond ((null? s) s)
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
                (cond ((null? s) s)
                      (else (cond ((symbol? (car s)) '())
                                  (else (cons (car s) (loop (cdr s)))))))))))

;; substitute-first : PProdSeq Nonterminal [Oneof String PProdSeq] -> PProdSeq
;; Returns new seq r = s[nt := t].  Does not assume nt appears in s.
(define (substitute-first s nt t)
  (let loop ((s s))
    (cond ((null? s) s)
          (else
           (cond ((eq? nt (car s))
                  (cond ((string? t)
                         (cons t (cdr s)))
                        (else
                         (append t (cdr s)))))
                 (else
                  (cons (car s) (loop (cdr s)))))))))

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
    (list todo done)))

(define (workset-todo w) (car w))
(define (workset-done w) (cadr w))

;; workset-more-todo? : [Worksetof X] -> Boolean
(define (workset-more-todo? s) (not (null? (workset-todo s))))

;; workset-next/!: [Worksetof X] -> values: X [Worksetof X]
;; requires: (workset-more-todo? set)
;; Takes an element x off the to-do list; returns x and set with x on
;; done list.
(define (workset-next/! set)
  (let* ((todo (workset-todo set))
         (done (workset-done set))
         (x (car todo)))
    (values x (workset (cdr todo) (cons x done)))))

;; workset-member? : X [Worksetof X] -> Boolean
(define (workset-member? x set)
  (or (member x (workset-todo set))
      (member x (workset-done set))
      #f))

;; workset-contents : [Worksetof X] -> [Listof X]
(define (workset-contents set)
  (append (workset-todo set) (workset-done set)))

;; workset-add/! : [Worksetof X] X -> [Worksetof X]
(define (workset-add/! set x)
  (if (workset-member? x set)
      set
      (let ((todo (workset-todo set))
            (done (workset-done set)))
        (workset (cons x todo) done))))

;; workset-add-all/! [Worksetof X] [Listof X] -> [Worksetof X]
(define (workset-add-all/! set l)
  (foldr (lambda (x set) (workset-add/! set x)) set l))

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

;; build-prefix-map : Grammar Nat -> [Listof (list Nonterm Lookahead)]
(define (build-prefix-map grammar k)
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
    (map cadr (filter (lambda (entry) (symbol=? (car entry) nt)) imap)))

  ;; map-incorporate : InterimMap Nonterm [Listof ProdSeq] ProdSeq -> [Maybe InterimMap]
  ;; If beta... has new prefixes, returns a new maap with them added; otherwise #f.
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
                                 (loop imap (cdr cursor))))))))))
    (writln `(map2: ,map2))
    (map (lambda (entry)
           (let ((D (entry-nonterm entry))
                 (alpha (entry-rewrite entry)))
             (list D
                   (sentence->string (take-term-prefix alpha))
                   alpha
                   (entry-origin entry)
                   )))
         map2)))

;; find-prefixes : Grammar Nat PProdSeq  -> [Listof Lookahead]
;; returns list [alpha | alpha is terminal, |alpha| = k, and gamma =>* alpha ^ beta for some beta.
(define (find-prefixes g k gamma)
  (define (displn x) (display x) (newline))
  (define (writln x) (write x) (newline))

  (let loop ((sentences '())
             (set (workset (list gamma))))
    (writln `(sentences: ,sentences set: ,set))
    (cond ((workset-more-todo? set)
           (let-values (((e set) (workset-next/! set)))
             (let ((prefix (take-prefix e k)))
               (writln `(prefix: ,prefix))
               (cond ((and (sentence? prefix) (= k (length prefix)))
                      (writln `(adding ,e to sentences: ,sentences))
                      (loop (cons (sentence->string prefix) sentences) set))
                     (else
                      (let* ((nt (first-nonterm e))
                             (_ (writln `(nt: ,nt)))
                             (results (grammar-results-for g nt))
                             (substitutions (map (lambda (result)
                                                   (substitute-first e nt result))
                                                 results))
                             (filtered (filter (lambda (subst)
                                                 (let ((prefix (take-prefix subst k)))
                                                   (or (not (sentence? prefix))
                                                       (not (member (sentence->string prefix) sentences)))))
                                               substitutions)))
                        (loop sentences (workset-add-all/! set filtered))))))))
          (else
           sentences))))

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
