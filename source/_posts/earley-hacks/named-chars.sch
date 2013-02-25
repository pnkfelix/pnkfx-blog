;; This is a funky one: it puts an overline *centered* above the *two*
;; characters that precede it.  (You can use it multiple times to
;; affect >2 characters.  E.g. "baa\x0305;a\x0305;" is "baaa" with a
;; line covering the middle a and broaching into the a's on the end.
(define combining-overline-char #\x0305)

(define leftwards-arrow-char #\x2190)
(define upwards-arrow-char #\x2191)
(define rightwards-arrow-char #\x2192)
(define downwards-arrow-char #\x2193)
(define complement-char #\x2201)
(define there-does-not-exist-char #\x2204)
(define therefore-char #\x2234)
(define because-char #\x2235)
(define does-not-prove-char #\x22AC)
(define not-true-char #\x22AD)
(define nand-char #\x22BC)
(define nor-char #\x22BD)

(define diamond-operator-char #\x22C4)
(define star-operator-char #\x22C6)
(define white-medium-square-char #\x25FB)
(define white-square-char #\x25A1)

(define middle-dot-char #\x00b7)

;; A nicely rendered -|
(define left-tack-char #\x22a3)
;; A nicely rendered |-
(define right-tack-char #\x22a2)


;; A nicely rendered |=, aka \models
(define models-char #\x22a7)
;; A nicely rendered |=, aka \models
(define true-char #\x22a8)

;; A nicely rendered ||-
(define forces=char #\x22a9)
;; A nicely rendered ->
(define right-arrow-char #\x2192)
;; A nicely rendered =>
(define thick-right-arrow-char #\x21D2)

;; <=>
(define thick-leftright-arrow-char #\x21D4)
;; === with three lines
(define equiv-char #\x2261)
;; <->
(define leftright-arrow #\x2194)

;; A nicely rendered sideways hockey-stick 
(define logical-not-char #\x00AC)
(define reversed-not-char #\x2310)

(define top-left-corner-char #\x231C)
(define top-right-corner-char #\x231D)

(define left-ceiling-char #\x2308)
(define right-ceiling-char #\x2309)

;; A nicely rendered /\
(define wedge-char #\x2227)
;; A nicely rendered \/
(define vee-char #\x2228)
;; A nicely rendered (+)
(define oplus-char #\x2295)
;; An underlined \/
(define veebar-char #\x22BB)

;; T
(define top-char #\x22A4)
;; _|_
(define bot-char #\x22A5)

;; Upside-down A
(define forall-char #\x2200)
;; Backwards E
(define exists-char #\x2203)

;; A nicely rendered :=
(define definition-char #\x2254)

;; A somewhat nicely rendered \phi
(define phi-char #\x03d5)

;; A rendered sideways U (superset and also logical implication)
(define supset-char #\x2283)
