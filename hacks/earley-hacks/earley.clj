(ns earley
  (:gen-class)
  :use clojure.set)

(def AE '((E -> T)
          (E -> E "+" T)
          (T -> P)
          (T -> T "*" P)
          (P -> "a")))

(defrecord workset [todo done])

(defrecord item [nonterm origin prefix current suffix])

;; Splits back into [c r] where c is first character we can find in
;; back (descending into strings if necessary), and r is the remainder
;; after splitting off that character.
;; Requires: back is in REC X : char or symboil or sequable(X).
(defn decompose-sym-back [back]
  (loop [back back
         others ()]
    (if-let [f (first back)]
      (cond (or (symbol? f) (char? f))
            [f (concat (rest back) others)]
            ;; Only other possibility (given require above): f is seqable.
            (empty? f)
            (recur (rest back) others)
            (string? f) ;; non-empty, since we checked empty? above
            [(first f) (concat (rest back) (rest f) others)]
            :else
            (recur f (concat (rest back) others)))
      nil)))

(deftype WorkSet [todo done]
  clojure.lang.ISeq
  (next [this]
    (if-let [n (next todo)]
      (WorkSet. n (conj done (first todo)))
      nil))
  (first [this]
    (if-let [f (first todo)]
      f
      nil))
  (more [this]
    (or (next this)
        '()))
  (cons [this obj]
    (cond (zero? (count obj this))
          (WorkSet (conj todo obj) done)
          :else
          this))
  (count [this]
    (+ (count todo) (count done)))
  (empty [this]
    (WorkSet. '() (union todo done))))

(deftype SymbolString [back]
  clojure.lang.ISeq
  (next [this]
    (if-let [[c r] (decompose-sym-back back)]
      (SymbolString. r)
      nil))
  (first [this]
    (if-let [[c r] (decompose-sym-back back)]
      c
      nil))
  (more [this]
    (or (next this)
        '()))
  (cons [this obj]
    (SymbolString. (conj back obj)))
  (count [this]
    (count back))
  (empty [this]
    (SymbolString. '()))
  (equiv [this obj]
    (= back obj))
  clojure.lang.Seqable
  (seq [this] (if (empty? this) nil this)))

(defrecord EarleyItem [nonterm origin prefix current suffix])

;; EarleyComp -> EarleyComp
(defn predict-step [ec]
  (let [{g :grammar in :input idx :index sets :worksets} ec
        {todo :todo done :done} (get sets idx)
        item (first todo)
        todo (rest todo)]
    
    '...))

(defn -main []
  (println "Hello world")
  (println AE)
  )
