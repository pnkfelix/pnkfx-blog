(ns fib
  (:gen-class))

(defn fib [n]
  (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(defn timeit [thunk]
  (let [start (. java.lang.System (clojure.core/nanoTime))
        result (thunk)
        finis (. java.lang.System (clojure.core/nanoTime))
        delta (- finis start)]
    [result delta]))

(defn report [expr result delta]
  (let* [dsecs (quot delta (long 10e9))
         dnanos (mod delta (long 10e9))
         dsuffix (quot dnanos (long 10e7))]
    (println (format "%s: %10d elapsed: %d.%02ds" expr result dsecs dsuffix))))

(defn -main []
  (let [[result1 delta1] (timeit (fn [] (fib 10)))
        [result2 delta2] (timeit (fn [] (fib 20)))
        [result3 delta3] (timeit (fn [] (fib 30)))
        [result4 delta4] (timeit (fn [] (fib 40)))
        [result5 delta5] (timeit (fn [] (fib 41)))
        [result6 delta6] (timeit (fn [] (fib 42)))
        [result7 delta7] (timeit (fn [] (fib 43)))
        ]
    (report "fib(10)" result1 delta1)
    (report "fib(20)" result2 delta2)
    (report "fib(30)" result3 delta3)
    (report "fib(40)" result4 delta4)
    (report "fib(41)" result5 delta5)
    (report "fib(42)" result6 delta6)
    (report "fib(43)" result7 delta7)
    ))
