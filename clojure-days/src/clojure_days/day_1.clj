(ns clojure-days.day-1)

;;; FIND
;; Examples using Clojure sequences
(comment
  (not-empty [1 2])   ; see http://clojure.org/sequences
  (distinct [1 2 1]))

;; The formal definition of a Clojure function
; in repl: (source fn)

;; A script for quickly invoking the repl in your environment
; lein repl


;;; DO
;; Implement a function called (big st n) that returns true if a string
;; st is longer than n characters.
(defn big
  "Is string `st` longer than `n` characters?"
  [st n]
  (> (count st) n))

;; Write a function called (collection-type col) that returns :list,
;; :map, or :vector based on the type of collection col.
(defn collection-type
  "Returns :list, :map, or :vector based on the type of collection
  `col`."
  [col]
  (if (= (type col) clojure.lang.PersistentList) :list
    (if (= (type col) clojure.lang.PersistentVector) :vector
      (if (= (type col) clojure.lang.PersistentArrayMap) :map :unknown))))

;; improved version
(defn collection-type-improved
  "Returns :list, :map, or :vector based on the type of collection
  `col`."
  [col]
  (cond
    (list? col) :list
    (map? col) :map
    (vector? col) :vector
    :else :unknown))

(defn -main
  "Test both functions above."
  [& args]
  (println (big "" 3))
  (println (big "h" 3))
  (println (big "he" 3))
  (println (big "hell" 3))
  (println (big "hello" 3))

  (println (collection-type '(1 2)))
  (println (collection-type [1 2]))
  (println (collection-type {:key 1}))
  (println (collection-type "any string"))

  (println (collection-type-improved '(1 2)))
  (println (collection-type-improved [1 2]))
  (println (collection-type-improved {:key 1}))
  (println (collection-type-improved "any string")))
