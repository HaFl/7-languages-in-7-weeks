(ns clojure-days.day-2)

;;; FIND

;; The implementation of some of the commonly used macros in the Clojure
;; language.

; Generally, see http://clojure.org/macros
; Other popular macros are: ns, for, ->, assert, ...  On the other hand `if`
; and `do` for example are primitives. However, many constructs which would
; have to be built-in in other languages are actually macros in clojure.
(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro with-out-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))

(defmacro with-open
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order."
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) close))))
    :else (throw (IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))


;; An example of defining your own lazy sequence
(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))
(take 10 (ints-from 7))


;; The current status of the defrecord and protocol features (these
;; features were still under development as this book was being
;; developed)

; defrecord: http://clojure.org/datatypes
; protocols: http://clojure.org/protocols



;;; DO
