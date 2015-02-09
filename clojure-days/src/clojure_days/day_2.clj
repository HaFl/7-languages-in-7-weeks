(ns clojure-days.day-2)

;;; FIND

;; The implementation of some of the commonly used macros in the Clojure
;; language.

; Generally, see http://clojure.org/macros
; Other popular macros are: ns, for, ->, assert, ...  On the other hand `if`
; and `do` for example are primitives. However, many constructs which would
; have to be built-in in other languages are actually macros in clojure.
(comment    ; to prevent WARNING
  (defmacro when
    "Evaluates test. If logical true, evaluates body in an implicit do."
    {:added "1.0"}
    [test & body]
    (list 'if test (cons 'do body))))

(comment    ; to prevent WARNING
  (defmacro with-out-str
    "Evaluates exprs in a context in which *out* is bound to a fresh
    StringWriter.  Returns the string created by any nested printing
    calls."
    {:added "1.0"}
    [& body]
    `(let [s# (new java.io.StringWriter)]
       (binding [*out* s#]
         ~@body
         (str s#)))))


;; An example of defining your own lazy sequence
(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))
(take 10 (ints-from 7))


;; The current status of the defrecord and protocol features (these
;; features were still under development as this book was being
;; developed)

; records:
; - http://clojure.org/datatypes
; - http://www.braveclojure.com/multimethods-records-protocols/
; Records are custom map-like data types (which implement abstractions e.g.,
; protocols). If you find yourself creating maps with the same fields over and
; over or if you would like to make use of protocols, then records are the way
; to go.

; protocols:
; - http://clojure.org/protocols,
; - http://stackoverflow.com/questions/
;     4509782/simple-explanation-of-clojure-protocols
; - http://www.braveclojure.com/multimethods-records-protocols/
; It's all about extensibility. Protocols allow to define abstractions (which
; is a named collection of operations) and to extend other abstractions (e.g.
; Java interfaces / classes) with those protocols (see extend-type,
; extend-protocol).
; Protocol methods are dispatched based on the type of the first argument (vs
; multimethods which dispatch based on multiple arguments). Turns out that the
; reason for protocols is performance as most host systems (e.g. JVM) have
; specialized high-performance support for dispatching only on the type of only
; the first argument.



;;; DO

;; Implement an unless with an else condition using macros.
(defmacro unless [condition body else-body]
  (list 'if (list 'not condition) body else-body))

(unless true (println "body") (println "else-body"))
(unless false (println "body") (println "else-body"))


;; Write a type using defrecord that implements a protocol.
(defprotocol Employee
  (nick [x] "The nickname of the Employee.")
  (joke [x] "Favorite joke of the Employee."))

(defrecord SoftwareEngineer [first-name last-name]
  Employee
  (nick [_]
    (clojure.string/lower-case (str (first first-name) last-name)))
  (joke [_]
    "How many Germans does it take to screw in a light bulb?\n\\
    One, they are efficient and not very funny."))

(nick (SoftwareEngineer. "Florian" "Hartl"))
