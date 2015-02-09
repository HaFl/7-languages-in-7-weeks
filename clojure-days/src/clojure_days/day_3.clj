(ns clojure-days.day-3)

;;; FIND

;; A queue implementation that blocks when the queue is empty and waits for a
;; new item in the queue.

; clojure.lang.PersistentQueue:
;   - http://stackoverflow.com/questions/2760017/ \\
;     producer-consumer-with-qualifications
;   - https://github.com/clojure/clojure/blob/master/ \\
;     src/jvm/clojure/lang/PersistentQueue.java



;;; DO

;; Use refs to create a vector of accounts in memory. Create debit and credit
;; functions to change the balance of an account.
(def accounts (ref []))

(defn credit
  [accounts account-number amount]
  (map
    (fn [account]
      (if (= (:number account) account-number)
        (assoc account :balance (+ (:balance account) amount))
        account))
    accounts))

(defn debit
  [accounts account-number amount]
  (credit accounts account-number (- amount)))

(dosync (alter accounts conj {:number 123, :balance 100}
                             {:number 456, :balance 200}
                             {:number 789, :balance -100}))

(dosync (alter accounts debit 123 99))
(dosync (alter accounts debit 456 399))
(dosync (alter accounts credit 789 199))


;; In this section, I’m going to outline a single problem called sleeping
;; barber. It was created by Edsger Dijkstra in 1965. It has these charac-
;; teristics:
;; - barber shop takes customers.
;; - Customers arrive at random intervals, from ten to thirty millisec- onds.
;; - The barber shop has three chairs in the waiting room.
;; - The barber shop has one barber and one barber chair.
;; - When the barber’s chair is empty, a customer sits in the chair, wakes up
;;   the barber, and gets a haircut.
;; - If the chairs are occupied, all new customers will turn away.
;; - Haircuts take twenty milliseconds.
;; - After a customer receives a haircut, he gets up and leaves.
;; Write a multithreaded program to determine how many haircuts a bar- ber can
;; give in ten seconds.
