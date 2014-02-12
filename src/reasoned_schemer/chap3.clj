(ns reasoned-schemer.chap3
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.chap2]))


(defn listo
  [l]
  (conde
    ((emptyo l) s#)
    ((pairo l) (fresh [d]
                  (resto l d)
                  (listo d)))))


(run* [q]
  (listo 3)
  (== q true))

(run* [q]
  (listo '(a b c))
  (== q true))

; 7
(run* [x]
  (listo ['a 'b' x 'd]))

; 10
(run* [x]
  (listo (llist 'a 'b 'c x)))

(run 4 [x]
  (listo (llist 'a 'b 'c x)))

(defn lol?
  [l]
  "Returns true if seq of seqs"
  (loop
    [r l]
    (if (empty? r)
      true
      (and (seq? (first r)) (recur (rest r))))))

(defn lolo
  [l]
  (conde
    ((emptyo l) s#)
    ((fresh [a]
       (firsto l a)
       (listo a))
     (fresh [d]
       (resto l d)
       (lolo d)))))

; 21
; symbols are seqable??

(run* [q]
  (lolo '('a))
  (== true q))

(run* [q]
  (fresh [x y]
    (lolo '(3))
    (== true q)))

; 24
(run 5 [x]
  (lolo (list (list 'a 'b) (list 'c 'd) x)))


; 31
(defn twinso
  [s]
  (fresh [x y]
    (conso x y s)
    (conso x '() y)))


(run* [x]
  (== x (list 'foo 'foo))
  (twinso x))

; 33
(run* [z]
  (twinso (list z 'tofu)))


(defn twins*
  [s]
  (fresh [x]
    (== (list x x) s)))

(run* [x]
  (== x (list 'foo 'foo))
  (twins* x))

(defn loto
  [l]
  (conde
    ((emptyo l) s#)
    ((fresh [a]
       (firsto l a)
       (twinso a))
     (fresh [d]
       (resto l d)
       (loto d)))))

; 38
(run* [z]
  (loto (list '(g g) z)))

; 42
(run 5 [z]
  (loto (llist '(:g :g) z)))

; 48

(defn listofo
  [predo l]
  (conde
   ((emptyo l) s#)
   ((fresh [a]
      (firsto l a)
      (predo a))
    (fresh [d]
      (resto l d)
      (listofo predo d)))))

; 49
(run 3 (out)
  (fresh [w x y z]
    (== (llist '(g g) (list 'e w) (list x y) z) out)
    (listofo twinso out)))

; 54
(defn eq-firsto
  [l x]
  (firsto l x))

(defn membero*
  [x l]
  (conde
   ;; ((emptyo l) u#)
   ((eq-firsto l x) s#)
   ((fresh [d]
      (resto l d)
      (membero* x d)))))

; 57
(run* [q]
  (membero* 'olive '(virgin olive oil))
  (== true q))

; 76
(run 5 [l]
  (membero* 'tofu l))

(defn pmembero
  [x l]
  (conde
   ((eq-firsto l x) (resto l nil))
   (s# (fresh [d]
         (resto l d)
         (pmembero x d)))))

(run 5 [l]
  (pmembero 'tofu l))

(run* [q]
  (pmembero 'tofu (list 'a 'b 'tofu 'd 'tofu))
  (== true q))

(defn pmembero*
  [x l]
  (conde
    ; ((emptyo l) u#)
    ((eq-firsto l x) (resto l nil))
    ((eq-firsto l x) s#)
    ((fresh [d]
       (resto l d)
       (pmembero* x d)))))

(run* [q]
  (pmembero* 'tofu '(a b tofu d tofu))
  (== true q))

(defn pmembero**
  [x l]
  (conde
    ;((empto l) u#))
    ((eq-firsto l x) (resto l nil))
    ((eq-firsto l x)
      (fresh [a d]
        (resto l (llist a d))))

    (s# (fresh [d]
          (resto l d)
          (pmembero** x d)))))

(run* [q]
  (pmembero 'tofu '(a b tofu d tofu))
  (== true q))

(run 12 [l]
  (pmembero** 'tofu l))

(defn first-value
  [l]
  (run 1 [y]
    (membero y l)))


; 98 membero in reverse
(defn memberrevo
  [x l]
  (conde
    ;((emptyo l) u#))
    (s# (fresh [d]
          (resto l d)
          (memberrevo x d)))
    (s# (eq-firsto l x))))

; 100 not working as expected
(run* [x]
  (memberrevo x '(pasta e fagioli)))
















