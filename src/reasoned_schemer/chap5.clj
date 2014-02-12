(ns reasoned-schemer.chap5
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.chap2]
        [reasoned-schemer.chap3]
        [reasoned-schemer.chap4]))

; 9
(defn appendo*
  [l s out]
  (conde
   [(emptyo l) (== s out)]
   [s#
    (fresh [a d res]
      (conso a d l)
      (appendo* d s res)
      (conso a res out))]))

; 10
(run* [x]
  (appendo
    '(cake)
    '(tastes yummy)
    x))

; 11
(run* [x]
  (fresh [y]

  (appendo*
    (list 'cake 'with 'ice y)
    '(tastes yummy)
    x)))

; 12
(run* [x]
  (fresh [y]
    (appendo* '(cake with ice cream)
      y
      x)))

; 13
(run 1
  [x]
  (fresh [y]
    (appendo* (llist 'cake 'with 'ice y) '(d t) x)))

(run 1
  [y]
  (fresh [x]
    (appendo* (llist 'cake 'with 'ice y) '(d t) x)))

(run 5
  [x]
  (fresh [y]
    (appendo* (llist 'cake 'with 'ice y) '(d t) x)))

; 19
(llist 'cake 'with 'ice '(_0 _1 _2))
(concat (llist 'cake 'with 'ice '(_0 _1 _2)) '(d t))

; 20
(run 5
  [x]
  (fresh [y]
    (appendo
      (llist 'cake 'with 'ice y)
      (llist 'd 't y)
      x)))

; 23
(run*
  [x]
  (fresh [y]
    (appendo* x y '(cake with ice d t))))

; Hangs
(run 7
  [q]
  (fresh [x y]
    (appendo* x y '(cake with ice d t))
    (== q [x y])))
(defn appendo**
  [l s out]
  (conde
    [(emptyo l) (== s out)]
    [s# (fresh [a d res]
          (conso a d l)
          (conso a res out)
          (appendo d s res))]))


(run 7
  [q]
  (fresh [x y]
    (appendo** x y '(cake with ice d t))
    (== q [x y])))

; 37
(run 7
  [r]
  (fresh [x y z]
    (appendo x y z)
    (== [x y z] r)))

(defn swappendo
  [l s out]
  (conde
    [(fresh [a d res]
      (conso a d l)
      (conso a res out)
      (swappendo d res out))]
    [(emptyo l) (== s out)]))
 
(run 1
  [z]
  (fresh [x y]
    (swappendo x y z)))



















