(ns reasoned-schemer.chap1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

s#
u#

; 10
(run* [q] u#)

; 11
(run* [q]
  (== true q))

; 12
(run* [q]
  u#
  (== true q))

; 13
(run* [q]
  s#
  (== true q))

; 15
(run* [r]
  s#
  (== 'corn r))

; 17
(run* [r]
  u#
  (== 'corn r))

; 18
(run* [q]
  s#
  (== false q))

; 20
(run* [q]
  (== q true)
  (== false q))

; 21
(run* [q]
  (== q false)
  (== false q))

; 23
(run* [q]
  (fresh [x]
    (== true x)
    (== true q)))


; 29
(run* [x]
  (let [x false]
    (fresh [x]
      (== x true))))

; 30
(run* [r]
  (fresh [x y]
    (== [x y] r)))

; 31
(run* [s]
  (fresh [t u]
    (== [t u] s)))

; 32
(run* [r]
  (fresh [x]
    (let [y x]
      (fresh [x]
        (== [y x y] r)))))

; 33
(run* [r]
  (fresh [x]
    (let [y x]
      (fresh [x]
        (== [x y x] r)))))

; 34
(run* [q]
  (== false q)
  (== true q))

; 36
(run* [q]
  (let [x q]
    (== true x)))

; 37
(run* [r]
  (fresh [x]
    (== r x)))

; 38
(run* [q]
  (fresh [x]
    (== x q)
    (== true x)))

; 41
(cond
  false true
  :else false)

(run* [q]
  (cond
    false u#
    :else s#))


; 44
(run* [q]
  (conde
    (u# s#)
    (s# u#)))

; 46
(run* [q]
  (conde (s# s#)
         (s# u#)))

; 47
(run* [x]
  (conde
    ((== 'olive x) s#)
    ((== 'oil x) s#)
    (s# u#)))

; 48
(run 1 [x]
  (conde
    ((== 'olive x) s#)
    ((== 'oil x) s#)))

; 49
(run* [x]
  (conde
    ((== 'virgin x) u#)
    ((== 'olive x) s#)
    (s# s#)
    ((== 'oil x) s#)
    (:else u#)))

; 53
(run* [r]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (== r [x y])))

; 54
(run* [r]
  (fresh [x y]
    (conde
     ((== 'split x) (== 'pea y))
     ((== 'navy x) (== 'bean y)))
    (== [x y] r)))

; 55
(run* [r]
  (fresh [x y]
    (conde
      ((== 'split x) (== 'pea y))
      ((== 'navy x) (== 'bean y)))
    (== [x y 'soup] r)))

; 56
(defn teacupo
  [x]
  (conde
    ((== 'tea x) s#)
    ((== 'cup x) s#)))

(run* [r]
  (teacupo r))

; 57
(run* [r]
  (fresh [x y]
    (conde
      ((teacupo x) (== true y) s#)
      ((== false x) (== true y)))
    (== [x y] r)))

; 58
(run* [r]
  (fresh [x y z]
    (conde
      ((== y x) (fresh [x] (== z x)))
      ((fresh [x] (== y x)) (== z x)))
    (== [y z] r)))

; 59
(run* [r]
  (fresh [x y z]
    (conde
      ((== y x) (fresh [x] (== z x)))
      ((fresh [x] (== y x)) (== z x)))
    (== false x)
    (== [y z] r)))

; 60
(run* [q]
  (let [a (== true q)
        b (== false q)]
    b))

; 61
(run* [q]
  (let [a (== true q)
        b (fresh [x]
            (== x q)
            (== false x))
        c (conde
            ((== true q) s#)
            (s# (== false q)))]
    b))
