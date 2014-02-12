(ns reasoned-schemer.chap4
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.chap2]
        [reasoned-schemer.chap3]))

(defn eq-first?
  [l x]
  (clojure.core/= (first l) x))

(defn mem
  [x l]
  (loop [r l]
    (cond
      (empty? r) false
      (clojure.core/= (first r) x) r
      :else (recur (rest r)))))

(mem 'tofu '(a b tofu d peas e))

; 7
(defn memo
  [x l out]
  (conde
    ;((empty? l) u#)
    ((firsto l x) (== l out))
    (s# (fresh [d]
          (resto l d)
          (memo x d out)))))

; 10
(run 1
  [out]
  (fresh [x]
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))

; 11
(run 1
  [out]
  (fresh [x]
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))

; 12
(run*
  [r]
  (memo r
    '(a b tofu d tofu e)
    '(tofu d tofu e)))

; 13
(run*
  [q]
  (memo 'tofu '(tofu e) '(tofu e))
  (== true q))

; 14
(run*
  [q]
  (memo 'tofu '(tofu e) '(tofu))
  (== true q))

; 15
(run* [x]
  (memo 'tofu '(tofu e) (list x 'e)))

; 17
(run* [out]
  (fresh [x]
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))

; 18
(run 12
  [z]
  (fresh [u]
    (memo 'tofu (llist 'a 'b 'tofu 'd 'tofu 'e z) u)))

; 22
(defn rember
  [x l]
  (cond
    (empty? l) nil
    (eq-first? l x) (rest l)
    :else (lcons (first l) (rember x (rest l)))))

; tail recursive (almost working)
(defn rember*
  [x l]
  (loop [acc nil
         l* l]
    (cond
      (empty? l*) nil
      (eq-first? l* x) (conj acc (rest l*))
      :else (recur (vec (cons acc (list (first l*)))) (rest l*)))))

(rember 'peas '(a b peas d peas e))

(rember* 'peas '(a b peas d peas e))

; 24
(defn rembero*
  [x l out]
  (conde
    [(emptyo l) (== nil out)]
    [(eq-firsto l x) (resto l out)]
    [s# (fresh [res]
      (fresh [d]
        (resto l d)
        (rembero* x d res))
      (fresh [a]
        (firsto l a)
        (conso a res out)))]))

; 26
(defn rembero**
  [x l out]
  (conde
    [(emptyo l) (== nil out)]
    [(eq-firsto l x) (resto l out)]
    [(fresh [a d res]
      (resto l d)
      (rembero** x d res)
      (firsto l a)
      (conso a res out))]))

(defn rembero***
  [x l out]
  (conde
    [(emptyo l) (== nil out)]
    [(eq-firsto l x) (resto l out)]
    [(fresh [a d res]
      (conso a d l)
      (rembero*** x d res)
      (conso a res out))]))
; 30
(run 1
  [out]
  (fresh [y]
    (rembero* 'peas (list 'a 'b y 'd 'peas 'e) out)))

(run 2
  [out]
  (fresh [y]
    (rembero* 'peas (list 'a 'b y 'd 'peas 'e) out)))

; 31
(run* [out]
  (fresh [y z]
    (rembero* y (list 'a 'b y 'd z 'e) out)))

; 49
(run* [r]
  (fresh [y z]
    (rembero* y (list y 'd z 'e) (list y 'd 'e))
    (== (list y z) r)))

; 56
(run 13 [w]
  (fresh [y z out]
    (rembero* y (llist 'a 'b y 'd z w) out)))

; 68
(defn surpriso
  [s]
  (rembero* s '(a b c) '(a b c)))

(run* [r]
  (== 'd r)
  (surpriso r))

; 71 SAY WHAT
(run* [r]
  (surpriso r)
  (== r 'b))

(run* [r]
  (== 'b r)
  (surpriso r))

(run* [r]
  (surpriso 'b)
  (== true r))










