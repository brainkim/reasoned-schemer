(ns reasoned-schemer.chap2
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

; 1
(let [x (fn [a] a)
      y 'c]
  (x y))

; 2
(run* [r]
  (fresh [y x]
    (== [x y] r)))

; 3
(run* [r]
  (fresh [v w]
    (== (let [x v
              y w]
          [x y])
        r)))

; 4
(first '(pizza grape raisin pear))

; 5
(first '(a c o r n))

; 6
(run* [q]
  (firsto '[a c o r n] q))

; 7
(run* [q]
  (firsto '[a c o r n] 'a)
  (== true q))

; 8
(run* [q]
  (fresh [x y]
    (firsto [q y] x)
    (== 'pear x)))

; 9
(defn caro
  [p a]
  (fresh [d]
    (== (lcons a d) p)))

(run* [q]
  (fresh [x y]
    (caro [q y] x)
    (== 'pear x)))

; 11
(run* [q]
  (fresh [x y]
    (firsto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) q)))

; 12
(rest '(grape raisin pair))

; 15
(run* [r]
  (fresh [v]
    (resto '(a c o r n) v)
    (firsto v r)))

; 16
(defn cdro
  [p d]
  (fresh [a]
    (== (lcons a d) p)))

(run* [r]
  (cdro '(a c o r n) r))

; 18
(run* [r]
  (fresh [x y]
    (resto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) r)))

; 20
(run* [q]
  (resto '(a c o r n) (cons q '(o r n))))

; 21
(run* [l]
  (fresh [x]
    (resto l '(c o r n))
    (firsto l x)
    (== 'a x)))

; 22
(run* [l]
  (conso '(a b c) '(d e) l))

; 23
(run* [x]
  (conso x '(a b c) '(d a b c)))

; 24
(run* [q]
  (fresh [x y z r]
    (== ['e 'a 'd x] r)
    (conso y ['a z 'c] r)
    (== q [x y z r])))

; 25
(run* [x]
  (conso x ['a x 'c] ['d 'a x 'c]))

; 26
(run* [l]
  (fresh [x]
    (== ['d 'a x 'c] l)
    (conso x ['a x 'c] l)))

; 27
(run* [l]
  (fresh [x]
    (conso x ['a x 'c] l)
    (== ['d 'a x 'c] l)))

; 28
(defn conso*
  [a d p]
  (== (lcons a d) p))

(run* [l]
  (conso* 'a ['b 'c 'd] [l 'b 'c 'd]))

; 29
(run* [l]
  (fresh [d x y w s]
    (conso w ['a 'n 's] s)
    (resto l s)
    (firsto l x)
    (== 'b x)
    (resto l d)
    (firsto d y)
    (conde
      ((== 'e y) s#)
      ((== 'l y) s#))))

; 32
(run* [q]
  (emptyo ['grape 'raisin 'pear])
  (== true q))

; 33
(run* [q]
  (emptyo [])
  (== true q))

; 34
(run* [x]
  (emptyo x))

; 35
(defn nullo [x]
  (== '() x))

(run* [x]
  (nullo [])
  (== true x))
(defn eqo
  [x y]
  (== x y))

(run* [q]
  (eqo 'pear 'plum)
  (== true q))

(defn pair? [x]
  (boolean (or (lcons? x) (and (coll? x) (seq x)))))
; 51
(lcons '(split) 'pea)

; 52
(run* [r]
  (fresh [x y]
    (== (llist x y 'salad) r)))

; 53
(defn pairo
  [p]
  (fresh [a d]
    (conso a d p)))

(run* [r]
  (pairo r))

; 54
(run* [q]
  (pairo (lcons q q))
  (== true q))

; 55
(run* [q]
  (pairo '())
  (== true q))

; 59

(defn caro*
  [p a]
  (fresh [d]
    (conso a d p)))

(run* [q]
  (caro* ['a 'b 'c] q))

(defn cdro*
  [p d]
  (fresh [a]
    (conso a d p)))

(run* [q]
  (cdro* ['a 'b 'c] q))
