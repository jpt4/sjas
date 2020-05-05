;20200421Z
;jpt4
;Implement the languages described in Willard 2017

(ns theta.core
    (:require [clojure.core.match :refer [match]]
              [clojure.core.logic :as lgc 
               :refer [== != appendo conde fresh lcons llist run]
               :rename {== ==o}]
              [theta.numbers
               :refer :all]
              :reload-all))

;;number variants
;build-num-w-0 : '(0) for 0
(defn build-num-w-0 [n]
      (cond
       (zero? n) '(0)
       (= n 1) '(1)
       (and (not (= n 1)) (odd? n))
       (cons 0 (build-num-w-0 (/ n 2)))
       (and (not (= n 0)) (even? n))
       (cons 1 (build-num-w-0 (/ (- n 1) 2)))))

;;define the grammar of natural numbers 
(defn numo [x]
  (!= x '(0))
  (conde
   [(==o x '())]
   [(fresh [d] (==o (lcons 1 d) x) (numo d))]
   [(fresh [d] (==o (lcons 0 d) x) (!= '() d) (numo d))]))

;;Starting Notation Conventions

;Constant symbols

(def c0 '()) ;zero
(def c1 '(1)) ;one
(def c2 '(0 1)) ;two

;Non-Growth functions

;TOFIX: isubo does not terminate on invalid numerical input, despite attempted 
;safeguards. (isubo x y '(1 0)) ought yield '(), as 
;(run 1 [x y z] (==o '(1 0) z) (numo z) (isubo x y z)) does.
(defn isubo [x y z]
  (fresh [dx dy dz]
    (==o dx x) (==o dy y) (==o dz z)
    (numo dx) (numo dy) (numo dz) 
(comment
  theta.core> (run 1 [q] (==o q '(1 0)) (numo q))
  ()
  theta.core> (run 1 [q] (numo q) (==o q '(1 0)))
  <evaluation interrupted by user>
  Late binding numo constraints prevents long/infinite walks through all nats.)
  (conde 
   [(==o dx x) (==o dy y) (==o dz z) (numo dx) (numo dy) (numo dz) (pluso dz dy dx) (<lo dy dx)
    ]
   ;theta.core> (run 1 [x y z] (isubo x y z) (==o z '(1 0)) (numo z))
   ;<evaluation interrupted by user>
   ;theta.core> (run 1 [x y z]  (==o z '(1 0)) (numo z) (isubo x y z))
   ;()
   ;Binding complex constraints after numo prevent another long walk.)
   ;(run 1 [x y] (<lo x y) (numo x) (numo y) (==o '(1) x) (==o '(1 0) y))
   ;theta.core> (run 1 [x y] (<lo x y) (==o '(1) x) (==o '(1 0) y) (numo x) (numo y))
   ;theta.core> (run 1 [x y] (==o '(1) x) (==o '(1 0) y) (numo x) (numo y) (<lo x y))
   ;()
   ;Order of constraints: literals, predicates, relations
   [(==o dz '()) (numo dx) (numo dy) (numo dz) (<=lo dx dy) 
    ])))

(defn idivo [x y z]
  (fresh [r]
    (conde
     [(==o y '()) (==o z '())]
     [(!= y '()) (divo x y z r) (numo x) (numo y) (numo z) (numo r) (<lo r y)])))
   
(comment
;;theta
(defn theta [x xout]
  (!= xout 1)
  (lgc/conde
   [(powero x 't) (powero out 't)]
   [(fresh [y yout]
	   (!= x y) (power x 't) (theta y yout) (!= xout yout))]
   [(powero x 'f) (== xout '())]))

;powero
(defn powero [x xout]
  (conde
   [(== x '(1)) (== xout 't)]
   [(fresh [s1 l1 l2] 
	   (isubo x s1)
	   (log2o x l1) 
	   (log2o s1 l2)
	   (!= l1 l2) 
	   (== xout 't))]
   [(fresh [s1 l1 l2] 
	   (isubo x s1)
	   (log2o x l1) 
	   (log2o s1 l2)
	   (== l1 l2) 
	   (== xout 'f))])))

(defn lg-eval [exp]
      (match exp
      'c0 'c0
      'c1 'c1
      'c2 'c2
;     [(isub x y)] (isub (lg-eval x) (lg-eval y))
))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
