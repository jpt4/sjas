;20200421Z
;jpt4
;Implement the languages described in Willard 2017

(ns theta.core
    (:require [clojure.core.match :refer [match]]
              [clojure.core.logic :as lgc]))

;;Because core.logic.arithmetic is non-relational, and CFD assumes the
;;size of the numeric domain in a way that Willard2017 does not, it is
;;necessary to re-implement the relational, reverse binary encoded
;;natural number system of The Reasoned Schemer, as written in
;;https://github.com/webyrd/faster-miniKanren/blob/master/numbers.scm

;appendo : included in core.logic

;build-num : decimal natural numbers to reverse binary lists
(defn build-num [n]
      (cond
	(odd? n) 
	(cons 1 (build-num (/ (- n 1) 2)))
	(and (not (zero? n)) (even? n))
	(cons 0 (build-num (/ n 2)))
	(zero? n) '()))

;;variants

;build-num-w-0 : '(0) for 0
(defn build-num-w-0 [n]
      (cond
       (zero? n) '(0)
       (= n 1) '(1)
       (and (not (= n 1)) (odd? n))
       (cons 0 (build-num-w-0 (/ n 2)))
       (and (not (= n 0)) (even? n))
       (cons 1 (build-num-w-0 (/ (- n 1) 2)))))


;;Starting Notation Conventions

;Constant symbols

(def c0 '()) ;zero
(def c1 '(1)) ;one
(def c2 '(0 1)) ;two

;Non-Growth functions

(comment
;;theta
(defn theta [x xout]
  (!= xout 1)
  (conde
   [(powero x) (powero out)]
   [(fresh [y yout]
	   (!= x y) (power x) (theta y yout) (!= xout yout))]
   [(fresh [pout]
	   (powero x pout) (!= pout 't) (== xout '()))]))

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
