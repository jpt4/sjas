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
(comment 
;Transferred to numbers.clj
(defn numo [x]
  (!= x '(0))
  (conde
   [(==o x '())]
   [(fresh [d] (==o (lcons 1 d) x) (numo d))]
   [(fresh [d] (==o (lcons 0 d) x) (!= '() d) (numo d))]))
)


;;Starting Notation Conventions

;Constant symbols

(def c0 '()) ;zero
(def c1 '(1)) ;one
(def c2 '(0 1)) ;two

;Non-Growth functions

;Integer Subtraction

(defn isubo [x y z]
  (numo x) (numo y) (numo z) 
  (conde 
   [(numo x) (numo y) (numo z) (pluso z y x) (<lo y x)
    ]
   [(==o z '()) (numo x) (numo y) (numo z) (<=lo x y) 
    ]))

;TOFIX: isubo does not terminate on invalid numerical input, despite attempted 
;safeguards. (isubo x y '(1 0)) ought yield '(), as 
;(run 1 [x y z] (==o '(1 0) z) (numo z) (isubo x y z)) does.
(comment
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
)

;Integer Division
(defn idivo [x y z]
  (fresh [r]
    (conde
     [(==o y '()) (==o z '())]
     [(!= y '()) (divo x y z r) (numo x) (numo y) (numo z) (numo r) (<lo r y)])))

;Maximum
;(run 10 [x y z] (maxo x '(1 0) z))
;()
;^works
(defn maxo [x y z]
  (conde
   [(<=lo x y) (==o z y) (numo x) (numo y) (numo z)]
   [(<lo y x) (==o z x) (numo x) (numo y) (numo z)]))

;Root
;TOFIX:
(comment
(run 10 [x y z] (==o '(1 0 1) x) (==o '(0 1) y) (nrooto x y z))
([(1 0 1) (0 1) ()]
 [(1 0 1) (0 1) (1)]
 [(1 0 1) (0 1) (1)]
 [(1 0 1) (0 1) (0 1)])
Duplicate and erroneous values, after the intial correct result.
)
(defn nrooto [x y z]
  (conde
   [(fresh [r]
      (<=lo '(1) y) (logo x z y r) (numo x) (numo y) (numo z))]
   [(fresh [r]
      (==o y '()) (<lo y '(1)) (numo x) (numo y) (numo z))]))

;Logarithm
;log base 2, integer part
(comment
TOFIX
(run 5 [x y] (ilogo x y))
<C-c C-c>
theta.core> (run 4 [x y] (ilogo x y))
([(1) ()] [(1) ()] [(0 1) ()] [(0 1) ()]))
(comment
 (run 5 [x y] (ilogo '(0 0 0 1)  y))
theta.core> (run 1 [x y] (ilogo '(0 0 0 1)  y))
([_0 (1 1)])
theta.core> (run 3 [x y] (ilogo '(0 0 0 1)  y))
theta.core> (run 2 [x y] (ilogo '(0 0 0 1)  y))
)

(comment 
;TOFIX: incorrect result values
theta.core> (run 1 [q] (ilog2o '(0 1) '()))
(_0)
theta.core> (run 1 [q] (ilog2o '(0 1) q))
((1))
theta.core> (run 2 [q] (ilog2o '(0 1) q))
((1) ())
theta.core> (run 3 [q] (ilog2o '(0 1) q))
((1) () ())
theta.core> (run 4 [q] (ilog2o '(0 1) q))
theta.core> 
)

(defn ilog2o [x y]
  (fresh [r]
    (numo x) (numo y) (numo r) (logo x '(0 1) y r)))

;Count
;TOFIX: not working, suspect pluso is failing
(defn counto [i j o]
  (numo i) (numo j) (numo o)
  (conde
   [(==o '() j) (==o '() o)]
   [(==o '() i) (==o '() o)]
   [(==o '(0) i) (==o '() o)]
   [(fresh [d res jm1]
      (numo i) (numo j) (numo o) (numo d) (numo res) (numo jm1)
      (!= '() j) (==o (lcons 1 d) i)
      (pluso 1 res o) 
      (pluso jm1 1 j) 
      (counto d jm1 res))]
   [(fresh [d res jm1]
      (numo i) (numo j) (numo o) (numo d) (numo res) (numo jm1)
      (!= '() j) (==o (lcons 0 d) i)
      (pluso 1 res o) (pluso jm1 1 j) (counto d jm1 res))]
   ))

;subtract 1
;auxiliary for powero
(defn isub1o [x y] (isubo x '(1) y))

;powero
;power of two? predicate with explicit true/false
(comment

)

(defn powero [x xout]
  (conde
   [(==o x '(1)) (==o xout 't)]
   [(fresh [s1 l1 l2] 
      (!= '(1) x)
      (numo s1) (numo l1) (numo l2)
      (isub1o x s1)
      (ilog2o x l1) 
      (ilog2o s1 l2)
      (==o l1 l2) 
      (==o xout l2)
      ;(==o xout 'f)
      )]))

(comment
Order disunify constraints before all others.
(run 1 [x]
              (fresh [s1 l1 l2]
              (numo l1) (numo l2) 
              (!= l1 l2)
              (isubo '(0 1) '(1) s1)))
;Error printing return value (IllegalArgumentException) at clojure.lang.RT/seqFrom (RT.java:557).
;Don't know how to create ISeq from: clojure.core.logic.LCons
theta.core> (run 1 [x]
              (fresh [s1 l1 l2]
                (!= l1 l2)
                (numo l1) (numo l2) 
                (isubo '(0 1) '(1) s1)))
(_0)
theta.core> (run 1 [x s1 l1 l2] 
              (!= l1 l2)
              (numo l1) (numo l2) 
            
              (isubo '(0 1) '(1) s1))
([_0 (1) () (1)])
theta.core> 
)

(comment
(defn powero [x xout]
  (conde
   [(==o x '(1)) (==o xout 't)]
   [(fresh [s1 l1 l2] 
      (!= '(1) x)
      (numo s1) (numo l1) (numo l2)
      (isub1o x s1)
      (ilog2o x l1) 
      (ilog2o s1 l2)
      (==o l1 l2) 
      (==o xout 'f))]
   [(fresh [s1 l1 l2] 
      (!= l1 l2) 
      (numo s1) (numo l1) (numo l2)
      (isub1o x s1)
      (ilog2o x l1) 
      (ilog2o s1 l2)
      (==o xout 't))]))
)



(comment
;;theta
(defn theta [x xout]
  (!= xout 1)
  (lgc/conde
   [(powero x 't) (powero out 't)]
   [(fresh [y yout]
	   (!= x y) (power x 't) (theta y yout) (!= xout yout))]
   [(powero x 'f) (== xout '())]))
)


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
