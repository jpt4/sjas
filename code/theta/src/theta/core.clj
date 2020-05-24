;20200421Z
;jpt4
;Implement the languages described in Willard 2017

(ns theta.core
    (:require [clojure.core.logic :as lgc
               :refer :all :rename {== ==o}]
              [clojure.core.logic.fd :as fd]
              :reload-all))

;;Starting Notation Conventions

;Constant symbols

(def c0 0)
(def c1 1)
(def c2 2)

;Non-Growth functions

;Integer Subtraction
(defn isubo [x y z]
  (conde 
   [(fd/<= x y) (fd/== 0 z)]
   [(fd/< y x) (fd/+ z y x)]))

(defn idivo [x y z]
  (conde
   [(fd/== 0 y) (fd/== 0 z)]
   [(fd/== 0 x) (fd/== 0 z)]
   [(fd/< x y) (fd/== 0 z)]
   [(fresh [p d]
      (fd/>= x y)
      (fd/> y 0) (fd/* y z p) (fd/- x p d) (fd/< d y))]))

(defn maxo [x y m]
  (conde
   [(fd/>= x y) (fd/== x m)]
   [(fd/> y x) (fd/== y m)]))

(defn ilogo [l u b n r]
  (fresh [q res]
    (fd/in l u b n r q res (fd/interval l u))
    (conde
     [(fd/== 0 n) (fd/== 0 r)]
     [(fd/== 1 n) (fd/== 0 r)]
     [(fd/> n 1) 
      (idivo n b q)
      (ilogo l u b q res)
      (fd/+ 1 res r)])))

(comment
(run* [q] (ilogo-acc 0 100 q 9 0 2))
(3)
theta.core> (run* [q] (ilogo-acc 0 100 q 16 0 2))
(3 4)
theta.core> (run* [q] (ilogo-acc 0 100 3 16 0 q))
(2)
theta.core> (run* [q] (ilogo-acc 0 100 4 16 0 q))
(2)
theta.core> )

(defn ilogo-acc [l u b n acc r]
  (fresh [q res nacc]
    (fd/in l u b n acc r q nacc res (fd/interval l u))
    (conde
     [(fd/== 0 n) (isubo acc 1 r)]
     [(fd/== 1 n) (fd/== acc r)]
     [(fd/> n 1) 
      (idivo n b q)
      (fd/+ 1 acc nacc)
      (ilogo-acc l u b q nacc r)])))

(defn iexpo [l u b e r]
  (fresh [res s1]
    (fd/in l u b e r res (fd/interval l u))
    (conde
     [(fd/== 0 b) (fd/== 0 e) (fd/== 1 r)]
     [(fd/== 0 b) (fd/< 0 e) (fd/== 0 r)]
     [(fd/< 0 b) (fd/== 0 e) (fd/== 1 r)]
     [(fd/< 0 b) (fd/< 0 e)
      (fd/* b res r)
      (fd/- e 1 s1)
      (iexpo l u b s1 res)])))

(defn ilogo-w-iexpo [l u b n r]
  (fresh [e1 r1 e2 r2]
;    (log l u b n r e1 r1 e2 r2)
    (fd/in l u b n 
           ;r 
           e1 r e2 r2 (fd/interval l u))
    (iexpo l u b e1 r1) (iexpo l u b e2 r2)
    (fd/- e1 e2 1) (fd/!= r1 r2) (fd/> r1 n) (fd/>= n r2)
 ;   (log l u b n r e1 r1 e2 r2)
    (==o r e2)
  ;  (log l u b n r e1 r1 e2 r2)
     ))

(defn idivo-repeat [l u x b n r]
  (fresh [newx newn res]
    (fd/in l u x b n r newx newn res (fd/interval l u))
    (conde
     [(fd/== 0 x) 
      ;(fd/!= 0 n) 
      (fd/== 0 r)]
     [(fd/!= 0 x) (fd/== 0 n) (fd/== x r)]
     [(fd/!= 0 x) (fd/!= 0 n)
      (idivo x b newx)
      (fd/- n 1 newn)
;      (fd/+ 1 res r)
      (idivo-repeat l u newx b newn r)
      ])))

(comment
  repeatedly divide x by two fresh b's, up to n times, counting down
  n. if the quotient becomes zero, then b overshoots/is too large. If the quotient is 1 after exactly n divisions, then b is the root. If the quotient is greater than 1 after n divisions, then b undershoots, is too small. if the difference between b1 and b2 is 1, such that b1 overshoots and b2 undershoots or equals 1, then b2 is the n-root of x.)

(defn irooto [l u x y r]
  (fresh [x1 x2 r1 r2 b1 b2]
  (fd/in l u x y r x1 x2 r1 r2 (fd/interval l u))
  (conde
   [(fd/== 0 y) (fd/== 0 r)]
   [(fd/== 1 y) (fd/== x r)]
   [(fd/> y 1)
    (idivo-repeat l u x b1 y r1)
    (idivo-repeat l u x b2 y r2)
    (fd/- b1 b2 1) (fd/== 0 r1) (fd/>= r2 1)
    (fd/== b2 r)
    ])))



(defn irooto-w-ilogo-w-iexpo [l u x y r]
  (fd/in l u x y r (fd/interval l u))
  (conde
   [(fd/== 0 y) (fd/== 0 r)]
   [(fd/>= y 1)
    (ilogo-w-iexpo l u r x y)]))

(comment
(run 1 [q] (irooto 0 100 8 3 q))
(2)
theta.core> (run 1 [q] (irooto 0 100 100 2 q))
(8)
theta.core> (run 2 [q] (irooto 0 100 100 2 q))
(8 9)
theta.core> (run 3 [q] (irooto 0 100 100 2 q))
(8 9 10)
theta.core> (run* [q] (irooto 0 100 100 2 q))
(8 9 10 5 6 7)
theta.core> 
)
