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

(defn irooto [l u x y r]
  (fd/in l u x y r (fd/interval l u))
  (conde
   [(fd/== 0 y) (fd/== 0 r)]
   [(fd/>= y 1)
    (ilogo-acc l u r x 0 y)]))


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
