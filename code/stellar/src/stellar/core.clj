;20210312Z
;jpt4
;Implementation of Stellar Resolution by Eng and Seiller, 2021.

(ns stellar.core
  (:require [clojure.core.logic :as lgc
             :refer :all :rename {== ==o}]
            :reload-all))

(defn stellar-resolution [diagram out]
  'stellar-resolution)

(define fusion [star1 ray1 star2 ray2 out]
  (fresh [su sol]
    (star-union star1 star2 su)
    (solution ray1 ray2 sol)
    (apply-solution sol su out)))
    

;(defn signature [[vars func arity]]
 ; (fresh [v]
  ;  (==o vars v

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
