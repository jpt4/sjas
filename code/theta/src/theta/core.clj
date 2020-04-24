;20200421Z
;jpt4
;Implement the languages described in Willard 2017

(ns theta.core
    (:require [clojure.core.match :refer [match]]
              [clojure.core.logic]))

;;Starting Notation Conventions

;Constant symbols

(def c0 'c0) ;zero
(def c1 'c1) ;one
(def c2 'c2) ;two

;Non-Growth functions

;(defn minuso
;(defn powero [x] 

;(defn theto [x res]
;      (fresh [
;      (cond
;      (not (power? x)) 'c0
;))

;(defn isub [x y] (if (<= x y) 'c0

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
