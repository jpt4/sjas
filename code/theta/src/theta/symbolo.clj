(ns theta.symbolo
  (:require [clojure.core.logic :as lgc
             :refer [lvar? remcg cgoal]]
            [clojure.core.logic.protocols :refer :all]))


;; symbolo and numbero definitions courtesy of David Nolen:
;; https://gist.github.com/swannodette/8876121

(declare numbero*)

(defn symbolo* [x]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ s]
          (let [x (walk s x)]
            (when (symbol? x)
              ((remcg this) s))))
        IRunnable
        (-runnable? [_]
          (not (lvar? (walk s x))))))
    IVerifyConstraint
    (-verify [_ a cs]
      (not (some (fn [c] (= (-rator c) `numbero))
                 (map (:cm cs) (get (:km cs) (root-var a x))))))
    IConstraintOp
    (-rator [_] `symbolo)
    (-rands [_] [x])
    IReifiableConstraint
    (-reifyc [c v r s]
      (when-not (lvar? (walk r x))
        `(symbolo ~(-reify s x r))))
    IConstraintWatchedStores
    (-watched-stores [this] #{:clojure.core.logic/subst})))

(defn symbolo [x]
  (cgoal (symbolo* x)))

(defn numbero* [x]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ s]
          (let [x (walk s x)]
            (when (number? x)
              ((remcg this) s))))
        IRunnable
        (-runnable? [_]
          (not (lvar? (walk s x))))))
    IVerifyConstraint
    (-verify [_ a cs]
      (not (some (fn [c] (= (-rator c) `symbolo))
                 (map (:cm cs) (get (:km cs) (root-var a x))))))
    IConstraintOp
    (-rator [_] `numbero)
    (-rands [_] [x])
    IReifiableConstraint
    (-reifyc [c v r s]
      (when-not (lvar? (walk r x))
        `(numbero ~(-reify s x r))))
    IConstraintWatchedStores
    (-watched-stores [this] #{:clojure.core.logic/subst})))

(defn numbero [x]
  (cgoal (numbero* x)))
