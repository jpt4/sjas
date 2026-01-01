(ns sjl.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  ;(:use [clojure.core.match :only [match]])
)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;(defn permo [l p]
 ;           (l/fresh [e eres res]
  ;           (l/conde
   ;           [(l/== '() l) (l/== '() p)]
    ;          [(l/rembero e l eres) (l/conso e res p) (permo eres res)])))


