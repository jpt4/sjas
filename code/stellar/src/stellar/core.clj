;20210312Z
;jpt4
;Implementation of Stellar Resolution by Eng and Seiller, 2021.

(ns stellar.core
  (:require [clojure.core.logic :as lgc
             :refer :all :rename {== ==o}]
            :reload-all))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
