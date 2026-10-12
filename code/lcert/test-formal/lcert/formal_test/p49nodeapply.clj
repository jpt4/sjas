(ns lcert.formal-test.p49nodeapply
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49nodeapply]))

(deftest live-node-recursive-applications
  (is (b/has? 'p49na_node_lblok))
  (is (b/has? 'p49na_forms))
  (is (b/has? 'p49na_usage))
  (is (b/has? 'p49na_node_branch)))
