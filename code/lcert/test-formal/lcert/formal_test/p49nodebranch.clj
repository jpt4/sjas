(ns lcert.formal-test.p49nodebranch
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49nodebranch]))

(deftest concrete-node-branch-result-type
  (is (b/has? 'p49nb_pair_node_tail))
  (is (b/has? 'p49nb_branch_type))
  (is (b/has? 'p49nb_branch_concrete)))
