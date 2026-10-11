(ns lcert.formal-test.p49nodelive
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49nodelive]))

(deftest live-node-continuation
  (is (b/has? 'p49nl_usage))
  (is (b/has? 'p49nl_label))
  (is (b/has? 'p49nl_node_cont))
  (is (b/has? 'p49nl_branch_usage))
  (is (b/has? 'p49nl_left_code))
  (is (b/has? 'p49nl_right_code))
  (is (b/has? 'p49nl_left_evidence))
  (is (b/has? 'p49nl_right_evidence)))
