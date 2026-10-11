(ns lcert.formal-test.p49nodemotive
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49nodemotive]))

(deftest dependent-node-label-motive
  (is (b/has? 'p49nm_motive_formed))
  (is (b/has? 'p49nm_len))
  (is (b/has? 'p49nm_label))
  (is (b/has? 'p49nm_subst))
  (is (b/has? 'p49nm_case_result))
  (is (b/has? 'p49nm_peel_result))
  (is (b/has? 'p49nm_branch_tail))
  (is (b/has? 'p49nm_node_cont)))
