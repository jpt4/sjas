(ns lcert.formal-test.p49leafcont
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49leafcont]))

(deftest concrete-leaf-continuation
  (is (b/has? 'p49lc_leaf_len))
  (is (b/has? 'p49lc_label))
  (is (b/has? 'p49lc_motive_formed))
  (is (b/has? 'p49lc_subst1_leaf))
  (is (b/has? 'p49lc_branch_type))
  (is (b/has? 'p49lc_result_type))
  (is (b/has? 'p49lc_leaf_cont)))
