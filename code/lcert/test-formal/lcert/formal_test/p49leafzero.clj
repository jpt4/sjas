(ns lcert.formal-test.p49leafzero
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49leafzero]))

(deftest depth-zero-leaf-continuation
  (is (b/has? 'p49lz_label))
  (is (b/has? 'p49lz_motive_formed))
  (is (b/has? 'p49lz_leaf_cont)))
