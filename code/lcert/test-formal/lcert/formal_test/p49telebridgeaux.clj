(ns lcert.formal-test.p49telebridgeaux
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49telebridgeaux]))

(deftest unary-tail-peel-result-shapes
  (is (b/has? 'p49tba_F))
  (is (b/has? 'p49tba_leafB_eq))
  (is (b/has? 'p49tba_nodeB_eq))
  (is (b/has? 'p49tba_leaf_tail))
  (is (b/has? 'p49tba_node_tail))
  (is (b/has? 'p49tba_leafB_inst))
  (is (b/has? 'p49tba_nodeB_inst))
  (is (b/has? 'p49tba_leaf_result_tail))
  (is (b/has? 'p49tba_node_result_tail)))
