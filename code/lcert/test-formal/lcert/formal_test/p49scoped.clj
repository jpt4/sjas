(ns lcert.formal-test.p49scoped
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49scoped]))

(deftest telescope-environment-scoped-congruence
  (is (b/has? 'p49sc_plug))
  (is (b/has? 'p49sc_step))
  (is (b/has? 'p49sc_tele))
  (is (b/has? 'p49sc_below))
  (is (b/has? 'p49sc_tele_below))
  (is (b/has? 'p49sc_pair_agree_lt))
  (is (b/has? 'p49sc_pair_tele))
  (is (b/has? 'p49sc_fresh_pair_tele))
  (is (b/has? 'p49sc_selected_pair_agree_lt))
  (is (b/has? 'p49sc_F_pair))
  (is (b/has? 'p49sc_F_pair_lift))
  (is (b/has? 'p49sc_F_pair_subst))
  (is (b/has? 'p49sc_pair_left))
  (is (b/has? 'p49sc_pair_right))
  (is (b/has? 'p49sc_node_result_pair))
  (is (b/has? 'p49sc_node_result_concrete)))
