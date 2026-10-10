(ns lcert.formal-test.p49telerepl
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49telerepl]))

(deftest terminal-telescope-replacement
  (is (b/has? 'p49tele_empty_repl))
  (is (b/has? 'p49tele_empty_leaf_repl))
  (is (b/has? 'p49tele_empty_node_repl))
  (is (b/has? 'p49tr_leaf_env))
  (is (b/has? 'p49tele_leaf_repl))
  (is (b/has? 'p49tr_plug_step))
  (is (b/has? 'p49tr_repl_env))
  (is (b/has? 'p49tele_repl))
  (is (b/has? 'p49tele_node_repl))
  (is (b/has? 'p49tr_slots_avoid_fresh_node))
  (is (b/has? 'p49tele_fresh_node_repl)))
