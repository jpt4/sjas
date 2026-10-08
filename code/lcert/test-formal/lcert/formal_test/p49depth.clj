(ns lcert.formal-test.p49depth
  "Depth-indexed syntax and substitution; this is not Proposition 4.9."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.p49depth]))

(deftest depth-algebra
  (doseq [nm '[p49dl_subst p49dl_leaf p49dl_node p49dl_zero_node
               p49dl_zero_agrees P49_zero_agrees p49dl_typed P49_formed prop49_zero]]
    (is (b/has? nm) (str nm)))
  (testing "successor bounds do not reject every node"
    (is (b/rejects? '[]
          '(Eq Exp (p49dl 1 (Exp.var 0)) (p49dl 0 (Exp.var 0)))
          '[(rfl)]))))
