(ns lcert.formal-test.s52inst
  "Theorem 5.2: code-recursion substitutions preserve truth."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52inst]))

(deftest s52-code-motive-instances
  (doseq [c '[s52_leafTy s52_nodeTy s52_y1Ty s52_y2Ty
             s52_y1_val s52_y2_val s52_node_raw]]
    (is (b/has? c) (str c)))
  (testing "substitution into an empty motive cannot make it inhabited"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(S52 chkf dec encTy cap Bool.false (leafTy Exp.tEmpty)
             (List.cons Sk Sk.lbl (List.nil Sk)) (Prod.mk 0 Unit.unit)
             Sk.unit RV.star Unit.unit)
          '[(constructor)]))))
