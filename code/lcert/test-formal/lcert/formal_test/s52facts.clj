(ns lcert.formal-test.s52facts
  "Theorem 5.2's two invariance facts, including truth-sensitive cases."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52facts]))

(deftest s52-invariance
  (testing "conversion, simultaneous and single substitution, and weakening"
    (doseq [c '[s52_cv s52_subst s52_subst1 s52_lift]]
      (is (b/has? c) (str c))))
  (testing "substitution at T reads the substituted Boolean"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool]
          '(S52 chkf dec encTy cap erasing
             (subst1 Exp.ff (Exp.tT (Exp.var 0)))
             (List.nil Sk) Unit.unit Sk.unit RV.star Unit.unit)
          '[(constructor) (rfl) (rfl)]))))
