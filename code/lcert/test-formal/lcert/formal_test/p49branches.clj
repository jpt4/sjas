(ns lcert.formal-test.p49branches
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49branches]))

(deftest complete-label-family
  (is (b/has? 'p49brs_typed))
  (is (b/has? 'p49case_typed))
  ;; An empty list is valid only beyond the final label, never as a full
  ;; list of 100 branches. This rejects the branch-coverage shortcut.
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (List.nil Exp) (List.nil U) Exp.bnil
           (Exp.tBrs Exp.tUnit 0))
        '[(exact (Rt.rBnil chkf (List.nil Exp) (List.nil U) Exp.tUnit rfl))])))
