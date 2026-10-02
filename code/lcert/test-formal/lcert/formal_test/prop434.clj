(ns lcert.formal-test.prop434
  "Formal suite (ADR-0006), lcert.formal.prop434: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop434]))
(deftest f4-prop43-44
  (testing "Propositions 4.3 and 4.4 (1), with their parts"
    (doseq [c '[TokSize TypeSize cnt_mask_le prop43 cTerm cterm_code box_closed negN negN_size cert_size prop44_1 box_in box_out box_evid prop44_2 prop44_3]]
      (is (b/has? c) (str c))))
  (testing "code terms decode to their codes; ¬¹1 has a one-node code under E5's shape"
    (is (not (b/rejects? '[] '(Eq (Option Code) (codeOf (cTerm (Code.sn 3 (Code.sl 1) (Code.sl 2)))) (Option.some Code (Code.sn 3 (Code.sl 1) (Code.sl 2)))) '[(rfl)])))
    (is (b/rejects? '[] '(Eq (Option Code) (codeOf (cTerm (Code.sl 1))) (Option.some Code (Code.sl 2))) '[(rfl)]))))
