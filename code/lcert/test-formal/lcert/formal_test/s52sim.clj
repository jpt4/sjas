(ns lcert.formal-test.s52sim
  "Formal suite (ADR-0006), lcert.formal.s52sim: runtime values equal up to the
  free tag of bnil (the only nondeterminism of the evaluator), for the
  universal Theorem 5.2."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52sim]))

(deftest f5-s52sim
  (testing "flatten, projections, injectivity, inversion, lookup"
    (doseq [c '[flV flL smBool smNat smLbl smCode smCert smPL smPR smCE smCB smBH smBT smTag
                smHd smTl smLTag
                sm_bool_inj sm_nat_inj sm_lbl_inj sm_code_inj sm_cert_inj
                sm_bool_cong sm_nat_cong sm_lbl_cong sm_code_cong sm_cert_cong
                sm_pair_cong sm_pair_inj sm_bcons_cong sm_bcons_inj sm_clos_cong sm_clos_inj
                sm_cons_cong sm_cons_inj sm_nil_cons
                sm_lbl_inv sm_clos_inv sm_bcons_inv sm_list_cons_inv
                sm_lookup_zero sm_lookup_aux]]
      (is (b/has? c) (str c))))
  (testing "two empty branch lists with different tags are equal up to tags"
    (is (not (b/rejects? '[] '(Eq RV (flV (RV.bnil Sk.unit)) (flV (RV.bnil Sk.bool))) '[(rfl)]))))
  (testing "...but are different values: the tag is real"
    (is (b/rejects? '[] '(Eq RV (RV.bnil Sk.unit) (RV.bnil Sk.bool)) '[(rfl)])))
  (testing "flattening forgets only tags: distinct data stay distinct"
    (is (b/rejects? '[] '(Eq RV (flV (RV.bool Bool.true)) (flV (RV.bool Bool.false))) '[(rfl)]))
    (is (b/rejects? '[] '(Eq RV (flV (RV.nat 0)) (flV (RV.lbl 0))) '[(rfl)])))
  (testing "a tag buried in a closure environment is forgotten too"
    (is (not (b/rejects? '[body :- Exp]
              '(Eq RV (flV (RV.clos (List.cons RV (RV.bnil Sk.unit) (List.nil RV)) body))
                      (flV (RV.clos (List.cons RV (RV.bnil Sk.nat) (List.nil RV)) body)))
              '[(rfl)])))))
