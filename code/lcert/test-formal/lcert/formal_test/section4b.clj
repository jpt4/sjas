(ns lcert.formal-test.section4b
  "Formal suite (ADR-0006), lcert.formal.section4b: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.section4b]))
(deftest f4-section4b
  (testing "Propositions 4.2, 4.10 and 4.5"
    (doseq [c '[codeTerm lit0 cv_lit prop42 prop410 cert_front prop45_d3 tensor_body prop45_contraction]]
      (is (b/has? c) (str c))))
  (testing "a leaf certificate inhabits □A: labels below L compute, and the checker hypothesis is used"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sl 0) (encTy A)) Bool.true)]
                         '(Rt chkf (thetaD (cnodes (Code.sl 0))) (thetaU (cnodes (Code.sl 0)))
                              (certTerm (Code.sl 0) (codeTerm (encTy A)))
                              (boxTy (codeTerm (encTy A))))
                         '[(exact (prop42 chkf dec encTy A (Code.sl 0) rfl hA hck))]))))
  (testing "a one-node certificate inhabits □A"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sn 1 (Code.sl 2) (Code.sl 3)) (encTy A)) Bool.true)]
                         '(Rt chkf
                              (thetaD (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))))
                              (thetaU (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))))
                              (certTerm (Code.sn 1 (Code.sl 2) (Code.sl 3)) (codeTerm (encTy A)))
                              (boxTy (codeTerm (encTy A))))
                         '[(exact (prop42 chkf dec encTy A (Code.sn 1 (Code.sl 2) (Code.sl 3)) rfl hA hck))]))))
  (testing "a label outside L is not a certificate: lblOk (leaf 100) is not true by computation"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      A :- Exp,
                      hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                      hck :- (Eq Bool (chkf (Code.sl 100) (encTy A)) Bool.true)]
                    '(Rt chkf (thetaD (cnodes (Code.sl 100))) (thetaU (cnodes (Code.sl 100)))
                         (certTerm (Code.sl 100) (codeTerm (encTy A)))
                         (boxTy (codeTerm (encTy A))))
                    '[(exact (prop42 chkf dec encTy A (Code.sl 100) rfl hA hck))])))
  (testing "boxed contraction on a one-node certificate uses both token blocks"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sn 1 (Code.sl 2) (Code.sl 3)) (encTy A)) Bool.true)]
                         '(Rt chkf
                              (thetaD (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                         (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))))
                              (thetaU (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                         (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))))
                              (Exp.lam U.u1 (boxTy (codeTerm (encTy A)))
                                (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))
                                  (Exp.pair (boxTy (codeTerm (encTy A)))
                                    ((litAt (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                     (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))) 1))
                                    Exp.star)
                                  (Exp.pair (boxTy (codeTerm (encTy A)))
                                    ((litAt (Code.sn 1 (Code.sl 2) (Code.sl 3))) 1) Exp.star)))
                              (Exp.tPi U.u1 (boxTy (codeTerm (encTy A)))
                                (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))))
                         '[(exact (prop45_contraction chkf dec encTy A (Code.sn 1 (Code.sl 2) (Code.sl 3)) rfl hA hck))]))))
  (testing "the same contraction is not derivable by that theorem at budget ‖v‖"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      A :- Exp, v :- Code,
                      hok :- (Eq Bool (lblOk v) Bool.true),
                      hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                      hck :- (Eq Bool (chkf v (encTy A)) Bool.true)]
                    '(Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v))
                         (Exp.lam U.u1 (boxTy (codeTerm (encTy A)))
                           (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))
                             (Exp.pair (boxTy (codeTerm (encTy A))) ((litAt v) (+ (cnodes v) 1)) Exp.star)
                             (Exp.pair (boxTy (codeTerm (encTy A))) ((litAt v) 1) Exp.star)))
                         (Exp.tPi U.u1 (boxTy (codeTerm (encTy A)))
                           (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))))
                    '[(exact (prop45_contraction chkf dec encTy A v hok hA hck))]))))
