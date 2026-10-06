(ns lcert.formal-test.certcanon
  "Formal suite (ADR-0006), lcert.formal.certcanon: F7's canonical,
  unpadded certificate format — the size facts derived from the encoding,
  completeness without padding, and the shape of every accepted code.
  Requiring the namespace kernel-checks its declarations; these tests check
  that the expected constants exist, that the checker accepts canonical
  certificates, and that it rejects everything the padded format let
  through."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.certcanon]))

(def ^:private nilE '(List.nil Exp))
(def ^:private rc (list 'DT.rConst nilE '(List.nil U) 'Exp.star 'Exp.tUnit))
(def ^:private fb (list 'DT.fBase nilE 'Exp.tUnit))
;; The canonical certificate of ⋆ : Unit at budget 0, and its left child.
(def ^:private cert (list 'encCert 0 'Exp.star 'Exp.tUnit rc fb))
(def ^:private X (list 'certX 0 'Exp.star 'Exp.tUnit rc fb))
(def ^:private unit '(encE Exp.tUnit))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))
(defn- accepts [c] (list 'Eq 'Bool (list 'Check 'decCert c unit) 'Bool.true))
(defn- rejects [c] (list 'Eq 'Bool (list 'Check 'decCert c unit) 'Bool.false))

(deftest f7-canon-labels
  (testing "F7's E6: no encoder puts label 96 on an internal node"
    (doseq [c '[nlbN nlbC nlbB nlbU nlbS nlbLN nlbLE nlbLU nlbLS nlbHd nlbSt nlbSkDT nlbDT
                nlbHd_delta nlbDT_rConst nlb_certX]]
      (is (b/has? c) (str c))))
  (testing "a certificate's root is labelled 96, and nothing below it is"
    (is (holds? (list 'Eq 'Bool (list 'nlb 96 X) 'Bool.true)))
    (is (holds? (list 'Eq 'Bool (list 'nlb 96 cert) 'Bool.false))))
  (testing "a δ-record's raw code carrying label 96 adds no internal 96 (it is a literal term)"
    (is (holds? '(Eq Bool (nlb 96 (encHd (HdDT.delta Exp.star Exp.star (Code.sn 96 (Code.sl 0) (Code.sl 0)) (Code.sl 0)))) Bool.true)))))

(deftest f7-canon-sizes
  (testing "the size facts follow from the encoding"
    (doseq [c '[hdB_le stepB_le dtB_le cert_nodes cert_size0 cert_size1 cert_size2 cert_size3 cert_size4
                budget_canon toksize_canon typesize_canon]]
      (is (b/has? c) (str c))))
  (testing "a δ-record pays for its code: hdB ≤ its encoding's nodes, though the raw code alone would not"
    ;; the code sn 5 (sl 0) (sl 0) has 1 node; hdB = 2; the record's encoding has more
    (is (holds? '(Eq Nat (hdB (HdDT.delta Exp.star Exp.star (Code.sn 5 (Code.sl 0) (Code.sl 0)) (Code.sl 0))) 2)))
    (is (holds? '(Eq Bool (Nat.ble 2 (cnodes (encHd (HdDT.delta Exp.star Exp.star (Code.sn 5 (Code.sl 0) (Code.sl 0)) (Code.sl 0))))) Bool.true)))
    (is (holds? '(Eq Bool (Nat.ble 2 (cnodes (Code.sn 5 (Code.sl 0) (Code.sl 0)))) Bool.false)))))

(deftest f7-canon-checker
  (testing "the canonical format: encoder, guarded decoder, canonicity, completeness"
    (doseq [c '[certX encCert encCertY guardC decCert decCert_enc none_ne_some_cd guard_bool guard_canon decCert_canon
                check_cert_ok check_complete check_complete_lbl lblBelow_encCert lbl_sub_l lbl_sub_r lblOk_certType
                acc_shape encCert_congr acc_encCert]]
      (is (b/has? c) (str c))))
  (testing "the canonical certificate of ⋆ : Unit is accepted for Unit, not for Bool, with no padding"
    (is (holds? (accepts cert)))
    (is (holds? (list 'Eq 'Bool (list 'Check 'decCert cert '(encE Exp.tBool)) 'Bool.false))))
  (testing "padding that is not canonical is rejected"
    (is (holds? (rejects (list 'Code.sn 96 X '(padC 3))))))
  (testing "a certificate carried as padding is rejected — the padded format accepted it"
    (is (holds? (rejects (list 'Code.sn 96 X cert))))
    (is (holds? (list 'Eq 'Bool (list 'Check 'decCertPad (list 'Code.sn 0 X cert) unit) 'Bool.true))))
  (testing "the root label must be 96"
    (is (holds? (rejects (list 'Code.sn 0 X '(Code.sl 0))))))
  (testing "the fuel must be the trees' (htDT T₁ + htDT T₂ + 1): a larger fuel decodes, but is not canonical"
    (let [X9 (list 'chHead (list 'encCertPad 9 0 'Exp.star 'Exp.tUnit rc fb '(Code.sl 0)))]
      (is (holds? (rejects (list 'Code.sn 96 X9 '(Code.sl 0)))))
      (is (holds? (list 'Eq 'Bool (list 'Check 'decCertPad (list 'Code.sn 0 X9 '(Code.sl 0)) unit) 'Bool.true)))))
  (testing "completeness: an accepted typing tree and formation tree make an accepted certificate, with labels in L"
    (is (not (b/rejects? '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
                           h1 :- (Eq Bool (dtCheck (Check decCert) T1 (DTJ.rt (thetaD m) (thetaU m) t A)) Bool.true),
                           h2 :- (Eq Bool (dtCheck (Check decCert) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit)) Bool.true),
                           hA :- (Eq Bool (closedTy A) Bool.true)]
      '(Exists (fn [c :- Code] (Eq Bool (Check decCert c (encE A)) Bool.true)))
      '[(exact (check_complete m t A T1 T2 h1 h2 hA))])))
    (is (holds? (list 'Eq 'Bool (list 'lblOk (list 'encCert 120 'Exp.star 'Exp.tUnit rc fb)) 'Bool.true))))
  (testing "canonicity is not vacuous: the guard drops a decoding that does not re-encode to the code"
    (is (holds? (list 'Eq '(Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
                      (list 'decCert (list 'Code.sn 96 X '(padC 3)))
                      '(Option.none (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))))))
    (is (not (holds? (list 'Eq '(Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
                           (list 'decCertPad (list 'Code.sn 96 X '(padC 3)))
                           '(Option.none (Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))))))))
