(ns lcert.formal-test.enc46f7
  "Formal suite (ADR-0006), lcert.formal.enc46f7: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.enc46f7]))

(def ^:private CD '(Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))

(deftest f4-enc46f7
  (testing "padding is free at F7's checker, and Enc46 fails there"
    (doseq [c '[bodyD_pad check_pad decCert_head ofCode hfill_ofCode hnodes_ofCode padK hfill_padK padK_nodes
                hasHole_padK isNodeH_padK pad_le_f7 enc46_F7_core enc46_F7 thm46_F7_vacuous]]
      (is (b/has? c) (str c))))
  (testing "decCert reads the root's left child only: the right child and the root label are free, the left child is not"
    (is (not (b/rejects? '[c0 :- Code, P :- Code, Q :- Code]
                         (list 'Eq (list 'Option CD) '(decCert (Code.sn 0 (chHead c0) P)) '(decCert (Code.sn 7 (chHead c0) Q)))
                         '[(rfl)])))
    (is (b/rejects? '[c0 :- Code, P :- Code]
                    (list 'Eq (list 'Option CD) '(decCert (Code.sn 0 P (chHead c0))) '(decCert c0))
                    '[(rfl)])))
  (testing "the hidden certificate costs padK nothing: hnodes counts padK's own nodes, not the plug's"
    ;; padK (sl 3) = sn 0 (sl 0) (sn 0 (sl 3) □): two own nodes
    (is (not (b/rejects? '[] '(Eq Nat (hnodes (padK (Code.sl 3))) 2) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Nat (hnodes (padK (Code.sl 3))) 1) '[(rfl)]))
    ;; filled with a two-node certificate the tree has four nodes; padK still counts two
    (is (not (b/rejects? '[] '(Eq Nat (cnodes (hfill (fn [i :- Nat] (Code.sn 1 (Code.sn 2 (Code.sl 0) (Code.sl 0)) (Code.sl 0))) (padK (Code.sl 3)))) 4)
                         '[(rfl)])))
    ;; its only hole is 0
    (is (not (b/rejects? '[] '(Eq Bool (holeIn 0 (padK (Code.sl 3))) Bool.true) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Bool (holeIn 1 (padK (Code.sl 3))) Bool.true) '[(rfl)])))
  (testing "Enc46 is refuted at F7's checker only with an accepted large certificate: without one, the checker that accepts nothing satisfies it"
    (is (not (b/rejects? '[sz :- (=> Exp Nat)] '(Enc46 (fn [a :- Code, b :- Code] Bool.false) (decOf decCert) sz)
                         '[(exact (enc46_sat (decOf decCert) sz))])))))
