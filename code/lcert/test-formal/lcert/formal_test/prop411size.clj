(ns lcert.formal-test.prop411size
  "Formal suite (ADR-0006), lcert.formal.prop411size: A10 reopened, the exact
  F7 size of Proposition 4.11's certificate family.  Requiring the namespace
  kernel-checks its declarations; these tests check each statement's exact
  form, the kernel-evaluated instances, and that off-by-one variants are
  rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop411size]))

(deftest a10-layer-size
  (testing "one successor layer of the open sharing tree adds 14n + 18d + 81 nodes"
    (is (not (b/rejects? '[n :- Nat, d :- Nat]
          '(Eq Nat (cnodes (encDT (sh4_open_tree (Nat.succ n) (sh4_syns d))))
             (+ (cnodes (encDT (sh4_open_tree n (sh4_syns (Nat.succ d)))))
                (+ (Nat.mul n 14) (+ (Nat.mul d 18) 81))))
          '[(exact (sh4_open_tree_nodes_step n d))])))))

(deftest a10-open-tree-closed-form
  (testing "the open sharing tree has 16n² + 18nd + 67n + 2d + 7 nodes"
    (is (not (b/rejects? '[n :- Nat, d :- Nat]
          '(Eq Nat (cnodes (encDT (sh4_open_tree n (sh4_syns d))))
             (+ (Nat.mul (Nat.mul n n) 16)
                (+ (Nat.mul (Nat.mul n d) 18)
                   (+ (Nat.mul n 67) (+ (Nat.mul d 2) 7)))))
          '[(exact (sh4_open_tree_nodes n d))])))
    (is (b/has? 'sh4_open_tree_ht))
    (is (b/has? 'sh4_cn_tree_nodes))
    (is (b/has? 'sh4_cn_tree_ht))))

(deftest a10-certificate-size
  (testing "the F7 certificate of c_n has exactly 16n² + 97n + 73 nodes"
    (is (not (b/rejects? '[n :- Nat]
          '(Eq Nat (cnodes (sh4_cn_cert n))
             (+ (Nat.mul (Nat.mul n n) 16) (+ (Nat.mul n 97) 73)))
          '[(exact (prop411_cert_size n))])))
    (is (b/rejects? '[]
          '(Eq Nat (cnodes (sh4_cn_cert 2)) 332)
          '[(rfl)]))))

(deftest a10-kernel-instances
  (testing "the table's F7 counts, by kernel evaluation of the certificates"
    (doseq [[nm n v] '[[prop411_cert_2 2 331] [prop411_cert_4 4 717]
                       [prop411_cert_6 6 1231] [prop411_cert_8 8 1873]]]
      (is (not (b/rejects? '[]
            (list 'Eq 'Nat (list 'cnodes (list 'sh4_cn_cert n)) v)
            [(list 'exact nm)]))
          (str nm)))))

(deftest a10-quadratic-bound
  (testing "the certificate grows at most quadratically"
    (is (not (b/rejects? '[n :- Nat]
          '(LE.le (cnodes (sh4_cn_cert n))
                  (Nat.mul (Nat.mul (+ n 1) (+ n 1)) 97))
          '[(exact (prop411_cert_quadratic n))])))))

(deftest a10-exponential-gap
  (testing "from n = 12 on, c_n's certificate is smaller than any certificate of its normal form"
    (is (b/has? 'sh4_pow_dominates))
    (is (not (b/rejects? '[n :- Nat, m :- Nat, T1 :- DT, T2 :- DT]
          '(LT.lt (cnodes (sh4_cn_cert (+ n 12)))
                  (cnodes (encCert m (codeTerm (sh4_bush (+ n 12))) Exp.tSyn T1 T2)))
          '[(exact (prop411_exponential_gap n m T1 T2))])))))

(deftest a10-normal-form-certificates
  (testing "Check accepts the normal form's certificate, with the table's counts"
    (doseq [[nm n v] '[[prop411_lit_cert_2 2 152] [prop411_lit_cert_4 4 780]
                       [prop411_lit_cert_6 6 3772] [prop411_lit_cert_8 8 17660]]]
      (is (not (b/rejects? '[]
            (list 'And (list 'Eq 'Bool (list 'Check 'decCert (list 'sh4_lit_cert n) '(encE Exp.tSyn)) 'Bool.true)
                       (list 'Eq 'Nat (list 'cnodes (list 'sh4_lit_cert n)) v))
            [(list 'exact nm)]))
          (str nm)))))
