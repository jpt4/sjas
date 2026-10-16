(ns lcert.formal-test.prop412size
  "Formal suite (ADR-0006), lcert.formal.prop412size: A06 reopened, the exact
  F7 size of Proposition 4.12(1)'s doubler certificate family."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop412size]))

(deftest a06-numeral-tree-size
  (testing "the unary numeral's proof tree has s(n) nodes, 2 s(n) = n² + 7n + 8"
    (is (not (b/rejects? '[n :- Nat]
          '(Eq Nat (+ (cnodes (encDT (sh4_num_tree n))) (cnodes (encDT (sh4_num_tree n))))
                   (+ (Nat.mul n n) (+ (Nat.mul n 7) 8)))
          '[(exact (sh4_num_tree_nodes n))])))
    (is (b/rejects? '[]
          '(Eq Nat (cnodes (encDT (sh4_num_tree 2))) 14)
          '[(rfl)]))))

(deftest a06-certificate-size
  (testing "the doubler certificate's size: term, height, tree and frame lemmas, and the closed form"
    (doseq [nm '[sh4_doubler_term_size sh4_doubler_tree_ht sh4_doubler_tree_nodes
                 sh4_doubler_cert_nodes prop412_cert_size
                 prop412_cert_4 prop412_cert_8 prop412_cert_12]]
      (is (b/has? nm) (str nm)))))

(deftest a06-exact-size
  (testing "twice the doubler certificate's size is n² + 13n + 732, and the table's three counts"
    (is (not (b/rejects? '[n :- Nat]
          '(Eq Nat (+ (cnodes (sh4_doubler_cert n)) (cnodes (sh4_doubler_cert n)))
                   (+ (Nat.mul n n) (+ (Nat.mul n 13) 732)))
          '[(exact (prop412_cert_size n))])))
    (doseq [[nm n v] '[[prop412_cert_4 4 400] [prop412_cert_8 8 450] [prop412_cert_12 12 516]]]
      (is (not (b/rejects? '[]
            (list 'Eq 'Nat (list 'cnodes (list 'sh4_doubler_cert n)) v)
            [(list 'exact nm)]))
          (str nm)))))
