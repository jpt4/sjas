(ns lcert.formal-test.enclabels
  "Formal suite (ADR-0006), lcert.formal.enclabels: every label the encodings
  introduce lies among the 97 encoding labels, so an encoding's labels are
  below any bound k ≥ 97 whenever the data it encodes (label constants, raw
  codes) are — whether or not the label set is later made a parameter."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.enclabels]))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest f7-encoding-labels
  (testing "the bound, the data predicate, and the theorems"
    (doseq [nm '[lblBelow lblsE lblBelow_encNat lblBelow_encE lblOk_lblBelow]]
      (is (b/has? nm) (str nm))))
  (testing "the branch-list pseudo-type is encoded with an encoding label (was 100)"
    (is (holds? '(Eq Bool (lblBelow 97 (encE (Exp.tBrs Exp.tUnit 5))) Bool.true))))
  (testing "numbers are unary: no label grows with the number"
    (is (holds? '(Eq Bool (lblBelow 97 (encNat 150)) Bool.true))))
  (testing "a label constant outside the bound is not hidden: the data predicate fails"
    (is (holds? '(Eq Bool (lblsE 97 (Exp.lbl 150)) Bool.false)))
    (is (holds? '(Eq Bool (lblBelow 97 (encE (Exp.lbl 150))) Bool.false))))
  (testing "a typical type and term: the code of Bool → Bool, and λx. x"
    (is (holds? '(Eq Bool (lblBelow 97 (encE (Exp.tPi U.uw Exp.tBool Exp.tBool))) Bool.true)))
    (is (holds? '(Eq Bool (lblBelow 97 (encE (Exp.lam U.u1 Exp.tBool (Exp.var 0)))) Bool.true)))))
