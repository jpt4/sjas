(ns lcert.formal-test.safety52
  "Theorem 5.2: the truth-sensitive relation and safe evaluation traces."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.safety52]))

(deftest f5-safety52
  (testing "safe traces project to evaluations, and S remembers truth"
    (doseq [c '[Trace52 S52 ok52_eval ok52_evalE
                s52_empty s52_unit s52_T s52_bool
                s52_no_refutation s52_no_contradiction]]
      (is (b/has? c) (str c))))
  (testing "an empty or false truth type has no related value"
    (doseq [A '[Exp.tEmpty (Exp.tT Exp.ff)]]
      (is (b/rejects?
            '[chkf :- (=> Code Code Bool),
              dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
              encTy :- (=> Exp Code), n :- Nat, erasing :- Bool]
            (list 'S52 'chkf 'dec 'encTy 'n 'erasing A
                  '(List.nil Sk) 'Unit.unit 'Sk.unit 'RV.star 'Unit.unit)
            '[(constructor) (rfl)]))))
  (testing "trace safety excludes a directly evaluated abort"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), n :- Nat]
          '(Ok chkf dec encTy n
             (EvSrc.tm (List.nil RV) (Exp.abort Exp.tUnit Exp.star)) RV.star)
          '[(exact (Ok.eStar chkf dec encTy n (List.nil RV)))]))))
