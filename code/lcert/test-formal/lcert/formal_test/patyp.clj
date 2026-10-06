(ns lcert.formal-test.patyp
  "F6.3: the exact closed arithmetic typings used by R4 §6.2.
  These checks apply the theorems at arbitrary checkers, so neither an
  accepting checker nor an unproved CheckSpec is a hidden hypothesis."
  (:require [lcert.formal.patyp]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(def signatures
  '[[trISZERO (Exp.tPi U.uw Exp.tNat Exp.tBool)]
    [trPRED (Exp.tPi U.uw Exp.tNat Exp.tNat)]
    [trEQ (Exp.tPi U.uw Exp.tNat (Exp.tPi U.uw Exp.tNat Exp.tBool))]
    [trPLUS (Exp.tPi U.u1 Exp.tNat (Exp.tPi U.uw Exp.tNat Exp.tNat))]
    [trTIMES (Exp.tPi U.uw Exp.tNat (Exp.tPi U.uw Exp.tNat Exp.tNat))]])

(deftest closed-arithmetic-typings
  (testing "every closed arithmetic constant has its precise runtime type"
    (doseq [[term ty] signatures
            :let [theorem (symbol (str term "_rt"))]]
      (is (b/has? theorem) (str theorem))
      (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                 (list 'Rt 'chkf '(List.nil Exp) '(List.nil U) term ty)
                 [(list 'exact (list theorem 'chkf))])) (str term))))
  (testing "constants appearing inside types also have type-level typings"
    (doseq [[term ty] (drop 2 signatures)
            :let [theorem (symbol (str term "_tl"))]]
      (is (b/has? theorem) (str theorem))
      (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                 (list 'Tl 'chkf 'Bool.false '(List.nil Exp) term ty)
                 [(list 'exact (list theorem 'chkf))])) (str term))))
  (testing "PLUS's first binder really has usage 1, and EQ returns Bool"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
          '(Rt chkf (List.nil Exp) (List.nil U) trPLUS
             (Exp.tPi U.uw Exp.tNat (Exp.tPi U.uw Exp.tNat Exp.tNat)))
          '[(exact (trPLUS_rt chkf))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
          '(Tl chkf Bool.false (List.nil Exp) trEQ
             (Exp.tPi U.uw Exp.tNat (Exp.tPi U.uw Exp.tNat Exp.tNat)))
          '[(exact (trEQ_tl chkf))])))
  (testing "the empty-context theorem cannot supply an extra usage entry"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
          '(Rt chkf (List.nil Exp) (List.cons U U.uw (List.nil U)) trPRED
             (Exp.tPi U.uw Exp.tNat Exp.tNat))
          '[(exact (trPRED_rt chkf))]))))
