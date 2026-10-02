(ns lcert.formal-test.check-dt
  "F7 derivation data: extracting a conclusion is separate from validating it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-dt :as c]))

(deftest derivation-tree-data
  (doseq [nm '[StepDT stepDTCheck stepDTCheck_sound DT DTJ concl]]
    (is (b/has? nm) (str nm)))
  (doseq [[family rule] c/dt-rules]
    (is (b/has? (symbol (str "DT." (first rule)))) (str family "/" (first rule))))
  (testing "a type-level constant record has its explicitly claimed conclusion"
    (is (not (b/rejects? '[]
      '(Eq (Option DTJ) (concl (DT.zConst (List.nil Exp) Exp.star Exp.tUnit))
         (Option.some DTJ (DTJ.tl Bool.false (List.nil Exp) Exp.star Exp.tUnit))) '[(rfl)]))))
  (testing "runtime application computes the conclusion's usage arithmetic"
    (is (not (b/rejects? '[]
      '(Eq (Option DTJ)
         (concl (DT.rApp (List.cons Exp Exp.tUnit (List.nil Exp))
                    (List.cons U U.u1 (List.nil U)) (List.cons U U.u1 (List.nil U)) U.u1
                    Exp.star Exp.star Exp.tUnit Exp.tUnit
                    (DT.zConst (List.nil Exp) Exp.star Exp.tUnit)
                    (DT.zConst (List.nil Exp) Exp.star Exp.tUnit)
                    (DT.fBase (List.nil Exp) Exp.tUnit) (DT.fBase (List.nil Exp) Exp.tUnit)))
         (Option.some DTJ (DTJ.rt (List.cons Exp Exp.tUnit (List.nil Exp))
           (List.cons U U.uw (List.nil U)) (Exp.app Exp.star Exp.star) Exp.tUnit))) '[(rfl)]))))
  (testing "conclusion extraction does not prove a malformed record's claim"
    (is (not (b/rejects? '[]
      '(Eq (Option DTJ) (concl (DT.zConst (List.nil Exp) Exp.tt Exp.tNat))
         (Option.some DTJ (DTJ.tl Bool.false (List.nil Exp) Exp.tt Exp.tNat))) '[(rfl)])))
    (is (b/rejects? '[]
      '(Tl (fn [c :- Code, d :- Code] Bool.false) Bool.false (List.nil Exp) Exp.tt Exp.tNat)
      '[(exact (DT.zConst (List.nil Exp) Exp.tt Exp.tNat))]))))

(deftest stored-step-check
  (is (not (b/rejects? '[]
    '(Eq Bool (stepDTCheck (fn [c :- Code, d :- Code] Bool.false)
               (StepDT.at (List.nil Nat) HdDT.tTT (Exp.tT Exp.tt) Exp.tUnit)) Bool.true) '[(rfl)])))
  (is (not (b/rejects? '[]
    '(Eq Bool (stepDTCheck (fn [c :- Code, d :- Code] Bool.false)
               (StepDT.at (List.nil Nat) HdDT.tTT (Exp.tT Exp.tt) Exp.tEmpty)) Bool.false) '[(rfl)]))))
