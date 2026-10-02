(ns lcert.formal-test.check-agree
  "Formal suite (ADR-0006), lcert.formal.check-agree: checking a derivation
  tree consults the checker only at the tree's δ-records, so two checkers
  that agree on first codes below the tree's bound check it alike (F7 5a)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-dt :as c]
            [lcert.formal.check-agree]))

(def ^:private delta-rec
  '(HdDT.delta (Exp.sleaf (Exp.lbl 3)) (Exp.sleaf (Exp.lbl 4)) (Code.sl 3) (Code.sl 4)))

(deftest f7-check-agreement
  (testing "bounds, agreement, and the transport, one lemma per rule"
    (doseq [nm '[hdB stepDTB dtB AgreeF agreeF_mono agreeF_zero hdTarget_agree hdCheck_agree
                 stepCheck_agree stepDTCheck_agree dtCheck_agree]]
      (is (b/has? nm) (str nm)))
    (doseq [[_ rule] c/dt-rules]
      (is (b/has? (symbol (str "dtCheck_" (first rule) "_agree"))) (str (first rule)))))
  (testing "a δ-record's bound exceeds its first code's size; other records have none"
    (is (not (b/rejects? [] (list 'Eq 'Nat (list 'hdB delta-rec) '(+ (cnodes (Code.sl 3)) 1)) '[(rfl)])))
    (is (not (b/rejects? [] '(Eq Nat (hdB (HdDT.iteT Exp.star Exp.star)) 0) '[(rfl)]))))
  (testing "the δ-step does consult the checker: two checkers give different contracta"
    (is (b/rejects? []
      (list 'Eq 'Exp (list 'hdTarget '(fn [c :- Code, d :- Code] Bool.true) delta-rec)
                     (list 'hdTarget '(fn [c :- Code, d :- Code] Bool.false) delta-rec))
      '[(rfl)])))
  (testing "a tree without δ-steps is checked alike by any two checkers (bound 0)"
    (is (not (b/rejects? '[chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool)]
      '(Eq Bool (dtCheck chk1 (DT.zConst (List.nil Exp) Exp.star Exp.tUnit) (DTJ.tl Bool.false (List.nil Exp) Exp.star Exp.tUnit))
                (dtCheck chk2 (DT.zConst (List.nil Exp) Exp.star Exp.tUnit) (DTJ.tl Bool.false (List.nil Exp) Exp.star Exp.tUnit)))
      '[(exact (dtCheck_agree chk1 chk2 (DT.zConst (List.nil Exp) Exp.star Exp.tUnit)
                 (agreeF_zero chk1 chk2)
                 (DTJ.tl Bool.false (List.nil Exp) Exp.star Exp.tUnit)))])))))
