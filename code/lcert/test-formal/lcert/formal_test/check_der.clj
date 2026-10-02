(ns lcert.formal-test.check-der
  "Formal suite (ADR-0006), lcert.formal.check-der: the checker for whole
  derivation trees (F7 step 3) and its soundness — dtCheck chkf t J = true
  implies the judgment J holds (Tl, Rt or Cv at chkf)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-dt :as c]
            [lcert.formal.check-der]))

(def ^:private chkf '[chkf :- (=> Code Code Bool)])
(def ^:private nilE '(List.nil Exp))

(deftest f7-judgment-equality
  (testing "decided equality of usages, lists and judgments, each sound"
    (doseq [nm '[f7UEq f7UEq_sound f7OptU f7OptU_sound
                 listEqE listEqE_sound listEqU listEqU_sound listEqS listEqS_sound
                 djEq djEq_sound Holds]]
      (is (b/has? nm) (str nm))))
  (testing "judgments differing only in the mode are distinguished"
    (is (not (b/rejects? []
      (list 'Eq 'Bool (list 'djEq (list 'DTJ.tl 'Bool.true nilE 'Exp.star 'Exp.tUnit)
                                  (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tUnit)) 'Bool.false)
      '[(rfl)])))
    (is (b/rejects? []
      (list 'Eq 'Bool (list 'djEq (list 'DTJ.tl 'Bool.true nilE 'Exp.star 'Exp.tUnit)
                                  (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tUnit)) 'Bool.true)
      '[(rfl)])))
  (testing "a type-level and a runtime judgment are never equal"
    (is (not (b/rejects? []
      (list 'Eq 'Bool (list 'djEq (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tUnit)
                                  (list 'DTJ.rt nilE '(List.nil U) 'Exp.star 'Exp.tUnit)) 'Bool.false)
      '[(rfl)])))))

(deftest f7-derivation-checker
  (testing "the checker and its soundness, one lemma per Tl/Rt/Cv rule"
    (doseq [nm '[dtCheck dtCheck_sound f7Step_tr]] (is (b/has? nm) (str nm)))
    (doseq [[_ rule] c/dt-rules]
      (is (b/has? (symbol (str "dtCheck_" (first rule) "_sound"))) (str (first rule)))))
  (testing "a valid constant record is accepted, at an arbitrary chkf"
    (is (not (b/rejects? chkf
      (list 'Eq 'Bool (list 'dtCheck 'chkf (list 'DT.zConst nilE 'Exp.star 'Exp.tUnit)
                            (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tUnit)) 'Bool.true)
      '[(rfl)]))))
  (testing "the same record is rejected for a judgment it does not conclude"
    (is (not (b/rejects? chkf
      (list 'Eq 'Bool (list 'dtCheck 'chkf (list 'DT.zConst nilE 'Exp.star 'Exp.tUnit)
                            (list 'DTJ.tl 'Bool.true nilE 'Exp.star 'Exp.tUnit)) 'Bool.false)
      '[(rfl)]))))
  (testing "a record whose side condition fails is rejected: ⋆ is not a Bool"
    (is (not (b/rejects? chkf
      (list 'Eq 'Bool (list 'dtCheck 'chkf (list 'DT.zConst nilE 'Exp.star 'Exp.tBool)
                            (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tBool)) 'Bool.false)
      '[(rfl)]))))
  (testing "a premise must conclude what its rule needs: fT over a non-Bool premise"
    (is (not (b/rejects? chkf
      (list 'Eq 'Bool (list 'dtCheck 'chkf
                            (list 'DT.fT nilE 'Exp.star (list 'DT.zConst nilE 'Exp.star 'Exp.tUnit))
                            (list 'DTJ.tl 'Bool.true nilE '(Exp.tT Exp.star) 'Exp.tUnit)) 'Bool.false)
      '[(rfl)]))))
  (testing "soundness yields the judgment: an accepted record derives Tl"
    (is (not (b/rejects? chkf
      (list 'Tl 'chkf 'Bool.false nilE 'Exp.star 'Exp.tUnit)
      [(list 'exact (list 'dtCheck_sound 'chkf (list 'DT.zConst nilE 'Exp.star 'Exp.tUnit)
                          (list 'DTJ.tl 'Bool.false nilE 'Exp.star 'Exp.tUnit) '(rfl)))])))))
