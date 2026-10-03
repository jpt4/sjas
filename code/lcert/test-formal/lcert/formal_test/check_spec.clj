(ns lcert.formal-test.check-spec
  "Formal suite (ADR-0006), lcert.formal.check-spec: the concrete checker
  Check decD (by recursion on the certificate's size) and the trust-base
  facts as theorems about it — CheckSpec, TokSize, TypeSize — for every
  decoder decD (F7 5b)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-spec]))

;; A decoder that reads every code as one fixed certificate: ⋆ : Unit at
;; budget 0, by rConst, with the formation tree fBase.
(def ^:private decD1
  '(fn [c :- Code]
     (Option.some (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))
       (Prod.mk Nat (Prod Exp (Prod Exp (Prod DT DT))) 0
         (Prod.mk Exp (Prod Exp (Prod DT DT)) Exp.star
           (Prod.mk Exp (Prod DT DT) Exp.tUnit
             (Prod.mk DT DT (DT.rConst (List.nil Exp) (List.nil U) Exp.star Exp.tUnit)
                            (DT.fBase (List.nil Exp) Exp.tUnit))))))))
(def ^:private decD0 '(fn [c :- Code] (Option.none (Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))))
(def ^:private one-node '(Code.sn 0 (Code.sl 0) (Code.sl 0)))

(defn- check-is [dec c d v]
  (not (b/rejects? [] (list 'Eq 'Bool (list 'Check dec c d) v) '[(rfl)])))

(deftest f7-concrete-checker
  (testing "the checker, its fixed point, and the trust base as theorems"
    (doseq [nm '[restrC bodyD bodyC chkN Check decOf restr_agree restr_self_agree bodyD_congr bodyC_congr
                 chkN_stable check_fix check_full check_spec check_toksize check_typesize]]
      (is (b/has? nm) (str nm))))
  (testing "a decoder that decodes nothing: every code is rejected"
    (is (check-is decD0 one-node '(encE Exp.tUnit) 'Bool.false)))
  (testing "a valid certificate of ⋆ : Unit is accepted, for the right type"
    (is (check-is decD1 one-node '(encE Exp.tUnit) 'Bool.true)))
  (testing "the same certificate is rejected for another type"
    (is (check-is decD1 one-node '(encE Exp.tBool) 'Bool.false)))
  (testing "a code too small for its budget is rejected (m < ‖c‖ is checked)"
    (is (check-is decD1 '(Code.sl 0) '(encE Exp.tUnit) 'Bool.false)))
  (testing "the trust base holds of Check, for an arbitrary decoder"
    (is (not (b/rejects? '[decD :- (=> Code (Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))))]
      '(And (CheckSpec (Check decD) (decOf decD) encE)
            (And (TokSize (Check decD) (decOf decD)) (TypeSize (Check decD) (decOf decD) encE)))
      '[(exact (And.intro (check_spec decD) (And.intro (check_toksize decD) (check_typesize decD))))])))))
