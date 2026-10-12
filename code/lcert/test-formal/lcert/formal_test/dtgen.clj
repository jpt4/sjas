(ns lcert.formal-test.dtgen
  "Tests for the untrusted F6.3 derivation-data generator.

  Kernel acceptance, including deliberately corrupted data, is the final
  check. Coverage targets the PA fragment (functions, Nat and Bool
  elimination, dependent conversion and usage allocation). The generator
  deliberately does not implement every Exp typing rule: certificates,
  labels and branch lists are outside this unit's coverage and purpose."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check-der]
            [lcert.formal.dtgen :as g]))

(defn accepted? [tree J]
  (not (b/rejects? '[chkf :- (=> Code Code Bool)]
         (list 'Eq 'Bool (list 'dtCheck 'chkf tree J) 'Bool.true) '[(rfl)])))

(deftest binders-and-substitution
  (testing "a substitution under a binder lifts its replacement"
    (let [e (g/compile-term {} '[ ] '(λ :uw [x Nat] x))]
      (is (= [:lam :uw [:tNat] [:var 0]] e)))
    (let [e [:lam :uw [:tNat] [:app [:var 1] [:var 0]]]
          u [:var 3]]
      (is (= [:lam :uw [:tNat] [:app [:var 4] [:var 0]]]
             (g/subst1 u e)))
      (is (not (b/rejects? []
                 (list 'Eq 'Exp (list 'subst1 (g/sx u) (g/sx e))
                       (g/sx (g/subst1 u e))) '[(rfl)])))))
  (testing "recN's step is under two binders; annotations use no runtime resource"
    (let [e [:recN [:tT [:var 1]] [:var 2] [:var 3] [:var 0]]]
      (is (= #{0 1 2} (g/rfv e)))
      (is (not (b/rejects? []
                 (list 'Eq 'Exp (list 'lift 2 0 (g/sx e)) (g/sx (g/lift 2 0 e)))
                 '[(rfl)]))))))

(deftest reflected-runtime-and-type-level
  (let [term (g/compile-term {} '(λ :u1 [n Nat] (succ n)))
        ty [:tPi :u1 [:tNat] [:tNat]]
        rt (g/rt [] [] term ty)
        tl (g/tl0-check [] term ty)
        Jrt (list 'DTJ.rt '(List.nil Exp) '(List.nil U) (g/sx term) (g/sx ty))
        Jtl (list 'DTJ.tl 'Bool.false '(List.nil Exp) (g/sx term) (g/sx ty))]
    (is (accepted? rt Jrt))
    (is (accepted? tl Jtl))
    (is (not (accepted? rt Jtl)) "a runtime tree is not type-level evidence")
    (is (not (accepted? rt (list 'DTJ.rt '(List.nil Exp) '(List.nil U)
                                (g/sx term) 'Exp.tBool))))))

(deftest conversion-and-sharing
  (let [term [:star] ty [:tT [:tt]]
        tree (g/rt [] [] term ty)
        J '(DTJ.rt (List.nil Exp) (List.nil U) Exp.star (Exp.tT Exp.tt))
        [defs shared] (g/share (str (gensym "dtgen_test_")) 3 tree)]
    (is (accepted? tree J) "T(tt) converts to Unit")
    (is (seq defs) "the sharing test actually creates definitions")
    (doseq [[nm sort body] defs] (b/kdef! nm sort body))
    (is (accepted? shared J))
    (is (not (accepted? shared
               '(DTJ.rt (List.nil Exp) (List.nil U) Exp.star (Exp.tT Exp.ff)))))))

(deftest affine-resource-failures
  (testing "the generator refuses duplication or omega scaling of usage 1"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"usage-1 variable used twice"
          (g/alloc [:u1] [{:fv #{0} :scale :u1} {:fv #{0} :scale :u1}])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"usage-1 variable in a scaled premise"
          (g/alloc [:u1] [{:fv #{0} :scale :uw}]))))
  (testing "kernel checking catches a forged variable usage even if generation is bypassed"
    (let [tree '(DT.rVar (List.cons Exp Exp.tNat (List.nil Exp))
                        (List.cons U U.u0 (List.nil U)) 0 Exp.tNat U.u0)
          J '(DTJ.rt (List.cons Exp Exp.tNat (List.nil Exp))
                     (List.cons U U.u0 (List.nil U)) (Exp.var 0) Exp.tNat)]
      (is (not (accepted? tree J))))))
