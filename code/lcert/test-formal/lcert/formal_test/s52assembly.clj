(ns lcert.formal-test.s52assembly
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52assembly]))

(deftest s52-assembly-support
  (testing "constant dispatch and context extension preserve S's hypotheses"
    (doseq [c '[s52_const s52_wfs_y1 s52_wfs_y2 s52_wfs_insp1 s52_wfs_insp2]]
      (is (b/has? c) (str c))))
  (testing "out-of-range label constants cannot supply S(Lbl)"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
        encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool]
      '(Result52 chkf dec encTy cap erasing (List.nil Sk) Unit.unit (List.nil RV)
         (Exp.lbl 100) Exp.tLbl (Exp.lbl 100))
      '[(exact (s52_const chkf dec encTy cap erasing (List.nil Sk) Unit.unit (List.nil RV)
                 (Exp.lbl 100) Exp.tLbl (Eq.refl Bool.true)))]))))

(deftest s52-assembly-environment
  (testing "entry evidence, environment lengths and the usage-free variable case"
    (doseq [c '[s52_ent_lbl s52_ent_nat s52_ent_syn s52_ent_cert env52_len s52_nth_cc s52_nth_ex
                s52_var_free s52_tf_absurd]]
      (is (b/has? c) (str c))))
  (testing "a label at the bound is not an S(Lbl) entry"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
        encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool]
      '(S52 chkf dec encTy cap erasing Exp.tLbl (List.nil Sk) Unit.unit Sk.lbl (RV.lbl 100) 100)
      '[(constructor) (rfl)]))))

(deftest s52-assembly-inductions
  (testing "the fundamental property for evalₙ, by induction on Tl and on Rt"
    (doseq [c '[S52JudgR S52JudgT s52_tl_step s52_rt_step]]
      (is (b/has? c) (str c))))
  (testing "a derivation of Empty gives no safe value: S(0) is empty"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
        encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool, v :- RV,
        a :- (Car Sk.unit)]
      '(S52 chkf dec encTy cap erasing Exp.tEmpty (List.nil Sk) Unit.unit Sk.unit v a)
      '[(constructor)]))))
