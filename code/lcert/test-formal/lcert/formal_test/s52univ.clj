(ns lcert.formal-test.s52univ
  "Formal suite (ADR-0006), lcert.formal.s52univ: Theorem 5.2, universal form."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52univ]))

(def ^:private P
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])

(deftest f5-s52univ
  (testing "the universal theorems"
    (doseq [c '[ue_step ue_below ue_all theorem52_all theorem52_all_base theorem52e_all
                theorem52_all_concrete]]
      (is (b/has? c) (str c))))
  (testing "the typing hypothesis is used: an Ev run of an untyped program reaches abort"
    ;; abort tUnit star is Ev-evaluable (to the default of Unit) but has no safe trace.
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV)])
                         '(And (Ev chkf dec encTy n (EvSrc.tm rho (Exp.abort Exp.tUnit Exp.star)) (rdflt Sk.unit))
                               (Not (Ok chkf dec encTy n (EvSrc.tm rho (Exp.abort Exp.tUnit Exp.star)) (rdflt Sk.unit))))
                         '[(constructor)
                           (exact (Ev.eAbort chkf dec encTy n rho Exp.tUnit Exp.star RV.star
                                    (Ev.eStar chkf dec encTy n rho)))
                           (exact (trace52_no_abort chkf dec encTy Bool.false n rho Exp.tUnit Exp.star
                                    (rdflt Sk.unit)))]))))
  (testing "two runs of a typed program that differ in a bnil tag are both safe"
    ;; bnil : tBrs tUnit NL is derivable in Θₙ; Ev may tag it with any skeleton.
    (is (not (b/rejects? (into P '[hcs :- (CheckSpec chkf dec encTy), n :- Nat])
                         '(And (Ok chkf dec encTy n (EvSrc.tm (rtokens n) Exp.bnil) (RV.bnil Sk.unit))
                               (Ok chkf dec encTy n (EvSrc.tm (rtokens n) Exp.bnil) (RV.bnil Sk.bool)))
                         '[(have hd (Rt chkf (thetaD n) (thetaU n) Exp.bnil (Exp.tBrs Exp.tUnit (NL)))
                             (Rt.rBnil chkf (thetaD n) (thetaU n) Exp.tUnit
                               (Eq.trans (lenU_theta n) (Eq.symm (lenE_theta n)))))
                           (constructor)
                           (exact ((theorem52_all chkf dec encTy hcs n Exp.bnil (Exp.tBrs Exp.tUnit (NL)) hd)
                                   (RV.bnil Sk.unit) (Ev.eBnil chkf dec encTy n (rtokens n) Sk.unit)))
                           (exact ((theorem52_all chkf dec encTy hcs n Exp.bnil (Exp.tBrs Exp.tUnit (NL)) hd)
                                   (RV.bnil Sk.bool) (Ev.eBnil chkf dec encTy n (rtokens n) Sk.bool)))]))))
  (testing "the Ev runs of bnil really have different values (the tag is real)"
    (is (b/rejects? '[] '(Eq RV (RV.bnil Sk.unit) (RV.bnil Sk.bool)) '[(rfl)])))
  (testing "no hypothesis on the checker at the concrete instance"
    (is (not (b/rejects? '[n :- Nat, t :- Exp, X :- Exp, hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t X)]
                         '(forall [v RV]
                            (=> (Eval (Check decCert) (decOf decCert) encE n (rtokens n) t v)
                                (Ok (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) t) v)))
                         '[(exact (And.left (theorem52_all_concrete n t X hd)))])))))
