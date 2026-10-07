(ns lcert.formal-test.s52inv
  "Formal suite (ADR-0006), lcert.formal.s52inv: inversion of Ev and EvE for the
  universal Theorem 5.2."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52inv]))

(def ^:private P
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])

(deftest f5-s52inv
  (testing "the inversion functions and theorems exist, for both evaluators"
    (doseq [c '[InvT_Ev InvAp_Ev InvIter_Ev InvRecs_Ev InvItr_Ev InvAll_Ev ev_inv
                InvT_EvE InvAp_EvE InvIter_EvE InvRecs_EvE InvItr_EvE InvAll_EvE evE_inv]]
      (is (b/has? c) (str c))))
  (testing "a derivation of a constant concludes that constant (inversion computes)"
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV), v :- RV,
                                   h :- (Ev chkf dec encTy n (EvSrc.tm rho Exp.star) v)])
                         '(Eq RV v RV.star)
                         '[(exact (ev_inv chkf dec encTy n (EvSrc.tm rho Exp.star) v h))]))))
  (testing "...and no other constant"
    (is (b/rejects? (into P '[n :- Nat, rho :- (List RV), v :- RV,
                              h :- (Ev chkf dec encTy n (EvSrc.tm rho Exp.star) v)])
                    '(Eq RV v RV.token)
                    '[(exact (ev_inv chkf dec encTy n (EvSrc.tm rho Exp.star) v h))])))
  (testing "an application is the three premises of eApp, and only that rule"
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV), f :- Exp, u :- Exp, w :- RV,
                                   h :- (Ev chkf dec encTy n (EvSrc.tm rho (Exp.app f u)) w)])
                         '(Exists (fn [vf :- RV] (Exists (fn [vu :- RV]
                            (And (Ev chkf dec encTy n (EvSrc.tm rho f) vf)
                              (And (Ev chkf dec encTy n (EvSrc.tm rho u) vu)
                                   (Ev chkf dec encTy n (EvSrc.ap vf vu) w)))))))
                         '[(exact (ev_inv chkf dec encTy n (EvSrc.tm rho (Exp.app f u)) w h))]))))
  (testing "the evaluator is nondeterministic exactly at bnil: two tags, two derivations"
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV)])
                         '(And (Ev chkf dec encTy n (EvSrc.tm rho Exp.bnil) (RV.bnil Sk.unit))
                               (Ev chkf dec encTy n (EvSrc.tm rho Exp.bnil) (RV.bnil Sk.bool)))
                         '[(constructor)
                           (exact (Ev.eBnil chkf dec encTy n rho Sk.unit))
                           (exact (Ev.eBnil chkf dec encTy n rho Sk.bool))])))
    (is (b/rejects? (into P '[n :- Nat, rho :- (List RV)])
                    '(And (Ev chkf dec encTy n (EvSrc.tm rho Exp.star) RV.star)
                          (Ev chkf dec encTy n (EvSrc.tm rho Exp.star) RV.token))
                    '[(constructor)
                      (exact (Ev.eStar chkf dec encTy n rho))
                      (exact (Ev.eStar chkf dec encTy n rho))]))))
