(ns lcert.formal-test.s52det
  "Formal suite (ADR-0006), lcert.formal.s52det: Ok is determined up to bnil
  tags (the main lemma of the universal Theorem 5.2)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52det]))

(def ^:private P
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])

(def ^:private rules
  '[eVar eStar eTT eFF eZero eLbl eIteT eIteF eElimT eElimF eSucc eRecN eIterZ eIterS
    eCaseL eBnil eBcons eSleaf eSnode eRecS eRecSL eRecSN eLeaf eNode eItR eItL eItN ePrn
    eLam eApp ePair eLet eChk eReflOk eReflNo eReflNone eInspT eInspF apClos apBconsZ apBconsS])

(deftest f5-s52det
  (testing "the budget hypothesis and the motives"
    (doseq [c '[MP_Ok MP_OkE UE_E BELOW_E MPB_OkE]] (is (b/has? c) (str c))))
  (testing "helper lemmas"
    (doseq [c '[dec_det none_ne_some_dec refl_data sm_star_inv sm_bool_inv sm_nat_inv sm_code_inv
                sm_cert_inv s52_base_unique s52_base_sim]]
      (is (b/has? c) (str c))))
  (testing "one case lemma per safe-trace rule, for Ok and OkE"
    (doseq [O '[Ok OkE], r rules]
      (let [c (symbol (str "mc_" O "_" r))] (is (b/has? c) (str c)))))
  (testing "the main lemma, for every safe trace"
    (doseq [c '[detOk detOkE detOk_tm detOkE_tm]] (is (b/has? c) (str c))))
  (testing "Ok has no apBnil: a safe trace never applies the empty branch list"
    (is (not (b/has? 'mc_Ok_apBnil)))
    (is (b/rejects? (into P '[n :- Nat, s :- Sk, arg :- RV])
                    '(Ok chkf dec encTy n (EvSrc.ap (RV.bnil s) arg) (rdflt s))
                    '[(exact (Ok.apBnil chkf dec encTy n s arg))])))
  (testing "Ev has abort nodes and Ok does not: the lemma is about Ok-referenced runs only"
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV)])
                         '(Ev chkf dec encTy n (EvSrc.tm rho (Exp.abort Exp.tUnit Exp.star)) (rdflt Sk.unit))
                         '[(exact (Ev.eAbort chkf dec encTy n rho Exp.tUnit Exp.star RV.star
                                    (Ev.eStar chkf dec encTy n rho)))])))
    (is (not (b/rejects? (into P '[n :- Nat, rho :- (List RV)])
                         '(Not (Ok chkf dec encTy n (EvSrc.tm rho (Exp.abort Exp.tUnit Exp.star)) (rdflt Sk.unit)))
                         '[(exact (trace52_no_abort chkf dec encTy Bool.false n rho Exp.tUnit Exp.star
                                    (rdflt Sk.unit)))])))))
