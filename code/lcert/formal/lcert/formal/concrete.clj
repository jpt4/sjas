(ns lcert.formal.concrete
  "Fidelity pass (afix, 2026-10-15): the paper's results at the concrete
  checker.

  R4 states its results for λᶜᵉʳᵗ₀, whose chk′ denotes one checker, Check.
  Most formal theorems are proved for any checker meeting the trust base
  CheckSpec (and, where needed, TokSize, TypeSize and typed conversion
  ConvCase).  Those premises are hypotheses about the checker, not about the
  paper's objects, so the audit classed the generic statements as weaker.
  Each theorem here is the generic one at F7's canonical checker
  Check decCert, its decoder decOf decCert and the encoder encE, with every
  checker premise discharged:
  - check_spec, check_toksize, check_typesize (check_spec.clj);
  - conv_all, typed conversion for every checker meeting CheckSpec
    (convcase.clj, ConvAll in lemma36.clj).
  What remains in each statement is a premise of the paper's own statement,
  or one recorded in R4 and the audit: a well-formed context (WFCtx), labels
  in the finite label set (lblOk), or a typed derivation."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.lemma36]
            [lcert.formal.check-spec]
            [lcert.formal.convcase]
            [lcert.formal.section4]
            [lcert.formal.prop434]
            [lcert.formal.lemma46a]
            [lcert.formal.theorem4]
            [lcert.formal.theorem4e]
            [lcert.formal.certcanon]
            [lcert.formal.section4b]
            [lcert.formal.thm46f7]))

;; Lemma 3.6 at Check: every runtime derivation in a well-formed context is
;; sound at every budget.
(thm lemma36_F7 [n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                 der :- (Rt (Check decCert) D us t A),
                 hw :- (WFCtx (Check decCert) D)]
  (Sound (Check decCert) (decOf decCert) encE n D us t A)
  (exact (lemma36 (Check decCert) (decOf decCert) encE (check_spec decCert)
           (conv_all (Check decCert) (decOf decCert) encE (check_spec decCert))
           n D us t A der hw)))

;; T3 at Check: an R value's footprint is at most the tokens supplied.
(thm theorem3_F7 [n :- Nat, D :- (List Exp), us :- (List U), t :- Exp,
                  der :- (Rt (Check decCert) D us t Exp.tR),
                  hw :- (WFCtx (Check decCert) D),
                  en :- (HEnv (skels D)), k :- Nat, hk :- (Nat.le k n),
                  hs :- (EnvSat (Check decCert) (decOf decCert) encE n D us en k)]
  (LE.le (cnodes (den (Check decCert) (decOf decCert) encE n t (skels D) Sk.cert en)) k)
  (exact (theorem3 (Check decCert) (decOf decCert) encE (check_spec decCert)
           (conv_all (Check decCert) (decOf decCert) encE (check_spec decCert))
           n D us t der hw en k hk hs)))

;; T3's token form at Check: at most the number of distinct tokens used.
(thm theorem3_tokens_F7 [m :- Nat, n :- Nat, t :- Exp,
                         der :- (Rt (Check decCert) (thetaD m) (thetaU m) t Exp.tR),
                         hn :- (Nat.le (cntU (maskUF (thetaU m) (freshF t))) n)]
  (LE.le (cnodes (den (Check decCert) (decOf decCert) encE n t (skels (thetaD m))
                   Sk.cert (tokEnvD m)))
         (cntU (maskUF (thetaU m) (freshF t))))
  (exact (theorem3_tokens (Check decCert) (decOf decCert) encE (check_spec decCert)
           (conv_all (Check decCert) (decOf decCert) encE (check_spec decCert))
           m n t der hn)))

;; Proposition 4.1 at Check: a closed R term denotes a leaf, and so does a
;; closed Syn ⊸ R function at every code with labels in L.
(thm prop41_closed_F7 [n :- Nat, t :- Exp,
                       der :- (Rt (Check decCert) (List.nil Exp) (List.nil U) t Exp.tR)]
  (Exists (fn [l :- Nat]
    (Eq Code (den (Check decCert) (decOf decCert) encE n t (List.nil Sk) Sk.cert Unit.unit)
             (Code.sl l))))
  (exact (prop41_closed (Check decCert) (decOf decCert) encE (check_spec decCert)
           (conv_all (Check decCert) (decOf decCert) encE (check_spec decCert))
           n t der)))

(thm prop41_fun_F7 [n :- Nat, f :- Exp,
                    der :- (Rt (Check decCert) (List.nil Exp) (List.nil U) f
                             (Exp.tPi U.u1 Exp.tSyn Exp.tR)),
                    c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)]
  (Exists (fn [l :- Nat]
    (Eq Code (den (Check decCert) (decOf decCert) encE n f (List.nil Sk)
               (Sk.arr Sk.syn Sk.cert) Unit.unit c)
             (Code.sl l))))
  (exact (prop41_fun (Check decCert) (decOf decCert) encE (check_spec decCert)
           (conv_all (Check decCert) (decOf decCert) encE (check_spec decCert))
           n f der c hc)))

;; Lemma 4.6a at Check: soundness under any phantom interpretation whose
;; labels lie in L, in a well-formed context.
(thm lemma46a_F7 [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), n :- Nat,
                  D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                  der :- (Rt (Check decCert) D us t A),
                  hw :- (WFCtx (Check decCert) D)]
  (Sound_ri (Check decCert) (decOf decCert) encE (phRI J sg hsg) n D us t A)
  (exact (lemma46a (Check decCert) (decOf decCert) encE (check_spec decCert)
           J sg hsg n D us t A der hw)))

;; T4 (adequacy of the budgeted evaluator) at Check, at every budget.
(thm theorem4_F7 [n :- Nat]
  (Adeq (Check decCert) (decOf decCert) encE n)
  (exact (theorem4 (Check decCert) (decOf decCert) encE (check_spec decCert) n)))

;; Corollary 5.1 at Check: a typed R program's value has at most n nodes.
(thm corollary51_nodes_F7 [n :- Nat, t :- Exp,
                           hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tR)]
  (LE.le (cnodes (den (Check decCert) (decOf decCert) encE n t (skels (thetaD n))
                   Sk.cert (tokEnvD n)))
         n)
  (exact (corollary51_nodes (Check decCert) (decOf decCert) encE (check_spec decCert)
           n t hd)))

;; Theorem 4′ at Check: the erasing evaluator computes a typed program's R
;; result within n nodes, for the erasure the runner computes.
(thm theorem4e_nodes_F7 [n :- Nat, t :- Exp,
                         hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tR),
                         e :- Exp, i :- (Er (Check decCert) (thetaD n) (thetaU n) t Exp.tR e)]
  (Exists (fn [c :- Code]
    (And (EvalE (Check decCert) (decOf decCert) encE n (rtokens n) e (RV.cert c))
         (LE.le (cnodes c) n))))
  (exact (theorem4e_nodes (Check decCert) (decOf decCert) encE (check_spec decCert)
           n t hd e i)))

(thm theorem4e_agree_F7 [n :- Nat, t :- Exp, X :- Exp,
                         hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t X),
                         hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (Exists (fn [e :- Exp]
    (And (Er (Check decCert) (thetaD n) (thetaU n) t X e)
         (Exists (fn [v :- RV]
           (And (EvalE (Check decCert) (decOf decCert) encE n (rtokens n) e v)
                (Eval (Check decCert) (decOf decCert) encE n (rtokens n) t v)))))))
  (exact (theorem4e_agree (Check decCert) (decOf decCert) encE (check_spec decCert)
           n t X hd hb)))

;; Proposition 4.5 at the paper's budgets.  prop45_contraction and prop45_d3
;; accept any accepted label-safe certificate; here the certificate is a
;; least one (thm46f7's least_cert_witness), so the budgets are exactly
;; 2 μ_L(A) and μ_L(□A), as R4 §4.4 states them.
(thm prop45_contraction_mu [A :- Exp, hA :- (Eq Bool (lblOk (encE A)) Bool.true),
                            h :- (Certifiable A)]
  (Exists (fn [t :- Exp]
    (Rt (Check decCert)
      (thetaD (Nat.add (muCert A h) (muCert A h)))
      (thetaU (Nat.add (muCert A h) (muCert A h)))
      t
      (Exp.tPi U.u1 (boxTy (codeTerm (encE A)))
        (Exp.tSig U.u1 (boxTy (codeTerm (encE A))) (boxTy (codeTerm (encE A))))))))
  (refine' (exT Code _ _ (least_cert_witness A h) _))
  (intro v hv)
  (have hs (And (Eq Bool (lblOk v) Bool.true) (Eq Bool (Check decCert v (encE A)) Bool.true))
    (And.left hv))
  (have he (Eq Nat (cnodes v) (muCert A h)) (And.left (And.right hv)))
  (rw [(Eq.symm he)])
  (exact (Exists.intro
    (Exp.lam U.u1 (boxTy (codeTerm (encE A)))
      (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encE A))) (boxTy (codeTerm (encE A))))
        (Exp.pair (boxTy (codeTerm (encE A))) (litAt v (Nat.add (cnodes v) 1)) Exp.star)
        (Exp.pair (boxTy (codeTerm (encE A))) (litAt v 1) Exp.star)))
    (prop45_contraction (Check decCert) (decOf decCert) encE A v
      (And.left hs) hA (And.right hs)))))

(thm prop45_d3_mu [A :- Exp, hA :- (Eq Bool (lblOk (encE A)) Bool.true),
                   hB :- (Eq Bool (lblOk (encE (boxTy (codeTerm (encE A))))) Bool.true),
                   h :- (Certifiable (boxTy (codeTerm (encE A))))]
  (Exists (fn [t :- Exp]
    (Rt (Check decCert)
      (thetaD (muCert (boxTy (codeTerm (encE A))) h))
      (thetaU (muCert (boxTy (codeTerm (encE A))) h))
      t
      (Exp.tPi U.u1 (boxTy (codeTerm (encE A)))
        (boxTy (codeTerm (encE (boxTy (codeTerm (encE A))))))))))
  (refine' (exT Code _ _ (least_cert_witness (boxTy (codeTerm (encE A))) h) _))
  (intro w hw)
  (have hs (And (Eq Bool (lblOk w) Bool.true)
               (Eq Bool (Check decCert w (encE (boxTy (codeTerm (encE A))))) Bool.true))
    (And.left hw))
  (have he (Eq Nat (cnodes w) (muCert (boxTy (codeTerm (encE A))) h))
    (And.left (And.right hw)))
  (rw [(Eq.symm he)])
  (exact (Exists.intro
    (Exp.lam U.u1 (boxTy (codeTerm (encE A)))
      (Exp.pair (boxTy (codeTerm (encE (boxTy (codeTerm (encE A)))))) (litAt w 1) Exp.star))
    (prop45_d3 (Check decCert) (decOf decCert) encE A w
      (And.left hs) hA hB (And.right hs)))))

;; Theorem 4.6 in the paper's form, k ≥ μ(B), at Check.  The closed
;; certifiable types A_i come with chosen certificates sg i (labels in L,
;; PhOk); thm46_types_F7 yields an accepted certificate of B with at most k
;; nodes, and μ(B) is at most its size (A13's muRaw).
(thm thm46_mu_F7 [r :- Nat, sg :- (=> Nat Code), hsg :- (PhOk (Nat.add r 1) sg),
                  As :- (=> Nat Exp), B :- Exp, k :- Nat, t :- Exp,
                  hclB :- (Eq Bool (closedTy B) Bool.true),
                  hc :- (forall [i Nat] (=> (LT.lt i (Nat.add r 1))
                          (Eq Bool (Check decCert (sg i) (encE (As i))) Bool.true))),
                  hne :- (forall [i Nat] (=> (LT.lt i (Nat.add r 1))
                           (Not (Eq Exp B (As i))))),
                  der :- (Rt (Check decCert) (thetaD k) (thetaU k) t
                           (Exp.tPi U.u1 (tensB (fn [i :- Nat] (encE (As i))) 0 r)
                             (boxTy (codeTerm (encE B))))),
                  hB :- (RawCertifiable B)]
  (LE.le (muRaw B hB) k)
  (refine' (exT Code _ _ (thm46_types_F7 r sg hsg As B k t hclB hc hne der) _))
  (intro v hv)
  (exact (Nat.le_trans (muRaw_lower B hB v (And.left hv))
                       (And.right (And.right hv)))))

;; Proposition 4.4 (2) and (3) at Check: the size premises TokSize and
;; TypeSize are check_toksize and check_typesize.
(thm prop44_2_F7 [k :- Nat, q :- Exp,
                  der :- (Rt (Check decCert) (thetaD k) (thetaU k) q
                           (Exp.tPi U.u1
                             (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encE (negN (Nat.add k 1))))))
                             (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0)
                               (cTerm (encE (Exp.tSig U.u1 Exp.tR
                                 (chkT (Exp.var 0) (cTerm (encE (negN (Nat.add k 1))))))))))))
                  v :- Code,
                  hv :- (Eq Bool (Check decCert v (encE (negN (Nat.add k 1)))) Bool.true),
                  hvok :- (Eq Bool (lblOk v) Bool.true),
                  hmin :- (forall [v2 Code]
                            (=> (Eq Bool (Check decCert v2 (encE (negN (Nat.add k 1)))) Bool.true)
                                (LE.le (cnodes v) (cnodes v2))))]
  False
  (exact (prop44_2 (Check decCert) (decOf decCert) encE (check_spec decCert)
           (check_toksize decCert) (check_typesize decCert) k q der v hv hvok hmin)))

(thm prop44_3_F7 [k :- Nat, q :- Exp,
                  der :- (Rt (Check decCert) (thetaD k) (thetaU k) q
                           (Exp.tPi U.u1
                             (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encE (negN (Nat.add k 1))))))
                             (Exp.tSig U.u1
                               (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encE (negN (Nat.add k 1))))))
                               (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encE (negN (Nat.add k 1)))))))))
                  v :- Code,
                  hv :- (Eq Bool (Check decCert v (encE (negN (Nat.add k 1)))) Bool.true),
                  hvok :- (Eq Bool (lblOk v) Bool.true),
                  hmin :- (forall [v2 Code]
                            (=> (Eq Bool (Check decCert v2 (encE (negN (Nat.add k 1)))) Bool.true)
                                (LE.le (cnodes v) (cnodes v2))))]
  False
  (exact (prop44_3 (Check decCert) (decOf decCert) encE (check_spec decCert)
           (check_typesize decCert) k q der v hv hvok hmin)))
