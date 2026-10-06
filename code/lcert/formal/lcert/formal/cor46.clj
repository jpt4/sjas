(ns lcert.formal.cor46
  "F4 — Corollaries 4.6′ (D2) and 4.6″ (D3, exactly) of R4-metatheory.md §4.5.

  Both pair a lower bound from Theorem 4.6 (theorem46.clj) with an upper
  bound from Proposition 4.2 (section4b.clj).  The lower bounds take the
  hypotheses of thm46: CheckSpec, Enc46 for a term measure sz, and for each
  input type a certificate whose decoded term exceeds the budget in sz (the
  paper's fᵢ; completeness of the checker would supply one at every size —
  LargeCert below, a hypothesis since CheckSpec gives soundness only).  The
  upper bounds take only the certificate of the output type and the labels
  of the codes (lblOk, as in prop42 / prop45_d3).

  Corollary 4.6′ (D2).  Let A ≠ B be closed, with A and A ⊸ B certifiable.
  - d2_lower: Θₖ ⊢ t :¹ □(A ⊸ B) ⊗ □A ⊸ □B gives a certificate of B with at
    most k nodes (thm46_types, two inputs).  B ≠ A ⊸ B needs no hypothesis:
    B is a proper subterm (pi_ne_cod46, by Exp's sizeOf).
  - d2_lower1: fixing the certificate of A ⊸ B does not help: Θₖ ⊢ t :¹
    □A ⊸ □B costs μ(B) too (one input).
  - d2_upper: for any certificate v of B, Θ_{‖v‖} ⊢ λz. (lit v, ⋆) :¹
    □(A ⊸ B) ⊗ □A ⊸ □B — affinity discards z (Proposition 4.2, cert_front).
  - cor46_prime: both, with certificates of A ⊸ B and A large at every
    budget: a term exists at k only if k ≥ μ(B), and exists at k = ‖v‖ for
    every certificate v of B, so at μ(B).
  - d2_upper_k46: the upper bound at every k ≥ ‖v‖: at Θ_{‖v‖+p} the p
    extra tokens are carried by ⋆, an axiom with any usages (cert_front_n46
    and the _n46 lemmas before it).
  - cor46_prime_iff, the corollary exactly: a term exists at Θₖ iff some
    certificate of B has at most k nodes, i.e. iff k ≥ μ(B).
  - d2_no_uniform: no budget serves every A and B.  At every k, A = 1 and
    B = ¬ᵏ⁺¹1 (prop434's negN, standing in for the paper's A_j) admit no
    term at Θₖ.  Every certificate of B is larger than ⌜B⌝, which has at
    least k + 1 nodes.  This adds TypeSize and E5 (CheckSpec) to the lower
    bound's hypotheses.

  Corollary 4.6″ (D3, exactly).  For closed certifiable A:
  - d3_lower: Θₖ ⊢ t :¹ □A ⊸ □□A gives a certificate of □A with at most k
    nodes, when ⌜□A⌝ ≠ ⌜A⌝ (thm46, one input, B = □A);
  - d3_upper: prop45_d3, Θ_{‖w‖} ⊢ λz. (lit w, ⋆) :¹ □A ⊸ □□A for every
    certificate w of □A;
  - d3_gap: μ(□A) > 2μ(A): prop43 (given TokSize), stated with a lower
    bound M of μ(A) as in prop434.clj;
  - cor46_dprime: the lower bound at every k and the upper bound at every
    certificate of □A.
  - d3_upper_k46, cor46_dprime_iff, the corollary exactly: a term of
    □A ⊸ □□A exists at Θₖ iff some certificate of □A has at most k nodes,
    i.e. iff k ≥ μ(□A).
  The hypothesis ⌜□A⌝ ≠ ⌜A⌝ is the paper's \"B different from A\" at B = □A.
  CheckSpec does not give it: E1 reduces it to □A ≠ A as types, which holds
  when ⌜A⌝ is at least as large as A (an encoding fact, E4 for types) but is
  not derivable from CheckSpec, where encTy is any injective encoding of
  closed types.  So it is an explicit hypothesis.

  The paper does not argue the upper bound above μ(B), where the extra tokens
  must be absorbed.  Here ⋆ absorbs them (d2_upper_k46, d3_upper_k46).

  At F7's concrete checker, Enc46 and LargeCert are never both true
  (enc46f7.clj, enc46_F7), so cor46_prime, cor46_prime_iff, cor46_dprime,
  cor46_dprime_iff and d2_no_uniform are vacuous there.  The upper bounds
  (d2_upper, d2_upper_k46, d3_upper_k46) assume neither Enc46 nor LargeCert,
  so they hold at F7's checker."
  (:require [ansatz.core :as a]
            [clojure.walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.outer]
            [lcert.formal.section4b]
            [lcert.formal.prop434]
            [lcert.formal.rint]
            [lcert.formal.enc46]
            [lcert.formal.theorem46]))

(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

;; --- shared facts ---------------------------------------------------------------------------

;; B is a proper subterm of A ⊸ B, so they differ (sizeOf, derived for Exp;
;; Exp._sizeOf_1 is the function behind the SizeOf instance).
(thm pi_ne_cod46 [A :- Exp, B :- Exp, h :- (Eq Exp B (Exp.tPi U.u1 A B))] False
  (have hs (Eq Nat (Exp._sizeOf_1 B) (Exp._sizeOf_1 (Exp.tPi U.u1 A B))) (congrArg Exp._sizeOf_1 h))
  (have hs2 (Eq Nat (Exp._sizeOf_1 B) (+ (+ (+ 1 (U._sizeOf_1 U.u1)) (Exp._sizeOf_1 A)) (Exp._sizeOf_1 B)))
    (Eq.trans hs (Exp.tPi.sizeOf_spec U.u1 A B)))
  (omega))

;; A canonical code term has no variables; so □c is closed.
(thm closed_code46 [c :- Code] (forall [d Nat] (Eq Bool ((closedF (codeTerm c)) d) Bool.true))
  (induction c)
  (intro d) (rfl)
  (intro d)
  (have e (Eq Bool ((closedF (codeTerm (Code.sn l a b))) d) (Bool.and Bool.true (Bool.and ((closedF (codeTerm a)) d) ((closedF (codeTerm b)) d)))) (rfl))
  (rw [e]) (rw [(ih_a d)]) (rw [(ih_b d)]))
(thm closed_box46 [c :- Code] (Eq Bool (closedTy (boxTy (codeTerm c))) Bool.true)
  (have h (Eq Bool ((closedF (codeTerm c)) 1) Bool.true) (closed_code46 c 1))
  (have e (Eq Bool (closedTy (boxTy (codeTerm c))) ((closedF (codeTerm c)) 1)) (rfl))
  (exact (Eq.trans e h)))

;; A property of the two input indices 0 and 1.
(thm absurd_lt2_46 [n :- Nat, h :- (LT.lt (+ (+ n 1) 1) 2)] False (omega))
(thm two_cases46 [P :- (=> Nat Prop), h0 :- (P 0), h1 :- (P 1)] (forall [i Nat] (=> (LT.lt i 2) (P i)))
  (intro i hi) (cases i) (exact h0) (cases n) (exact h1)
  (have hf False (absurd_lt2_46 n hi)) (exact (False.elim hf)))
(thm one_case46 [P :- (=> Nat Prop), h0 :- (P 0)] (forall [i Nat] (=> (LT.lt i 1) (P i)))
  (intro i hi) (cases i) (exact h0)
  (have hf False (Nat.not_lt_zero n (Nat.lt_of_succ_lt_succ hi))) (exact (False.elim hf)))

;; A certificate of the code d whose every decoding has a term of measure
;; above k: the large certificates thm46 needs.
(defn- big [c d k]
  (list 'And (list 'Eq 'Bool (list 'chkf c d) 'Bool.true)
   (list 'And (list 'Eq 'Bool (list 'lblOk c) 'Bool.true)
    (list 'forall '[m Nat] (list 'forall '[u Exp] (list 'forall '[A' Exp]
      (list '=> (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec c) '(Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A'))))
                (list 'LT.lt k '(sz u)))))))))
;; LargeCert chkf dec sz d: d has certificates of arbitrarily large term
;; measure.  The paper builds them as (λ(y :₀ Nat). g) N̄ from any certificate
;; g, which needs completeness of the checker; here it is a hypothesis.
(eval (list 'kdef 'LargeCert '(=> (=> Code Code Bool) (=> Code (Option (Prod Nat (Prod Exp Exp)))) (=> Exp Nat) Code Prop)
  (list 'fn '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), sz :- (=> Exp Nat), d :- Code]
    (list 'forall '[k Nat] (list 'Exists (list 'fn '[c :- Code] (big 'c 'd 'k)))))))
;; "k ≥ μ(d)": a certificate of d with at most k nodes.
(defn- cert-le [d k] (list 'Exists (list 'fn '[v :- Code] (list 'And (list 'Eq 'Bool (list 'chkf 'v d) 'Bool.true)
                                                            (list 'And '(Eq Bool (lblOk v) Bool.true) (list 'LE.le '(cnodes v) k))))))

;; --- one input: □A ⊸ □B --------------------------------------------------------------------

;; Theorem 4.6 with one input (j = 1), over codes: a large certificate c of
;; ca and cb ≠ ca.
(eval (list 'lcert.formal.base/thm 'thm46_one
  (into P3 ['hcs :- '(CheckSpec chkf dec encTy) 'sz :- '(=> Exp Nat) 'henc :- '(Enc46 chkf dec sz)
            'ca :- 'Code 'cb :- 'Code 'k :- 'Nat 't :- 'Exp 'c :- 'Code 'hbig :- (big 'c 'ca 'k)
            'hne :- '(=> (Eq Code cb ca) False)
            'der :- '(Rt chkf (thetaD k) (thetaU k) t (Exp.tPi U.u1 (boxTy (codeTerm ca)) (boxTy (codeTerm cb))))])
  (cert-le 'cb 'k)
  '(exact (thm46 chkf dec encTy hcs sz henc 0 (fn [i :- Nat] c)
            (one_case46 (fn [i :- Nat] (Eq Bool (lblOk c) Bool.true)) (And.left (And.right hbig)))
            (fn [i :- Nat] ca) cb k t
            (one_case46 (fn [i :- Nat] (Eq Bool (chkf c ca) Bool.true)) (And.left hbig))
            (one_case46 (fn [i :- Nat] (forall [m Nat] (forall [u Exp] (forall [A' Exp]
                           (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A'))))
                               (LT.lt k (sz u)))))))
                        (And.right (And.right hbig)))
            (one_case46 (fn [i :- Nat] (=> (Eq Code cb ca) False)) hne)
            der))))

;; --- Corollary 4.6′ (D2) -----------------------------------------------------------------------

;; □(A ⊸ B) ⊗ □A ⊸ □B
(def ^:private ABc '(encTy (Exp.tPi U.u1 A B)))
(def ^:private D2T (list 'Exp.tPi 'U.u1 (list 'Exp.tSig 'U.u1 (list 'boxTy (list 'codeTerm ABc)) '(boxTy (codeTerm (encTy A))))
                         '(boxTy (codeTerm (encTy B)))))
;; The two input types: index 0 is A ⊸ B, index 1 is A.
(kdef d2As (=> Exp Exp Nat Exp)
  (fn [A :- Exp, B :- Exp, i :- Nat] (Bool.rec$1 (fn [_ :- Bool] Exp) A (Exp.tPi U.u1 A B) (Nat.beq i 0))))
(def ^:private DP (into P3 '[hcs :- (CheckSpec chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz),
                             A :- Exp, B :- Exp, k :- Nat, t :- Exp,
                             hclA :- (Eq Bool (closedTy A) Bool.true), hclB :- (Eq Bool (closedTy B) Bool.true),
                             hclAB :- (Eq Bool (closedTy (Exp.tPi U.u1 A B)) Bool.true),
                             hne :- (=> (Eq Exp B A) False)]))

;; Corollary 4.6′, lower bound (j = 2): thm46_types at the inputs A ⊸ B, A.
(eval (list 'lcert.formal.base/thm 'd2_lower
  (into DP ['c0 :- 'Code 'c1 :- 'Code 'h0 :- (big 'c0 ABc 'k) 'h1 :- (big 'c1 '(encTy A) 'k)
            'der :- (list 'Rt 'chkf '(thetaD k) '(thetaU k) 't D2T)])
  (cert-le '(encTy B) 'k)
  '(exact (thm46_types chkf dec encTy hcs sz henc 1
            (fn [i :- Nat] (Bool.rec$1 (fn [_ :- Bool] Code) c1 c0 (Nat.beq i 0)))
            (two_cases46 (fn [i :- Nat] (Eq Bool (lblOk (Bool.rec$1 (fn [_ :- Bool] Code) c1 c0 (Nat.beq i 0))) Bool.true))
                         (And.left (And.right h0)) (And.left (And.right h1)))
            (d2As A B) B k t
            (two_cases46 (fn [i :- Nat] (Eq Bool (closedTy (d2As A B i)) Bool.true)) hclAB hclA)
            hclB
            (two_cases46 (fn [i :- Nat] (Eq Bool (chkf (Bool.rec$1 (fn [_ :- Bool] Code) c1 c0 (Nat.beq i 0)) (encTy (d2As A B i))) Bool.true))
                         (And.left h0) (And.left h1))
            (two_cases46 (fn [i :- Nat] (forall [m Nat] (forall [u Exp] (forall [A' Exp]
                           (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (Bool.rec$1 (fn [_ :- Bool] Code) c1 c0 (Nat.beq i 0)))
                                   (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A'))))
                               (LT.lt k (sz u)))))))
                         (And.right (And.right h0)) (And.right (And.right h1)))
            (two_cases46 (fn [i :- Nat] (=> (Eq Exp B (d2As A B i)) False)) (pi_ne_cod46 A B) hne)
            der))))

;; Corollary 4.6′, fixing the certificate of A ⊸ B (j = 1): □A ⊸ □B costs
;; μ(B) too.  E1 turns B ≠ A into ⌜B⌝ ≠ ⌜A⌝.
(eval (list 'lcert.formal.base/thm 'd2_lower1
  (into DP ['c1 :- 'Code 'h1 :- (big 'c1 '(encTy A) 'k)
            'der :- '(Rt chkf (thetaD k) (thetaU k) t (Exp.tPi U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy B)))))])
  (cert-le '(encTy B) 'k)
  '(exact (thm46_one chkf dec encTy hcs sz henc (encTy A) (encTy B) k t c1 h1
            (fn [e :- (Eq Code (encTy B) (encTy A))] (hne ((And.right (And.right (And.right hcs))) B A hclB hclA e)))
            der))))

;; Corollary 4.6′, upper bound: λz. (lit v, ⋆) at budget ‖v‖, for any
;; certificate v of B (Proposition 4.2 under a binder that affinity discards).
(eval (list 'lcert.formal.base/thm 'd2_upper
  '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), A :- Exp, B :- Exp, v :- Code,
    hok :- (Eq Bool (lblOk v) Bool.true),
    hAB :- (Eq Bool (lblOk (encTy (Exp.tPi U.u1 A B))) Bool.true),
    hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
    hB :- (Eq Bool (lblOk (encTy B)) Bool.true),
    hck :- (Eq Bool (chkf v (encTy B)) Bool.true)]
  (list 'Rt 'chkf '(thetaD (cnodes v)) '(thetaU (cnodes v))
        (list 'Exp.lam 'U.u1 (second (rest D2T)) '(Exp.pair (boxTy (codeTerm (encTy B))) ((litAt v) 1) Exp.star))
        D2T)
  (list 'exact (list 'Rt.rLam 'chkf '(thetaD (cnodes v)) '(thetaU (cnodes v)) 'U.u1 (second (rest D2T))
                     '(Exp.pair (boxTy (codeTerm (encTy B))) ((litAt v) 1) Exp.star) '(boxTy (codeTerm (encTy B)))
                     (list 'Tl.fSig 'chkf '(thetaD (cnodes v)) 'U.u1 (list 'boxTy (list 'codeTerm ABc)) '(boxTy (codeTerm (encTy A)))
                           (list 'tl_boxTy 'chkf '(thetaD (cnodes v)) ABc 'hAB)
                           (list 'tl_boxTy 'chkf (list 'List.cons 'Exp (list 'boxTy (list 'codeTerm ABc)) '(thetaD (cnodes v))) '(encTy A) 'hA))
                     (list 'cert_front 'chkf 'v '(encTy B) 'hok 'hB 'hck (second (rest D2T)))))))

;; Corollary 4.6′: with certificates of A ⊸ B and of A large at every budget,
;; a term of □(A ⊸ B) ⊗ □A ⊸ □B at Θₖ forces k ≥ μ(B), and every certificate v
;; of B gives one at Θ_{‖v‖}.
(eval (list 'lcert.formal.base/thm 'cor46_prime
  (into DP '[hLAB :- (LargeCert chkf dec sz (encTy (Exp.tPi U.u1 A B))), hLA :- (LargeCert chkf dec sz (encTy A)),
             hAB :- (Eq Bool (lblOk (encTy (Exp.tPi U.u1 A B))) Bool.true),
             hA :- (Eq Bool (lblOk (encTy A)) Bool.true), hB :- (Eq Bool (lblOk (encTy B)) Bool.true)])
  (list 'And
    (list 'forall '[kk Nat] (list 'forall '[tt Exp]
      (list '=> (list 'Rt 'chkf '(thetaD kk) '(thetaU kk) 'tt (clojure.walk/postwalk-replace {'k 'kk} D2T)) (cert-le '(encTy B) 'kk))))
    (list 'forall '[v Code] (list '=> '(Eq Bool (lblOk v) Bool.true) '(Eq Bool (chkf v (encTy B)) Bool.true)
      (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt 'chkf '(thetaD (cnodes v)) '(thetaU (cnodes v)) 'tt D2T))))))
  '(constructor)
  '(intro kk tt der)
  '(refine' (exT Code _ _ (hLAB kk) _)) '(intro c0 h0)
  '(refine' (exT Code _ _ (hLA kk) _)) '(intro c1 h1)
  '(exact (d2_lower chkf dec encTy hcs sz henc A B kk tt hclA hclB hclAB hne c0 c1 h0 h1 der))
  '(intro v hok hck)
  '(constructor) '(exact (Exp.lam U.u1 (Exp.tSig U.u1 (boxTy (codeTerm (encTy (Exp.tPi U.u1 A B)))) (boxTy (codeTerm (encTy A))))
                                (Exp.pair (boxTy (codeTerm (encTy B))) ((litAt v) 1) Exp.star)))
  '(exact (d2_upper chkf encTy A B v hok hAB hA hB hck))))

;; --- Corollary 4.6″ (D3, exactly) ---------------------------------------------------------

;; □A and □□A
(def ^:private BA '(boxTy (codeTerm (encTy A))))
(def ^:private BBA (list 'boxTy (list 'codeTerm (list 'encTy BA))))

;; Corollary 4.6″, lower bound: Θₖ ⊢ t :¹ □A ⊸ □□A forces a certificate of □A
;; with at most k nodes (thm46_one at B = □A).
(eval (list 'lcert.formal.base/thm 'd3_lower
  (into P3 ['hcs :- '(CheckSpec chkf dec encTy) 'sz :- '(=> Exp Nat) 'henc :- '(Enc46 chkf dec sz)
            'A :- 'Exp 'k :- 'Nat 't :- 'Exp 'c :- 'Code 'hbig :- (big 'c '(encTy A) 'k)
            'hne :- (list '=> (list 'Eq 'Code (list 'encTy BA) '(encTy A)) 'False)
            'der :- (list 'Rt 'chkf '(thetaD k) '(thetaU k) 't (list 'Exp.tPi 'U.u1 BA BBA))])
  (cert-le (list 'encTy BA) 'k)
  (list 'exact (list 'thm46_one 'chkf 'dec 'encTy 'hcs 'sz 'henc '(encTy A) (list 'encTy BA) 'k 't 'c 'hbig 'hne 'der))))

;; Corollary 4.6″, the gap: every certificate of □A has more than 2μ(A)
;; nodes (Proposition 4.3, given TokSize; M any lower bound of μ(A)).
(eval (list 'lcert.formal.base/thm 'd3_gap
  (into P3 ['hcs :- '(CheckSpec chkf dec encTy) 'hes :- '(TokSize chkf dec) 'A :- 'Exp
            'w :- 'Code 'hw :- (list 'Eq 'Bool (list 'chkf 'w (list 'encTy BA)) 'Bool.true)
            'M :- 'Nat 'hM :- '(forall [v Code] (=> (Eq Bool (chkf v (encTy A)) Bool.true) (LE.le M (cnodes v))))])
  '(LT.lt (+ M M) (cnodes w))
  '(exact (prop43 chkf dec encTy hcs hes A (codeTerm (encTy A)) (codeTerm_of (encTy A)) (closed_box46 (encTy A)) w hw M hM))))

;; Corollary 4.6″: with certificates of A large at every budget and ⌜□A⌝ ≠
;; ⌜A⌝, a term of □A ⊸ □□A at Θₖ forces k ≥ μ(□A), and every certificate w
;; of □A gives one at Θ_{‖w‖} (prop45_d3).
(eval (list 'lcert.formal.base/thm 'cor46_dprime
  (into P3 ['hcs :- '(CheckSpec chkf dec encTy) 'sz :- '(=> Exp Nat) 'henc :- '(Enc46 chkf dec sz) 'A :- 'Exp
            'hLA :- '(LargeCert chkf dec sz (encTy A))
            'hne :- (list '=> (list 'Eq 'Code (list 'encTy BA) '(encTy A)) 'False)
            'hA :- '(Eq Bool (lblOk (encTy A)) Bool.true)
            'hBA :- (list 'Eq 'Bool (list 'lblOk (list 'encTy BA)) 'Bool.true)])
  (list 'And
    (list 'forall '[kk Nat] (list 'forall '[tt Exp]
      (list '=> (list 'Rt 'chkf '(thetaD kk) '(thetaU kk) 'tt (list 'Exp.tPi 'U.u1 BA BBA)) (cert-le (list 'encTy BA) 'kk))))
    (list 'forall '[w Code] (list '=> '(Eq Bool (lblOk w) Bool.true) (list 'Eq 'Bool (list 'chkf 'w (list 'encTy BA)) 'Bool.true)
      (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt 'chkf '(thetaD (cnodes w)) '(thetaU (cnodes w)) 'tt (list 'Exp.tPi 'U.u1 BA BBA)))))))
  '(constructor)
  '(intro kk tt der)
  '(refine' (exT Code _ _ (hLA kk) _)) '(intro c hc)
  '(exact (d3_lower chkf dec encTy hcs sz henc A kk tt c hc hne der))
  '(intro w hok hck)
  '(constructor) (list 'exact (list 'Exp.lam 'U.u1 BA (list 'Exp.pair BBA '((litAt w) 1) 'Exp.star)))
  '(exact (prop45_d3 chkf dec encTy A w hok hA hBA hck))))

;; --- the upper bounds at every budget k ≥ ‖v‖ ---------------------------------------------

;; "Exists exactly when k ≥ μ(B)" needs a term at every k ≥ μ(B), not only at
;; k = ‖v‖ (d2_upper, prop45_d3).  At Θ_{‖v‖+p} the p extra tokens are taken by
;; ⋆: an axiom carries any usages (rConst), just as ⋆ already carries the
;; discarded input's.  The lemmas are the inner copy of section4b.clj's boxed
;; contraction (lit_inner, lit_left, cv_inner, cv_left, tl_lit_left,
;; tl_ev_left, rt_star_left), which appends ‖v‖ unused tokens, with ‖v‖
;; replaced by any p.  The one change is ⋆'s usage vector, which now carries the p tokens.

;; lit0 v mentions only variables below ‖v‖: a lift at cutoff ‖v‖ fixes it.
(thm lit_fixed_n46 [v :- Code, p :- Nat] (Eq Exp (lift p (cnodes v) (lit0 v)) (lit0 v))
  (exact (closed_lift (lit0 v) (cnodes v) (lit_closed_block v) p (cnodes v) (le_refl (cnodes v)))))

;; lit v on its own ‖v‖ tokens, with p unused tokens appended.
(thm lit_inner_n46 [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true), p :- Nat]
  (Rt chkf (thetaD (+ (cnodes v) p)) (prefixU (cnodes v) U.u1 (vzero p)) (lit0 v) Exp.tR)
  (exact (rt_reindex chkf
    (thetaD (+ (cnodes v) p)) (prefixU (cnodes v) U.u1 (vzero p)) (lift p (cnodes v) (lit0 v)) Exp.tR
    (thetaD (+ (cnodes v) p)) (prefixU (cnodes v) U.u1 (vzero p)) (lit0 v) Exp.tR
    (rt_theta_append chkf (cnodes v) (lit0 v) Exp.tR closed_tR ((rt_lit chkf v) hok) p)
    rfl rfl (lit_fixed_n46 v p) rfl)))

;; The same under the input binder XX: its variables move up by one.
(thm lit_left_n46 [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true), XX :- Exp, p :- Nat]
  (Rt chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (List.cons U U.u0 (prefixU (cnodes v) U.u1 (vzero p)))
      ((litAt v) 1) Exp.tR)
  (exact (rt_reindex chkf
    (insD 0 XX (thetaD (+ (cnodes v) p)))
    (insU 0 U.u0 (prefixU (cnodes v) U.u1 (vzero p)))
    (lift 1 0 (lit0 v)) (lift 1 0 Exp.tR)
    (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (List.cons U U.u0 (prefixU (cnodes v) U.u1 (vzero p)))
    ((litAt v) 1) Exp.tR
    (rt_weaken chkf (thetaD (+ (cnodes v) p)) (prefixU (cnodes v) U.u1 (vzero p)) (lit0 v) Exp.tR
      (lit_inner_n46 chkf v hok p) 0 XX)
    (insD_zero XX (thetaD (+ (cnodes v) p)))
    (insU_zero U.u0 (prefixU (cnodes v) U.u1 (vzero p)))
    (Eq.trans (congrArg (fn [t :- Exp] (lift 1 0 t)) (lit0_at v))
      (Eq.trans (lit_shift v 0 1) (congrArg (fn [i :- Nat] ((litAt v) i)) (Nat.zero_add 1))))
    (closedTy_lift Exp.tR closed_tR 1 0))))

;; Proposition 4.2's Cv chain, padded by p tokens, then shifted under XX.
(thm cv_inner_n46 [chkf :- (=> Code Code Bool), v :- Code, cA :- Code, hck :- (Eq Bool (chkf v cA) Bool.true), p :- Nat]
  (Cv chkf (skels (thetaD (+ (cnodes v) p))) Exp.tUnit (evTy (lit0 v) (codeTerm cA)))
  (exact (cv_cast_start chkf (skels (thetaD (+ (cnodes v) p)))
    (lift p (cnodes v) Exp.tUnit) Exp.tUnit
    (evTy (lit0 v) (codeTerm cA))
    (cv_cast_end chkf (skels (thetaD (+ (cnodes v) p)))
      (lift p (cnodes v) Exp.tUnit)
      (lift p (cnodes v) (evTy (lit0 v) (codeTerm cA)))
      (evTy (lit0 v) (codeTerm cA))
      (cv_theta_append chkf (cnodes v) Exp.tUnit (evTy (lit0 v) (codeTerm cA)) (cv_lit chkf v cA hck) p)
      (Eq.trans (lift_ev_at p (cnodes v) (lit0 v) (codeTerm cA))
        (Eq.trans (congrArg (fn [t :- Exp] (evTy t (lift p (cnodes v) (codeTerm cA)))) (lit_fixed_n46 v p))
          (congrArg (fn [c :- Exp] (evTy (lit0 v) c)) (lift_code cA p (cnodes v))))))
    (lift_unit_at p (cnodes v)))))

(thm cv_left_n46 [chkf :- (=> Code Code Bool), v :- Code, cA :- Code, hck :- (Eq Bool (chkf v cA) Bool.true), XX :- Exp, p :- Nat]
  (Cv chkf (skels (List.cons Exp XX (thetaD (+ (cnodes v) p)))) Exp.tUnit (evTy ((litAt v) 1) (codeTerm cA)))
  (exact (cv_cast_G chkf
    (skels (insD 0 XX (thetaD (+ (cnodes v) p))))
    (skels (List.cons Exp XX (thetaD (+ (cnodes v) p))))
    Exp.tUnit (evTy ((litAt v) 1) (codeTerm cA))
    (cv_cast_start chkf (skels (insD 0 XX (thetaD (+ (cnodes v) p))))
      (lift 1 0 Exp.tUnit) Exp.tUnit (evTy ((litAt v) 1) (codeTerm cA))
      (cv_cast_end chkf (skels (insD 0 XX (thetaD (+ (cnodes v) p))))
        (lift 1 0 Exp.tUnit) (lift 1 0 (evTy (lit0 v) (codeTerm cA)))
        (evTy ((litAt v) 1) (codeTerm cA))
        (cv_weaken chkf (thetaD (+ (cnodes v) p)) Exp.tUnit (evTy (lit0 v) (codeTerm cA)) (cv_inner_n46 chkf v cA hck p) 0 XX)
        (Eq.trans (lift_ev 1 (lit0 v) (codeTerm cA))
          (Eq.trans (congrArg (fn [t :- Exp] (evTy t (lift 1 0 (codeTerm cA))))
                      (Eq.trans (congrArg (fn [t :- Exp] (lift 1 0 t)) (lit0_at v))
                        (Eq.trans (lit_shift v 0 1)
                          (congrArg (fn [i :- Nat] ((litAt v) i)) (Nat.zero_add 1)))))
            (congrArg (fn [c :- Exp] (evTy ((litAt v) 1) c)) (lift_code cA 1 0)))))
      (lift_unit 1))
    (congrArg skels (insD_zero XX (thetaD (+ (cnodes v) p)))))))

;; The evidence type T(chk′ (print (lit v)) ⌜A⌝) is formed in the larger context.
(thm tl_lit_left_n46 [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true), XX :- Exp, p :- Nat]
  (Tl chkf Bool.false (List.cons Exp XX (thetaD (+ (cnodes v) p))) ((litAt v) 1) Exp.tR)
  (exact (tl_cast3 chkf Bool.false (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (lift 1 0 ((litAt v) 0)) ((litAt v) 1) (lift 1 0 Exp.tR) Exp.tR
    (tl_cast_D chkf Bool.false
      (insD 0 XX (thetaD (+ 0 (+ (cnodes v) p))))
      (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (lift 1 0 ((litAt v) 0)) (lift 1 0 Exp.tR)
      (tl_weaken chkf Bool.false (thetaD (+ 0 (+ (cnodes v) p))) ((litAt v) 0) Exp.tR (((tl_lit chkf v) hok) 0 p) 0 XX)
      (Eq.trans (insD_zero XX (thetaD (+ 0 (+ (cnodes v) p))))
        (congrArg (fn [D :- (List Exp)] (List.cons Exp XX D)) (congrArg thetaD (Nat.zero_add (+ (cnodes v) p))))))
    (Eq.trans (lit_shift v 0 1) (congrArg (fn [i :- Nat] ((litAt v) i)) (Nat.zero_add 1)))
    (closedTy_lift Exp.tR closed_tR 1 0))))
(thm tl_ev_left_n46 [chkf :- (=> Code Code Bool), v :- Code, cA :- Code, hok :- (Eq Bool (lblOk v) Bool.true),
                     hA :- (Eq Bool (lblOk cA) Bool.true), XX :- Exp, p :- Nat]
  (Tl chkf Bool.true (List.cons Exp XX (thetaD (+ (cnodes v) p))) (evTy ((litAt v) 1) (codeTerm cA)) Exp.tUnit)
  (exact (Tl.fT chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (Exp.chk (Exp.prn ((litAt v) 1)) (codeTerm cA))
    (Tl.zChk chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (Exp.prn ((litAt v) 1)) (codeTerm cA)
      (Tl.zPrn chkf (List.cons Exp XX (thetaD (+ (cnodes v) p))) ((litAt v) 1) (tl_lit_left_n46 chkf v hok XX p))
      ((tl_code chkf (List.cons Exp XX (thetaD (+ (cnodes v) p))) cA) hA)))))

;; ⋆ at any usage vector r :: us of the right length, converting to the evidence
;; type of the shifted literal.
(thm len_front_n46 [XX :- Exp, r :- U, us :- (List U), N :- Nat, hl :- (Eq Nat (lenU us) N)]
  (Eq Nat (lenU (List.cons U r us)) (lenE (List.cons Exp XX (thetaD N))))
  (exact (Eq.trans (lenU_cons r us)
    (Eq.trans (congrArg Nat.succ hl)
      (Eq.trans (Eq.symm (congrArg Nat.succ (lenE_theta N)))
                (Eq.symm (lenE_cons XX (thetaD N))))))))
(thm rt_star_left_n46 [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                       hok :- (Eq Bool (lblOk v) Bool.true), hA :- (Eq Bool (lblOk cA) Bool.true),
                       hck :- (Eq Bool (chkf v cA) Bool.true), XX :- Exp, r :- U, us :- (List U), p :- Nat,
                       hl :- (Eq Nat (lenU us) (+ (cnodes v) p))]
  (Rt chkf (List.cons Exp XX (thetaD (+ (cnodes v) p))) (List.cons U r us) Exp.star
      (subst1 ((litAt v) 1) (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))))
  (exact (rt_cast chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (List.cons U r us) (List.cons U r us) Exp.star
    (evTy ((litAt v) 1) (codeTerm cA))
    (subst1 ((litAt v) 1) (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
    (Rt.rConv chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (List.cons U r us) Exp.star Exp.tUnit
      (evTy ((litAt v) 1) (codeTerm cA))
      (Rt.rConst chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
        (List.cons U r us) Exp.star Exp.tUnit
        (len_front_n46 XX r us (+ (cnodes v) p) hl) star_unit)
      (tl_ev_left_n46 chkf v cA hok hA XX p)
      (cv_left_n46 chkf v cA hck XX p))
    rfl (Eq.symm (subst_ev_code ((litAt v) 1) cA)))))

;; ⋆'s usages: none on the literal's ‖v‖ tokens, one on each of the p extra ones.
(thm len_prefix_n46 [p :- Nat, m :- Nat] (Eq Nat (lenU (prefixU m U.u0 (thetaU p))) (+ m p))
  (induction m)
  (exact (Eq.trans (lenU_theta p) (Eq.symm (Nat.zero_add p))))
  (exact (Eq.trans (congrArg lenU (prefixU_succ n U.u0 (thetaU p)))
    (Eq.trans (lenU_cons U.u0 (prefixU n U.u0 (thetaU p)))
      (Eq.trans (congrArg Nat.succ ih_n) (Eq.symm (Nat.succ_add n p)))))))
;; The literal's block plus ⋆'s block is Θ_{‖v‖+p}.
(thm vadd_blocks_n46 [p :- Nat, m :- Nat]
  (Eq (List U) (vadd (prefixU m U.u1 (vzero p)) (prefixU m U.u0 (thetaU p))) (thetaU (+ m p)))
  (induction m)
  (exact (Eq.trans (vadd_zero_theta p) (congrArg thetaU (Eq.symm (Nat.zero_add p)))))
  (exact (Eq.trans (vadd_cons2 U.u1 U.u0 (prefixU n U.u1 (vzero p)) (prefixU n U.u0 (thetaU p)))
    (Eq.trans (congrArg (fn [us :- (List U)] (List.cons U (uadd U.u1 U.u0) us)) ih_n)
      (Eq.trans (congrArg (fn [r :- U] (List.cons U r (thetaU (+ n p)))) uadd_one_zero)
        (Eq.symm (Eq.trans (congrArg thetaU (Nat.succ_add n p)) (thetaU_succ (+ n p)))))))))
;; Pair at usage 1: the literal's 0 on the binder plus ⋆'s 1, and the two blocks.
(thm us_sum_n46 [m :- Nat, p :- Nat]
  (Eq (List U)
    (vadd (vscale U.u1 (List.cons U U.u0 (prefixU m U.u1 (vzero p))))
          (List.cons U U.u1 (prefixU m U.u0 (thetaU p))))
    (List.cons U U.u1 (thetaU (+ m p))))
  (exact (Eq.trans
    (congrArg (fn [us :- (List U)] (vadd us (List.cons U U.u1 (prefixU m U.u0 (thetaU p)))))
      (vscale_one (List.cons U U.u0 (prefixU m U.u1 (vzero p)))))
    (Eq.trans (vadd_cons2 U.u0 U.u1 (prefixU m U.u1 (vzero p)) (prefixU m U.u0 (thetaU p)))
      (Eq.trans (congrArg (fn [us :- (List U)] (List.cons U (uadd U.u0 U.u1) us)) (vadd_blocks_n46 p m))
        (congrArg (fn [r :- U] (List.cons U r (thetaU (+ m p)))) (uadd_zero_left U.u1)))))))

;; (lit v, ⋆) :¹ □cA under one discarded entry XX, in Θ_{‖v‖+p}: section4b's
;; cert_front with p extra tokens, which ⋆ carries.
(thm cert_front_n46 [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                     hok :- (Eq Bool (lblOk v) Bool.true), hA :- (Eq Bool (lblOk cA) Bool.true),
                     hck :- (Eq Bool (chkf v cA) Bool.true), XX :- Exp, p :- Nat]
  (Rt chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (List.cons U U.u1 (thetaU (+ (cnodes v) p)))
      (Exp.pair (boxTy (codeTerm cA)) ((litAt v) 1) Exp.star)
      (boxTy (codeTerm cA)))
  (exact (rt_reindex chkf
    (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (vadd (vscale U.u1 (List.cons U U.u0 (prefixU (cnodes v) U.u1 (vzero p))))
          (List.cons U U.u1 (prefixU (cnodes v) U.u0 (thetaU p))))
    (Exp.pair (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))) ((litAt v) 1) Exp.star)
    (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
    (List.cons Exp XX (thetaD (+ (cnodes v) p)))
    (List.cons U U.u1 (thetaU (+ (cnodes v) p)))
    (Exp.pair (boxTy (codeTerm cA)) ((litAt v) 1) Exp.star)
    (boxTy (codeTerm cA))
    (Rt.rPair chkf (List.cons Exp XX (thetaD (+ (cnodes v) p)))
      (List.cons U U.u0 (prefixU (cnodes v) U.u1 (vzero p)))
      (List.cons U U.u1 (prefixU (cnodes v) U.u0 (thetaU p)))
      U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))
      ((litAt v) 1) Exp.star
      nonzero_u1
      (Tl.fBase chkf (List.cons Exp XX (thetaD (+ (cnodes v) p))) Exp.tR base_tR)
      (tl_box_body chkf (List.cons Exp XX (thetaD (+ (cnodes v) p))) cA hA)
      (lit_left_n46 chkf v hok XX p)
      (rt_star_left_n46 chkf v cA hok hA hck XX U.u1 (prefixU (cnodes v) U.u0 (thetaU p)) p (len_prefix_n46 p (cnodes v))))
    rfl
    (us_sum_n46 (cnodes v) p)
    (congrArg (fn [S :- Exp] (Exp.pair S ((litAt v) 1) Exp.star)) (Eq.symm (boxTy_unfold cA)))
    (Eq.symm (boxTy_unfold cA)))))

;; k = ‖v‖ + (k − ‖v‖) when ‖v‖ ≤ k.
(thm add_sub_n46 [a :- Nat, k :- Nat, h :- (LE.le a k)] (Eq Nat (+ a (- k a)) k) (omega))

;; Corollary 4.6′, upper bound at Θ_{‖v‖+p}, and so at every k ≥ ‖v‖.
(def ^:private D2L (list 'Exp.lam 'U.u1 (second (rest D2T)) '(Exp.pair (boxTy (codeTerm (encTy B))) ((litAt v) 1) Exp.star)))
(def ^:private UP2 '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), A :- Exp, B :- Exp, v :- Code,
                    hok :- (Eq Bool (lblOk v) Bool.true),
                    hAB :- (Eq Bool (lblOk (encTy (Exp.tPi U.u1 A B))) Bool.true),
                    hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                    hB :- (Eq Bool (lblOk (encTy B)) Bool.true),
                    hck :- (Eq Bool (chkf v (encTy B)) Bool.true)])
(eval (list 'lcert.formal.base/thm 'd2_upper_n46 (conj UP2 'p :- 'Nat)
  (list 'Rt 'chkf '(thetaD (+ (cnodes v) p)) '(thetaU (+ (cnodes v) p)) D2L D2T)
  (list 'exact (list 'Rt.rLam 'chkf '(thetaD (+ (cnodes v) p)) '(thetaU (+ (cnodes v) p)) 'U.u1 (second (rest D2T))
                     '(Exp.pair (boxTy (codeTerm (encTy B))) ((litAt v) 1) Exp.star) '(boxTy (codeTerm (encTy B)))
                     (list 'Tl.fSig 'chkf '(thetaD (+ (cnodes v) p)) 'U.u1 (list 'boxTy (list 'codeTerm ABc)) '(boxTy (codeTerm (encTy A)))
                           (list 'tl_boxTy 'chkf '(thetaD (+ (cnodes v) p)) ABc 'hAB)
                           (list 'tl_boxTy 'chkf (list 'List.cons 'Exp (list 'boxTy (list 'codeTerm ABc)) '(thetaD (+ (cnodes v) p))) '(encTy A) 'hA))
                     (list 'cert_front_n46 'chkf 'v '(encTy B) 'hok 'hB 'hck (second (rest D2T)) 'p)))))
(eval (list 'lcert.formal.base/thm 'd2_upper_k46 (into UP2 '[k :- Nat, hk :- (LE.le (cnodes v) k)])
  (list 'Rt 'chkf '(thetaD k) '(thetaU k) D2L D2T)
  '(have e (Eq Nat (+ (cnodes v) (- k (cnodes v))) k) (add_sub_n46 (cnodes v) k hk))
  (list 'exact (list 'rt_reindex 'chkf '(thetaD (+ (cnodes v) (- k (cnodes v)))) '(thetaU (+ (cnodes v) (- k (cnodes v)))) D2L D2T
                     '(thetaD k) '(thetaU k) D2L D2T
                     '(d2_upper_n46 chkf encTy A B v hok hAB hA hB hck (- k (cnodes v)))
                     '(congrArg thetaD e) '(congrArg thetaU e) 'rfl 'rfl))))

;; Corollary 4.6′, exactly: with certificates of A ⊸ B and of A large at every
;; budget, a term of □(A ⊸ B) ⊗ □A ⊸ □B exists at Θₖ iff some certificate of
;; B has at most k nodes — iff k ≥ μ(B).
(eval (list 'lcert.formal.base/thm 'cor46_prime_iff
  (into DP '[hLAB :- (LargeCert chkf dec sz (encTy (Exp.tPi U.u1 A B))), hLA :- (LargeCert chkf dec sz (encTy A)),
             hAB :- (Eq Bool (lblOk (encTy (Exp.tPi U.u1 A B))) Bool.true),
             hA :- (Eq Bool (lblOk (encTy A)) Bool.true), hB :- (Eq Bool (lblOk (encTy B)) Bool.true)])
  (list 'forall '[kk Nat]
    (list 'Iff (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt 'chkf '(thetaD kk) '(thetaU kk) 'tt D2T)))
               (cert-le '(encTy B) 'kk)))
  '(intro kk)
  '(constructor)
  '(intro hx)
  '(refine' (exT Exp _ _ hx _)) '(intro tt der)
  '(refine' (exT Code _ _ (hLAB kk) _)) '(intro c0 h0)
  '(refine' (exT Code _ _ (hLA kk) _)) '(intro c1 h1)
  '(exact (d2_lower chkf dec encTy hcs sz henc A B kk tt hclA hclB hclAB hne c0 c1 h0 h1 der))
  '(intro hx)
  '(refine' (exT Code _ _ hx _)) '(intro v hv)
  '(constructor)
  (list 'exact (clojure.walk/postwalk-replace {'v 'v} D2L))
  '(exact (d2_upper_k46 chkf encTy A B v (And.left (And.right hv)) hAB hA hB (And.left hv) kk (And.right (And.right hv))))))

;; Corollary 4.6″, upper bound at Θ_{‖w‖+p}, and so at every k ≥ ‖w‖.
(def ^:private D3T (list 'Exp.tPi 'U.u1 BA BBA))
(def ^:private D3L (list 'Exp.lam 'U.u1 BA (list 'Exp.pair BBA '((litAt w) 1) 'Exp.star)))
(def ^:private UP3 (vec (concat '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), A :- Exp, w :- Code,
                                  hok :- (Eq Bool (lblOk w) Bool.true), hA :- (Eq Bool (lblOk (encTy A)) Bool.true)]
                                ['hBA :- (list 'Eq 'Bool (list 'lblOk (list 'encTy BA)) 'Bool.true)
                                 'hck :- (list 'Eq 'Bool (list 'chkf 'w (list 'encTy BA)) 'Bool.true)])))
(eval (list 'lcert.formal.base/thm 'd3_upper_n46 (conj UP3 'p :- 'Nat)
  (list 'Rt 'chkf '(thetaD (+ (cnodes w) p)) '(thetaU (+ (cnodes w) p)) D3L D3T)
  (list 'exact (list 'Rt.rLam 'chkf '(thetaD (+ (cnodes w) p)) '(thetaU (+ (cnodes w) p)) 'U.u1 BA
                     (list 'Exp.pair BBA '((litAt w) 1) 'Exp.star) BBA
                     '(tl_boxTy chkf (thetaD (+ (cnodes w) p)) (encTy A) hA)
                     (list 'cert_front_n46 'chkf 'w (list 'encTy BA) 'hok 'hBA 'hck BA 'p)))))
(eval (list 'lcert.formal.base/thm 'd3_upper_k46 (into UP3 '[k :- Nat, hk :- (LE.le (cnodes w) k)])
  (list 'Rt 'chkf '(thetaD k) '(thetaU k) D3L D3T)
  '(have e (Eq Nat (+ (cnodes w) (- k (cnodes w))) k) (add_sub_n46 (cnodes w) k hk))
  (list 'exact (list 'rt_reindex 'chkf '(thetaD (+ (cnodes w) (- k (cnodes w)))) '(thetaU (+ (cnodes w) (- k (cnodes w)))) D3L D3T
                     '(thetaD k) '(thetaU k) D3L D3T
                     '(d3_upper_n46 chkf encTy A w hok hA hBA hck (- k (cnodes w)))
                     '(congrArg thetaD e) '(congrArg thetaU e) 'rfl 'rfl))))

;; Corollary 4.6″, exactly: with certificates of A large at every budget and
;; ⌜□A⌝ ≠ ⌜A⌝, a term of □A ⊸ □□A exists at Θₖ iff some certificate of □A has
;; at most k nodes — iff k ≥ μ(□A).
(eval (list 'lcert.formal.base/thm 'cor46_dprime_iff
  (into P3 ['hcs :- '(CheckSpec chkf dec encTy) 'sz :- '(=> Exp Nat) 'henc :- '(Enc46 chkf dec sz) 'A :- 'Exp
            'hLA :- '(LargeCert chkf dec sz (encTy A))
            'hne :- (list '=> (list 'Eq 'Code (list 'encTy BA) '(encTy A)) 'False)
            'hA :- '(Eq Bool (lblOk (encTy A)) Bool.true)
            'hBA :- (list 'Eq 'Bool (list 'lblOk (list 'encTy BA)) 'Bool.true)])
  (list 'forall '[kk Nat]
    (list 'Iff (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt 'chkf '(thetaD kk) '(thetaU kk) 'tt D3T)))
               (cert-le (list 'encTy BA) 'kk)))
  '(intro kk)
  '(constructor)
  '(intro hx)
  '(refine' (exT Exp _ _ hx _)) '(intro tt der)
  '(refine' (exT Code _ _ (hLA kk) _)) '(intro c hc)
  '(exact (d3_lower chkf dec encTy hcs sz henc A kk tt c hc hne der))
  '(intro hx)
  '(refine' (exT Code _ _ hx _)) '(intro w hw)
  '(constructor)
  (list 'exact D3L)
  '(exact (d3_upper_k46 chkf encTy A w (And.left (And.right hw)) hA hBA (And.left hw) kk (And.right (And.right hw))))))

;; --- Corollary 4.6′: no budget serves every A and B -----------------------------------------

;; ¬ʲ1 (prop434.clj's negN, the paper's A_j up to the choice of 0 for 1 as
;; codomain) is closed at every depth.
(thm negN_closedF46 [j :- Nat] (forall [d Nat] (Eq Bool ((closedF (negN j)) d) Bool.true))
  (induction j)
  (intro d) (rfl)
  (intro d)
  (change (Eq Bool (Bool.and ((closedF (negN n)) d) Bool.true) Bool.true))
  (rw [(ih_n d)]))
(thm unit_ne_negN46 [j :- Nat, e :- (Eq Exp (negN (+ j 1)) Exp.tUnit)] False
  (exact (Exp.noConfusion e)))
(thm arith_nu46 [a :- Nat, b :- Nat, k :- Nat, h1 :- (LT.lt a b), h2 :- (LE.le (+ k 1) a), h3 :- (LE.le b k)] False (omega))

;; At every budget k, the instance A = 1, B = ¬ᵏ⁺¹1 has no term of
;; □(A ⊸ B) ⊗ □A ⊸ □B at Θₖ: the lower bound (d2_lower) would give a
;; certificate of ¬ᵏ⁺¹1 with at most k nodes, but every certificate of it is
;; larger than its type's code (TypeSize, cert_size), which has at least
;; k + 1 nodes (E5, negN_size).  1 ⊸ ¬ᵏ⁺¹1 and 1 are certifiable (here: have
;; large certificates, LargeCert), so no single budget serves every such A, B.
(eval (list 'lcert.formal.base/thm 'd2_no_uniform
  (into P3 '[hcs :- (CheckSpec chkf dec encTy), hts :- (TypeSize chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz), k :- Nat,
             hLAB :- (LargeCert chkf dec sz (encTy (Exp.tPi U.u1 Exp.tUnit (negN (+ k 1))))),
             hLA :- (LargeCert chkf dec sz (encTy Exp.tUnit))])
  (list 'forall '[t Exp] (list '=> (list 'Rt 'chkf '(thetaD k) '(thetaU k) 't
                                         (clojure.walk/postwalk-replace {'A 'Exp.tUnit 'B '(negN (+ k 1))} D2T))
                               'False))
  '(intro t der)
  '(refine' (exT Code _ _ (hLAB k) _)) '(intro c0 h0)
  '(refine' (exT Code _ _ (hLA k) _)) '(intro c1 h1)
  (list 'have 'hv (cert-le '(encTy (negN (+ k 1))) 'k)
        '(d2_lower chkf dec encTy hcs sz henc Exp.tUnit (negN (+ k 1)) k t (Eq.refl$1 Bool.true) (negN_closed (+ k 1))
                   (negN_closedF46 (+ k 1) 1) (unit_ne_negN46 k) c0 c1 h0 h1 der))
  '(refine' (exT Code _ _ hv _)) '(intro v hv2)
  '(have hs (LT.lt (cnodes (encTy (negN (+ k 1)))) (cnodes v)) (cert_size chkf dec encTy hcs hts (negN (+ k 1)) v (And.left hv2)))
  '(have hn (LE.le (+ k 1) (cnodes (encTy (negN (+ k 1))))) (negN_size encTy (And.left (And.right (And.right hcs))) (+ k 1)))
  '(exact (arith_nu46 (cnodes (encTy (negN (+ k 1)))) (cnodes v) k hs hn (And.right (And.right hv2))))))
