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

  Corollary 4.6″ (D3, exactly).  For closed certifiable A:
  - d3_lower: Θₖ ⊢ t :¹ □A ⊸ □□A gives a certificate of □A with at most k
    nodes, when ⌜□A⌝ ≠ ⌜A⌝ (thm46, one input, B = □A);
  - d3_upper: prop45_d3, Θ_{‖w‖} ⊢ λz. (lit w, ⋆) :¹ □A ⊸ □□A for every
    certificate w of □A;
  - d3_gap: μ(□A) > 2μ(A): prop43 (given TokSize), stated with a lower
    bound M of μ(A) as in prop434.clj;
  - cor46_dprime: the lower bound at every k and the upper bound at every
    certificate of □A.
  The hypothesis ⌜□A⌝ ≠ ⌜A⌝ is the paper's \"B different from A\" at B = □A.
  CheckSpec does not give it: E1 reduces it to □A ≠ A as types, which holds
  when ⌜A⌝ is at least as large as A (an encoding fact, E4 for types) but is
  not derivable from CheckSpec, where encTy is any injective encoding of
  closed types.  So it is an explicit hypothesis.

  Not formalized: \"exists exactly when k ≥ μ(B)\" above μ(B) (that a term at
  budget ‖v‖ gives one at every larger budget: the extra tokens would have
  to be absorbed, e.g. by ⋆'s axiom, a derivation construction not built
  here; the paper does not argue it either), and Corollary 4.6′'s \"no budget
  serves every A and B\" (the §4.4 family), which would add TypeSize."
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
