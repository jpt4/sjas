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

  At F7's first, padded certificate format, Enc46 and LargeCert are never
  both true (enc46f7.clj, enc46_F7), so cor46_prime, cor46_prime_iff,
  cor46_dprime, cor46_dprime_iff and d2_no_uniform are vacuous there.  At
  F7's canonical format Enc46 holds for every measure, and thm46f7.clj
  states the corollaries at Check decCert without Enc46 or LargeCert
  (cor46_prime_F7, cor46_prime_iff_F7, cor46_dprime_F7,
  cor46_dprime_iff_F7).  The upper bounds (d2_upper, d2_upper_k46,
  d3_upper_k46) assume neither Enc46 nor LargeCert."
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
  ;; ‹proof elided: lines 87–90 of the full file; kernel-checked›
  )

;; A canonical code term has no variables; so □c is closed.
;; ‹lemma closed_code46: statement and proof elided, lines 93–98›
;; ‹lemma closed_box46: statement and proof elided, lines 99–102›

;; A property of the two input indices 0 and 1.
;; ‹lemma absurd_lt2_46: statement and proof elided, lines 105–105›
;; ‹lemma two_cases46: statement and proof elided, lines 106–108›
;; ‹lemma one_case46: statement and proof elided, lines 109–111›

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
;; ‹lemma lit_fixed_n46: statement and proof elided, lines 298–299›

;; lit v on its own ‖v‖ tokens, with p unused tokens appended.
;; ‹lemma lit_inner_n46: statement and proof elided, lines 302–308›

;; The same under the input binder XX: its variables move up by one.
;; ‹lemma lit_left_n46: statement and proof elided, lines 311–328›

;; Proposition 4.2's Cv chain, padded by p tokens, then shifted under XX.
;; ‹lemma cv_inner_n46: statement and proof elided, lines 331–344›

;; ‹lemma cv_left_n46: statement and proof elided, lines 346–365›

;; The evidence type T(chk′ (print (lit v)) ⌜A⌝) is formed in the larger context.
;; ‹lemma tl_lit_left_n46: statement and proof elided, lines 368–380›
;; ‹lemma tl_ev_left_n46: statement and proof elided, lines 381–389›

;; ⋆ at any usage vector r :: us of the right length, converting to the evidence
;; type of the shifted literal.
;; ‹lemma len_front_n46: statement and proof elided, lines 393–398›
;; ‹lemma rt_star_left_n46: statement and proof elided, lines 399–417›

;; ⋆'s usages: none on the literal's ‖v‖ tokens, one on each of the p extra ones.
;; ‹lemma len_prefix_n46: statement and proof elided, lines 420–425›
;; The literal's block plus ⋆'s block is Θ_{‖v‖+p}.
;; ‹lemma vadd_blocks_n46: statement and proof elided, lines 427–434›
;; Pair at usage 1: the literal's 0 on the binder plus ⋆'s 1, and the two blocks.
;; ‹lemma us_sum_n46: statement and proof elided, lines 436–446›

;; (lit v, ⋆) :¹ □cA under one discarded entry XX, in Θ_{‖v‖+p}: section4b's
;; cert_front with p extra tokens, which ⋆ carries.
;; ‹lemma cert_front_n46: statement and proof elided, lines 450–480›

;; k = ‖v‖ + (k − ‖v‖) when ‖v‖ ≤ k.
;; ‹lemma add_sub_n46: statement and proof elided, lines 483–483›

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
;; ‹lemma negN_closedF46: statement and proof elided, lines 581–586›
;; ‹lemma unit_ne_negN46: statement and proof elided, lines 587–588›
;; ‹lemma arith_nu46: statement and proof elided, lines 589–589›

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
