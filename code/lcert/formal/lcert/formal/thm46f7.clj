(ns lcert.formal.thm46f7
  "F4/F7 — Theorem 4.6 and Corollaries 4.6′ and 4.6″ at F7's concrete
  checker Check decCert, with no encoding hypothesis (R4-metatheory.md §4.5;
  design note nachlass/docs-f7-enc46-design.md; ADR-0006).

  theorem46.clj proves Theorem 4.6 for every checker meeting CheckSpec and
  Enc46, given certificates of the inputs whose terms are large in the same
  measure sz.  F7's first format refuted Enc46 (enc46f7.clj): its padding,
  and every other position its decoder never read, let an accepted
  certificate carry another one for free.  The current format (certcanon.clj,
  2026-10-06) is canonical and unpadded, with the certificate label 96 on the
  root only.  Here:

  - check_nest_free: no accepted code holds an accepted code strictly inside.
    An accepted code is sn 96 X (sl 0) with every internal label of X below
    96 (acc_shape); a subtree of X has the same property (nlb_sub); an
    accepted code has root label 96 (acc_root_ff).  This is F7's form of the
    paper's E6, and stronger than the paper needs: in the paper a
    sub-derivation may itself be a certificate, so case 3 of the proof must
    argue that it is paid for; F7 wraps only the root, so case 3 never
    arises.
  - enc46_canon: Enc46 holds at Check decCert for every term measure sz — its
    premise (an accepted tree with an accepted tree strictly inside) is never
    met.
  - thm46_F7, thm46_types_F7: Theorem 4.6 at Check decCert.  The hypotheses
    are the paper's only: certifiable inputs (an accepted certificate with
    labels in L for each), B different from each Aᵢ, and the derivation.  No
    CheckSpec (check_spec), no Enc46 (enc46_canon), and no large
    certificates: since Enc46 holds for every sz, thm46 is applied at the
    measure that is constantly k + 1, at which every certificate is large.
    The paper's large fᵢ = (λ(y :₀ Nat). gᵢ) N̄ (which would need weakening of
    derivation trees) is not needed.  Closedness of the Aᵢ follows from
    their certificates (cert_closed).
  - cor46_prime_F7, cor46_prime_iff_F7 (Corollary 4.6′, D2) and
    cor46_dprime_F7, cor46_dprime_iff_F7, d3_gap_F7 (Corollary 4.6″, D3):
    the corollaries at Check decCert.  Further hypotheses of cor46.clj are
    derived: the closedness of certified types (cert_closed), the labels of
    the type codes (cert_type_lbl: a type's code is a subtree of a canonical
    certificate), and ⌜□A⌝ ≠ ⌜A⌝ (box_code_ne: ⌜□A⌝ is larger)."
  (:require [ansatz.core :as a]
            [clojure.walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.model :refer :all]
            ;; exN
            [lcert.formal.splitting :refer :all]
            ;; exT, thetaD, thetaU
            [lcert.formal.outer]
            ;; boxTy, codeTerm, litAt
            [lcert.formal.section4b]
            [lcert.formal.check]
            [lcert.formal.check-spec]
            ;; nlb, code_enc_gt
            [lcert.formal.encsize]
            ;; decCert, acc_shape, acc_encCert, lblOk_certType
            [lcert.formal.certcanon]
            [lcert.formal.rint]
            ;; HTree, hfill, holeIn, hasHole, isNodeH, Enc46, bor_true_or, bor_left_true, bor_right_true
            [lcert.formal.enc46]
            ;; thm46, thm46_types
            [lcert.formal.theorem46]
            ;; d2_lower, d2_upper, d2_upper_k46, d3_lower, d3_upper_k46, d3_gap
            [lcert.formal.cor46]))

;; ---------------------------------------------------------------------------
;; Accepted codes never nest.

;; A subtree at a hole of a tree whose internal labels are below nb has its
;; internal labels below nb too.
(thm nlb_sub [nb :- Nat, sg :- (=> Nat Code), ix :- Nat, K :- HTree]
  (=> (Eq Bool (nlb nb (hfill sg K)) Bool.true) (Eq Bool (holeIn ix K) Bool.true) (Eq Bool (nlb nb (sg ix)) Bool.true))
  (induction K)
  (intro hn hi) (exact (False.elim (Bool.noConfusion hi)))
  (intro hn hi) (exact (Eq.mpr (congrArg (fn [z :- Nat] (Eq Bool (nlb nb (sg z)) Bool.true)) (Nat.eq_of_beq_eq_true hi)) hn))
  (intro hn hi)
  (have hab (Eq Bool (Bool.and (nlb nb (hfill sg a)) (nlb nb (hfill sg b))) Bool.true)
    (band_right (Nat.blt l nb) (Bool.and (nlb nb (hfill sg a)) (nlb nb (hfill sg b))) hn))
  (have hor (Or (Eq Bool (holeIn ix a) Bool.true) (Eq Bool (holeIn ix b) Bool.true)) (bor_true_or (holeIn ix a) (holeIn ix b) hi))
  (cases hor)
  (exact (ih_a (band_left (nlb nb (hfill sg a)) (nlb nb (hfill sg b)) hab) h))
  (exact (ih_b (band_right (nlb nb (hfill sg a)) (nlb nb (hfill sg b)) hab) h)))

;; A node over holed trees that fills to sn 96 X (sl 0), nlb 96 X: each hole's
;; plug has its internal labels below 96 (the left child fills to X, the right
;; to a leaf).
(thm nest_node [sg :- (=> Nat Code), ix :- Nat, X :- Code, l :- Nat, Ka :- HTree, Kb :- HTree,
                he :- (Eq Code (hfill sg (HTree.hnode l Ka Kb)) (Code.sn 96 X (Code.sl 0))),
                hX :- (Eq Bool (nlb 96 X) Bool.true),
                hi :- (Eq Bool (holeIn ix (HTree.hnode l Ka Kb)) Bool.true)]
  (Eq Bool (nlb 96 (sg ix)) Bool.true)
  (have ea (Eq Code (hfill sg Ka) X) (congrArg chHead he))
  (have eb (Eq Code (hfill sg Kb) (Code.sl 0)) (congrArg chTail he))
  (have ha (Eq Bool (nlb 96 (hfill sg Ka)) Bool.true) (Eq.mpr (congrArg (fn [z :- Code] (Eq Bool (nlb 96 z) Bool.true)) ea) hX))
  (have hb (Eq Bool (nlb 96 (hfill sg Kb)) Bool.true)
    (Eq.mpr (congrArg (fn [z :- Code] (Eq Bool (nlb 96 z) Bool.true)) eb) (Eq.refl$1 Bool.true)))
  (have hor (Or (Eq Bool (holeIn ix Ka) Bool.true) (Eq Bool (holeIn ix Kb) Bool.true)) (bor_true_or (holeIn ix Ka) (holeIn ix Kb) hi))
  (cases hor)
  (exact (nlb_sub 96 sg ix Ka ha h))
  (exact (nlb_sub 96 sg ix Kb hb h)))
;; The same for any holed tree that is a node (a leaf or a bare hole is not).
(thm nest_K [sg :- (=> Nat Code), ix :- Nat, X :- Code, hX :- (Eq Bool (nlb 96 X) Bool.true), K :- HTree]
  (=> (Eq Code (hfill sg K) (Code.sn 96 X (Code.sl 0))) (Eq Bool (isNodeH K) Bool.true) (Eq Bool (holeIn ix K) Bool.true)
      (Eq Bool (nlb 96 (sg ix)) Bool.true))
  (cases K)
  (intro he hK hi) (exact (False.elim (Bool.noConfusion hK)))
  (intro he hK hi) (exact (False.elim (Bool.noConfusion hK)))
  (intro he hK hi) (exact (nest_node sg ix X l a b he hX hi)))

;; An accepted code is labelled 96 at the root: nlb 96 fails on it.
(thm acc_root_ff [c :- Code, d :- Code, h :- (Eq Bool (Check decCert c d) Bool.true)] (Eq Bool (nlb 96 c) Bool.false)
  (refine' (exT Code _ _ (acc_shape c d h) _)) (intro Y hY)
  (exact (Eq.mpr (congrArg (fn [z :- Code] (Eq Bool (nlb 96 z) Bool.false)) (And.left hY)) (Eq.refl$1 Bool.false))))

;; No accepted code holds an accepted code strictly inside (F7's E6): filling
;; the holes of a node K with σ, if the result is accepted, no hole's plug is.
(thm check_nest_free [sg :- (=> Nat Code), K :- HTree, d :- Code, ix :- Nat, d2 :- Code,
                      h :- (Eq Bool (Check decCert (hfill sg K) d) Bool.true),
                      hK :- (Eq Bool (isNodeH K) Bool.true),
                      hi :- (Eq Bool (holeIn ix K) Bool.true),
                      h2 :- (Eq Bool (Check decCert (sg ix) d2) Bool.true)]
  False
  (refine' (exT Code _ _ (acc_shape (hfill sg K) d h) _)) (intro X hX)
  (have ht (Eq Bool (nlb 96 (sg ix)) Bool.true) (nest_K sg ix X (And.right hX) K (And.left hX) hK hi))
  (have hf (Eq Bool (nlb 96 (sg ix)) Bool.false) (acc_root_ff (sg ix) d2 h2))
  (exact (Bool.noConfusion (Eq.trans (Eq.symm hf) ht))))

;; ---------------------------------------------------------------------------
;; Enc46 at Check decCert, for every measure.

;; A tree with a hole has a hole with an index.
(thm hasHole_ex [K :- HTree] (=> (Eq Bool (hasHole K) Bool.true) (Exists (fn [ix :- Nat] (Eq Bool (holeIn ix K) Bool.true))))
  (induction K)
  (intro hh) (exact (False.elim (Bool.noConfusion hh)))
  (intro hh) (constructor) (exact i) (exact (nbeq_refl i))
  (intro hh)
  (have hor (Or (Eq Bool (hasHole a) Bool.true) (Eq Bool (hasHole b) Bool.true)) (bor_true_or (hasHole a) (hasHole b) hh))
  (cases hor)
  (refine' (exN _ _ (ih_a h) _)) (intro ix hix) (constructor) (exact ix) (exact (bor_left_true (holeIn ix a) (holeIn ix b) hix))
  (refine' (exN _ _ (ih_b h) _)) (intro ix hix) (constructor) (exact ix) (exact (bor_right_true (holeIn ix a) (holeIn ix b) hix)))

;; Enc46 holds at F7's checker for every term measure: its premise — an
;; accepted node with a hole whose plug is accepted — never holds
;; (check_nest_free).
(thm enc46_canon [sz :- (=> Exp Nat)] (Enc46 (Check decCert) (decOf decCert) sz)
  (intro sg K d hacc hK hH hall)
  (refine' (exN _ _ (hasHole_ex K hH) _)) (intro ix hix)
  (refine' (exT Code _ _ (hall ix hix) _)) (intro D hD)
  (exact (False.elim (check_nest_free sg K d ix D hacc hK hix hD))))

;; ---------------------------------------------------------------------------
;; Theorem 4.6 at F7's checker.

;; Theorem 4.6 over codes, at Check decCert: Θₖ ⊢ t :¹ □(ca 0) ⊗ ⋯ ⊗ □(ca r) ⊸ □cb,
;; certificates σ i of the ca i with labels in L (PhOk), and cb ≠ ca i: some
;; certificate of cb has at most k nodes — k ≥ μ(cb).  thm46 with CheckSpec
;; (check_spec), Enc46 (enc46_canon) and the measure constantly k + 1, at
;; which every certificate is large.
(thm thm46_F7 [r :- Nat, sg :- (=> Nat Code), hsg :- (PhOk (+ r 1) sg), ca :- (=> Nat Code), cb :- Code, k :- Nat, t :- Exp,
               hc :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Bool (Check decCert (sg i) (ca i)) Bool.true))),
               hne :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Code cb (ca i)) False)),
               der :- (Rt (Check decCert) (thetaD k) (thetaU k) t (Exp.tPi U.u1 (tensB ca 0 r) (boxTy (codeTerm cb))))]
  (Exists (fn [v :- Code] (And (Eq Bool (Check decCert v cb) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) k)))))
  (exact (thm46 (Check decCert) (decOf decCert) encE (check_spec decCert) (fn [u :- Exp] (+ k 1)) (enc46_canon (fn [u :- Exp] (+ k 1)))
                r sg hsg ca cb k t hc
                (fn [i :- Nat, hi :- (LT.lt i (+ r 1)), m :- Nat, u :- Exp, A :- Exp,
                     e :- (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCert (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))]
                  (Nat.lt_succ_self k))
                hne der)))

;; A certified type is closed (CheckSpec's decoding and E1).
(thm cert_closed [c :- Code, X :- Exp, h :- (Eq Bool (Check decCert c (encE X)) Bool.true)] (Eq Bool (closedTy X) Bool.true)
  (refine' (exT Nat _ _ (check_full decCert c (encE X) h) _)) (intro m hm)
  (refine' (exT Exp _ _ hm _)) (intro t ht)
  (refine' (exT Exp _ _ ht _)) (intro A hA)
  (have eA (Eq Exp A X) (encE_inj A X (And.left (And.right (And.right (And.right (And.right hA)))))))
  (exact (Eq.mp (congrArg (fn [Z :- Exp] (Eq Bool (closedTy Z) Bool.true)) eA) (And.left (And.right (And.right (And.right hA)))))))

;; Theorem 4.6 as the paper states it, at Check decCert: closed certifiable
;; A₀ … A_r (certificates σ i with labels in L), a closed B different from
;; each, and Θₖ ⊢ t :¹ □A₀ ⊗ ⋯ ⊗ □A_r ⊸ □B give k ≥ μ(B).
(thm thm46_types_F7 [r :- Nat, sg :- (=> Nat Code), hsg :- (PhOk (+ r 1) sg), As :- (=> Nat Exp), B :- Exp, k :- Nat, t :- Exp,
                     hclB :- (Eq Bool (closedTy B) Bool.true),
                     hc :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Bool (Check decCert (sg i) (encE (As i))) Bool.true))),
                     hne :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Exp B (As i)) False)),
                     der :- (Rt (Check decCert) (thetaD k) (thetaU k) t
                                (Exp.tPi U.u1 (tensB (fn [i :- Nat] (encE (As i))) 0 r) (boxTy (codeTerm (encE B)))))]
  (Exists (fn [v :- Code] (And (Eq Bool (Check decCert v (encE B)) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) k)))))
  (exact (thm46_types (Check decCert) (decOf decCert) encE (check_spec decCert) (fn [u :- Exp] (+ k 1)) (enc46_canon (fn [u :- Exp] (+ k 1)))
                      r sg hsg As B k t
                      (fn [i :- Nat, hi :- (LT.lt i (+ r 1))] (cert_closed (sg i) (As i) (hc i hi)))
                      hclB hc
                      (fn [i :- Nat, hi :- (LT.lt i (+ r 1)), m :- Nat, u :- Exp, A :- Exp,
                           e :- (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCert (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))]
                        (Nat.lt_succ_self k))
                      hne der)))

;; ---------------------------------------------------------------------------
;; Facts the corollaries' hypotheses reduce to at F7.

;; A type's code is a subtree of a canonical certificate of it, so a
;; certificate with labels in L has a type code with labels in L.
(thm cert_type_lbl [v :- Code, X :- Exp, h :- (Eq Bool (Check decCert v (encE X)) Bool.true), hl :- (Eq Bool (lblOk v) Bool.true)]
  (Eq Bool (lblOk (encE X)) Bool.true)
  (refine' (exT Nat _ _ (check_full decCert v (encE X) h) _)) (intro m hm)
  (refine' (exT Exp _ _ hm _)) (intro t ht)
  (refine' (exT Exp _ _ ht _)) (intro A hA)
  (refine' (exT DT _ _ (acc_encCert v (encE X) m t A h (And.left hA)) _)) (intro T1 q1)
  (refine' (exT DT _ _ q1 _)) (intro T2 q2)
  (have hl2 (Eq Bool (lblOk (encCert m t A T1 T2)) Bool.true) (Eq.mp (congrArg (fn [z :- Code] (Eq Bool (lblOk z) Bool.true)) q2) hl))
  (have hA2 (Eq Bool (lblOk (encE A)) Bool.true) (lblOk_certType m t A T1 T2 hl2))
  (exact (Eq.mp (congrArg (fn [z :- Code] (Eq Bool (lblOk z) Bool.true)) (And.left (And.right (And.right (And.right (And.right hA)))))) hA2)))

;; ⌜□A⌝ ≠ ⌜A⌝: ⌜□c⌝ holds ⌜codeTerm c⌝, which is larger than c (code_enc_gt).
(thm box_nodes_gt [c :- Code] (LT.lt (cnodes c) (cnodes (encE (boxTy (codeTerm c)))))
  (have e (Eq Nat (cnodes (encE (boxTy (codeTerm c))))
                  (+ 1 (+ 0 (+ 1 (+ (+ 1 (+ (+ 1 (+ (+ 1 (+ 0 0)) 0)) (cnodes (encE (codeTerm c))))) 0)))))
    (rfl))
  (have g (LT.lt (cnodes c) (cnodes (encE (codeTerm c)))) (code_enc_gt c))
  (omega))
(thm box_code_ne [A :- Exp, e :- (Eq Code (encE (boxTy (codeTerm (encE A)))) (encE A))] False
  (have g (LT.lt (cnodes (encE A)) (cnodes (encE (boxTy (codeTerm (encE A)))))) (box_nodes_gt (encE A)))
  (have e2 (Eq Nat (cnodes (encE (boxTy (codeTerm (encE A))))) (cnodes (encE A))) (congrArg cnodes e))
  (omega))

;; ---------------------------------------------------------------------------
;; Corollary 4.6′ (D2) at F7's checker.

;; □(A ⊸ B) ⊗ □A ⊸ □B, its term λz. (lit v, ⋆), and "k ≥ μ(d)".
(def ^:private ABc '(encE (Exp.tPi U.u1 A B)))
(def ^:private D2T (list 'Exp.tPi 'U.u1 (list 'Exp.tSig 'U.u1 (list 'boxTy (list 'codeTerm ABc)) '(boxTy (codeTerm (encE A))))
                         '(boxTy (codeTerm (encE B)))))
(def ^:private D2L (list 'Exp.lam 'U.u1 (second (rest D2T)) '(Exp.pair (boxTy (codeTerm (encE B))) ((litAt v) 1) Exp.star)))
(defn- cert-le [d k] (list 'Exists (list 'fn '[v :- Code] (list 'And (list 'Eq 'Bool (list 'Check 'decCert 'v d) 'Bool.true)
                                                            (list 'And '(Eq Bool (lblOk v) Bool.true) (list 'LE.le '(cnodes v) k))))))
;; A certificate large at budget kk in the measure that is constantly kk + 1.
(defn- big-at [c d kk h l]
  (list 'And.intro h (list 'And.intro l
    (list 'fn ['m :- 'Nat 'u :- 'Exp 'A' :- 'Exp
               'e :- (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'decOf 'decCert c) '(Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A'))))]
      (list 'Nat.lt_succ_self kk)))))
(def ^:private D2P '[A :- Exp, B :- Exp, hclB :- (Eq Bool (closedTy B) Bool.true), hne :- (=> (Eq Exp B A) False),
                     c0 :- Code, h0 :- (Eq Bool (Check decCert c0 (encE (Exp.tPi U.u1 A B))) Bool.true), l0 :- (Eq Bool (lblOk c0) Bool.true),
                     c1 :- Code, h1 :- (Eq Bool (Check decCert c1 (encE A)) Bool.true), l1 :- (Eq Bool (lblOk c1) Bool.true)])
(defn- d2-lower [kk tt]
  (list 'd2_lower '(Check decCert) '(decOf decCert) 'encE '(check_spec decCert) (list 'fn '[u :- Exp] (list '+ kk 1))
        (list 'enc46_canon (list 'fn '[u :- Exp] (list '+ kk 1)))
        'A 'B kk tt '(cert_closed c1 A h1) 'hclB '(cert_closed c0 (Exp.tPi U.u1 A B) h0) 'hne 'c0 'c1
        (big-at 'c0 ABc kk 'h0 'l0) (big-at 'c1 '(encE A) kk 'h1 'l1) 'der))

;; Corollary 4.6′ at F7's checker.  Let A ≠ B, B closed, and A ⊸ B and A
;; certifiable (certificates c₀, c₁ with labels in L).  Every term of
;; □(A ⊸ B) ⊗ □A ⊸ □B at Θₖ forces k ≥ μ(B), and every certificate v of B
;; (labels in L) gives one at Θ_{‖v‖}.
(eval (list 'lcert.formal.base/thm 'cor46_prime_F7 D2P
  (list 'And
    (list 'forall '[kk Nat] (list 'forall '[tt Exp]
      (list '=> (list 'Rt '(Check decCert) '(thetaD kk) '(thetaU kk) 'tt D2T) (cert-le '(encE B) 'kk))))
    (list 'forall '[v Code] (list '=> '(Eq Bool (lblOk v) Bool.true) '(Eq Bool (Check decCert v (encE B)) Bool.true)
      (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt '(Check decCert) '(thetaD (cnodes v)) '(thetaU (cnodes v)) 'tt D2T))))))
  '(constructor)
  '(intro kk tt der)
  (list 'exact (d2-lower 'kk 'tt))
  '(intro v hok hck)
  '(constructor) (list 'exact D2L)
  '(exact (d2_upper (Check decCert) encE A B v hok (cert_type_lbl c0 (Exp.tPi U.u1 A B) h0 l0) (cert_type_lbl c1 A h1 l1)
                    (cert_type_lbl v B hck hok) hck))))

;; Corollary 4.6′ exactly, at F7's checker: a term of □(A ⊸ B) ⊗ □A ⊸ □B
;; exists at Θₖ iff some certificate of B has at most k nodes — iff k ≥ μ(B).
(eval (list 'lcert.formal.base/thm 'cor46_prime_iff_F7 D2P
  (list 'forall '[kk Nat]
    (list 'Iff (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt '(Check decCert) '(thetaD kk) '(thetaU kk) 'tt D2T)))
               (cert-le '(encE B) 'kk)))
  '(intro kk)
  '(constructor)
  '(intro hx)
  '(refine' (exT Exp _ _ hx _)) '(intro tt der)
  (list 'exact (d2-lower 'kk 'tt))
  '(intro hx)
  '(refine' (exT Code _ _ hx _)) '(intro v hv)
  '(constructor)
  (list 'exact D2L)
  '(exact (d2_upper_k46 (Check decCert) encE A B v (And.left (And.right hv)) (cert_type_lbl c0 (Exp.tPi U.u1 A B) h0 l0)
                        (cert_type_lbl c1 A h1 l1) (cert_type_lbl v B (And.left hv) (And.left (And.right hv)))
                        (And.left hv) kk (And.right (And.right hv))))))

;; ---------------------------------------------------------------------------
;; Corollary 4.6″ (D3) at F7's checker.

(def ^:private BA '(boxTy (codeTerm (encE A))))
(def ^:private BBA (list 'boxTy (list 'codeTerm (list 'encE BA))))
(def ^:private D3T (list 'Exp.tPi 'U.u1 BA BBA))
(def ^:private D3L (list 'Exp.lam 'U.u1 BA (list 'Exp.pair BBA '((litAt w) 1) 'Exp.star)))
(def ^:private D3P '[A :- Exp, c :- Code, hc :- (Eq Bool (Check decCert c (encE A)) Bool.true), lc :- (Eq Bool (lblOk c) Bool.true)])
(defn- d3-lower [kk tt]
  (list 'd3_lower '(Check decCert) '(decOf decCert) 'encE '(check_spec decCert) (list 'fn '[u :- Exp] (list '+ kk 1))
        (list 'enc46_canon (list 'fn '[u :- Exp] (list '+ kk 1)))
        'A kk tt 'c (big-at 'c '(encE A) kk 'hc 'lc) '(box_code_ne A) 'der))

;; Corollary 4.6″ at F7's checker.  For A certifiable (a certificate c with
;; labels in L), every term of □A ⊸ □□A at Θₖ forces k ≥ μ(□A), and every
;; certificate w of □A (labels in L) gives one at Θ_{‖w‖}.
(eval (list 'lcert.formal.base/thm 'cor46_dprime_F7 D3P
  (list 'And
    (list 'forall '[kk Nat] (list 'forall '[tt Exp]
      (list '=> (list 'Rt '(Check decCert) '(thetaD kk) '(thetaU kk) 'tt D3T) (cert-le (list 'encE BA) 'kk))))
    (list 'forall '[w Code] (list '=> '(Eq Bool (lblOk w) Bool.true) (list 'Eq 'Bool (list 'Check 'decCert 'w (list 'encE BA)) 'Bool.true)
      (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt '(Check decCert) '(thetaD (cnodes w)) '(thetaU (cnodes w)) 'tt D3T))))))
  '(constructor)
  '(intro kk tt der)
  (list 'exact (d3-lower 'kk 'tt))
  '(intro w hok hck)
  '(constructor) (list 'exact D3L)
  (list 'exact (list 'prop45_d3 '(Check decCert) '(decOf decCert) 'encE 'A 'w 'hok '(cert_type_lbl c A hc lc)
                     (list 'cert_type_lbl 'w BA 'hck 'hok) 'hck))))

;; Corollary 4.6″ exactly, at F7's checker: a term of □A ⊸ □□A exists at Θₖ
;; iff some certificate of □A has at most k nodes — iff k ≥ μ(□A).
(eval (list 'lcert.formal.base/thm 'cor46_dprime_iff_F7 D3P
  (list 'forall '[kk Nat]
    (list 'Iff (list 'Exists (list 'fn '[tt :- Exp] (list 'Rt '(Check decCert) '(thetaD kk) '(thetaU kk) 'tt D3T)))
               (cert-le (list 'encE BA) 'kk)))
  '(intro kk)
  '(constructor)
  '(intro hx)
  '(refine' (exT Exp _ _ hx _)) '(intro tt der)
  (list 'exact (d3-lower 'kk 'tt))
  '(intro hx)
  '(refine' (exT Code _ _ hx _)) '(intro w hw)
  '(constructor)
  (list 'exact D3L)
  (list 'exact (list 'd3_upper_k46 '(Check decCert) 'encE 'A 'w '(And.left (And.right hw)) '(cert_type_lbl c A hc lc)
                     (list 'cert_type_lbl 'w BA '(And.left hw) '(And.left (And.right hw)))
                     '(And.left hw) 'kk '(And.right (And.right hw))))))

;; The gap μ(□A) > 2μ(A) at F7's checker (Proposition 4.3, TokSize a theorem
;; there): every certificate of □A is larger than twice any lower bound M of
;; the certificates of A.
(thm d3_gap_F7 [A :- Exp, w :- Code, hw :- (Eq Bool (Check decCert w (encE (boxTy (codeTerm (encE A))))) Bool.true),
                M :- Nat, hM :- (forall [v Code] (=> (Eq Bool (Check decCert v (encE A)) Bool.true) (LE.le M (cnodes v))))]
  (LT.lt (+ M M) (cnodes w))
  (exact (d3_gap (Check decCert) (decOf decCert) encE (check_spec decCert) (check_toksize decCert) A w hw M hM)))
