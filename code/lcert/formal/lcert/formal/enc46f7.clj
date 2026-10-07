(ns lcert.formal.enc46f7
  "F4/F7 — F7's first, padded certificate format does not satisfy Enc46
  (R4-metatheory.md §4.5, case 3 of the proof of Theorem 4.6; design notes
  nachlass/docs-theorem46-design.md §5 and docs-f7-enc46-design.md;
  ADR-0006).  This is the historical counterexample: the format Check
  decCert now reads is certcanon.clj's canonical one, which satisfies Enc46
  (thm46f7.clj's enc46_canon).

  Theorem 4.6 is proved (theorem46.clj) for every checker that meets
  CheckSpec and Enc46, given large certificates of the input types.  Enc46
  (enc46.clj) is case 3's encoding fact: an accepted tree that embeds
  accepted trees strictly inside pays, in its own nodes, for the term of one
  of them.  The paper derives it from E2, E3, E4 and E6: every rule-labelled
  subtree of an accepted code is a derivation node, so an embedded
  certificate is a sub-derivation whose term the root's judgment records.

  F7's padded certificates (certenc.clj's encCertPad) have the form
      sn 0 ⌜(fuel, m, t, A, typing tree, formation tree)⌝ pad.
  Neither the decoder nor the checker reads pad except through its size.
  decCertPad reads only the root's left child.  Check's body (check_spec.clj)
  tests the certificate c only through cnodes c: the size facts, and the
  bound below which the trees' δ-steps may consult the checker.  So a
  certificate stays accepted when its padding is replaced by any larger tree,
  including one that contains another accepted certificate.  That costs
  the outer certificate no nodes of its own beyond the old ones.

  Results:
  - bodyD_pad, check_pad: for every decoder decD, Check decD accepts c′ at d
    when it accepts c at d, c′ decodes as c does, and ‖c‖ ≤ ‖c′‖.  Padding
    is free.
  - decCertPad_head: decCertPad c depends only on c's left child.
  - padK c₀: the holed tree sn 0 (left c₀) (sn 0 c₀ □) whose one hole □
    is the padding's right child.  Filled with c₁ it is a certificate that
    decodes as c₀ and is at least as large (hfill_padK, pad_le_f7).  Its own
    node count is 2 + ‖left c₀‖ + ‖c₀‖ (padK_nodes).
  - enc46_F7_core: take an accepted c₀, and an accepted c₁ every decoding of
    whose term exceeds hnodes (padK c₀) in sz.  Then Enc46 fails at
    Check decCertPad, for that measure sz.
  - enc46_F7: LargeCert, for any code d, refutes Enc46 at Check decCertPad,
    for every measure sz.  So at the padded format the hypotheses of
    Corollaries 4.6′ and 4.6″ (cor46_prime and cor46_dprime take Enc46 and
    LargeCert) cannot all hold.
  - thm46_F7_vacuous: at the padded format, Theorem 4.6's hypotheses are
    contradictory at every budget k ≥ hnodes (padK c₀), for any accepted c₀.
    The contradictory pair is Enc46 together with an accepted σ 0 whose term
    exceeds k in sz (thm46's hc and hbig at i = 0).

  What this does not say: that Theorem 4.6 is false at the padded format.
  Only the phantom model's case 3 fails there, because the model cannot see
  that a program cannot type the evidence for a certificate it builds around
  its input.  The way out was a format without free positions, whose size
  facts are derived from the encoding, as in the paper's Lemmas 2.6–2.7,
  rather than tested: certcanon.clj, where Theorem 4.6 holds with no
  encoding hypothesis (thm46f7.clj)."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.check-hd :as h]
            [lcert.formal.check-spec]
            ;; decCertPad, chHead, band_tt
            [lcert.formal.certenc]
            ;; exT
            [lcert.formal.outer]
            ;; HTree, hfill, hnodes, holeIn, hasHole, isNodeH, Enc46, add1_eq, bor_right_true
            [lcert.formal.enc46]
            ;; ex_acc46
            [lcert.formal.theorem46]
            ;; LargeCert
            [lcert.formal.cor46]))

;; The decoded certificate of check_spec.clj: (m, t, A, typing tree,
;; formation tree), written CD in the forms below.
(def ^:private CD '(Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
(defn- x [form] (lv (walk/postwalk-replace {'CD CD} form)))

;; ---------------------------------------------------------------------------
;; Padding is free: Check reads a certificate only through its decoding and
;; its size.

;; The body's checks (check_spec.clj), at the certificate cc: the two trees
;; at the checker restricted below cc, closedness, the type's code, then five
;; size tests Nat.ble X (cnodes cc).
(def ^:private checks @#'lcert.formal.check-spec/body-checks)
(defn- checks-at [cc] (walk/postwalk-replace {'c cc} checks))

;; If the body accepts (m, t, A, T1, T2) at c, it accepts them at any c2 at
;; least as large.  For the trees: each is checked at r restricted below c,
;; and the body bounds every δ-code of the tree by ‖c‖ (dtB T ≤ ‖c‖), so the
;; restriction agrees with r on everything the tree consults (restr_self_agree,
;; dtCheck_agree), at c and at c2 alike.  The size tests are monotone in ‖c‖.
(let [cs (checks-at 'c)
      c2s (checks-at 'c2)
      pr (fn [i] (h/f7-projection cs i 'hb))
      le-c (fn [i] (list 'Nat.le_of_ble_eq_true (pr i)))
      le-c2 (fn [i] (list 'Nat.le_trans (le-c i) 'hle))
      tree (fn [T J i-check i-bound]
             (list 'Eq.trans
               (list 'dtCheck_agree '(restrC r c2) 'r T (list 'restr_self_agree 'r 'c2 (list 'dtB T) (le-c2 i-bound)) J)
               (list 'Eq.trans
                 (list 'Eq.symm (list 'dtCheck_agree '(restrC r c) 'r T (list 'restr_self_agree 'r 'c (list 'dtB T) (le-c i-bound)) J))
                 (pr i-check))))
      proofs (into [(tree 'T1 '(DTJ.rt (thetaD m) (thetaU m) t A) 0 7)
                    (tree 'T2 '(DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit) 1 8)
                    (pr 2)
                    (pr 3)]
                   (map (fn [i] (list 'Nat.ble_eq_true_of_le (le-c2 i))) (range 4 9)))
      n (count c2s)
      conj-true (reduce (fn [acc i] (list 'band_tt (nth c2s i) (h/f7-and (drop (inc i) c2s)) (nth proofs i) acc))
                        '(Eq.refl Bool.true) (range (dec n) -1 -1))]
  (a/prove-theorem 'bodyD_pad
    (lv '[r :- (=> Code Code Bool), c :- Code, c2 :- Code, d :- Code, m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
          hle :- (LE.le (cnodes c) (cnodes c2)),
          hb :- (Eq Bool (bodyD r c d m t A T1 T2) Bool.true)])
    (lv '(Eq Bool (bodyD r c2 d m t A T1 T2) Bool.true))
    (lv [(list 'have 'hpad (list 'Eq 'Bool (h/f7-and c2s) 'Bool.true) conj-true)
         '(exact hpad)])))

;; Check decD accepts c2 at d if it accepts c at d, c2 decodes as c does, and
;; c2 is at least as large: Check is its body at Check (check_fix), and the
;; body sees c2's decoding, which is c's, at a size no smaller (bodyD_pad).
(a/prove-theorem 'check_pad
  (x '[decD :- (=> Code (Option CD)), c :- Code, c2 :- Code, d :- Code,
       hdec :- (Eq (Option CD) (decD c2) (decD c)), hle :- (LE.le (cnodes c) (cnodes c2)),
       h :- (Eq Bool (Check decD c d) Bool.true)])
  (x '(Eq Bool (Check decD c2 d) Bool.true))
  (x '[(have hb (Eq Bool (bodyO (Check decD) c d (decD c)) Bool.true)
         (Eq.trans (Eq.symm (check_fix decD c d)) h))
       (refine' (exT _ _ _ (bodyO_true (Check decD) c d (decD c) hb) _))
       (intro y hy)
       (exact (Eq.trans (check_fix decD c2 d)
                (Eq.trans (congrArg (fn [o :- (Option CD)] (bodyO (Check decD) c2 d o)) (Eq.trans hdec (And.left hy)))
                  (bodyD_pad (Check decD) c c2 d (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                             (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))
                             hle (And.right hy)))))]))

;; decCertPad reads only the root's left child: a node over c₀'s left child
;; decodes as c₀, whatever its right child.
(a/prove-theorem 'decCertPad_head
  (x '[c0 :- Code, P :- Code])
  (x '(Eq (Option CD) (decCertPad (Code.sn 0 (chHead c0) P)) (decCertPad c0)))
  '[(rfl)])

;; ---------------------------------------------------------------------------
;; The holed tree that hides a certificate in the padding.

;; ofCode c: c as a holed tree with no hole.
(kdef ofCode (=> Code HTree)
  (fn [c :- Code]
    (Code.rec$1 (fn [_ :- Code] HTree)
      (fn [l :- Nat] (HTree.hleaf l))
      (fn [l :- Nat, a :- Code, b :- Code, ha :- HTree, hb :- HTree] (HTree.hnode l ha hb))
      c)))
(thm hfill_ofCode [sg :- (=> Nat Code), c :- Code] (Eq Code (hfill sg (ofCode c)) c)
  (induction c)
  (rfl)
  (have h (Eq Code (Code.sn l (hfill sg (ofCode a)) (hfill sg (ofCode b))) (Code.sn l a b))
    (congr (congrArg (Code.sn l) ih_a) ih_b))
  (exact h))
(thm hnodes_ofCode [c :- Code] (Eq Nat (hnodes (ofCode c)) (cnodes c))
  (induction c)
  (rfl)
  (have h (Eq Nat (+ 1 (+ (hnodes (ofCode a)) (hnodes (ofCode b)))) (+ 1 (+ (cnodes a) (cnodes b))))
    (add1_eq (hnodes (ofCode a)) (hnodes (ofCode b)) (cnodes a) (cnodes b) ih_a ih_b))
  (exact h))

;; padK c₀ = sn 0 (left c₀) (sn 0 c₀ □): c₀'s decodable part, then a padding
;; that holds c₀ itself (so the filled tree is at least as large as c₀) and
;; the hole.
(kdef padK (=> Code HTree)
  (fn [c0 :- Code] (HTree.hnode 0 (ofCode (chHead c0)) (HTree.hnode 0 (ofCode c0) (HTree.hhole 0)))))

(thm hfill_padK [sg :- (=> Nat Code), c0 :- Code]
  (Eq Code (hfill sg (padK c0)) (Code.sn 0 (chHead c0) (Code.sn 0 c0 (sg 0))))
  (have e (Eq Code (hfill sg (padK c0)) (Code.sn 0 (hfill sg (ofCode (chHead c0))) (Code.sn 0 (hfill sg (ofCode c0)) (sg 0))))
    (rfl))
  (exact (Eq.trans e (congr (congrArg (Code.sn 0) (hfill_ofCode sg (chHead c0)))
                            (congrArg (fn [z :- Code] (Code.sn 0 z (sg 0))) (hfill_ofCode sg c0))))))

;; padK c₀'s own nodes: the root, c₀'s left child, the padding's node, c₀.
(thm padK_nodes [c0 :- Code]
  (Eq Nat (hnodes (padK c0)) (+ 1 (+ (cnodes (chHead c0)) (+ 1 (+ (cnodes c0) 0)))))
  (have e (Eq Nat (hnodes (padK c0)) (+ 1 (+ (hnodes (ofCode (chHead c0))) (+ 1 (+ (hnodes (ofCode c0)) 0)))))
    (rfl))
  (rw [e])
  (rw [(hnodes_ofCode (chHead c0))])
  (rw [(hnodes_ofCode c0)]))

;; padK c₀ is a node with a hole.
(thm hasHole_padK [c0 :- Code] (Eq Bool (hasHole (padK c0)) Bool.true)
  (exact (bor_right_true (hasHole (ofCode (chHead c0))) (Bool.or (hasHole (ofCode c0)) Bool.true)
           (bor_right_true (hasHole (ofCode c0)) Bool.true (Eq.refl$1 Bool.true)))))
(thm isNodeH_padK [c0 :- Code] (Eq Bool (isNodeH (padK c0)) Bool.true) (rfl))

;; The filled tree is at least as large as c₀.
(thm pad_le_f7 [c0 :- Code, c1 :- Code] (LE.le (cnodes c0) (cnodes (Code.sn 0 (chHead c0) (Code.sn 0 c0 c1))))
  (have e (Eq Nat (cnodes (Code.sn 0 (chHead c0) (Code.sn 0 c0 c1))) (+ 1 (+ (cnodes (chHead c0)) (+ 1 (+ (cnodes c0) (cnodes c1))))))
    (rfl))
  (rw [e])
  (omega))

;; ---------------------------------------------------------------------------
;; Enc46 fails at the padded format's checker (Check decCertPad).

;; One accepted certificate c₀, and an accepted c₁ every decoding of whose
;; term exceeds padK c₀'s own nodes in sz, refute Enc46 at Check decCertPad.
;; Filling padK c₀ with c₁ gives a tree Check accepts where it accepts c₀
;; (check_pad: it decodes as c₀, decCertPad_head, and is no smaller, pad_le_f7).
;; Enc46 then bounds c₁'s term by hnodes (padK c₀), against hbig.
(thm enc46_F7_core [sz :- (=> Exp Nat), c0 :- Code, d0 :- Code, h0 :- (Eq Bool (Check decCertPad c0 d0) Bool.true),
                    c1 :- Code, d1 :- Code, h1 :- (Eq Bool (Check decCertPad c1 d1) Bool.true),
                    hbig :- (forall [m Nat] (forall [u Exp] (forall [A Exp]
                              (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCertPad c1)
                                      (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))
                                  (LT.lt (hnodes (padK c0)) (sz u))))))]
  (=> (Enc46 (Check decCertPad) (decOf decCertPad) sz) False)
  (intro henc)
  (have hacc (Eq Bool (Check decCertPad (hfill (fn [i :- Nat] c1) (padK c0)) d0) Bool.true)
    (Eq.mpr (congrArg (fn [z :- Code] (Eq Bool (Check decCertPad z d0) Bool.true)) (hfill_padK (fn [i :- Nat] c1) c0))
      (check_pad decCertPad c0 (Code.sn 0 (chHead c0) (Code.sn 0 c0 c1)) d0
                 (decCertPad_head c0 (Code.sn 0 c0 c1)) (pad_le_f7 c0 c1) h0)))
  (have hX (Exists (fn [i :- Nat]
             (And (Eq Bool (holeIn i (padK c0)) Bool.true)
                  (forall [m Nat] (forall [t Exp] (forall [A Exp]
                    (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCertPad c1)
                            (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
                        (LE.le (sz t) (hnodes (padK c0))))))))))
    (henc (fn [i :- Nat] c1) (padK c0) d0 hacc (isNodeH_padK c0) (hasHole_padK c0)
      (fn [i :- Nat, hi :- (Eq Bool (holeIn i (padK c0)) Bool.true)] (ex_acc46 (Check decCertPad) c1 d1 h1))))
  (refine' (exT Nat _ _ hX _)) (intro i hi)
  (refine' (exT Nat _ _ (check_full decCertPad c1 d1 h1) _)) (intro m hm)
  (refine' (exT Exp _ _ hm _)) (intro t ht)
  (refine' (exT Exp _ _ ht _)) (intro A hA)
  (have hs (LE.le (sz t) (hnodes (padK c0))) ((And.right hi) m t A (And.left hA)))
  (have hl (LT.lt (hnodes (padK c0)) (sz t)) (hbig m t A (And.left hA)))
  (exact (Nat.lt_irrefl (sz t) (Nat.lt_of_le_of_lt hs hl))))

;; Large certificates of any one code refute Enc46 at the padded format, for every
;; measure: the certificate at budget 0 serves as c₀, the one above
;; hnodes (padK c₀) as c₁.  So Enc46 and LargeCert, which cor46_prime and
;; cor46_dprime take together, are never jointly true at Check decCertPad.
(thm enc46_F7 [sz :- (=> Exp Nat), d :- Code, hL :- (LargeCert (Check decCertPad) (decOf decCertPad) sz d)]
  (=> (Enc46 (Check decCertPad) (decOf decCertPad) sz) False)
  (refine' (exT Code _ _ (hL 0) _)) (intro c0 h0)
  (refine' (exT Code _ _ (hL (hnodes (padK c0))) _)) (intro c1 h1)
  (exact (enc46_F7_core sz c0 d (And.left h0) c1 d (And.left h1) (And.right (And.right h1)))))

;; Theorem 4.6 is vacuous at the padded format above a constant budget: for any
;; accepted c₀ and k ≥ hnodes (padK c₀), Enc46 and a certificate c₁ (thm46's
;; σ 0, accepted at its type) whose every term exceeds k in sz (thm46's hbig
;; at i = 0) are contradictory.
(thm thm46_F7_vacuous [sz :- (=> Exp Nat), henc :- (Enc46 (Check decCertPad) (decOf decCertPad) sz),
                       c0 :- Code, d0 :- Code, h0 :- (Eq Bool (Check decCertPad c0 d0) Bool.true),
                       k :- Nat, hk :- (LE.le (hnodes (padK c0)) k),
                       c1 :- Code, d1 :- Code, h1 :- (Eq Bool (Check decCertPad c1 d1) Bool.true),
                       hbig :- (forall [m Nat] (forall [u Exp] (forall [A Exp]
                                 (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCertPad c1)
                                         (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))
                                     (LT.lt k (sz u))))))]
  False
  (exact (enc46_F7_core sz c0 d0 h0 c1 d1 h1
           (fn [m :- Nat, u :- Exp, A :- Exp,
                e :- (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCertPad c1)
                         (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))]
             (Nat.lt_of_le_of_lt hk (hbig m u A e)))
           henc)))
