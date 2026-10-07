(ns lcert.formal.theorem46
  "F4 — Theorem 4.6 (R4-metatheory.md §4.5): transforming certificates costs
  a fresh proof.  Design note nachlass/docs-theorem46-design.md §5.

  The paper: let A₁ … A_j (j ≥ 1) be closed certifiable types and B a closed
  type different from each.  If Θₖ ⊢ t :¹ □A₁ ⊗ ⋯ ⊗ □A_j ⊸ □B, then
  k ≥ μ(B).

  Formal statement (thm46), over codes rather than types, with the encoding
  facts the proof uses as explicit hypotheses:
    - the inputs are r + 1 boxes □(ca 0) ⊗ ⋯ ⊗ □(ca r) (tensB ca 0 r, right-
      nested usage-1 Σ), □c = boxTy (codeTerm c) as in section4b.clj, and the
      output is □cb;
    - CheckSpec (the trust base);
    - Enc46 chkf dec sz (enc46.clj): an accepted tree that embeds accepted
      trees strictly inside pays, in its own nodes, at least the term size
      sz of one of them.  This is case 3's encoding argument (E2, E3, E4, E6
      and two facts about the rules), packaged as the one fact the proof
      reads, for any term measure sz;
    - for each i ≤ r, a certificate σ i of ca i (accepted, labels in L) whose
      every decoding has a term of measure sz above k (the paper's large
      fᵢ = (λ(y :₀ Nat). gᵢ) N̄, N > k, which needs completeness of the
      checker; CheckSpec gives only soundness, so the certificates are
      hypotheses);
    - cb ≠ ca i for each i (the paper's \"B different from each Aᵢ\", at the
      codes).
  Conclusion: some certificate v of cb (accepted, labels in L) has at most k
  internal nodes — k ≥ μ(cb), μ as a lower bound, as in prop434.clj.

  The proof (§4.5): Lemma 4.6a (lemma46a.clj) at the phantom interpretation
  phRI (r+1) σ, budget k, in the all-token environment, applied to the
  phantom input phIn = ((★₀, ⋆), …, (★ᵣ, ⋆)), which lies in V₀ (phin_V: a
  phantom has no node and prints to an accepted certificate).  The output
  is (r″, ⋆) with ‖r″‖ ≤ k and chkf (print r″) cb = tt (thm46_out's
  premises).  Three cases (thm46_out):
    1. r″ phantom-free: print r″ is the certificate, with ‖r″‖ nodes (L3);
    2. r″ = ★ᵢ: σ i is accepted at cb and at ca i; CheckSpec decodes it
       once, so cb = ca i — excluded (CheckSpec's decoding only; E1 is not
       used: the hypothesis is on codes);
    3. r″ a node with a phantom inside: Enc46 at the holed tree toH r″
       gives an embedded σ i whose term measure is at most ‖r″‖ ≤ k,
       against its being large.

  thm46_types: the paper's form, over closed types A₀ … A_r, B with B ≠ Aᵢ:
  E1 (CheckSpec's fourth clause) turns B ≠ Aᵢ into ⌜B⌝ ≠ ⌜Aᵢ⌝.

  Findings recorded in ADR-0006: E1 is needed only to pass from types to
  codes; the encoding enters only through Enc46.  F7's first, padded
  certificate format does not satisfy Enc46 (its padding is read only
  through size tests: enc46f7.clj); its canonical format does, and
  thm46f7.clj states the theorem at Check decCert with the paper's
  hypotheses only (thm46_F7, thm46_types_F7)."
  (:require [ansatz.core :as a]
            [clojure.walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.model :refer :all]
            ;; exN: ∃-elimination
            [lcert.formal.splitting :refer :all]
            ;; thetaD, thetaU, tokEnvD, wf_theta
            [lcert.formal.outer]
            ;; boxTy, codeTerm, codeTerm_of
            [lcert.formal.section4b]
            [lcert.formal.rint]
            [lcert.formal.enc46]
            [lcert.formal.ri-den]
            [lcert.formal.ri-sem]
            [lcert.formal.ri-unfold]
            [lcert.formal.ri-conversion]
            [lcert.formal.ri-outer]
            [lcert.formal.lemma46a]))

;; --- the input type and the phantom input -----------------------------------------------

;; tensB ca i r = □(ca i) ⊗ □(ca (i+1)) ⊗ ⋯ ⊗ □(ca (i+r)): r + 1 boxes,
;; right-nested usage-1 Σ.  Each box is closed, so the binder of each Σ does
;; not occur in its body.  tensF ca r is the recursion on r, returning the
;; function of the first index (the recursor is not over-applied).
(kdef tensF (=> (=> Nat Code) Nat (=> Nat Exp))
  (fn [ca :- (=> Nat Code), r :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (=> Nat Exp))
      (fn [j :- Nat] (boxTy (codeTerm (ca j))))
      (fn [r0 :- Nat, ih :- (=> Nat Exp)] (fn [j :- Nat] (Exp.tSig U.u1 (boxTy (codeTerm (ca j))) (ih (+ j 1)))))
      r)))
(kdef tensB (=> (=> Nat Code) Nat Nat Exp)
  (fn [ca :- (=> Nat Code), i :- Nat, r :- Nat] (tensF ca r i)))

;; phIn ca r i = ((★ᵢ, ⋆), ((★ᵢ₊₁, ⋆), … (★ᵢ₊ᵣ, ⋆))): the phantom ★ⱼ is the
;; leaf sl j (rint.clj, phRI).
(kdef phIn (forall [ca (=> Nat Code)] (forall [r Nat] (forall [i Nat] (Car (skel (tensB ca i r))))))
  (fn [ca :- (=> Nat Code), r :- Nat]
    (Nat.rec$1 (fn [r0 :- Nat] (forall [i Nat] (Car (skel (tensB ca i r0)))))
      (fn [i :- Nat] (Prod.mk (Code.sl i) Unit.unit))
      (fn [r0 :- Nat, ih :- (forall [i Nat] (Car (skel (tensB ca i r0))))]
        (fn [i :- Nat] (Prod.mk (Prod.mk (Code.sl i) Unit.unit) (ih (+ i 1)))))
      r)))

;; --- the phantom input lies in V₀ ---------------------------------------------------------

;; ⟦chk′ (print x) ⌜c⌝⟧ at (v, η) is the check on the print of v and c (the
;; canonical code term denotes its code: den_codeOf_ri, codeTerm_of).
(thm den_boxev_ri [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, cap :- Nat,
                   G :- (List Sk), c :- Code, v :- Code, en :- (HEnv G)]
  (Eq Bool (den_ri chkf dec encTy ri cap (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm c)) (List.cons Sk Sk.cert G) Sk.bool (Prod.mk v en))
           (chkf (riPr ri v) c))
  (rw [(den_chk_val_ri chkf dec encTy ri cap (Exp.prn (Exp.var 0)) (codeTerm c) (List.cons Sk Sk.cert G) (Prod.mk v en))])
  (rw [(den_prn_val_ri chkf dec encTy ri cap (Exp.var 0) (List.cons Sk Sk.cert G) (Prod.mk v en))])
  (rw [(den_var_at_ri chkf dec encTy ri cap 0 (List.cons Sk Sk.cert G) Sk.cert (Prod.mk v en))])
  (rw [(den_codeOf_ri chkf dec encTy ri cap (List.cons Sk Sk.cert G) (Prod.mk v en) (codeTerm c) c (codeTerm_of c))]))

;; (v, ⋆) ∈ V₀(□c) when v ∈ V₀(R) and its print is accepted at c.
(thm phbox_V [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, cap :- Nat,
              G :- (List Sk), en :- (HEnv G), c :- Code, v :- Code,
              hv :- (V_ri chkf dec encTy ri cap Exp.tR G en 0 Sk.cert v),
              hc :- (Eq Bool (chkf (riPr ri v) c) Bool.true)]
  (V_ri chkf dec encTy ri cap (boxTy (codeTerm c)) G en 0 (skel (boxTy (codeTerm c))) (Prod.mk v Unit.unit))
  (constructor) (exact 0)
  (constructor) (exact (Nat.le_refl 0))
  (constructor) (exact hv)
  (exact (Eq.trans (den_boxev_ri chkf dec encTy ri cap G c v en) hc)))

;; A phantom ★ᵢ (i < J) lies in V₀(R): no node, and its print σ i has its
;; labels in L.
(thm ph_leaf_V [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), cap :- Nat, G :- (List Sk), en :- (HEnv G), i :- Nat, hi :- (LT.lt i J)]
  (V_ri chkf dec encTy (phRI J sg hsg) cap Exp.tR G en 0 Sk.cert (Code.sl i))
  (exact (And.intro (Nat.le_refl 0)
           (Eq.mpr (congrArg (fn [x :- Code] (Eq Bool (lblOk x) Bool.true)) (phPr_ph J sg i hi)) (hsg i hi)))))
;; ★ᵢ prints as σ i, so the check on its print is the check on σ i.
(thm ph_leaf_chk [chkf :- (=> Code Code Bool), J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), i :- Nat, hi :- (LT.lt i J), c :- Code,
                  hc :- (Eq Bool (chkf (sg i) c) Bool.true)]
  (Eq Bool (chkf (riPr (phRI J sg hsg) (Code.sl i)) c) Bool.true)
  (exact (Eq.mpr (congrArg (fn [x :- Code] (Eq Bool (chkf x c) Bool.true)) (phPr_ph J sg i hi)) hc)))
(thm lt_add_l46 [i :- Nat, r :- Nat, J :- Nat, h :- (LT.lt (+ i r) J)] (LT.lt i J) (omega))
(thm lt_add_s46 [i :- Nat, r :- Nat, J :- Nat, h :- (LT.lt (+ i (+ r 1)) J)] (LT.lt (+ (+ i 1) r) J) (omega))

;; The phantom input phIn ca r i lies in V₀(tensB ca i r), at every context and
;; environment (the tensor's later factors are read under the earlier ones'
;; binders), when σ certifies each ca j, j ≤ i + r < J.
(thm phin_V [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
             J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), ca :- (=> Nat Code),
             hc :- (forall [i Nat] (=> (LT.lt i J) (Eq Bool (chkf (sg i) (ca i)) Bool.true))), cap :- Nat]
  (forall [r Nat] (forall [i Nat] (=> (LT.lt (+ i r) J)
     (forall [G (List Sk)] (forall [en (HEnv G)]
        (V_ri chkf dec encTy (phRI J sg hsg) cap (tensB ca i r) G en 0 (skel (tensB ca i r)) (phIn ca r i)))))))
  (intro r) (induction r)
  (intro i hi G en)
  (have hi0 (LT.lt i J) (lt_add_l46 i 0 J hi))
  (exact (phbox_V chkf dec encTy (phRI J sg hsg) cap G en (ca i) (Code.sl i)
           (ph_leaf_V chkf dec encTy J sg hsg cap G en i hi0) (ph_leaf_chk chkf J sg hsg i hi0 (ca i) (hc i hi0))))
  (intro i hi G en)
  (have hi0 (LT.lt i J) (lt_add_l46 i (+ n 1) J hi))
  (have hi1 (LT.lt (+ (+ i 1) n) J) (lt_add_s46 i n J hi))
  (constructor) (exact 0)
  (constructor) (exact (Nat.le_refl 0))
  (constructor)
  (exact (phbox_V chkf dec encTy (phRI J sg hsg) cap G en (ca i) (Code.sl i)
           (ph_leaf_V chkf dec encTy J sg hsg cap G en i hi0) (ph_leaf_chk chkf J sg hsg i hi0 (ca i) (hc i hi0))))
  (exact (ih_n (+ i 1) hi1 (List.cons Sk (skel (boxTy (codeTerm (ca i)))) G) (Prod.mk (Prod.mk (Code.sl i) Unit.unit) en))))

;; --- the three cases ----------------------------------------------------------------------

;; CheckSpec's conjunction for an accepted code (as in lemma36.clj), and its
;; i-th part.
(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])
(defn- Cb [cv dv mm tt AA]
  (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec cv) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
   (list 'And (list 'Rt 'chkf (list 'thetaD mm) (list 'thetaU mm) tt AA)
   (list 'And (list 'Tl 'chkf 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
   (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
   (list 'And (list 'Eq 'Code (list 'encTy AA) dv) (list 'Nat.lt mm (list 'cnodes cv))))))))
(defn- Cex [cv dv] (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (Cb cv dv 'mm 'tt 'AA))))))))
(defn- gets [q i] (let [f (fn f [x j] (if (zero? j) (list 'And.left x) (f (list 'And.right x) (dec j))))]
                    (if (= i 5) (list 'And.right (list 'And.right (list 'And.right (list 'And.right (list 'And.right q))))) (f q i))))
(def ^:private DT '(Prod Nat (Prod Exp Exp)))

;; The hypotheses on the phantom certificates σ i, i < J: σ i certifies ca i;
;; every decoding of σ i has a term of measure above k; cb is not ca i.
(def ^:private HC '[hc :- (forall [i Nat] (=> (LT.lt i J) (Eq Bool (chkf (sg i) (ca i)) Bool.true)))])
(def ^:private HB '[hbig :- (forall [i Nat] (=> (LT.lt i J) (forall [m Nat] (forall [u Exp] (forall [A Exp]
                      (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))
                          (LT.lt k (sz u))))))))])
(def ^:private HN '[hne :- (forall [i Nat] (=> (LT.lt i J) (Eq Code cb (ca i)) False))])
;; "k ≥ μ(cb)": a certificate of cb with at most k nodes.
(def ^:private EXV '(Exists (fn [v :- Code] (And (Eq Bool (chkf v cb) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) k))))))

;; A leaf sl l that is not phantom-free is a phantom: l < J.
(thm ble_ff_lt46 [J :- Nat, l :- Nat, h :- (Eq Bool (Nat.ble J l) Bool.false)] (LT.lt l J)
  (exact (Nat.gt_of_not_le (fn [hle :- (LE.le J l)] (Bool.noConfusion (Eq.trans (Eq.symm (Nat.ble_eq_true_of_le hle)) h))))))

;; One code accepted at two codes: CheckSpec decodes it once (dec is a
;; function), and the decoded type's code is each of them, so they are equal.
(eval (list 'lcert.formal.base/thm 'acc_same
  (into P3 '[hcs :- (CheckSpec chkf dec encTy), c :- Code, d1 :- Code, d2 :- Code,
             h1 :- (Eq Bool (chkf c d1) Bool.true), h2 :- (Eq Bool (chkf c d2) Bool.true)])
  '(Eq Code d1 d2)
  (list 'have 'X1 (Cex 'c 'd1) '((And.left hcs) c d1 h1))
  '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
  (list 'have 'qa (Cb 'c 'd1 'ma 'ta 'Aa) 'hAa)
  (list 'have 'X2 (Cex 'c 'd2) '((And.left hcs) c d2 h2))
  '(refine' (exT Nat _ _ X2 _)) '(intro mb hmb) '(refine' (exT Exp _ _ hmb _)) '(intro tb htb) '(refine' (exT Exp _ _ htb _)) '(intro Ab hAb)
  (list 'have 'qb (Cb 'c 'd2 'mb 'tb 'Ab) 'hAb)
  (list 'have 'e (list 'Eq DT '(Prod.mk ma (Prod.mk ta Aa)) '(Prod.mk mb (Prod.mk tb Ab)))
        (list 'Option.some.inj (list 'Eq.trans (list 'Eq.symm (gets 'qa 0)) (gets 'qb 0))))
  (list 'have 'eA '(Eq Exp Aa Ab) (list 'congrArg (list 'fn ['p :- DT] '(Prod.snd (Prod.snd p))) 'e))
  (list 'exact (list 'Eq.trans (list 'Eq.symm (gets 'qa 4)) (list 'Eq.trans '(congrArg encTy eA) (gets 'qb 4))))))

;; Case 1: the output r″ is phantom-free.  Its print is the certificate, with
;; the same node count (L3, phRI_nodes).
(eval (list 'lcert.formal.base/thm 'out_pf46
  '[chkf :- (=> Code Code Bool), J :- Nat, sg :- (=> Nat Code), cb :- Code, k :- Nat, w :- Code,
    hpf :- (Eq Bool (phPf J w) Bool.true), hw :- (LE.le (cnodes w) k),
    hk :- (Eq Bool (chkf (phPr J sg w) cb) Bool.true), hl :- (Eq Bool (lblOk (phPr J sg w)) Bool.true)]
  EXV
  '(constructor) '(exact (phPr J sg w))
  '(exact (And.intro hk (And.intro hl (Eq.mpr (congrArg (fn [z :- Nat] (LE.le z k)) (phRI_nodes J sg w hpf)) hw))))))

;; Case 2: r″ is a phantom ★ₗ.  Its print σ l is accepted at cb and at ca l,
;; so cb = ca l (acc_same: CheckSpec's decoding, not E1) — excluded.
(eval (list 'lcert.formal.base/thm 'out_leaf46
  (into P3 (into '[hcs :- (CheckSpec chkf dec encTy), J :- Nat, sg :- (=> Nat Code), ca :- (=> Nat Code), cb :- Code]
                 (into HC (into HN '[l :- Nat,
                   hf :- (Eq Bool (phPf J (Code.sl l)) Bool.false),
                   hk :- (Eq Bool (chkf (phPr J sg (Code.sl l)) cb) Bool.true)]))))
  'False
  '(have hl (LT.lt l J) (ble_ff_lt46 J l hf))
  '(have hk2 (Eq Bool (chkf (sg l) cb) Bool.true) (Eq.mp (congrArg (fn [x :- Code] (Eq Bool (chkf x cb) Bool.true)) (phPr_ph J sg l hl)) hk))
  '(exact (hne l hl (acc_same chkf dec encTy hcs (sg l) cb (ca l) hk2 (hc l hl))))))

;; An accepted code, for Enc46's premise on the plugged trees.
(thm ex_acc46 [chkf :- (=> Code Code Bool), c :- Code, d :- Code, h :- (Eq Bool (chkf c d) Bool.true)]
  (Exists (fn [D :- Code] (Eq Bool (chkf c D) Bool.true)))
  (constructor) (exact d) (exact h))
(thm le_lt_absurd46 [a :- Nat, b :- Nat, k :- Nat, h1 :- (LE.le a b), h2 :- (LE.le b k), h3 :- (LT.lt k a)] False (omega))

;; Case 3: r″ = sn l a b is a node with a phantom inside.  Its print is
;; hfill σ K for the holed tree K = toH J r″ (toH_fill), a node with a hole
;; whose holes are phantoms below J (toH_node, toH_hasHole, toH_hole_lt).
;; Enc46 gives a hole i with every decoding's term measure at most
;; hnodes K = ‖r″‖ ≤ k (toH_nodes); σ i decodes (CheckSpec), and its term's
;; measure exceeds k — a contradiction.
(eval (list 'lcert.formal.base/thm 'out_node46
  (into P3 (into '[hcs :- (CheckSpec chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz),
                   J :- Nat, sg :- (=> Nat Code), ca :- (=> Nat Code), cb :- Code, k :- Nat]
                 (into HC (into HB '[l :- Nat, a :- Code, b :- Code,
                   hf :- (Eq Bool (phPf J (Code.sn l a b)) Bool.false),
                   hw :- (LE.le (cnodes (Code.sn l a b)) k),
                   hk :- (Eq Bool (chkf (phPr J sg (Code.sn l a b)) cb) Bool.true)]))))
  'False
  '(have hfill (Eq Bool (chkf (hfill sg (toH J (Code.sn l a b))) cb) Bool.true)
     (Eq.mpr (congrArg (fn [x :- Code] (Eq Bool (chkf x cb) Bool.true)) (toH_fill J sg (Code.sn l a b))) hk))
  '(have hX (Exists (fn [i :- Nat]
             (And (Eq Bool (holeIn i (toH J (Code.sn l a b))) Bool.true)
                  (forall [m Nat] (forall [t Exp] (forall [A Exp]
                    (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
                        (LE.le (sz t) (hnodes (toH J (Code.sn l a b)))))))))))
     (henc sg (toH J (Code.sn l a b)) cb hfill (toH_node J l a b) (toH_hasHole J (Code.sn l a b) hf)
       (fn [i :- Nat, hi :- (Eq Bool (holeIn i (toH J (Code.sn l a b))) Bool.true)]
         (ex_acc46 chkf (sg i) (ca i) (hc i (toH_hole_lt J i (Code.sn l a b) hi))))))
  '(refine' (exT Nat _ _ hX _)) '(intro i hi)
  '(have hiJ (LT.lt i J) (toH_hole_lt J i (Code.sn l a b) (And.left hi)))
  (list 'have 'X1 (Cex '(sg i) '(ca i)) '((And.left hcs) (sg i) (ca i) (hc i hiJ)))
  '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
  (list 'have 'q (Cb '(sg i) '(ca i) 'ma 'ta 'Aa) 'hAa)
  (list 'have 'hs '(LE.le (sz ta) (hnodes (toH J (Code.sn l a b)))) (list '(And.right hi) 'ma 'ta 'Aa (gets 'q 0)))
  '(have hn (LE.le (hnodes (toH J (Code.sn l a b))) k) (Eq.mpr (congrArg (fn [z :- Nat] (LE.le z k)) (toH_nodes J (Code.sn l a b))) hw))
  (list 'exact (list 'le_lt_absurd46 '(sz ta) '(hnodes (toH J (Code.sn l a b))) 'k 'hs 'hn (list 'hbig 'i 'hiJ 'ma 'ta 'Aa (gets 'q 0))))))

(def ^:private PO (into P3 '[hcs :- (CheckSpec chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz),
                             J :- Nat, sg :- (=> Nat Code), ca :- (=> Nat Code), cb :- Code, k :- Nat]))
;; Cases 2 and 3, by the shape of r″.
(eval (list 'lcert.formal.base/thm 'out_ph46 (into PO (into HC (into HB HN)))
  '(forall [w Code] (=> (Eq Bool (phPf J w) Bool.false) (LE.le (cnodes w) k) (Eq Bool (chkf (phPr J sg w) cb) Bool.true) False))
  '(intro w) '(cases w)
  '(intro hf hw hk) '(exact (out_leaf46 chkf dec encTy hcs J sg ca cb hc hne l hf hk))
  '(intro hf hw hk) '(exact (out_node46 chkf dec encTy hcs sz henc J sg ca cb k hc hbig l a b hf hw hk))))
;; The three cases: an output r″ of at most k nodes whose print is accepted at
;; cb (with labels in L) yields a certificate of cb with at most k nodes.
(eval (list 'lcert.formal.base/thm 'thm46_out (into PO (into HC (into HB (into HN '[w :- Code, hw :- (LE.le (cnodes w) k),
             hk :- (Eq Bool (chkf (phPr J sg w) cb) Bool.true), hl :- (Eq Bool (lblOk (phPr J sg w)) Bool.true)]))))
  EXV
  (list 'exact (list 'bool_case (list 'fn '[bb :- Bool] EXV) '(phPf J w)
     '(fn [hpf :- (Eq Bool (phPf J w) Bool.true)] (out_pf46 chkf J sg cb k w hpf hw hk hl))
     '(fn [hpf :- (Eq Bool (phPf J w) Bool.false)] (False.elim (out_ph46 chkf dec encTy hcs sz henc J sg ca cb k hc hbig hne w hpf hw hk)))))))

;; --- Theorem 4.6 --------------------------------------------------------------------------

(defn- J! [form] (clojure.walk/postwalk-replace {'J '(+ r 1)} form))
(def ^:private RI '(phRI (+ r 1) sg hsg))
(def ^:private TT '(tensB ca 0 r))
(def ^:private TY (list 'Exp.tPi 'U.u1 TT '(boxTy (codeTerm cb))))
(def ^:private G0 '(skels (thetaD k)))
(def ^:private G1 (list 'List.cons 'Sk (list 'skel TT) G0))
(def ^:private E1 '(Prod.mk (phIn ca r 0) (tokEnvD k)))
;; ⟦t⟧*(z), z the phantom input
(def ^:private FA (list (list 'den_ri 'chkf 'dec 'encTy RI 'k 't G0 (list 'skel TY) '(tokEnvD k)) '(phIn ca r 0)))
(thm lt_r46 [r :- Nat] (LT.lt (+ 0 r) (+ r 1)) (omega))
(thm le_k046 [w :- Nat, j :- Nat, k :- Nat, h1 :- (Nat.le w j), h2 :- (Nat.le j (+ k 0))] (LE.le w k) (omega))

;; Theorem 4.6, over codes.  Θₖ ⊢ t :¹ □(ca 0) ⊗ ⋯ ⊗ □(ca r) ⊸ □cb, with
;; certificates σ i of the ca i whose terms are larger than k, cb ≠ ca i,
;; CheckSpec and Enc46: some certificate of cb has at most k nodes.
;; Lemma 4.6a at phRI (r+1) σ, budget k, all-token environment (hsnd, hv);
;; the phantom input is in V₀ (ha); the output (r″, ⋆) lies in Vₖ(□cb) (ho,
;; ho2): ‖r″‖ ≤ j ≤ k, labels of its print in L, and its print accepted at
;; cb (hk); then the three cases (thm46_out).
(eval (list 'lcert.formal.base/thm 'thm46
  (J! (into P3 (into '[hcs :- (CheckSpec chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz),
                       r :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), ca :- (=> Nat Code), cb :- Code, k :- Nat, t :- Exp]
                     (into HC (into HB (into HN ['der :- (list 'Rt 'chkf '(thetaD k) '(thetaU k) 't TY)]))))))
  EXV
  (list 'have 'hsnd (list 'Sound_ri 'chkf 'dec 'encTy RI 'k '(thetaD k) '(thetaU k) 't TY)
        (list 'lemma46a 'chkf 'dec 'encTy 'hcs '(+ r 1) 'sg 'hsg 'k '(thetaD k) '(thetaU k) 't TY 'der '(wf_theta chkf k)))
  (list 'have 'hv (list 'V_ri 'chkf 'dec 'encTy RI 'k TY G0 '(tokEnvD k) 'k (list 'skel TY) (list 'den_ri 'chkf 'dec 'encTy RI 'k 't G0 (list 'skel TY) '(tokEnvD k)))
        (list 'hsnd '(tokEnvD k) 'k '(Nat.le_refl k) (list 'tok_sat_ri 'chkf 'dec 'encTy RI 'k 'k)))
  (list 'have 'ha (list 'V_ri 'chkf 'dec 'encTy RI 'k TT G0 '(tokEnvD k) 0 (list 'skel TT) '(phIn ca r 0))
        (list 'phin_V 'chkf 'dec 'encTy '(+ r 1) 'sg 'hsg 'ca 'hc 'k 'r 0 '(lt_r46 r) G0 '(tokEnvD k)))
  (list 'have 'ho (list 'V_ri 'chkf 'dec 'encTy RI 'k '(boxTy (codeTerm cb)) G1 E1 '(+ k 0) '(skel (boxTy (codeTerm cb))) FA)
        '(hv 0 (Nat.le_refl k) (phIn ca r 0) ha))
  (list 'have 'ho2 (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j (+ k 0))
                      (list 'And (list 'V_ri 'chkf 'dec 'encTy RI 'k 'Exp.tR G1 E1 'j 'Sk.cert (list 'Prod.fst FA))
                                 (list 'V_ri 'chkf 'dec 'encTy RI 'k '(Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cb)))
                                       (list 'List.cons 'Sk 'Sk.cert G1) (list 'Prod.mk (list 'Prod.fst FA) E1) '(- (+ k 0) j) 'Sk.unit (list 'Prod.snd FA))))))
        'ho)
  '(refine' (exT Nat _ _ ho2 _)) '(intro j hj)
  (list 'have 'hk (list 'Eq 'Bool (list 'chkf (list 'riPr RI (list 'Prod.fst FA)) 'cb) 'Bool.true)
        (list 'Eq.trans (list 'Eq.symm (list 'den_boxev_ri 'chkf 'dec 'encTy RI 'k G1 'cb (list 'Prod.fst FA) E1)) '(And.right (And.right hj))))
  (list 'have 'hw (list 'LE.le (list 'cnodes (list 'Prod.fst FA)) 'k)
        (list 'le_k046 (list 'cnodes (list 'Prod.fst FA)) 'j 'k '(And.left (And.left (And.right hj))) '(And.left hj)))
  (list 'exact (list 'thm46_out 'chkf 'dec 'encTy 'hcs 'sz 'henc '(+ r 1) 'sg 'ca 'cb 'k 'hc 'hbig 'hne (list 'Prod.fst FA) 'hw 'hk
                     '(And.right (And.left (And.right hj)))))))

;; Theorem 4.6 as the paper states it: closed types A₀ … A_r and B, B ≠ Aᵢ.
;; E1 (CheckSpec's last clause) turns B ≠ Aᵢ into ⌜B⌝ ≠ ⌜Aᵢ⌝ — the only use
;; of E1.
(eval (list 'lcert.formal.base/thm 'thm46_types
  (into P3 (into '[hcs :- (CheckSpec chkf dec encTy), sz :- (=> Exp Nat), henc :- (Enc46 chkf dec sz),
                   r :- Nat, sg :- (=> Nat Code), hsg :- (PhOk (+ r 1) sg), As :- (=> Nat Exp), B :- Exp, k :- Nat, t :- Exp,
                   hclA :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Bool (closedTy (As i)) Bool.true))),
                   hclB :- (Eq Bool (closedTy B) Bool.true),
                   hc :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Bool (chkf (sg i) (encTy (As i))) Bool.true))),
                   hbig :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (forall [m Nat] (forall [u Exp] (forall [A Exp]
                             (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A))))
                                 (LT.lt k (sz u))))))))
                   hne :- (forall [i Nat] (=> (LT.lt i (+ r 1)) (Eq Exp B (As i)) False)),
                   der :- (Rt chkf (thetaD k) (thetaU k) t
                              (Exp.tPi U.u1 (tensB (fn [i :- Nat] (encTy (As i))) 0 r) (boxTy (codeTerm (encTy B)))))]))
  '(Exists (fn [v :- Code] (And (Eq Bool (chkf v (encTy B)) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) k)))))
  '(exact (thm46 chkf dec encTy hcs sz henc r sg hsg (fn [i :- Nat] (encTy (As i))) (encTy B) k t hc hbig
            (fn [i :- Nat, hi :- (LT.lt i (+ r 1)), e :- (Eq Code (encTy B) (encTy (As i)))]
              (hne i hi ((And.right (And.right (And.right hcs))) B (As i) hclB (hclA i hi) e)))
            der))))

;; --- why B must differ from each Aᵢ ---------------------------------------------------------

;; At B = A₁ the term that returns its input costs nothing (§4.5, review
;; R4-07): λz. z : □c ⊸ □c at Θ₀ (id_box46), while no accepted code has
;; fewer than one node (cert_pos46: CheckSpec decodes it at a budget below
;; its node count).  So thm46 fails without hne: thm46_ne_needed is the
;; derivation at r = 0, ca 0 = cb = c, k = 0, together with the falsity of
;; thm46's conclusion there.
(thm lift_box_l46 [c :- Code] (Eq Exp (lift 1 0 (boxTy (codeTerm c))) (boxTy (lift 1 1 (codeTerm c)))) (rfl))
(thm lift_box46 [c :- Code] (Eq Exp (lift 1 0 (boxTy (codeTerm c))) (boxTy (codeTerm c)))
  (exact (Eq.trans (lift_box_l46 c) (congrArg boxTy (lift_code c 1 1)))))
(thm id_box46 [chkf :- (=> Code Code Bool), c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)]
  (Rt chkf (thetaD 0) (thetaU 0) (Exp.lam U.u1 (boxTy (codeTerm c)) (Exp.var 0)) (Exp.tPi U.u1 (boxTy (codeTerm c)) (boxTy (codeTerm c))))
  (exact (Rt.rLam chkf (thetaD 0) (thetaU 0) U.u1 (boxTy (codeTerm c)) (Exp.var 0) (boxTy (codeTerm c))
           (tl_boxTy chkf (thetaD 0) c hc)
           (rt_cast chkf (List.cons Exp (boxTy (codeTerm c)) (thetaD 0)) (List.cons U U.u1 (thetaU 0)) (List.cons U U.u1 (thetaU 0))
              (Exp.var 0) (lift 1 0 (boxTy (codeTerm c))) (boxTy (codeTerm c))
              (Rt.rVar chkf (List.cons Exp (boxTy (codeTerm c)) (thetaD 0)) (List.cons U U.u1 (thetaU 0)) 0 (boxTy (codeTerm c)) U.u1
                 rfl (nthE.eq_2 (boxTy (codeTerm c)) (thetaD 0)) (nthU.eq_2 U.u1 (thetaU 0)) nonzero_u1)
              rfl (lift_box46 c)))))
(eval (list 'lcert.formal.base/thm 'cert_pos46 (into P3 '[hcs :- (CheckSpec chkf dec encTy), v :- Code, c :- Code, h :- (Eq Bool (chkf v c) Bool.true)])
  '(LT.lt 0 (cnodes v))
  (list 'have 'X1 (Cex 'v 'c) '((And.left hcs) v c h))
  '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
  (list 'have 'q (Cb 'v 'c 'ma 'ta 'Aa) 'hAa)
  (list 'have 'hl (list 'LT.lt 'ma '(cnodes v)) (gets 'q 5))
  '(omega)))
(eval (list 'lcert.formal.base/thm 'thm46_ne_needed (into P3 '[hcs :- (CheckSpec chkf dec encTy), c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)])
  '(And (Rt chkf (thetaD 0) (thetaU 0) (Exp.lam U.u1 (boxTy (codeTerm c)) (Exp.var 0)) (Exp.tPi U.u1 (tensB (fn [i :- Nat] c) 0 0) (boxTy (codeTerm c))))
        (=> (Exists (fn [v :- Code] (And (Eq Bool (chkf v c) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) 0))))) False))
  '(constructor)
  '(exact (id_box46 chkf c hc))
  '(intro hx)
  '(refine' (exT Code _ _ hx _)) '(intro v hv)
  '(have h0 (LT.lt 0 (cnodes v)) (cert_pos46 chkf dec encTy hcs v c (And.left hv)))
  '(have h1 (LE.le (cnodes v) 0) (And.right (And.right hv)))
  '(omega)))
