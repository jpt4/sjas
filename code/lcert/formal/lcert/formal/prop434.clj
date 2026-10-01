(ns lcert.formal.prop434
  "F4 — Propositions 4.3 (quotation cost) and 4.4 (1) (no uniform budget),
  R4-metatheory.md §4.3–4.4.

  - prop43: for closed A, every certificate w of □A has ‖w‖ > 2μ(A) — stated
    with any lower bound M of μ(A) (M ≤ ‖v‖ for every accepted v).
  - prop44_1: for every k, no q with Θₖ, x :₁ R ⊢ q :¹ R maps every
    certificate of every closed A to a certificate of □A (stated for a
    minimal certificate of ¬ᵏ⁺¹1, whose existence the paper presumes).
  Both assume CheckSpec and EncSize (E2–E4's size consequence); 4.4 (2), (3)
  are open."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.unfold :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.outer :refer :all]
            [lcert.formal.lemma36 :refer :all]
            [lcert.formal.strengthen]
            [lcert.formal.conversion]
            [lcert.formal.convcase]))

;; --- E2–E4, as the size of an accepted certificate ------------------------------------------

;; EncSize: an accepted certificate of Θₘ ⊢ t : A has at least 1 + m + f + ‖⌜A⌝‖
;; internal nodes, f the tokens free in t: its root is a derivation node (E2),
;; its judgment holds the context (m entries), the term (a node per variable
;; occurrence) and the type as disjoint subtrees (E3, E4).  Part of the trust
;; base (ADR-0006 §2: the encoding properties), a hypothesis where used.

;; E2–E4's consequence for an accepted certificate (part of the trust base, ADR-0006 §2)
(kdef EncSize (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (=> (=> Exp Code) Prop)))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
    (forall [c Code] (forall [d Code] (forall [m Nat] (forall [t Exp] (forall [A Exp]
      (=> (Eq Bool (chkf c d) Bool.true) (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
          (LE.le (+ 1 (+ m (+ (cntU (maskUF (thetaU m) (freshF t))) (cnodes (encTy A))))) (cnodes c))))))))))
(thm ucnt_sel [bb :- Bool] (LE.le (ucnt (Bool.rec$1 (fn [_ :- Bool] U) U.u1 U.u0 bb)) 1) (cases bb) (exact (Nat.le_refl 1)) (exact (Nat.zero_le 1)))
(thm add_le_succ [a :- Nat, b :- Nat, m :- Nat, ha :- (LE.le a 1), hb :- (LE.le b m)] (LE.le (+ a b) (+ m 1)) (omega))
(thm cnt_mask_le [m :- Nat] (forall [g (=> Nat Bool)] (LE.le (cntU (maskUF (thetaU m) g)) m))
  (induction m) (intro g) (exact (Nat.le_refl 0))
  (intro g) (exact (add_le_succ _ _ n (ucnt_sel (g 0)) (ih_n (fn [j :- Nat] (g (+ j 1)))))))

(thm den_var0c [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                  G :- (List Sk), en :- (HEnv G), v :- Code]
  (Eq Code (den chkf dec encTy n (Exp.var 0) (List.cons Sk Sk.cert G) Sk.cert (Prod.mk v en)) v)
  (rw [(den_var_at chkf dec encTy n 0 (List.cons Sk Sk.cert G) Sk.cert (Prod.mk v en))]))
(defn- Cb [cv dv mm tt AA]
  (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec cv) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
   (list 'And (list 'Rt 'chkf (list 'thetaD mm) (list 'thetaU mm) tt AA)
   (list 'And (list 'Tl 'chkf 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
   (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
   (list 'And (list 'Eq 'Code (list 'encTy AA) dv) (list 'Nat.lt mm (list 'cnodes cv))))))))
(defn- Cex [cv dv] (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (Cb cv dv 'mm 'tt 'AA))))))))
(defn- gets [q i] (let [f (fn f [x j] (if (zero? j) (list 'And.left x) (f (list 'And.right x) (dec j))))]
                    (if (= i 5) (list 'And.right (list 'And.right (list 'And.right (list 'And.right (list 'And.right q))))) (f q i))))
(def ^:private box '(Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA)))
(def ^:private ftok '(cntU (maskUF (thetaU mm) (freshF tt))))
(def ^:private Gm '(skels (thetaD mm)))
(def ^:private em '(tokEnvD mm))
(def ^:private Pv (list 'den 'chkf 'dec 'encTy 'mm 'tt Gm '(Sk.prod Sk.cert Sk.unit) em))
(thm p43_arith [M :- Nat, c :- Nat, j :- Nat, f :- Nat, m :- Nat, s :- Nat, w :- Nat,
                  h1 :- (LE.le M c), h2 :- (LE.le c j), h3 :- (LE.le j f), h4 :- (LE.le f m), h5 :- (LE.le (+ 1 (+ m (+ f s))) w)]
  (LT.lt (+ M M) w) (omega))
(eval (list 'lcert.formal.base/thm 'prop43
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
    hcs :- (CheckSpec chkf dec encTy), hes :- (EncSize chkf dec encTy),
    A :- Exp, cA :- Exp, hcA :- (Eq (Option Code) (codeOf cA) (Option.some Code (encTy A))),
    hbc :- (Eq Bool (closedTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA))) Bool.true),
    w :- Code, hw :- (Eq Bool (chkf w (encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA)))) Bool.true),
    M :- Nat, hM :- (forall [v Code] (=> (Eq Bool (chkf v (encTy A)) Bool.true) (LE.le M (cnodes v))))]
  '(LT.lt (+ M M) (cnodes w))
  (list 'have 'X1 (Cex 'w (list 'encTy box)) (list '(And.left hcs) 'w (list 'encTy box) 'hw))
  '(refine' (exT Nat _ _ X1 _)) '(intro mm hmm) '(refine' (exT Exp _ _ hmm _)) '(intro tt htt) '(refine' (exT Exp _ _ htt _)) '(intro AA hAA)
  (list 'have 'q (Cb 'w (list 'encTy box) 'mm 'tt 'AA) 'hAA)
  (list 'have 'eB (list 'Eq 'Exp 'AA box) (list '(And.right (And.right (And.right hcs))) 'AA box (gets 'q 3) 'hbc (gets 'q 4)))
  (list 'have 'hRt (list 'Rt 'chkf '(thetaD mm) '(thetaU mm) 'tt box)
        (list 'Eq.mp (list 'congrArg '(fn [Z :- Exp] (Rt chkf (thetaD mm) (thetaU mm) tt Z)) 'eB) (gets 'q 1)))
  (list 'have 'hsz (list 'LE.le (list '+ 1 (list '+ 'mm (list '+ ftok '(cnodes (encTy AA))))) '(cnodes w))
        (list 'hes 'w (list 'encTy box) 'mm 'tt 'AA 'hw (gets 'q 0)))
  (list 'have 'hfle (list 'LE.le ftok 'mm) '(cnt_mask_le mm (freshF tt)))
  (list 'have 'hsnd (list 'Sound 'chkf 'dec 'encTy 'mm '(thetaD mm) '(maskUF (thetaU mm) (freshF tt)) 'tt box)
        (list 'lemma36 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) 'mm '(thetaD mm) '(maskUF (thetaU mm) (freshF tt)) 'tt box
              (list 'rt_mask 'chkf '(thetaD mm) '(thetaU mm) 'tt box 'hRt '(freshF tt) '(fn [j :- Nat, h :- (Eq Bool ((freshF tt) j) Bool.true)] h))
              '(wf_theta chkf mm)))
  (list 'have 'hv (list 'Exists (list 'fn '[j :- Nat] (list 'And (list 'Nat.le 'j ftok)
                     (list 'And (list 'V 'chkf 'dec 'encTy 'mm 'Exp.tR Gm em 'j 'Sk.cert (list 'Prod.fst Pv))
                                (list 'V 'chkf 'dec 'encTy 'mm '(chkT (Exp.var 0) cA) (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em)
                                      (list '- ftok 'j) 'Sk.unit (list 'Prod.snd Pv))))))
        (list 'hsnd em ftok 'hfle '(tok_sat_mask chkf dec encTy mm mm (freshF tt))))
  '(refine' (exN _ _ hv _)) '(intro j hj)
  (list 'have 'hch (list 'Eq 'Bool (list 'chkf (list 'den 'chkf 'dec 'encTy 'mm '(Exp.var 0) (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.cert (list 'Prod.mk (list 'Prod.fst Pv) em))
                                       (list 'den 'chkf 'dec 'encTy 'mm 'cA (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.syn (list 'Prod.mk (list 'Prod.fst Pv) em))) 'Bool.true)
        (list 'chk_true 'chkf 'dec 'encTy 'mm (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em) (list '- ftok 'j) '(Exp.var 0) 'cA (list 'Prod.snd Pv) '(And.right (And.right hj))))
  (list 'have 'hacc (list 'Eq 'Bool (list 'chkf (list 'Prod.fst Pv) '(encTy A)) 'Bool.true)
        (list 'Eq.trans (list 'Eq.symm (list 'Eq.trans
              (list 'congrArg (list 'fn '[x :- Code] (list 'chkf 'x (list 'den 'chkf 'dec 'encTy 'mm 'cA (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.syn (list 'Prod.mk (list 'Prod.fst Pv) em))))
                    (list 'den_var0c 'chkf 'dec 'encTy 'mm Gm em (list 'Prod.fst Pv)))
              (list 'congrArg (list 'fn '[y :- Code] (list 'chkf (list 'Prod.fst Pv) 'y))
                    (list 'den_codeOf 'chkf 'dec 'encTy 'mm (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em) 'cA '(encTy A) 'hcA))))
              'hch))
  (list 'exact (list 'p43_arith 'M (list 'cnodes (list 'Prod.fst Pv)) 'j ftok 'mm '(cnodes (encTy AA)) '(cnodes w)
                     (list 'hM (list 'Prod.fst Pv) 'hacc) '(And.left (And.left (And.right hj))) '(And.left hj) 'hfle 'hsz))))


;; --- Proposition 4.4 (1): no uniform budget -----------------------------------------------

;; cTerm c: the canonical code term of c (codeOf (cTerm c) = c); ¬ʲ1 (negN j),
;; whose code has at least j nodes by E5 — the paper's A_j is 1 ⊸ ⋯ ⊸ 1, any
;; closed type with a large code serves; every certificate of A exceeds ⌜A⌝
;; (cert_size).

(kdef cTerm (=> Code Exp)
  (fn [c :- Code] (Code.rec$1 (fn [_ :- Code] Exp) (fn [l :- Nat] (Exp.sleaf (Exp.lbl l)))
     (fn [l :- Nat, a :- Code, b :- Code, ta :- Exp, tb :- Exp] (Exp.snode (Exp.lbl l) ta tb)) c)))

(kdef snF (=> Nat (Option Code) (Option Code) (Option Code))
  (fn [l :- Nat, o1 :- (Option Code), o2 :- (Option Code)]
    (Option.rec$1$0 Code (fn [_ :- (Option Code)] (Option Code)) (Option.none Code)
      (fn [a :- Code] (Option.rec$1$0 Code (fn [_ :- (Option Code)] (Option Code)) (Option.none Code) (fn [b :- Code] (Option.some Code (Code.sn l a b))) o2)) o1)))
(thm codeOf_sn_eq [l :- Nat, c1 :- Exp, c2 :- Exp] (Eq (Option Code) (codeOf (Exp.snode (Exp.lbl l) c1 c2)) (snF l (codeOf c1) (codeOf c2))) (rfl))
(thm cterm_step [l :- Nat, a :- Code, b :- Code, ha :- (Eq (Option Code) (codeOf (cTerm a)) (Option.some Code a)), hb :- (Eq (Option Code) (codeOf (cTerm b)) (Option.some Code b))]
  (Eq (Option Code) (codeOf (Exp.snode (Exp.lbl l) (cTerm a) (cTerm b))) (Option.some Code (Code.sn l a b)))
  (rw [(codeOf_sn_eq l (cTerm a) (cTerm b)) ha hb]))
(thm cterm_code [c :- Code] (Eq (Option Code) (codeOf (cTerm c)) (Option.some Code c))
  (induction c) (rfl) (exact (cterm_step l a b ih_a ih_b)))

(thm cterm_closed [c :- Code] (forall [n Nat] (Eq Bool ((closedF (cTerm c)) n) Bool.true))
  (induction c) (intro n) (rfl)
  (intro n)
  (change (Eq Bool (Bool.and Bool.true (Bool.and ((closedF (cTerm a)) n) ((closedF (cTerm b)) n))) Bool.true))
  (rw [(ih_a n) (ih_b n)]))
(thm box_closed [c :- Code] (Eq Bool (closedTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm c)))) Bool.true)
  (change (Eq Bool (Bool.and Bool.true (Bool.and (Nat.blt 0 1) ((closedF (cTerm c)) 1))) Bool.true))
  (rw [(cterm_closed c 1)]))
(kdef negN (=> Nat Exp) (fn [j :- Nat] (Nat.rec$1 (fn [_ :- Nat] Exp) Exp.tUnit (fn [i :- Nat, A :- Exp] (Exp.tPi U.u1 A Exp.tEmpty)) j)))
(thm negN_closed [j :- Nat] (Eq Bool (closedTy (negN j)) Bool.true)
  (induction j) (rfl)
  (change (Eq Bool (Bool.and ((closedF (negN n)) 0) Bool.true) Bool.true))
  (rw [ih_n]))
(thm arith3 [a :- Nat, n :- Nat, h :- (LE.le n a)] (LE.le (+ n 1) (+ 1 (+ a 0))) (omega))
(thm negN_size [encTy :- (=> Exp Code),
                  e5 :- (forall [A Exp] (=> (Eq Bool (closedTy A) Bool.true) (Eq Code (encTy (Exp.tPi U.u1 A Exp.tEmpty)) (Code.sn 25 (encTy A) (Code.sl 15)))))]
  (forall [j Nat] (LE.le j (cnodes (encTy (negN j)))))
  (intro j) (induction j) (exact (Nat.zero_le _))
  (rw [(e5 (negN n) (negN_closed n))])
  (exact (arith3 (cnodes (encTy (negN n))) n ih_n)))

(thm arith4 [m :- Nat, f :- Nat, s :- Nat, c :- Nat, h :- (LE.le (+ 1 (+ m (+ f s))) c)] (LT.lt s c) (omega))
(eval (list 'lcert.formal.base/thm 'cert_size
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
    hcs :- (CheckSpec chkf dec encTy), hes :- (EncSize chkf dec encTy), A :- Exp, v :- Code, hv :- (Eq Bool (chkf v (encTy A)) Bool.true)]
  '(LT.lt (cnodes (encTy A)) (cnodes v))
  (list 'have 'X1 (Cex 'v '(encTy A)) '((And.left hcs) v (encTy A) hv))
  '(refine' (exT Nat _ _ X1 _)) '(intro mm hmm) '(refine' (exT Exp _ _ hmm _)) '(intro tt htt) '(refine' (exT Exp _ _ htt _)) '(intro AA hAA)
  (list 'have 'q (Cb 'v '(encTy A) 'mm 'tt 'AA) 'hAA)
  (list 'have 'hsz (list 'LE.le (list '+ 1 (list '+ 'mm (list '+ ftok '(cnodes (encTy AA))))) '(cnodes v)) (list 'hes 'v '(encTy A) 'mm 'tt 'AA 'hv (gets 'q 0)))
  (list 'exact (list 'arith4 'mm ftok '(cnodes (encTy A)) '(cnodes v)
                     (list 'Eq.mp (list 'congrArg (list 'fn '[z :- Code] (list 'LE.le (list '+ 1 (list '+ 'mm (list '+ ftok '(cnodes z)))) '(cnodes v))) (gets 'q 4)) 'hsz)))))
(thm arith5 [cv :- Nat, cw :- Nat, k :- Nat, s :- Nat, h1 :- (LT.lt (+ cv cv) cw), h2 :- (LE.le cw (+ cv k)), h3 :- (LE.le (+ k 1) s), h4 :- (LT.lt s cv)] False (omega))
(thm arith6 [a :- Nat, b :- Nat] (LE.le (+ a b) (+ b a)) (omega))

;; Proposition 4.4 (1): no q with Θₖ, x :₁ R ⊢ q :¹ R maps every certificate of
;; every closed A to a certificate of □A — at ¬ᵏ⁺¹1 and a minimal certificate v
;; of it, the output has at most ‖v‖ + k nodes (Theorem 3) but needs more than
;; 2‖v‖ (Proposition 4.3), while ‖v‖ > k + 1.

(def ^:private A44 '(negN (+ k 1)))
(def ^:private Gk '(List.cons Sk Sk.cert (skels (thetaD k))))
(def ^:private env44 '(Prod.mk v (tokEnvD k)))
(defn- w44 [n] (list 'den 'chkf 'dec 'encTy n 'q Gk 'Sk.cert env44))
(def ^:private boxA (list 'Exp.tSig 'U.u1 'Exp.tR (list 'chkT '(Exp.var 0) (list 'cTerm (list 'encTy A44)))))
(eval (list 'lcert.formal.base/thm 'prop44_1
  ['chkf :- '(=> Code Code Bool), 'dec :- '(=> Code (Option (Prod Nat (Prod Exp Exp)))), 'encTy :- '(=> Exp Code),
   'hcs :- '(CheckSpec chkf dec encTy), 'hes :- '(EncSize chkf dec encTy), 'k :- 'Nat, 'q :- 'Exp,
   'der :- '(Rt chkf (List.cons Exp Exp.tR (thetaD k)) (List.cons U U.u1 (thetaU k)) q Exp.tR),
   'hmap :- (list 'forall '[n Nat] (list 'forall '[A Exp] (list 'forall '[v Code]
              (list '=> '(Eq Bool (closedTy A) Bool.true) '(Eq Bool (chkf v (encTy A)) Bool.true) '(Eq Bool (lblOk v) Bool.true)
                    (list 'Eq 'Bool (list 'chkf (w44 'n) '(encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy A)))))) 'Bool.true)))))
   'v :- 'Code, 'hv :- (list 'Eq 'Bool (list 'chkf 'v (list 'encTy A44)) 'Bool.true), 'hvok :- '(Eq Bool (lblOk v) Bool.true),
   'hmin :- (list 'forall '[v2 Code] (list '=> (list 'Eq 'Bool (list 'chkf 'v2 (list 'encTy A44)) 'Bool.true) '(LE.le (cnodes v) (cnodes v2))))]
  'False
  (list 'have 'hw (list 'Eq 'Bool (list 'chkf (w44 '(+ k (cnodes v))) (list 'encTy boxA)) 'Bool.true)
        (list 'hmap '(+ k (cnodes v)) A44 'v (list 'negN_closed '(+ k 1)) 'hv 'hvok))
  (list 'have 'h43 (list 'LT.lt '(+ (cnodes v) (cnodes v)) (list 'cnodes (w44 '(+ k (cnodes v)))))
        (list 'prop43 'chkf 'dec 'encTy 'hcs 'hes A44 (list 'cTerm (list 'encTy A44)) (list 'cterm_code (list 'encTy A44))
              (list 'box_closed (list 'encTy A44)) (w44 '(+ k (cnodes v))) 'hw '(cnodes v) 'hmin))
  (list 'have 'hsat (list 'EnvSat 'chkf 'dec 'encTy '(+ k (cnodes v)) '(List.cons Exp Exp.tR (thetaD k)) '(List.cons U U.u1 (thetaU k)) env44 '(+ (cnodes v) k))
        '(EnvSat_cons chkf dec encTy (+ k (cnodes v)) Exp.tR (thetaD k) U.u1 (thetaU k) v (tokEnvD k) (cnodes v) k
           (tok_sat chkf dec encTy (+ k (cnodes v)) k) (And.intro (Nat.le_refl (cnodes v)) hvok)))
  (list 'have 'h3 (list 'LE.le (list 'cnodes (w44 '(+ k (cnodes v)))) '(+ (cnodes v) k))
        (list 'theorem3 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) '(+ k (cnodes v)) '(List.cons Exp Exp.tR (thetaD k)) '(List.cons U U.u1 (thetaU k)) 'q 'der
              '(And.intro (Tl.fBase chkf (thetaD k) Exp.tR (Eq.refl$1 Bool.true)) (wf_theta chkf k)) env44 '(+ (cnodes v) k) '(arith6 (cnodes v) k) 'hsat))
  (list 'exact (list 'arith5 '(cnodes v) (list 'cnodes (w44 '(+ k (cnodes v)))) 'k (list 'cnodes (list 'encTy A44)) 'h43 'h3
                     (list 'negN_size 'encTy '(And.left (And.right (And.right hcs))) '(+ k 1))
                     (list 'cert_size 'chkf 'dec 'encTy 'hcs 'hes A44 'v 'hv)))))
