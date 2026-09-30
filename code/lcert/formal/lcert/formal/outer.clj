(ns lcert.formal.outer
  "F3m — the cases of the fundamental lemma (R4-metatheory.md Lemma 3.6) that
  use the outer induction on the budget: Reflect and H₁ (§3.5, and §3.6: why
  the induction is not circular).

  Both read a certificate through the checker's specification CheckSpec
  (model.clj): an accepted code decodes to a derivation Θₘ ⊢ t :¹ A at a
  budget m below its node count, hence below the footprint that pays for it,
  hence below n.  The outer hypothesis OuterIH n is Lemma 3.6 at every m < n
  for the all-token context Θₘ and environment; the assembly of Lemma 3.6
  (strong induction on n) supplies it.

  - F_refl: ⟦reflect_X r e⟧ ∈ Vⁿₖ(X).
  - F_h1: the premises of H₁ are unsatisfiable (their composition, Lemma 2.4,
    would put a value in V(0) = ∅ at a smaller budget)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
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
            [lcert.formal.mono]
            [lcert.formal.skeletons]
            [lcert.formal.subst]
            [lcert.formal.substitution]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.derivations]))

;; The case-lemma builders, as in fundamental.clj.
(def ^:private P6 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, D :- (List Exp)])
(def ^:private ENV '[en :- (HEnv (skels D)), k :- Nat, hk :- (Nat.le k n)])
(defn- concl [A t] (list 'V 'chkf 'dec 'encTy 'n A '(skels D) 'en 'k (list 'skel A) (list 'den 'chkf 'dec 'encTy 'n t '(skels D) (list 'skel A) 'en)))
(defn- SND [us t A] (list 'Sound 'chkf 'dec 'encTy 'n 'D us t A))
(defn- ES [us kk] (list 'EnvSat 'chkf 'dec 'encTy 'n 'D us 'en kk))
(defn- split-steps [h a b K ka kb hp]
  (let [body (list 'And (list 'Nat.le (list '+ ka kb) K) (list 'And (ES a ka) (ES b kb)))
        ex (list 'Exists (list 'fn [ka :- 'Nat] (list 'Exists (list 'fn [kb :- 'Nat] body))))
        hx (symbol (str hp "_ex")) hq (symbol (str hp "_q")) hr (symbol (str hp "_r"))]
    [(list 'have hx ex (list 'EnvSat_split 'chkf 'dec 'encTy 'n 'D a b 'en K h))
     (list 'refine' (list 'exN '_ '_ hx '_)) (list 'intro ka hq)
     (list 'refine' (list 'exN '_ '_ hq '_)) (list 'intro kb hr)
     (list 'have hp body hr)]))

;; --- the token environment Θₘ ---------------------------------------------------------

;; tokEnvD m: the all-token environment, typed by the context's skeletons.  The
;; Refl clause of ⟦·⟧ (den_gen.clj) runs decoded programs at (thetaSk m,
;; tokenEnv m) instead; the two are equal as dependent pairs (tok_pair), so
;; any denotation transfers (tok_transfer).  The tokens satisfy Θₘ at
;; footprint m (tok_sat), and Θₘ is well-formed (wf_theta).

(kdef tokEnvD (forall [m Nat] (HEnv (skels (thetaD m))))
  (fn [m :- Nat]
    (Nat.rec$1 (fn [k :- Nat] (HEnv (skels (thetaD k)))) Unit.unit
      (fn [k :- Nat, e :- (HEnv (skels (thetaD k)))] (Prod.mk Unit.unit e))
      m)))
(thm tok_pair [m :- Nat]
  (= (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) (skels (thetaD m)) (tokEnvD m))
     (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) (thetaSk m) (tokenEnv m)))
  (induction m)
  (rfl)
  (exact (congrArg (fn [p :- (PSigma (fn [G :- (List Sk)] (HEnv G)))]
                     (AT_PSigma.mk (List Sk) (fn [G :- (List Sk)] (HEnv G)) (List.cons Sk Sk.dia (PSigma.fst p)) (Prod.mk Unit.unit (PSigma.snd p))))
                   ih_n)))
(thm tok_transfer [m :- Nat, f :- DenBody, s :- Sk]
  (= (f (thetaSk m) s (tokenEnv m)) (f (skels (thetaD m)) s (tokEnvD m)))
  (exact (congrArg (fn [p :- (PSigma (fn [G :- (List Sk)] (HEnv G)))] (f (PSigma.fst p) s (PSigma.snd p))) (Eq.symm (tok_pair m)))))
(thm tok_sat [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, m :- Nat]
  (EnvSat chkf dec encTy cap (thetaD m) (thetaU m) (tokEnvD m) m)
  (induction m)
  (exact True.intro)
  (constructor) (exact 1) (constructor) (exact n) (constructor)
  (exact (Nat.le_of_eq (Nat.add_comm 1 n))) (constructor) (exact ih_n) (exact (Nat.le_refl 1)))
(thm wf_theta [chkf :- (=> Code Code Bool), m :- Nat] (WFCtx chkf (thetaD m))
  (induction m) (exact True.intro) (constructor) (exact (Tl.fBase chkf (thetaD n) Exp.tDia rfl)) (exact ih_n))

;; --- base types and their codes ------------------------------------------------------

(thm someC_inj [x :- Code, y :- Code, h :- (Eq (Option Code) (Option.some Code x) (Option.some Code y))] (Eq Code x y) (cases h) (rfl))
;; its code term denotes its encoding, under CheckSpec's second clause
(thm den_cleaf [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                  G :- (List Sk), en :- (HEnv G), l :- Nat]
  (Eq Code (den chkf dec encTy n (cLeaf l) G Sk.syn en) (Code.sl l))
  (change (Eq Code (den chkf dec encTy n (Exp.sleaf (Exp.lbl l)) G Sk.syn en) (Code.sl l)))
  (rw [(den_sleaf_at chkf dec encTy n (Exp.lbl l) G Sk.syn en)])
  (rw [(den_lbl_eq chkf dec encTy n l)]))
;; a base type (baseCode defined) is a base data type and closed
(thm base_is_base [X :- Exp]
  (forall [cd Exp] (=> (Eq (Option Exp) (baseCode X) (Option.some Exp cd)) (And (Eq Bool (isBaseTy X) Bool.true) (Eq Bool (closedTy X) Bool.true))))
  (cases X) (all_goals (intro cd hb))
  (all_goals (first (exact (And.intro rfl rfl)) (skip)))
  (all_goals (exact (False.elim$0 (none_ne_someE cd hb)))))
(a/defn leafOf [D :- Exp] Nat
  (match D [tEmpty 15] [tUnit 16] [tBool 17] [tNat 18] [tLbl 19] [tSyn 20] [tR 22] [_ 0]))
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private base-label '{tEmpty 15 tUnit 16 tBool 17 tNat 18 tLbl 19 tSyn 20 tR 22})
(a/prove-theorem 'base_leaf '[D :- Exp]
  (lv '(forall [cd Exp] (=> (Eq (Option Exp) (baseCode D) (Option.some Exp cd)) (Eq Exp cd (cLeaf (leafOf D))))))
  (lv (into ['(cases D)]
        (mapcat (fn [[ctor _]] (if (base-label ctor)
                                 [(list 'intro 'cd 'hb) (list 'exact (list 'Eq.symm (list 'some_inj (list 'cLeaf (base-label ctor)) 'cd 'hb)))]
                                 '[(intro cd hb) (exact (False.elim$0 (none_ne_someE cd hb)))]))
                exp-fields))))
(thm base_den [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                 hcs :- (forall [D Exp] (forall [cd Exp] (=> (Eq (Option Exp) (baseCode D) (Option.some Exp cd)) (Eq (Option Code) (codeOf cd) (Option.some Code (encTy D)))))),
                 X :- Exp, cd :- Exp, hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)), G :- (List Sk), en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n cd G Sk.syn en) (encTy X))
  (have e (Eq Exp cd (cLeaf (leafOf X))) (base_leaf X cd hb))
  (have h3 (Eq (Option Code) (codeOf (cLeaf (leafOf X))) (Option.some Code (encTy X)))
    (Eq.mp (congrArg (fn [c :- Exp] (Eq (Option Code) (codeOf c) (Option.some Code (encTy X)))) e) (hcs X cd hb)))
  (have h4 (Eq Code (Code.sl (leafOf X)) (encTy X)) (someC_inj (Code.sl (leafOf X)) (encTy X) h3))
  (exact (Eq.mpr (congrArg (fn [c :- Exp] (Eq Code (den chkf dec encTy n c G Sk.syn en) (encTy X))) e)
                 (Eq.trans (den_cleaf chkf dec encTy n G en (leafOf X)) h4))))

;; --- the check, forced by a certificate of chk′ -----------------------------------------

(thm den_chk_val [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    c :- Exp, d :- Exp, G :- (List Sk), en :- (HEnv G)]
  (Eq Bool (den chkf dec encTy n (Exp.chk c d) G Sk.bool en) (chkf (den chkf dec encTy n c G Sk.syn en) (den chkf dec encTy n d G Sk.syn en)))
  (rw [(den_chk_at chkf dec encTy n c d G Sk.bool en)]))
(thm den_prn_val [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    r :- Exp, G :- (List Sk), en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (Exp.prn r) G Sk.syn en) (den chkf dec encTy n r G Sk.cert en))
  (rw [(den_prn_at chkf dec encTy n r G Sk.syn en)]))
;; e ∈ V(chkT r cd) forces the check on ⟦r⟧ and ⟦cd⟧
(thm chk_true [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                 G :- (List Sk), en :- (HEnv G), k :- Nat, r :- Exp, cd :- Exp, v :- Unit,
                 h :- (V chkf dec encTy n (chkT r cd) G en k Sk.unit v)]
  (Eq Bool (chkf (den chkf dec encTy n r G Sk.cert en) (den chkf dec encTy n cd G Sk.syn en)) Bool.true)
  (have h1 (Eq Bool (den chkf dec encTy n (Exp.chk (Exp.prn r) cd) G Sk.bool en) Bool.true) h)
  (have h2 (Eq Bool (chkf (den chkf dec encTy n (Exp.prn r) G Sk.syn en) (den chkf dec encTy n cd G Sk.syn en)) Bool.true)
    (Eq.trans (Eq.symm (den_chk_val chkf dec encTy n (Exp.prn r) cd G en)) h1))
  (exact (Eq.mp (congrArg (fn [q :- Code] (Eq Bool (chkf q (den chkf dec encTy n cd G Sk.syn en)) Bool.true)) (den_prn_val chkf dec encTy n r G en)) h2)))

;; --- Reflect -----------------------------------------------------------------------

;; The outer hypothesis (the induction on the budget): Lemma 3.6 at every
;; smaller budget m, for all-token contexts and the token environment.

(def ^:private rv '(den chkf dec encTy n r G Sk.cert en))
(eval (list 'lcert.formal.base/thm 'den_refl_some
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
    X :- Exp, r :- Exp, e :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G), m :- Nat, t :- Exp, A :- Exp,
    hc :- (Eq Bool (Bool.and (Nat.ble (cnodes (den chkf dec encTy n r G Sk.cert en)) n) (chkf (den chkf dec encTy n r G Sk.cert en) (encTy X))) Bool.true),
    hd :- (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (den chkf dec encTy n r G Sk.cert en)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))]
  '(Eq (Car sk) (den chkf dec encTy n (Exp.refl X r e) G sk en)
       (coe (skel X) sk (denPrev chkf dec encTy n m t (thetaSk m) (skel X) (tokenEnv m))))
  '(rw [(den_refl_at chkf dec encTy n X r e G sk en)])
  (list 'change (list 'Eq '(Car sk)
     (list 'Bool.rec$1 '(fn [_ :- Bool] (Car sk)) '(dflt sk)
       (list 'Option.rec$1$0 '(Prod Nat (Prod Exp Exp)) '(fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car sk)) '(dflt sk)
             '(fn [tr :- (Prod Nat (Prod Exp Exp))] (coe (skel X) sk (denPrev chkf dec encTy n (Prod.fst tr) (Prod.fst (Prod.snd tr)) (thetaSk (Prod.fst tr)) (skel X) (tokenEnv (Prod.fst tr)))))
             (list 'dec rv))
       (list 'Bool.and (list 'Nat.ble (list 'cnodes rv) 'n) (list 'chkf rv '(encTy X))))
     '(coe (skel X) sk (denPrev chkf dec encTy n m t (thetaSk m) (skel X) (tokenEnv m)))))
  '(rw [hc hd])))
(thm exT [α :- Type, P :- (=> α Prop), Q :- Prop, h :- (Exists P), f :- (forall [x α] (=> (P x) Q))] Q
  (exact (AT_Exists.rec α P (fn [_ :- (Exists P)] Q) f h)))
(thm denPrev_stable [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                       n :- Nat, m :- Nat, t :- Exp, h :- (LT.lt m n)]
  (Eq DenBody (denPrev chkf dec encTy n m t) (den chkf dec encTy m t))
  (cases n)
  (exact (absurd h (Nat.not_lt_zero m)))
  (exact (denT_stable chkf dec encTy n t m (Nat.le_of_lt_succ h))))
(kdef OuterIH (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)] (=> Nat Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (forall [m Nat] (=> (LT.lt m n) (forall [t Exp] (forall [A Exp] (=> (Rt chkf (thetaD m) (thetaU m) t A)
      (V chkf dec encTy m A (skels (thetaD m)) (tokEnvD m) m (skel A) (den chkf dec encTy m t (skels (thetaD m)) (skel A) (tokEnvD m))))))))))
(thm refl_arith [m :- Nat, c :- Nat, k1 :- Nat, k2 :- Nat, k :- Nat, n :- Nat,
                   h1 :- (LT.lt m c), h2 :- (LE.le c k1), h3 :- (LE.le (+ k1 k2) k), h4 :- (LE.le k n)]
  (And (LE.le m k) (And (LT.lt m n) (LE.le c n))) (constructor) (omega) (constructor) (omega) (omega))

;; Reflect (the paper's case): e's IH forces chk′ (print ⟦r⟧) ⌜X⌝ = tt; by
;; CheckSpec ⟦r⟧ decodes to Θₘ ⊢ t′ : A′ with ⌜A′⌝ = ⌜X⌝ and m < ‖⟦r⟧‖ ≤ k₁,
;; so A′ = X (E1, both closed) and the cap is met; ⟦refl⟧ is ⟦t′⟧ᵐ at the
;; tokens (denPrev_stable, tok_transfer), in Vᵐₘ(X) by the outer hypothesis
;; at m < n, hence in Vⁿₖ(X) by Lemma 3.4 (m ≤ k).

(def ^:private v1 '(den chkf dec encTy n r (skels D) Sk.cert en))
(def ^:private C1body (fn [mm tt AA] (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec v1) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
                     (list 'And (list 'Rt 'chkf (list 'thetaD mm) (list 'thetaU mm) tt AA)
                     (list 'And (list 'Tl 'chkf 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
                     (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
                     (list 'And (list 'Eq 'Code (list 'encTy AA) '(encTy X)) (list 'Nat.lt mm (list 'cnodes v1)))))))))
(def ^:private run '(den chkf dec encTy mm tt (skels (thetaD mm)) (skel X) (tokEnvD mm)))
(eval (concat (list 'lcert.formal.base/thm 'F_refl
  (into P6 (into '[us1 :- (List U), us2 :- (List U), X :- Exp, cd :- Exp, r :- Exp, e :- Exp,
                   hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                   hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH chkf dec encTy n)]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihe :- (SND 'us2 'e '(chkT r cd))]
                       (conj ENV 'hs :- (ES '(vadd us1 us2) 'k)))))
  (concl 'X '(Exp.refl X r e)))
  (concat
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'p)
    ['(have hkn (Nat.le (+ k1 k2) n) (Nat.le_trans (And.left p) hk))
     '(have hk1 (Nat.le k1 n) (Nat.le_trans (Nat.le_add_right k1 k2) hkn))
     '(have hk2 (Nat.le k2 n) (Nat.le_trans (Nat.le_add_left k2 k1) hkn))
     (list 'have 'vr (list 'LE.le (list 'cnodes v1) 'k1) '(ihr en k1 hk1 (And.left (And.right p))))
     '(have ve (V chkf dec encTy n (chkT r cd) (skels D) en k2 Sk.unit (den chkf dec encTy n e (skels D) Sk.unit en))
        (ihe en k2 hk2 (And.right (And.right p))))
     (list 'have 'hchk0 (list 'Eq 'Bool (list 'chkf v1 '(den chkf dec encTy n cd (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true chkf dec encTy n (skels D) en k2 r cd (den chkf dec encTy n e (skels D) Sk.unit en) ve))
     '(have hcdv (Eq Code (den chkf dec encTy n cd (skels D) Sk.syn en) (encTy X))
        (base_den chkf dec encTy n (And.left (And.right hcs)) X cd hb (skels D) en))
     (list 'have 'hchk (list 'Eq 'Bool (list 'chkf v1 '(encTy X)) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf v1 'q) 'Bool.true)) 'hcdv) 'hchk0))
     '(have hbase (And (Eq Bool (isBaseTy X) Bool.true) (Eq Bool (closedTy X) Bool.true)) (base_is_base X cd hb))
     (list 'have 'hC1 (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (C1body 'mm 'tt 'AA)))))))
        (list '(And.left hcs) v1 '(encTy X) 'hchk))
     '(refine' (exT Nat _ _ hC1 _)) '(intro mm hm) '(refine' (exT Exp _ _ hm _)) '(intro tt htt) '(refine' (exT Exp _ _ htt _)) '(intro AA hAA)
     (list 'have 'q (C1body 'mm 'tt 'AA) 'hAA)
     '(have eA (Eq Exp AA X) ((And.right (And.right (And.right hcs))) AA X (And.left (And.right (And.right (And.right q)))) (And.right hbase)
                              (And.left (And.right (And.right (And.right (And.right q)))))))
     '(have hRtX (Rt chkf (thetaD mm) (thetaU mm) tt X) (Eq.mp (congrArg (fn [Z :- Exp] (Rt chkf (thetaD mm) (thetaU mm) tt Z)) eA) (And.left (And.right q))))
     (list 'have 'har (list 'And '(LE.le mm k) (list 'And '(LT.lt mm n) (list 'LE.le (list 'cnodes v1) 'n)))
        (list 'refl_arith 'mm (list 'cnodes v1) 'k1 'k2 'k 'n '(And.right (And.right (And.right (And.right (And.right q))))) 'vr '(And.left p) 'hk))
     (list 'have 'hc (list 'Eq 'Bool (list 'Bool.and (list 'Nat.ble (list 'cnodes v1) 'n) (list 'chkf v1 '(encTy X))) 'Bool.true)
        (list 'Eq.trans (list 'congrArg (list 'fn '[b :- Bool] (list 'Bool.and 'b (list 'chkf v1 '(encTy X))))
                              (list 'Nat.ble_eq_true_of_le '(And.right (And.right har))))
                        (list 'congrArg '(fn [b :- Bool] (Bool.and Bool.true b)) 'hchk)))
     '(rw [(den_refl_some chkf dec encTy n X r e (skels D) (skel X) en mm tt AA hc (And.left q))])
     '(rw [(coe_self (skel X) (denPrev chkf dec encTy n mm tt (thetaSk mm) (skel X) (tokenEnv mm)))])
     '(rw [(congrArg (fn [f :- DenBody] (f (thetaSk mm) (skel X) (tokenEnv mm))) (denPrev_stable chkf dec encTy n mm tt (And.left (And.right har))))])
     '(rw [(tok_transfer mm (den chkf dec encTy mm tt) (skel X))])
     (list 'have 'vo (list 'V 'chkf 'dec 'encTy 'mm 'X '(skels (thetaD mm)) '(tokEnvD mm) 'mm '(skel X) run)
        '(hout mm (And.left (And.right har)) tt X hRtX))
     (list 'exact (list 'Lemma_3_4 'chkf 'dec 'encTy 'X 'mm 'n 'k '(skels (thetaD mm)) '(skels D) '(tokEnvD mm) 'en '(skel X) run
                        '(And.left hbase) '(And.left har) 'vo))])))

;; --- H₁ ------------------------------------------------------------------------------

;; H₁ (the paper's case): the two IHs force chk′ (print ⟦r⟧) ⌜c⌝ and
;; chk′ (print ⟦s⟧) (neg ⌜c⌝); CheckSpec decodes them to Θₘ₁ ⊢ t₁ : A and
;; Θₘ₂ ⊢ t₂ : B with ⌜B⌝ = neg ⌜A⌝ = ⌜A ⊸ 0⌝ (E5), so B = A ⊸ 0 (E1);
;; Lemma 2.4 composes them into Θₘ₁₊ₘ₂ ⊢ t₂ t₁ : 0 with m₁ + m₂ < n; the
;; outer hypothesis puts its value in V(0) = ∅.  So no environment
;; satisfies the premises: the case is vacuous.

(thm den_negT [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                 c :- Exp, G :- (List Sk), en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (negT c) G Sk.syn en) (Code.sn 25 (den chkf dec encTy n c G Sk.syn en) (Code.sl 15)))
  (change (Eq Code (den chkf dec encTy n (Exp.snode (Exp.lbl 25) c (Exp.sleaf (Exp.lbl 15))) G Sk.syn en) (Code.sn 25 (den chkf dec encTy n c G Sk.syn en) (Code.sl 15))))
  (rw [(den_snode_at chkf dec encTy n (Exp.lbl 25) c (Exp.sleaf (Exp.lbl 15)) G Sk.syn en)])
  (rw [(den_lbl_eq chkf dec encTy n 25) (den_sleaf_eq chkf dec encTy n (Exp.lbl 15)) (den_lbl_eq chkf dec encTy n 15)]))
(thm le_chain1 [k1 :- Nat, m1 :- Nat, k :- Nat, n :- Nat, o1 :- (LE.le (+ k1 m1) k), hk :- (LE.le k n)] (LE.le k1 n) (omega))
(thm le_chain2 [k1 :- Nat, m1 :- Nat, k2 :- Nat, m2 :- Nat, k :- Nat, n :- Nat, o1 :- (LE.le (+ k1 m1) k), o2 :- (LE.le (+ k2 m2) m1), hk :- (LE.le k n)] (LE.le k2 n) (omega))
(thm le_chain4 [k1 :- Nat, m1 :- Nat, k2 :- Nat, m2 :- Nat, k3 :- Nat, m3 :- Nat, k4 :- Nat, k5 :- Nat, k :- Nat, n :- Nat,
                  o1 :- (LE.le (+ k1 m1) k), o2 :- (LE.le (+ k2 m2) m1), o3 :- (LE.le (+ k3 m3) m2), o4 :- (LE.le (+ k4 k5) m3), hk :- (LE.le k n)] (LE.le k4 n) (omega))
(thm le_chain5 [k1 :- Nat, m1 :- Nat, k2 :- Nat, m2 :- Nat, k3 :- Nat, m3 :- Nat, k4 :- Nat, k5 :- Nat, k :- Nat, n :- Nat,
                  o1 :- (LE.le (+ k1 m1) k), o2 :- (LE.le (+ k2 m2) m1), o3 :- (LE.le (+ k3 m3) m2), o4 :- (LE.le (+ k4 k5) m3), hk :- (LE.le k n)] (LE.le k5 n) (omega))
(thm le_assoc [k1 :- Nat, k2 :- Nat, m2 :- Nat, m1 :- Nat, k :- Nat, o1 :- (LE.le (+ k1 m1) k), o2 :- (LE.le (+ k2 m2) m1)] (LE.le (+ k1 (+ k2 m2)) k) (omega))
(thm h1_arith [m1 :- Nat, c1 :- Nat, k1 :- Nat, m2 :- Nat, c2 :- Nat, k2 :- Nat, kk :- Nat, k :- Nat, n :- Nat,
                 a1 :- (LT.lt m1 c1), a2 :- (LE.le c1 k1), b1 :- (LT.lt m2 c2), b2 :- (LE.le c2 k2),
                 s1 :- (LE.le (+ k2 kk) (- k k1)), s0 :- (LE.le k1 k), hk :- (LE.le k n)]
  (LT.lt (+ m1 m2) n) (omega))
(def ^:private vv '(den chkf dec encTy n r (skels D) Sk.cert en))
(def ^:private ww '(den chkf dec encTy n s (skels D) Sk.cert en))
(def ^:private dc '(den chkf dec encTy n c (skels D) Sk.syn en))
(defn- Cb [cv dv mm tt AA] (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec cv) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
                     (list 'And (list 'Rt 'chkf (list 'thetaD mm) (list 'thetaU mm) tt AA)
                     (list 'And (list 'Tl 'chkf 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
                     (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
                     (list 'And (list 'Eq 'Code (list 'encTy AA) dv) (list 'Nat.lt mm (list 'cnodes cv))))))))
(defn- Cex [cv dv] (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (Cb cv dv 'mm 'tt 'AA))))))))
(def ^:private ndc (list 'Code.sn 25 dc '(Code.sl 15)))
(defn- gets [q i] ;; i-th conjunct of Cb (0-based, 5 = last)
  (let [f (fn f [x j] (if (zero? j) (list 'And.left x) (f (list 'And.right x) (dec j))))]
    (if (= i 5) (list 'And.right (list 'And.right (list 'And.right (list 'And.right (list 'And.right q))))) (f q i))))
(eval (concat (list 'lcert.formal.base/thm 'F_h1
  (into P6 (into '[us1 :- (List U), us2 :- (List U), us3 :- (List U), us4 :- (List U), us5 :- (List U),
                   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
                   hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH chkf dec encTy n)]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihs :- (SND 'us2 's 'Exp.tR)
                        'ih1 :- (SND 'us4 'e1 '(chkT r c)) 'ih2 :- (SND 'us5 'e2 '(chkT s (negT c)))]
                       (conj ENV 'hs :- (ES '(vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) 'k)))))
  (concl 'Exp.tEmpty '(Exp.h1 r s c e1 e2)))
  (concat
    (split-steps 'hs 'us1 '(vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5))) 'k 'k1 'm1 'p1)
    (split-steps '(And.right (And.right p1)) 'us2 '(vadd (vscale U.uw us3) (vadd us4 us5)) 'm1 'k2 'm2 'p2)
    (split-steps '(And.right (And.right p2)) '(vscale U.uw us3) '(vadd us4 us5) 'm2 'k3 'm3 'p3)
    (split-steps '(And.right (And.right p3)) 'us4 'us5 'm3 'k4 'k5 'p4)
    ['(have o1 (LE.le (+ k1 m1) k) (And.left p1)) '(have o2 (LE.le (+ k2 m2) m1) (And.left p2))
     '(have o3 (LE.le (+ k3 m3) m2) (And.left p3)) '(have o4 (LE.le (+ k4 k5) m3) (And.left p4))
     '(have hk1 (Nat.le k1 n) (le_chain1 k1 m1 k n o1 hk))
     '(have hk2 (Nat.le k2 n) (le_chain2 k1 m1 k2 m2 k n o1 o2 hk))
     '(have hk4 (Nat.le k4 n) (le_chain4 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     '(have hk5 (Nat.le k5 n) (le_chain5 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     (list 'have 'vr (list 'LE.le (list 'cnodes vv) 'k1) '(ihr en k1 hk1 (And.left (And.right p1))))
     (list 'have 'vs (list 'LE.le (list 'cnodes ww) 'k2) '(ihs en k2 hk2 (And.left (And.right p2))))
     (list 'have 'hc1 (list 'Eq 'Bool (list 'chkf vv dc) 'Bool.true)
        '(chk_true chkf dec encTy n (skels D) en k4 r c (den chkf dec encTy n e1 (skels D) Sk.unit en) (ih1 en k4 hk4 (And.left (And.right p4)))))
     (list 'have 'hc2a (list 'Eq 'Bool (list 'chkf ww '(den chkf dec encTy n (negT c) (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true chkf dec encTy n (skels D) en k5 s (negT c) (den chkf dec encTy n e2 (skels D) Sk.unit en) (ih2 en k5 hk5 (And.right (And.right p4)))))
     (list 'have 'hc2 (list 'Eq 'Bool (list 'chkf ww ndc) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf ww 'q) 'Bool.true)) '(den_negT chkf dec encTy n c (skels D) en)) 'hc2a))
     (list 'have 'X1 (Cex vv dc) (list '(And.left hcs) vv dc 'hc1))
     '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
     (list 'have 'qa (Cb vv dc 'ma 'ta 'Aa) 'hAa)
     (list 'have 'X2 (Cex ww ndc) (list '(And.left hcs) ww ndc 'hc2))
     '(refine' (exT Nat _ _ X2 _)) '(intro mb hmb) '(refine' (exT Exp _ _ hmb _)) '(intro tb htb) '(refine' (exT Exp _ _ htb _)) '(intro Ab hAb)
     (list 'have 'qb (Cb ww ndc 'mb 'tb 'Ab) 'hAb)
     ;; A_b = A_a ⊸ 0 by E5 and E1
     (list 'have 'hcl (list 'Eq 'Bool '(closedTy (Exp.tPi U.u1 Aa Exp.tEmpty)) 'Bool.true) (list 'andb_intro '(closedTy Aa) 'true (gets 'qa 3) '(Eq.refl$1 Bool.true)))
     (list 'have 'henc (list 'Eq 'Code '(encTy (Exp.tPi U.u1 Aa Exp.tEmpty)) '(encTy Ab))
        (list 'Eq.trans (list '(And.left (And.right (And.right hcs))) 'Aa (gets 'qa 3))
              (list 'Eq.trans (list 'congrArg '(fn [q :- Code] (Code.sn 25 q (Code.sl 15))) (gets 'qa 4)) (list 'Eq.symm (gets 'qb 4)))))
     (list 'have 'eB '(Eq Exp (Exp.tPi U.u1 Aa Exp.tEmpty) Ab) (list '(And.right (And.right (And.right hcs))) '(Exp.tPi U.u1 Aa Exp.tEmpty) 'Ab 'hcl (gets 'qb 3) 'henc))
     (list 'have 'hRtb '(Rt chkf (thetaD mb) (thetaU mb) tb (Exp.tPi U.u1 Aa Exp.tEmpty))
        (list 'Eq.mpr (list 'congrArg '(fn [Z :- Exp] (Rt chkf (thetaD mb) (thetaU mb) tb Z)) 'eB) (gets 'qb 1)))
     (list 'have 'hcomp '(Rt chkf (thetaD (+ ma mb)) (thetaU (+ ma mb)) (Exp.app (lift ma 0 tb) (lift mb ma ta)) Exp.tEmpty)
        (list 'lemma24 'chkf 'ma 'mb 'ta 'tb 'Aa (gets 'qa 3) (gets 'qa 2) (gets 'qa 1) 'hRtb))
     (list 'have 'hlt '(LT.lt (+ ma mb) n)
        (list 'h1_arith 'ma (list 'cnodes vv) 'k1 'mb (list 'cnodes ww) 'k2 'm2 'k 'n (gets 'qa 5) 'vr (gets 'qb 5) 'vs
              '(le_sub_add k1 (+ k2 m2) k (le_assoc k1 k2 m2 m1 k o1 o2)) '(Nat.le_trans (Nat.le_add_right k1 m1) o1) 'hk))
     '(have hf False (hout (+ ma mb) hlt (Exp.app (lift ma 0 tb) (lift mb ma ta)) Exp.tEmpty hcomp))
     '(exact (False.elim hf))])))
