(ns lcert.formal.fundamental
  "F3j — the fundamental lemma (R4-metatheory.md Lemma 3.6), case by case.

  Sound n D us t A is the lemma's conclusion for one judgment Γ ⊢ t :¹ A with
  Γ = (D, us): for every environment η ⊨ⁿₖ Γ with k ≤ n, ⟦t⟧ⁿη ∈ Vⁿₖ(A)η.
  Lemma 3.6 is then an induction on the Rt derivation whose motive is Sound;
  each rule's case is a lemma F_<rule> below, taking the induction
  hypotheses of the rule's premises as Sound hypotheses.

  Each F_<rule> states its conclusion explicitly (V … (den … t …)) rather
  than as Sound: after `intro` through Sound, Ansatz's rewrite does not find
  the den subterm, while an explicit goal rewrites with the den_<ctor>_at
  equations of unfold.clj.  The final induction applies F_<rule> by `exact`.

  Proved here (the cases that need neither substitution nor conversion):
  - Const (⋆, tt, ff, zero, labels), Succ, Sleaf, Snode, Prn, Chk: the
    result type's set is all of its carrier, except Lbl, where the label is
    below NL by constTyped;
  - Lam, at each usage 0, 1, ω: the Π clause of V, with the extended
    environment satisfying the extended context;
  - Abort: V(0) is empty, so the premise's set is;
  - If: Γ₁ + Γ₂ splits (Lemma 3.5); both branches are in V(C) at Γ₂'s
    footprint, raised to k; the scrutinee picks one;
  - Leaf, Node: ‖leaf‖ = 0; ‖node d x r₁ r₂‖ = 1 + ‖r₁‖ + ‖r₂‖, with the 1
    paid by the token d (V₁(◇) needs footprint ≥ 1), all within k by
    splitting Γ₁ + Γ₂ + Γ₃ + Γ₄;
  - Bnil: its set is vacuous at NL (no label l with NL ≤ l < NL).

  - App, at usage 1 and ω: skOf of the argument is its skeleton (skOf_rt),
    so ⟦f u⟧ = ⟦f⟧(⟦u⟧); V(B[u/x])η = V(B)(η, ⟦u⟧) (Lemma 3.3, V_subst1);
    the Π clause of ⟦f⟧'s IH at ⟦u⟧'s footprint, raised to k (V_mono).

  Pending (they need Lemma 3.1/3.3's substitution or weakening clauses, 3.2,
  or the outer induction on n): Var, App₀, Pair₀, Pair, Let, Conv,
  ElimBool, RecN, CaseL, Bcons, RecS, ItR, H₁, Refl, Inspect."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.subst]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.unfold :refer :all]
            [lcert.formal.mono]
            [lcert.formal.substitution]
            [lcert.formal.skeletons]
            [lcert.formal.skof]))

;; --- the motive ----------------------------------------------------------------

(kdef Sound
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat (List Exp) (List U) Exp Exp Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
       n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp]
    (forall [en (HEnv (skels D))] (forall [k Nat]
      (=> (Nat.le k n) (EnvSat chkf dec encTy n D us en k)
        (V chkf dec encTy n A (skels D) en k (skel A) (den chkf dec encTy n t (skels D) (skel A) en)))))))

;; A predicate true of both branches is true of Bool.rec's choice.
(thm bool_rec_pred [α :- Type, Q :- (=> α Prop), x :- α, y :- α, bv :- Bool, hx :- (Q x), hy :- (Q y)]
  (Q (Bool.rec$1 (fn [_ :- Bool] α) x y bv))
  (cases bv) (exact hx) (exact hy))

;; --- statement builders ------------------------------------------------------------

;; The common parameters of every case lemma.
(def ^:private P6
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
    n :- Nat, D :- (List Exp), us :- (List U)])

;; η, k and k ≤ n.  The context hypothesis hs is added per rule, since its
;; usage vector is the rule's.
(def ^:private ENV '[en :- (HEnv (skels D)), k :- Nat, hk :- (Nat.le k n)])

(defn- concl
  "⟦t⟧ⁿη ∈ Vⁿₖ(A)η, explicitly."
  [A t]
  (list 'V 'chkf 'dec 'encTy 'n A '(skels D) 'en 'k (list 'skel A)
        (list 'den 'chkf 'dec 'encTy 'n t '(skels D) (list 'skel A) 'en)))

(defn- SND "The IH of a premise Γ′ ⊢ t :¹ A over the same D." [us t A]
  (list 'Sound 'chkf 'dec 'encTy 'n 'D us t A))

(defn- ES "η ⊨ₖ (D, us)." [us kk] (list 'EnvSat 'chkf 'dec 'encTy 'n 'D us 'en kk))

(defn- case!
  "State and prove a case lemma: params beyond P6, conclusion, tactics."
  [nm extra concl-form tactics]
  (eval (list* 'lcert.formal.base/thm nm (into P6 extra) concl-form (vec tactics))))

(defn- split-steps
  "Tactics splitting a hypothesis `h` : η ⊨_K (vadd a b) by EnvSat_split into
  footprints ka, kb, leaving `hp` : ka + kb ≤ K ∧ η ⊨_ka a ∧ η ⊨_kb b."
  [h a b K ka kb hp]
  (let [body (list 'And (list 'Nat.le (list '+ ka kb) K) (list 'And (ES a ka) (ES b kb)))
        ex (list 'Exists (list 'fn [ka :- 'Nat] (list 'Exists (list 'fn [kb :- 'Nat] body))))
        hx (symbol (str hp "_ex")) hq (symbol (str hp "_q")) hr (symbol (str hp "_r"))]
    [(list 'have hx ex (list 'EnvSat_split 'chkf 'dec 'encTy 'n 'D a b 'en K h))
     (list 'refine' (list 'exN '_ '_ hx '_)) (list 'intro ka hq)
     (list 'refine' (list 'exN '_ '_ hq '_)) (list 'intro kb hr)
     (list 'have hp body hr)]))

;; --- constants ----------------------------------------------------------------

;; Const: t ⋆/tt/ff/zero/lbl l at the type constTyped assigns.  Every other
;; (t, A) pair has constTyped t A = false and is refuted; the five real cases
;; have V = everything, except Lbl: l < NL, which constTyped checks.
(case! 'F_const ENV
  (list 'forall '[t Exp] (list 'forall '[A Exp] (list '=> '(Eq Bool (constTyped t A) Bool.true) (concl 'A 't))))
  '[(intro t) (cases t) (all_goals (intro A))
    (all_goals (first (and_then (intro hct) (exact (Bool.noConfusion hct))) (skip)))
    (all_goals (cases A))
    (all_goals (first (and_then (intro hct) (exact (Bool.noConfusion hct))) (skip)))
    ;; ⋆ : 1, tt : Bool, ff : Bool, zero : Nat
    (all_goals (first (exact True.intro) (skip)))
    ;; lbl l : Lbl
    (rw [(den_lbl_at chkf dec encTy n l (skels D) (skel Exp.tLbl) en)])
    (rw [(coe_self Sk.lbl l)])
    (have hb (Eq Bool (Nat.ble (+ l 1) 100) Bool.true) hct)
    (exact (Nat.le_of_ble_eq_true hb))])

;; --- data whose set is the whole carrier --------------------------------------------

(case! 'F_succ (into '[m :- Exp] ENV) (concl 'Exp.tNat '(Exp.succ m)) '[(exact True.intro)])
(case! 'F_sleaf (into '[x :- Exp] ENV) (concl 'Exp.tSyn '(Exp.sleaf x)) '[(exact True.intro)])
(case! 'F_snode (into '[x :- Exp, c1 :- Exp, c2 :- Exp] ENV) (concl 'Exp.tSyn '(Exp.snode x c1 c2)) '[(exact True.intro)])
(case! 'F_prn (into '[r :- Exp] ENV) (concl 'Exp.tSyn '(Exp.prn r)) '[(exact True.intro)])
(case! 'F_chk (into '[c :- Exp, d :- Exp] ENV) (concl 'Exp.tBool '(Exp.chk c d)) '[(exact True.intro)])

;; --- functions -----------------------------------------------------------------

;; Lam at usage r.  ⟦λx.t⟧η = a ↦ ⟦t⟧(η, a).  The extended environment
;; satisfies (A :: D, r :: us): at r = 0 with an entry of footprint 0 (the
;; entry is unconstrained); at r = 1 with the argument's footprint j, total
;; k + j ≤ n; at r = ω with footprint 0 and the argument in V₀(A).
(defn- lam-case [nm r]
  (let [Pi (list 'Exp.tPi r 'A 'B) lam (list 'Exp.lam r 'A 't)
        ext '(List.cons Sk (skel A) (skels D))
        body (fn [env kk] (list 'V 'chkf 'dec 'encTy 'n 'B ext env kk '(skel B)
                                (list 'den 'chkf 'dec 'encTy 'n 't ext '(skel B) env)))
        cD '(List.cons Exp A D) cU (list 'List.cons 'U r 'us)]
    (case! nm
      (into ['A :- 'Exp 't :- 'Exp 'B :- 'Exp 'ih :- (list 'Sound 'chkf 'dec 'encTy 'n cD cU 't 'B)]
            (conj ENV 'hs :- (ES 'us 'k)))
      (concl Pi lam)
      (concat
        [(list 'rw [(list 'den_lam_at 'chkf 'dec 'encTy 'n r 'A 't '(skels D) (list 'skel Pi) 'en)])
         (list 'have 'ih2 (list 'forall '[en2 (HEnv (skels (List.cons Exp A D)))] (list 'forall '[k2 Nat]
                 (list '=> '(Nat.le k2 n) (list 'EnvSat 'chkf 'dec 'encTy 'n cD cU 'en2 'k2)
                   (list 'V 'chkf 'dec 'encTy 'n 'B (list 'skels cD) 'en2 'k2 '(skel B)
                         (list 'den 'chkf 'dec 'encTy 'n 't (list 'skels cD) '(skel B) 'en2)))))
               'ih)]
        (case r
          U.u0 [(list 'change (list 'forall '[a (Car (skel A))] (body '(Prod.mk a en) 'k)))
                '(intro a) '(apply (ih2 (Prod.mk a en) k hk))
                '(constructor) '(exact 0) '(constructor) '(exact k) '(constructor) '(exact (Nat.le_of_eq (Nat.zero_add k)))
                '(constructor) '(exact hs) '(exact rfl)]
          U.u1 [(list 'change (list 'forall '[j Nat] (list '=> '(Nat.le (+ k j) n) (list 'forall '[a (Car (skel A))]
                   (list '=> '(V chkf dec encTy n A (skels D) en j (skel A) a) (body '(Prod.mk a en) '(+ k j)))))))
                '(intro j hj a ha) '(apply (ih2 (Prod.mk a en) (+ k j) hj))
                '(constructor) '(exact j) '(constructor) '(exact k) '(constructor) '(exact (Nat.le_of_eq (Nat.add_comm j k)))
                '(constructor) '(exact hs) '(exact ha)]
          U.uw [(list 'change (list 'forall '[a (Car (skel A))]
                   (list '=> '(V chkf dec encTy n A (skels D) en 0 (skel A) a) (body '(Prod.mk a en) 'k))))
                '(intro a ha) '(apply (ih2 (Prod.mk a en) k hk))
                '(constructor) '(exact 0) '(constructor) '(exact k) '(constructor) '(exact (Nat.le_of_eq (Nat.zero_add k)))
                '(constructor) '(exact hs) '(constructor) '(exact rfl) '(exact ha)])))))

(lam-case 'F_lam0 'U.u0)
(lam-case 'F_lam1 'U.u1)
(lam-case 'F_lamw 'U.uw)

;; Lam at any usage, in the motive's form (cases r must see r in the goal,
;; since Ansatz's cases does not rewrite hypotheses).
(thm F_lam [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
            n :- Nat, D :- (List Exp), us :- (List U), A :- Exp, t :- Exp, B :- Exp]
  (forall [r U] (=> (Sound chkf dec encTy n (List.cons Exp A D) (List.cons U r us) t B)
    (Sound chkf dec encTy n D us (Exp.lam r A t) (Exp.tPi r A B))))
  (intro r) (cases r) (all_goals (intro ih en k hk hs))
  ;; the goal order after cases is not the constructor order; each lemma
  ;; typechecks against exactly one goal
  (all_goals (first (exact (F_lam0 chkf dec encTy n D us A t B ih en k hk hs))
                    (exact (F_lam1 chkf dec encTy n D us A t B ih en k hk hs))
                    (exact (F_lamw chkf dec encTy n D us A t B ih en k hk hs)))))

;; --- the empty type ------------------------------------------------------------------

(case! 'F_abort
  (into ['A :- 'Exp 't :- 'Exp 'ih :- (SND 'us 't 'Exp.tEmpty)] (conj ENV 'hs :- (ES 'us 'k)))
  (concl 'A '(Exp.abort A t))
  '[(have hv False (ih en k hk hs)) (exact (False.elim hv))])

;; --- booleans -------------------------------------------------------------------

(case! 'F_ite
  (into '[us1 :- (List U), us2 :- (List U), b :- Exp, t :- Exp, e :- Exp, C :- Exp]
        (into ['ihb :- (SND 'us1 'b 'Exp.tBool) 'iht :- (SND 'us2 't 'C) 'ihe :- (SND 'us2 'e 'C)]
              (conj ENV 'hs :- (ES '(vadd us1 us2) 'k))))
  (concl 'C '(Exp.ite b t e))
  (concat
    ['(rw [(den_ite_at chkf dec encTy n b t e (skels D) (skel C) en)])]
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'p)
    ['(have hle (Nat.le k2 k) (Nat.le_trans (Nat.le_add_left k2 k1) (And.left p)))
     '(have hs2 (EnvSat chkf dec encTy n D us2 en k) (EnvSat_mono chkf dec encTy n D us2 en k2 k hle (And.right (And.right p))))
     '(exact (bool_rec_pred (Car (skel C)) (fn [v :- (Car (skel C))] (V chkf dec encTy n C (skels D) en k (skel C) v))
               (den chkf dec encTy n e (skels D) (skel C) en) (den chkf dec encTy n t (skels D) (skel C) en)
               (den chkf dec encTy n b (skels D) Sk.bool en) (ihe en k hk hs2) (iht en k hk hs2)))]))

;; --- certificates -------------------------------------------------------------------

(case! 'F_leaf (into '[x :- Exp] ENV) (concl 'Exp.tR '(Exp.leaf x))
  '[(rw [(den_leaf_at chkf dec encTy n x (skels D) (skel Exp.tR) en)])
    (rw [(coe_self Sk.cert (Code.sl (den chkf dec encTy n x (skels D) Sk.lbl en)))])
    (exact (Nat.zero_le k))])

;; Node: split Γ₁ + (Γ₂ + (Γ₃ + Γ₄)) three times, then ‖node‖ = 1 + ‖r₁‖ + ‖r₂‖
;; ≤ k₁ + k₃ + k₄ ≤ k, the 1 because d ∈ V_k₁(◇) forces k₁ ≥ 1.
(case! 'F_node
  (into '[us1 :- (List U), us2 :- (List U), us3 :- (List U), us4 :- (List U), d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp]
        (into ['ihd :- (SND 'us1 'd 'Exp.tDia) 'ih1 :- (SND 'us3 'r1 'Exp.tR) 'ih2 :- (SND 'us4 'r2 'Exp.tR)]
              (conj ENV 'hs :- (ES '(vadd us1 (vadd us2 (vadd us3 us4))) 'k))))
  (concl 'Exp.tR '(Exp.node d x r1 r2))
  (concat
    ['(rw [(den_node_at chkf dec encTy n d x r1 r2 (skels D) (skel Exp.tR) en)])
     '(rw [(coe_self Sk.cert (Code.sn (den chkf dec encTy n x (skels D) Sk.lbl en) (den chkf dec encTy n r1 (skels D) Sk.cert en)
                                      (den chkf dec encTy n r2 (skels D) Sk.cert en)))])]
    (split-steps 'hs 'us1 '(vadd us2 (vadd us3 us4)) 'k 'k1 'm1 'p1)
    (split-steps '(And.right (And.right p1)) 'us2 '(vadd us3 us4) 'm1 'k2 'm2 'p2)
    (split-steps '(And.right (And.right p2)) 'us3 'us4 'm2 'k3 'k4 'p3)
    ['(have o1 (LE.le (+ k1 m1) k) (And.left p1)) '(have o2 (LE.le (+ k2 m2) m1) (And.left p2))
     '(have o3 (LE.le (+ k3 k4) m2) (And.left p3))
     ;; each footprint is within the cap, as the IHs require
     '(have b1 (Nat.le k1 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_right k1 m1) (And.left p1)) hk))
     '(have bm (Nat.le m2 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_left m2 k2) (And.left p2))
                               (Nat.le_trans (Nat.le_trans (Nat.le_add_left m1 k1) (And.left p1)) hk)))
     '(have b3 (Nat.le k3 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_right k3 k4) (And.left p3)) bm))
     '(have b4 (Nat.le k4 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_left k4 k3) (And.left p3)) bm))
     '(have vd (LE.le 1 k1) (ihd en k1 b1 (And.left (And.right p1))))
     '(have v1 (LE.le (cnodes (den chkf dec encTy n r1 (skels D) Sk.cert en)) k3) (ih1 en k3 b3 (And.left (And.right p3))))
     '(have v2 (LE.le (cnodes (den chkf dec encTy n r2 (skels D) Sk.cert en)) k4) (ih2 en k4 b4 (And.right (And.right p3))))
     '(change (LE.le (+ 1 (+ (cnodes (den chkf dec encTy n r1 (skels D) Sk.cert en)) (cnodes (den chkf dec encTy n r2 (skels D) Sk.cert en)))) k))
     '(omega)]))

;; --- branch lists --------------------------------------------------------------------

;; bnil : tBrs P NL.  The set quantifies over labels l with NL ≤ l < NL: none.
(case! 'F_bnil (into '[P :- Exp] ENV) (concl '(Exp.tBrs P (NL)) 'Exp.bnil)
  '[(intro l h1 h2) (have h3 (LE.le 100 l) h1) (have h4 (LT.lt l 100) h2) (exfalso) (omega)])

;; --- application -------------------------------------------------------------------

;; ⟦f u⟧ when skOf finds u's skeleton: the application clause of den_gen.clj.
(thm den_app_some [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                   f :- Exp, u :- Exp, G :- (List Sk), su :- Sk, sk :- Sk, en :- (HEnv G),
                   h :- (Eq (Option Sk) (skOf G u) (Option.some Sk su))]
  (Eq (Car sk) (den chkf dec encTy n (Exp.app f u) G sk en) ((den chkf dec encTy n f G (Sk.arr su sk) en) (den chkf dec encTy n u G su en)))
  (rw [(den_app_at chkf dec encTy n f u G sk en)])
  (change (Eq (Car sk) (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Car sk)) (dflt sk)
                         (fn [s2 :- Sk] ((den chkf dec encTy n f G (Sk.arr s2 sk) en) (den chkf dec encTy n u G s2 en))) (skOf G u))
                  ((den chkf dec encTy n f G (Sk.arr su sk) en) (den chkf dec encTy n u G su en))))
  (rw [h]))

;; The common opening of the App cases: from the premises, the facts the
;; substitution lemma needs (A, B well-formed; u skeleton-typed with skOf its
;; skeleton; skel u = Unit), then skel (B[u/x]) = skel B, ⟦f u⟧ = ⟦f⟧(⟦u⟧),
;; and V(B[u/x])η = V(B)(η, ⟦u⟧).
(def ^:private app-facts
  '[(have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
    (have hBS (SkJ Bool.true (List.cons Sk (skel A) (skels D)) B Sk.unit) (lemma25_tl_type chkf (List.cons Exp A D) B hB))
    (have hclA (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
    (have huS (SkJ Bool.false (skels D) u (skel A)) (lemma25_rt chkf D us2 u A hu))
    (have hsk (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A))) (skOf_rt chkf D us2 u A hu hclA))
    (have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false (skels D) u (skel A) huS rfl))
    (rw [(skel_subst1 u B hU)])
    (rw [(den_app_some chkf dec encTy n f u (skels D) (skel A) (skel B) en hsk)])
    (refine' (Iff.mpr (V_subst1 chkf dec encTy n (skels D) (skel A) B hBS u huS hsk en k _) _))])

(def ^:private app-params
  '[us1 :- (List U), us2 :- (List U), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
    hu :- (Rt chkf D us2 u A), hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)])

(def ^:private fu '((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) en) (den chkf dec encTy n u (skels D) (skel A) en)))
(def ^:private uen '(Prod.mk (den chkf dec encTy n u (skels D) (skel A) en) en))
(def ^:private ctxA '(List.cons Sk (skel A) (skels D)))

;; App at usage 1: Γ₁ + Γ₂ splits as k₁ + k₂ ≤ k; ⟦u⟧ ∈ V_k₂(A); the Π₁
;; clause at j = k₂ puts ⟦f⟧(⟦u⟧) in V_{k₁+k₂}(B), raised to k.
(case! 'F_app1
  (into app-params (into ['ihf :- (SND 'us1 'f '(Exp.tPi U.u1 A B)) 'ihu :- (SND 'us2 'u 'A)]
                         (conj ENV 'hs :- (ES '(vadd us1 (vscale U.u1 us2)) 'k))))
  (concl '(subst1 u B) '(Exp.app f u))
  (concat app-facts
    (split-steps 'hs 'us1 '(vscale U.u1 us2) 'k 'k1 'k2 'p)
    ['(have hs2 (EnvSat chkf dec encTy n D us2 en k2) (EnvSat_one chkf dec encTy n D us2 en k2 (And.right (And.right p))))
     '(have hkn (Nat.le (+ k1 k2) n) (Nat.le_trans (And.left p) hk))
     '(have hk1 (Nat.le k1 n) (Nat.le_trans (Nat.le_add_right k1 k2) hkn))
     '(have hk2 (Nat.le k2 n) (Nat.le_trans (Nat.le_add_left k2 k1) hkn))
     (list 'have 'hvf (list 'forall '[j Nat] (list '=> '(Nat.le (+ k1 j) n) (list 'forall '[a (Car (skel A))]
          (list '=> '(V chkf dec encTy n A (skels D) en j (skel A) a)
              (list 'V 'chkf 'dec 'encTy 'n 'B ctxA '(Prod.mk a en) '(+ k1 j) '(skel B)
                 '((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) en) a))))))
        '(ihf en k1 hk1 (And.left (And.right p))))
     '(have hvu (V chkf dec encTy n A (skels D) en k2 (skel A) (den chkf dec encTy n u (skels D) (skel A) en)) (ihu en k2 hk2 hs2))
     (list 'have 'hv (list 'V 'chkf 'dec 'encTy 'n 'B ctxA uen '(+ k1 k2) '(skel B) fu)
        '(hvf k2 hkn (den chkf dec encTy n u (skels D) (skel A) en) hvu))
     (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'B ctxA uen '(+ k1 k2) 'k '(skel B) fu '(And.left p) 'hk 'hv))]))

;; App at usage ω: Γ₂ is ω-scaled, so ⟦u⟧ ∈ V₀(A) (Lemma 3.5 ii), and the Πω
;; clause puts ⟦f⟧(⟦u⟧) in V_k₁(B), raised to k.
(case! 'F_appw
  (into app-params (into ['ihf :- (SND 'us1 'f '(Exp.tPi U.uw A B)) 'ihu :- (SND 'us2 'u 'A)]
                         (conj ENV 'hs :- (ES '(vadd us1 (vscale U.uw us2)) 'k))))
  (concl '(subst1 u B) '(Exp.app f u))
  (concat app-facts
    (split-steps 'hs 'us1 '(vscale U.uw us2) 'k 'k1 'k2 'p)
    ['(have hs2 (EnvSat chkf dec encTy n D us2 en 0) (EnvSat_omega chkf dec encTy n D us2 en k2 (And.right (And.right p))))
     '(have hk1k (Nat.le k1 k) (Nat.le_trans (Nat.le_add_right k1 k2) (And.left p)))
     '(have hk1 (Nat.le k1 n) (Nat.le_trans hk1k hk))
     (list 'have 'hvf (list 'forall '[a (Car (skel A))]
          (list '=> '(V chkf dec encTy n A (skels D) en 0 (skel A) a)
              (list 'V 'chkf 'dec 'encTy 'n 'B ctxA '(Prod.mk a en) 'k1 '(skel B)
                 '((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) en) a))))
        '(ihf en k1 hk1 (And.left (And.right p))))
     '(have hvu (V chkf dec encTy n A (skels D) en 0 (skel A) (den chkf dec encTy n u (skels D) (skel A) en)) (ihu en 0 (Nat.zero_le n) hs2))
     (list 'have 'hv (list 'V 'chkf 'dec 'encTy 'n 'B ctxA uen 'k1 '(skel B) fu)
        '(hvf (den chkf dec encTy n u (skels D) (skel A) en) hvu))
     (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'B ctxA uen 'k1 'k '(skel B) fu 'hk1k 'hk 'hv))]))

;; App at any nonzero usage, in the motive's form.  (After cases r, usage 0
;; is refuted by nonzero; the goals left are u1 then ω, checked with peek.
;; Each lemma is applied to its own goal: inside `first`, Ansatz's exact can
;; accept a term for the wrong goal, which the kernel then rejects.)
(thm F_app [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
            n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
            hu :- (Rt chkf D us2 u A), hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
  (forall [r U] (=> (Eq Bool (nonzero r) Bool.true)
    (Sound chkf dec encTy n D us1 f (Exp.tPi r A B)) (Sound chkf dec encTy n D us2 u A)
    (Sound chkf dec encTy n D (vadd us1 (vscale r us2)) (Exp.app f u) (subst1 u B))))
  (intro r) (cases r) (all_goals (intro hr ihf ihu en k hk hs))
  (all_goals (first (exact (Bool.noConfusion hr)) (skip)))
  (exact (F_app1 chkf dec encTy n D (List.nil U) us1 us2 f u A B hu hA hB ihf ihu en k hk hs))
  (exact (F_appw chkf dec encTy n D (List.nil U) us1 us2 f u A B hu hA hB ihf ihu en k hk hs)))
