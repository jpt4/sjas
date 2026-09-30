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

  - ElimBool: the motive at ⟦b⟧; each branch's IH at P[tt], P[ff] becomes
    V(P) at (tt, η), (ff, η) by V_subst1, and the scrutinee picks one.

  - Pair, at usage 1 and ω; Let, at every usage; App₀ and Pair₀; RecN (see
    the sections).
  - Var: in a well-formed context, the entry's value, read through the lift
    of its type by V_lift (vweaken.clj).

  Pending (they need Lemma 3.1/3.3's substitution or weakening clauses, 3.2,
  or the outer induction on n): Conv,
  CaseL, Bcons, RecS, ItR, H₁, Refl, Inspect."
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
            [lcert.formal.skof]
            [lcert.formal.vweaken]))

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

;; --- dependent elimination of booleans -------------------------------------------------

;; A property of a family of values g s, carried along an equality of
;; skeletons (the value's carrier depends on the skeleton, so rewriting a
;; hypothesis directly is not available).
(thm sk_transport [F :- (forall [s Sk] (=> (Car s) Prop)), g :- (forall [s Sk] (Car s)), s1 :- Sk, s2 :- Sk,
                   h :- (Eq Sk s1 s2), hf :- (F s1 (g s1))]
  (F s2 (g s2))
  (subst h) (exact hf))

;; Bool.rec under a predicate that may depend on the scrutinee.
(thm bool_rec_dep [α :- Type, Q :- (=> Bool α Prop), x :- α, y :- α, bv :- Bool, hx :- (Q Bool.false x), hy :- (Q Bool.true y)]
  (Q bv (Bool.rec$1 (fn [_ :- Bool] α) x y bv))
  (cases bv) (exact hx) (exact hy))

(thm den_tt_bool [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, G :- (List Sk), en :- (HEnv G)]
  (Eq Bool (den chkf dec encTy n Exp.tt G Sk.bool en) Bool.true)
  (rw [(den_tt_at chkf dec encTy n G Sk.bool en)]))

(thm den_ff_bool [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, G :- (List Sk), en :- (HEnv G)]
  (Eq Bool (den chkf dec encTy n Exp.ff G Sk.bool en) Bool.false)
  (rw [(den_ff_at chkf dec encTy n G Sk.bool en)]))

(def ^:private GB '(List.cons Sk Sk.bool (skels D)))
(defn- VPb "V(P) at (bv, η)." [bv v] (list 'V 'chkf 'dec 'encTy 'n 'P GB (list 'Prod.mk bv 'en) 'k '(skel P) v))

(defn- bool-branch
  "From a branch's IH at P[c/x] (c = tt or ff, typed by rule sc), V(P) at
  (⟦c⟧, η): the IH, its skeleton carried from skel (P[c/x]) to skel P, then
  V_subst1."
  [nm c dt ih sc]
  (let [h0 (symbol (str nm "0")) h1 (symbol (str nm "1")) h2 (symbol (str nm "2"))
        dv (fn [s] (list 'den 'chkf 'dec 'encTy 'n dt '(skels D) s 'en))
        Vc (fn [s v] (list 'V 'chkf 'dec 'encTy 'n (list 'subst1 c 'P) '(skels D) 'en 'k s v))]
    [(list 'have h0 (Vc (list 'skel (list 'subst1 c 'P)) (dv (list 'skel (list 'subst1 c 'P)))) (list ih 'en 'k 'hk 'hs2))
     (list 'have h1 (Vc '(skel P) (dv '(skel P)))
           (list 'sk_transport (list 'fn '[s :- Sk, v :- (Car s)] (Vc 's 'v)) (list 'fn '[s :- Sk] (dv 's))
                 (list 'skel (list 'subst1 c 'P)) '(skel P) (list 'skel_subst1 c 'P 'rfl) h0))
     (list 'have h2 (VPb (list 'den 'chkf 'dec 'encTy 'n c '(skels D) 'Sk.bool 'en) (dv '(skel P)))
           (list 'Iff.mp (list 'V_subst1 'chkf 'dec 'encTy 'n '(skels D) 'Sk.bool 'P 'hPS c (list sc '(skels D)) 'rfl 'en 'k (dv '(skel P))) h1))]))

;; ElimBool.  ⟦elimB P b t e⟧ = if ⟦b⟧ then ⟦t⟧ else ⟦e⟧, and the goal
;; V(P[b/x])η is V(P) at (⟦b⟧, η) (V_subst1); split Γ₁ + Γ₂ and use each
;; branch's IH, the value of ⟦b⟧ choosing.
(case! 'F_elimB
  (into '[us1 :- (List U), us2 :- (List U), P :- Exp, b :- Exp, t :- Exp, e :- Exp,
          hb :- (Rt chkf D us1 b Exp.tBool), hP :- (Tl chkf Bool.true (List.cons Exp Exp.tBool D) P Exp.tUnit)]
        (into ['iht :- (SND 'us2 't '(subst1 Exp.tt P)) 'ihe :- (SND 'us2 'e '(subst1 Exp.ff P))]
              (conj ENV 'hs :- (ES '(vadd us1 us2) 'k))))
  (concl '(subst1 b P) '(Exp.elimB P b t e))
  (concat
    ['(have hPS (SkJ Bool.true (List.cons Sk Sk.bool (skels D)) P Sk.unit) (lemma25_tl_type chkf (List.cons Exp Exp.tBool D) P hP))
     '(have hbS (SkJ Bool.false (skels D) b Sk.bool) (lemma25_rt chkf D us1 b Exp.tBool hb))
     '(have hbU (Eq Sk (skel b) Sk.unit) (skj_term_unit Bool.false (skels D) b Sk.bool hbS rfl))
     '(have hbk (Eq (Option Sk) (skOf (skels D) b) (Option.some Sk Sk.bool)) (skOf_rt chkf D us1 b Exp.tBool hb rfl))
     '(rw [(skel_subst1 b P hbU)])
     '(rw [(den_elimB_at chkf dec encTy n P b t e (skels D) (skel P) en)])
     '(refine' (Iff.mpr (V_subst1 chkf dec encTy n (skels D) Sk.bool P hPS b hbS hbk en k _) _))]
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'p)
    ['(have hle (Nat.le k2 k) (Nat.le_trans (Nat.le_add_left k2 k1) (And.left p)))
     '(have hs2 (EnvSat chkf dec encTy n D us2 en k) (EnvSat_mono chkf dec encTy n D us2 en k2 k hle (And.right (And.right p))))]
    (bool-branch 'vt 'Exp.tt 't 'iht 'SkJ.sTT)
    (bool-branch 've 'Exp.ff 'e 'ihe 'SkJ.sFF)
    [(list 'have 'vt3 (VPb 'Bool.true '(den chkf dec encTy n t (skels D) (skel P) en))
       (list 'Eq.mp (list 'congrArg (list 'fn '[bv :- Bool] (VPb 'bv '(den chkf dec encTy n t (skels D) (skel P) en)))
                          '(den_tt_bool chkf dec encTy n (skels D) en)) 'vt2))
     (list 'have 've3 (VPb 'Bool.false '(den chkf dec encTy n e (skels D) (skel P) en))
       (list 'Eq.mp (list 'congrArg (list 'fn '[bv :- Bool] (VPb 'bv '(den chkf dec encTy n e (skels D) (skel P) en)))
                          '(den_ff_bool chkf dec encTy n (skels D) en)) 've2))
     (list 'exact (list 'bool_rec_dep '(Car (skel P)) (list 'fn '[bv :- Bool, v :- (Car (skel P))] (VPb 'bv 'v))
                        '(den chkf dec encTy n e (skels D) (skel P) en) '(den chkf dec encTy n t (skels D) (skel P) en)
                        '(den chkf dec encTy n b (skels D) Sk.bool en) 've3 'vt3))]))

;; --- variables -------------------------------------------------------------------

;; Var.  ⟦var i⟧η = lookup i η, and its type is the i-th entry's, lifted past
;; the entries before it: lift (i+1) 0 A.  By induction on the context: at
;; the head, V_lift reads V(lift 1 0 A) at (a, η′) as V(A) at η′, where the
;; entry's footprint condition puts a (usage nonzero); deeper, the IH at the
;; tail, lift (i+2) 0 A = lift 1 0 (lift (i+1) 0 A) (lift_comp), and V_lift
;; again.  V_lift needs the lifted type well-formed (var_wf), which is why
;; the lemma assumes a well-formed context (WFCtx).  Footprints are raised to
;; k by V_mono.  The cap is named cap here (Nat.succ's field is n).
;; The i-th entry's type, lifted past the entries before it, is well-formed.
(thm var_wf [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (=> (WFCtx chkf D) (forall [i Nat] (forall [A Exp] (=> (Eq (Option Exp) (nthE D i) (Option.some Exp A))
    (SkJ Bool.true (skels D) (lift (+ i 1) 0 A) Sk.unit)))))
  (induction D)
  (intro hw i A h) (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  (intro hw i) (cases i) (all_goals (intro A h))
  (have hX (SkJ Bool.true (skels tail) (lift (+ n 1) 0 A) Sk.unit) (ih_tail (And.right hw) n A (Eq.trans (Eq.symm (nthE.eq_3 head tail n)) h)))
  (have hW (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) (lift 1 0 (lift (+ n 1) 0 A)) Sk.unit)
    (skj_weaken Bool.true (skels tail) (lift (+ n 1) 0 A) Sk.unit hX 0 (skel head)))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) e Sk.unit)) (lift_comp A 1 (+ n 1) 0)) hW))
  (have e (Eq Exp head A) (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h)))
  (exact (Eq.mp (congrArg (fn [X :- Exp] (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) (lift 1 0 X) Sk.unit)) e)
                (skj_weaken Bool.true (skels tail) head Sk.unit (lemma25_tl_type chkf tail head (And.left hw)) 0 (skel head)))))

(thm entry_nz [r :- U, j :- Nat, P :- (=> Nat Prop)]
  (=> (Eq Bool (nonzero r) Bool.true) (EntryOK r j P) (Exists (fn [j2 :- Nat] (And (Nat.le j2 j) (P j2)))))
  (cases r) (all_goals (intro hr he))
  ;; ω: footprint 0, value in V₀
  (have hw2 (And (Eq Nat j 0) (P 0)) he) (constructor) (exact 0) (constructor) (exact (Nat.zero_le j)) (exact (And.right hw2))
  ;; 1: footprint j
  (have h12 (P j) he) (constructor) (exact j) (constructor) (exact (Nat.le_refl j)) (exact h12)
  ;; 0: excluded
  (exact (Bool.noConfusion hr)))

(thm some_injU [x :- U, y :- U, h :- (Eq (Option U) (Option.some U x) (Option.some U y))] (Eq U x y) (cases h) (rfl))
(def ^:private CP '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat])
(defn- VAR-GOAL [D i A en k]
  (list 'V 'chkf 'dec 'encTy 'cap (list 'lift (list '+ i 1) 0 A) (list 'skels D) en k (list 'skel (list 'lift (list '+ i 1) 0 A))
        (list 'lookup (list 'skels D) i (list 'skel (list 'lift (list '+ i 1) 0 A)) en)))
(defn- VAR-MOTIVE [D]
  (list '=> (list 'WFCtx 'chkf D) (list 'forall '[i Nat] (list 'forall '[A Exp] (list '=> (list 'Eq '(Option Exp) (list 'nthE D 'i) '(Option.some Exp A))
    (list 'forall '[us (List U)] (list 'forall '[r U] (list 'forall ['en (list 'HEnv (list 'skels D))] (list 'forall '[k Nat]
      (list '=> '(Eq (Option U) (nthU us i) (Option.some U r)) '(Eq Bool (nonzero r) Bool.true) '(Nat.le k cap)
            (list 'EnvSat 'chkf 'dec 'encTy 'cap D 'us 'en 'k)
            (VAR-GOAL D 'i 'A 'en 'k)))))))))))
(def ^:private CT '(List.cons Exp head tail))
(def ^:private GT '(List.cons Sk (skel head) (skels tail)))
(def ^:private X '(lift (+ m 1) 0 A))
(defn- EOK [r0 jj] (list 'EntryOK r0 jj (list 'fn '[jx :- Nat] '(V chkf dec encTy cap head (skels tail) (Prod.snd en) jx (skel head) (Prod.fst en)))))
(defn- cons-open [r0 us2]
  [(list 'have 'hs2 (list 'Exists (list 'fn '[j :- Nat] (list 'Exists (list 'fn '[kk :- Nat]
      (list 'And '(Nat.le (+ j kk) k) (list 'And (list 'EnvSat 'chkf 'dec 'encTy 'cap 'tail us2 '(Prod.snd en) 'kk) (EOK r0 'j))))))) 'hs)
   '(refine' (exN _ _ hs2 _)) '(intro j hj) '(refine' (exN _ _ hj _)) '(intro kk hkk)
   (list 'have 'p (list 'And '(Nat.le (+ j kk) k) (list 'And (list 'EnvSat 'chkf 'dec 'encTy 'cap 'tail us2 '(Prod.snd en) 'kk) (EOK r0 'j))) 'hkk)])
(eval (list* 'lcert.formal.base/thm 'var_succ
  (into CP ['head :- 'Exp 'tail :- '(List Exp) 'm :- 'Nat 'A :- 'Exp 'r0 :- 'U 'us2 :- '(List U) 'r :- 'U
            'en :- (list 'HEnv (list 'skels CT)) 'k :- 'Nat
            'hw :- (list 'WFCtx 'chkf CT) 'ih :- (VAR-MOTIVE 'tail)
            'h :- (list 'Eq '(Option Exp) (list 'nthE CT '(Nat.succ m)) '(Option.some Exp A))
            'hu :- '(Eq (Option U) (nthU (List.cons U r0 us2) (Nat.succ m)) (Option.some U r)) 'hr :- '(Eq Bool (nonzero r) Bool.true)
            'hk :- '(Nat.le k cap) 'hs :- (list 'EnvSat 'chkf 'dec 'encTy 'cap CT '(List.cons U r0 us2) 'en 'k)])
  (VAR-GOAL CT '(Nat.succ m) 'A 'en 'k)
  (concat
    ['(have hA2 (Eq (Option Exp) (nthE tail m) (Option.some Exp A)) (Eq.trans (Eq.symm (nthE.eq_3 head tail m)) h))
     '(have hu2 (Eq (Option U) (nthU us2 m) (Option.some U r)) (Eq.trans (Eq.symm (nthU.eq_3 r0 us2 m)) hu))
     (list 'have 'hX (list 'SkJ 'Bool.true '(skels tail) X 'Sk.unit) '(var_wf chkf tail (And.right hw) m A hA2))]
    (cons-open 'r0 'us2)
    ['(have hkk2 (Nat.le kk k) (Nat.le_trans (Nat.le_add_left kk j) (And.left p)))
     '(have hkkc (Nat.le kk cap) (Nat.le_trans hkk2 hk))
     (list 'have 'hv (VAR-GOAL 'tail 'm 'A '(Prod.snd en) 'kk) '(ih (And.right hw) m A hA2 us2 r (Prod.snd en) kk hu2 hr hkkc (And.left (And.right p))))
     (list 'change (list 'V 'chkf 'dec 'encTy 'cap '(lift (+ (+ m 1) 1) 0 A) GT 'en 'k '(skel (lift (+ (+ m 1) 1) 0 A))
                         (list 'lookup GT '(+ m 1) '(skel (lift (+ (+ m 1) 1) 0 A)) 'en)))
     '(rw [(Eq.symm (lift_comp A 1 (+ m 1) 0))])
     (list 'rw [(list 'skel_lift X 1 0)])
     (list 'have 'hvk (list 'V 'chkf 'dec 'encTy 'cap X '(skels tail) '(Prod.snd en) 'k (list 'skel X) (list 'lookup '(skels tail) 'm (list 'skel X) '(Prod.snd en)))
       (list 'V_mono 'chkf 'dec 'encTy 'cap X '(skels tail) '(Prod.snd en) 'kk 'k (list 'skel X) (list 'lookup '(skels tail) 'm (list 'skel X) '(Prod.snd en)) 'hkk2 'hk 'hv))
     (list 'exact (list 'Eq.mpr (list 'V_lift 'chkf 'dec 'encTy 'cap '(skels tail) X 'hX 0 '(skel head) '(Prod.snd en) '(Prod.fst en) 'k
                                      (list 'lookup '(skels tail) 'm (list 'skel X) '(Prod.snd en))) 'hvk))])))

(eval (list* 'lcert.formal.base/thm 'var_zero
  (into CP ['head :- 'Exp 'tail :- '(List Exp) 'A :- 'Exp 'r0 :- 'U 'us2 :- '(List U) 'r :- 'U
            'en :- (list 'HEnv (list 'skels CT)) 'k :- 'Nat
            'hw :- (list 'WFCtx 'chkf CT)
            'h :- (list 'Eq '(Option Exp) (list 'nthE CT 'Nat.zero) '(Option.some Exp A))
            'hu :- '(Eq (Option U) (nthU (List.cons U r0 us2) Nat.zero) (Option.some U r)) 'hr :- '(Eq Bool (nonzero r) Bool.true)
            'hk :- '(Nat.le k cap) 'hs :- (list 'EnvSat 'chkf 'dec 'encTy 'cap CT '(List.cons U r0 us2) 'en 'k)])
  (VAR-GOAL CT 'Nat.zero 'A 'en 'k)
  (concat
    ['(have e (Eq Exp head A) (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h)))
     '(have er (Eq U r0 r) (some_injU r0 r (Eq.trans (Eq.symm (nthU.eq_2 r0 us2)) hu)))
     '(have hr0 (Eq Bool (nonzero r0) Bool.true) (Eq.mp (congrArg (fn [q :- U] (Eq Bool (nonzero q) Bool.true)) (Eq.symm er)) hr))
     '(rw [(Eq.symm e)])
     (list 'change (list 'V 'chkf 'dec 'encTy 'cap '(lift 1 0 head) GT 'en 'k '(skel (lift 1 0 head)) (list 'lookup GT 0 '(skel (lift 1 0 head)) 'en)))
     '(rw [(skel_lift head 1 0)])
     (list 'change (list 'V 'chkf 'dec 'encTy 'cap '(lift 1 0 head) GT 'en 'k '(skel head) '(coe (skel head) (skel head) (Prod.fst en))))
     '(rw [(coe_self (skel head) (Prod.fst en))])]
    (cons-open 'r0 'us2)
    ['(have he (Exists (fn [j2 :- Nat] (And (Nat.le j2 j) (V chkf dec encTy cap head (skels tail) (Prod.snd en) j2 (skel head) (Prod.fst en)))))
        (entry_nz r0 j (fn [jx :- Nat] (V chkf dec encTy cap head (skels tail) (Prod.snd en) jx (skel head) (Prod.fst en))) hr0 (And.right (And.right p))))
     '(refine' (exN _ _ he _)) '(intro j2 hj2)
     '(have q (And (Nat.le j2 j) (V chkf dec encTy cap head (skels tail) (Prod.snd en) j2 (skel head) (Prod.fst en))) hj2)
     '(have hjk (LE.le j2 k) (Nat.le_trans (And.left q) (Nat.le_trans (Nat.le_add_right j kk) (And.left p))))
     '(have hvk (V chkf dec encTy cap head (skels tail) (Prod.snd en) k (skel head) (Prod.fst en))
        (V_mono chkf dec encTy cap head (skels tail) (Prod.snd en) j2 k (skel head) (Prod.fst en) hjk hk (And.right q)))
     '(exact (Eq.mpr (V_lift chkf dec encTy cap (skels tail) head (lemma25_tl_type chkf tail head (And.left hw)) 0 (skel head)
                             (Prod.snd en) (Prod.fst en) k (Prod.fst en)) hvk))])))

(eval (list 'lcert.formal.base/thm 'var_sem (conj CP 'D :- '(List Exp))
  (VAR-MOTIVE 'D)
  '(induction D)
  '(intro hw i A h) '(exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  '(intro hw i) '(cases i) '(all_goals (intro A h us)) '(all_goals (cases us)) '(all_goals (intro r en k hu hr hk hs))
  '(exact (var_succ chkf dec encTy cap head tail _ A _ _ r en k hw ih_tail h hu hr hk hs))
  '(exact (False.elim hs))
  '(exact (var_zero chkf dec encTy cap head tail A _ _ r en k hw h hu hr hk hs))
  '(exact (False.elim hs))))

(case! 'F_var
  (into '[i :- Nat, A :- Exp, r :- U, hwf :- (WFCtx chkf D), hA :- (Eq (Option Exp) (nthE D i) (Option.some Exp A)),
          hu :- (Eq (Option U) (nthU us i) (Option.some U r)), hr :- (Eq Bool (nonzero r) Bool.true)]
        (conj ENV 'hs :- (ES 'us 'k)))
  (concl '(lift (+ i 1) 0 A) '(Exp.var i))
  '[(rw [(den_var_at chkf dec encTy n i (skels D) (skel (lift (+ i 1) 0 A)) en)])
    (exact (var_sem chkf dec encTy n D hwf i A hA us r en k hu hr hk hs))])

;; --- pairs ---------------------------------------------------------------------------

(thm le_sub_add [a :- Nat, b :- Nat, c :- Nat, h :- (LE.le (+ a b) c)] (LE.le b (- c a)) (omega))

;; Pair at usage 1 and ω.  ⟦pair x y⟧ = (⟦x⟧, ⟦y⟧) at the product skeleton.
;; y's IH at B[x/y] becomes V(B) at (⟦x⟧, η) (sk_transport, V_subst1; skOf x
;; is its skeleton by skOf_rt).  At usage 1 the Σ₁ clause splits the
;; footprint as k₁ + (k − k₁), k₁ the first component's; at ω the first
;; component is in V₀ (Lemma 3.5 ii).
(def ^:private ydB '(den chkf dec encTy n y (skels D) (skel B) en))
(def ^:private xdA '(den chkf dec encTy n x (skels D) (skel A) en))
(defn- yB-steps [kk]
  ;; from the IH for y at footprint kk: V(B) at (⟦x⟧, η)
  [(list 'have 'vy0 (list 'V 'chkf 'dec 'encTy 'n '(subst1 x B) '(skels D) 'en kk '(skel (subst1 x B)) '(den chkf dec encTy n y (skels D) (skel (subst1 x B)) en))
         (list 'ihy 'en kk 'hky 'hsy))
   (list 'have 'vy1 (list 'V 'chkf 'dec 'encTy 'n '(subst1 x B) '(skels D) 'en kk '(skel B) ydB)
         (list 'sk_transport (list 'fn '[s :- Sk, v :- (Car s)] (list 'V 'chkf 'dec 'encTy 'n '(subst1 x B) '(skels D) 'en kk 's 'v))
               '(fn [s :- Sk] (den chkf dec encTy n y (skels D) s en)) '(skel (subst1 x B)) '(skel B) '(skel_subst1 x B hxU) 'vy0))
   (list 'have 'vy2 (list 'V 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk xdA 'en) kk '(skel B) ydB)
         (list 'Iff.mp (list 'V_subst1 'chkf 'dec 'encTy 'n '(skels D) '(skel A) 'B 'hBS 'x 'hxS 'hxk 'en kk ydB) 'vy1))])
(def ^:private pair-facts
  '[(have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
    (have hBS (SkJ Bool.true (List.cons Sk (skel A) (skels D)) B Sk.unit) (lemma25_tl_type chkf (List.cons Exp A D) B hB))
    (have hclA (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
    (have hxS (SkJ Bool.false (skels D) x (skel A)) (lemma25_rt chkf D us1 x A hx))
    (have hxk (Eq (Option Sk) (skOf (skels D) x) (Option.some Sk (skel A))) (skOf_rt chkf D us1 x A hx hclA))
    (have hxU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false (skels D) x (skel A) hxS rfl))])
(eval (list* 'lcert.formal.base/thm 'F_pair1
  (into P6 (into '[us1 :- (List U), us2 :- (List U), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
                   hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit), hx :- (Rt chkf D us1 x A)]
                 (into ['ihx :- (SND 'us1 'x 'A) 'ihy :- (SND 'us2 'y '(subst1 x B))]
                       (conj ENV 'hs :- (ES '(vadd (vscale U.u1 us1) us2) 'k)))))
  (concl '(Exp.tSig U.u1 A B) '(Exp.pair (Exp.tSig U.u1 A B) x y))
  (concat pair-facts
    ['(rw [(den_pair_at chkf dec encTy n (Exp.tSig U.u1 A B) x y (skels D) (skel (Exp.tSig U.u1 A B)) en)])]
    (split-steps 'hs '(vscale U.u1 us1) 'us2 'k 'k1 'k2 'p)
    ['(have hsx (EnvSat chkf dec encTy n D us1 en k1) (EnvSat_one chkf dec encTy n D us1 en k1 (And.left (And.right p))))
     '(have hsy (EnvSat chkf dec encTy n D us2 en k2) (And.right (And.right p)))
     '(have hkn (Nat.le (+ k1 k2) n) (Nat.le_trans (And.left p) hk))
     '(have hkx (Nat.le k1 n) (Nat.le_trans (Nat.le_add_right k1 k2) hkn))
     '(have hky (Nat.le k2 n) (Nat.le_trans (Nat.le_add_left k2 k1) hkn))
     (list 'have 'vx (list 'V 'chkf 'dec 'encTy 'n 'A '(skels D) 'en 'k1 '(skel A) xdA) '(ihx en k1 hkx hsx))]
    (yB-steps 'k2)
    ['(have o1 (LE.le (+ k1 k2) k) (And.left p))
     '(have hle (LE.le k2 (- k k1)) (le_sub_add k1 k2 k o1))
     (list 'change (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j k)
        (list 'And (list 'V 'chkf 'dec 'encTy 'n 'A '(skels D) 'en 'j '(skel A) xdA)
                   (list 'V 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk xdA 'en) '(- k j) '(skel B) ydB))))))
     '(constructor) '(exact k1) '(constructor) '(exact (Nat.le_trans (Nat.le_add_right k1 k2) (And.left p))) '(constructor) '(exact vx)
     (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk xdA 'en) 'k2 '(- k k1) '(skel B) ydB
                        'hle '(Nat.le_trans (Nat.sub_le k k1) hk) 'vy2))])))

(eval (list* 'lcert.formal.base/thm 'F_pairw
  (into P6 (into '[us1 :- (List U), us2 :- (List U), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
                   hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit), hx :- (Rt chkf D us1 x A)]
                 (into ['ihx :- (SND 'us1 'x 'A) 'ihy :- (SND 'us2 'y '(subst1 x B))]
                       (conj ENV 'hs :- (ES '(vadd (vscale U.uw us1) us2) 'k)))))
  (concl '(Exp.tSig U.uw A B) '(Exp.pair (Exp.tSig U.uw A B) x y))
  (concat pair-facts
    ['(rw [(den_pair_at chkf dec encTy n (Exp.tSig U.uw A B) x y (skels D) (skel (Exp.tSig U.uw A B)) en)])]
    (split-steps 'hs '(vscale U.uw us1) 'us2 'k 'k1 'k2 'p)
    ['(have hsx (EnvSat chkf dec encTy n D us1 en 0) (EnvSat_omega chkf dec encTy n D us1 en k1 (And.left (And.right p))))
     '(have hsy (EnvSat chkf dec encTy n D us2 en k2) (And.right (And.right p)))
     '(have hk2k (Nat.le k2 k) (Nat.le_trans (Nat.le_add_left k2 k1) (And.left p)))
     '(have hky (Nat.le k2 n) (Nat.le_trans hk2k hk))
     (list 'have 'vx (list 'V 'chkf 'dec 'encTy 'n 'A '(skels D) 'en 0 '(skel A) xdA) '(ihx en 0 (Nat.zero_le n) hsx))]
    (yB-steps 'k2)
    [(list 'change (list 'And (list 'V 'chkf 'dec 'encTy 'n 'A '(skels D) 'en 0 '(skel A) xdA)
                              (list 'V 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk xdA 'en) 'k '(skel B) ydB)))
     '(constructor) '(exact vx)
     (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk xdA 'en) 'k2 'k '(skel B) ydB 'hk2k 'hk 'vy2))])))

;; Pair at any nonzero usage, in the motive's form (goals: u1, then ω).
(thm F_pair [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
             n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
             hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit), hx :- (Rt chkf D us1 x A)]
  (forall [r U] (=> (Eq Bool (nonzero r) Bool.true)
    (Sound chkf dec encTy n D us1 x A) (Sound chkf dec encTy n D us2 y (subst1 x B))
    (Sound chkf dec encTy n D (vadd (vscale r us1) us2) (Exp.pair (Exp.tSig r A B) x y) (Exp.tSig r A B))))
  (intro r) (cases r) (all_goals (intro hr ihx ihy en k hk hs))
  (all_goals (first (exact (Bool.noConfusion hr)) (skip)))
  (exact (F_pair1 chkf dec encTy n D (List.nil U) us1 us2 A B x y hA hB hx ihx ihy en k hk hs))
  (exact (F_pairw chkf dec encTy n D (List.nil U) us1 us2 A B x y hA hB hx ihx ihy en k hk hs)))

;; --- let ---------------------------------------------------------------------------

;; Let at usage r.  ⟦let (x, y) = p in t⟧ = ⟦t⟧ at (⟦p⟧₂, (⟦p⟧₁, η)), since
;; skOf p is p's Σ skeleton (skOf_rt).  Γ₁ + Γ₂ splits as k₁ + k₂; p's IH puts
;; ⟦p⟧ in V_k₁(Σ r A B), whose clause gives the footprints of the two new
;; entries (usage 1: j and k₁ − j; ω and 0: 0 and k₁); with Γ₂'s k₂ they
;; satisfy the extended context (EnvSat_cons), within k.  t's IH, at
;; lift 2 0 C, is read back as V(C) by V_lift2_fam and raised to k.

(thm le_let [j :- Nat, k1 :- Nat, k2 :- Nat, k :- Nat, h1 :- (LE.le j k1), h2 :- (LE.le (+ k1 k2) k)] (LE.le (+ (- k1 j) (+ j k2)) k) (omega))

(thm den_letp_some [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                      C :- Exp, p :- Exp, t :- Exp, G :- (List Sk), sa :- Sk, sb :- Sk, sk :- Sk, en :- (HEnv G),
                      h :- (Eq (Option Sk) (skOf G p) (Option.some Sk (Sk.prod sa sb)))]
  (Eq (Car sk) (den chkf dec encTy n (Exp.letp C p t) G sk en)
     (den chkf dec encTy n t (List.cons Sk sb (List.cons Sk sa G)) sk
          (Prod.mk (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) en)) (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) en)) en))))
  (rw [(den_letp_at chkf dec encTy n C p t G sk en)])
  (change (Eq (Car sk) (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Car sk)) (dflt sk)
                         (fn [sp :- Sk] (splitProd sk sp (den chkf dec encTy n p G sp en)
                            (fn [a :- Sk, b :- Sk, va :- (Car a), vb :- (Car b)] (den chkf dec encTy n t (sk2 b a G) sk (Prod.mk vb (Prod.mk va en))))))
                         (skOf G p))
                  (den chkf dec encTy n t (List.cons Sk sb (List.cons Sk sa G)) sk
                       (Prod.mk (Prod.snd (den chkf dec encTy n p G (Sk.prod sa sb) en)) (Prod.mk (Prod.fst (den chkf dec encTy n p G (Sk.prod sa sb) en)) en)))))
  (rw [h]))

(thm EnvSat_cons [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    A :- Exp, D :- (List Exp), r :- U, us :- (List U), a :- (Car (skel A)), en :- (HEnv (skels D)), j :- Nat, kk :- Nat,
                    hs :- (EnvSat chkf dec encTy n D us en kk),
                    he :- (EntryOK r j (fn [jx :- Nat] (V chkf dec encTy n A (skels D) en jx (skel A) a)))]
  (EnvSat chkf dec encTy n (List.cons Exp A D) (List.cons U r us) (Prod.mk a en) (+ j kk))
  (constructor) (exact j) (constructor) (exact kk) (constructor) (exact (Nat.le_refl (+ j kk))) (constructor) (exact hs) (exact he))

(def ^:private pv '(den chkf dec encTy n p (skels D) (Sk.prod (skel A) (skel B)) en))
(def ^:private va (list 'Prod.fst pv))
(def ^:private vb (list 'Prod.snd pv))
(def ^:private E (list 'Prod.mk vb (list 'Prod.mk va 'en)))
(def ^:private G3 '(List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))))
(defn- VA [jj] (list 'V 'chkf 'dec 'encTy 'n 'A '(skels D) 'en jj '(skel A) va))
(defn- VB [jj] (list 'V 'chkf 'dec 'encTy 'n 'B '(List.cons Sk (skel A) (skels D)) (list 'Prod.mk va 'en) jj '(skel B) vb))
(defn- let-open [r]
  [(list 'have 'hSS (list 'SkJ 'Bool.true '(skels D) (list 'Exp.tSig r 'A 'B) 'Sk.unit)
         (list 'SkJ.wSig '(skels D) r 'A 'B '(lemma25_tl_type chkf D A hA) '(lemma25_tl_type chkf (List.cons Exp A D) B hB)))
   (list 'have 'hpk (list 'Eq '(Option Sk) '(skOf (skels D) p) '(Option.some Sk (Sk.prod (skel A) (skel B))))
         (list 'skOf_rt 'chkf 'D 'us1 'p (list 'Exp.tSig r 'A 'B) 'hp (list 'skj_clean 'Bool.true '(skels D) (list 'Exp.tSig r 'A 'B) 'Sk.unit 'hSS)))
   '(rw [(den_letp_some chkf dec encTy n C p t (skels D) (skel A) (skel B) (skel C) en hpk)])
   '(have hCS (SkJ Bool.true (skels D) C Sk.unit) (lemma25_tl_type chkf D C hC))])
(defn- let-close [r K hKk]
  ;; the body's IH at the extended environment and footprint K, read back through lift 2
  [(list 'have 'vt (list 'V 'chkf 'dec 'encTy 'n '(lift 2 0 C) G3 E K '(skel (lift 2 0 C)) (list 'den 'chkf 'dec 'encTy 'n 't G3 '(skel (lift 2 0 C)) E))
         (list 'iht E K '(Nat.le_trans hKk hk) 'hES))
   (list 'have 'vt2 (list 'V 'chkf 'dec 'encTy 'n 'C '(skels D) 'en K '(skel C) (list 'den 'chkf 'dec 'encTy 'n 't G3 '(skel C) E))
         (list 'V_lift2_fam 'chkf 'dec 'encTy 'n '(skels D) 'C 'hCS '(skel A) '(skel B) 'en va vb K
               (list 'fn '[s :- Sk] (list 'den 'chkf 'dec 'encTy 'n 't G3 's E)) 'vt))
   (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'C '(skels D) 'en K 'k '(skel C) (list 'den 'chkf 'dec 'encTy 'n 't G3 '(skel C) E) hKk 'hk 'vt2))])
(defn- let-thm [nm r]
  (list 'lcert.formal.base/thm nm
    (into P6 (into ['us1 :- '(List U) 'us2 :- '(List U) 'A :- 'Exp 'B :- 'Exp 'C :- 'Exp 'p :- 'Exp 't :- 'Exp
                    'hp :- (list 'Rt 'chkf 'D 'us1 'p (list 'Exp.tSig r 'A 'B)) 'hC :- '(Tl chkf Bool.true D C Exp.tUnit)
                    'hA :- '(Tl chkf Bool.true D A Exp.tUnit) 'hB :- '(Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)
                    'ihp :- (SND 'us1 'p (list 'Exp.tSig r 'A 'B))
                    'iht :- (list 'Sound 'chkf 'dec 'encTy 'n '(List.cons Exp B (List.cons Exp A D)) (list 'List.cons 'U 'U.u1 (list 'List.cons 'U r 'us2)) 't '(lift 2 0 C))]
                   (conj ENV 'hs :- (ES '(vadd us1 us2) 'k))))
    (concl 'C '(Exp.letp C p t))))
(def ^:private let1-tactics
  (concat (let-open 'U.u1)
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'q)
    ['(have hk1 (Nat.le k1 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_right k1 k2) (And.left q)) hk))
     (list 'have 'vp (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j k1) (list 'And (VA 'j) (VB '(- k1 j))))))
           '(ihp en k1 hk1 (And.left (And.right q))))
     '(refine' (exN _ _ vp _)) '(intro j hj)
     (list 'have 'hq (list 'And '(Nat.le j k1) (list 'And (VA 'j) (VB '(- k1 j)))) 'hj)
     '(have hKk (LE.le (+ (- k1 j) (+ j k2)) k) (le_let j k1 k2 k (And.left hq) (And.left q)))
     (list 'have 'hES (list 'EnvSat 'chkf 'dec 'encTy 'n '(List.cons Exp B (List.cons Exp A D)) '(List.cons U U.u1 (List.cons U U.u1 us2)) E '(+ (- k1 j) (+ j k2)))
       (list 'EnvSat_cons 'chkf 'dec 'encTy 'n 'B '(List.cons Exp A D) 'U.u1 '(List.cons U U.u1 us2) vb (list 'Prod.mk va 'en) '(- k1 j) '(+ j k2)
             (list 'EnvSat_cons 'chkf 'dec 'encTy 'n 'A 'D 'U.u1 'us2 va 'en 'j 'k2 '(And.right (And.right q)) '(And.left (And.right hq)))
             '(And.right (And.right hq))))]
    (let-close 'U.u1 '(+ (- k1 j) (+ j k2)) 'hKk)))
(eval (concat (let-thm 'F_let1 'U.u1) let1-tactics))

(thm le_let0 [k1 :- Nat, k2 :- Nat, k :- Nat, h :- (LE.le (+ k1 k2) k)] (LE.le (+ k1 (+ 0 k2)) k) (omega))
(defn- let0-tactics [r vp-type entry]
  (concat (let-open r)
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'q)
    ['(have hk1 (Nat.le k1 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_right k1 k2) (And.left q)) hk))
     (list 'have 'vp vp-type '(ihp en k1 hk1 (And.left (And.right q))))
     '(have hKk (LE.le (+ k1 (+ 0 k2)) k) (le_let0 k1 k2 k (And.left q)))
     (list 'have 'hES (list 'EnvSat 'chkf 'dec 'encTy 'n '(List.cons Exp B (List.cons Exp A D)) (list 'List.cons 'U 'U.u1 (list 'List.cons 'U r 'us2)) E '(+ k1 (+ 0 k2)))
       (list 'EnvSat_cons 'chkf 'dec 'encTy 'n 'B '(List.cons Exp A D) 'U.u1 (list 'List.cons 'U r 'us2) vb (list 'Prod.mk va 'en) 'k1 '(+ 0 k2)
             (list 'EnvSat_cons 'chkf 'dec 'encTy 'n 'A 'D r 'us2 va 'en 0 'k2 '(And.right (And.right q)) entry)
             (if (= r 'U.uw) '(And.right vp) 'vp)))]
    (let-close r '(+ k1 (+ 0 k2)) 'hKk)))
(eval (concat (let-thm 'F_letw 'U.uw) (let0-tactics 'U.uw (list 'And (VA 0) (VB 'k1)) '(And.intro rfl (And.left vp)))))
(eval (concat (let-thm 'F_let0 'U.u0) (let0-tactics 'U.u0 (VB 'k1) 'rfl)))

;; Let at any usage, in the motive's form.  (Goals after cases r: ω, 1, 0.)
(thm F_let [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
            n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U), A :- Exp, B :- Exp, C :- Exp, p :- Exp, t :- Exp,
            hC :- (Tl chkf Bool.true D C Exp.tUnit), hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
  (forall [r U] (=> (Rt chkf D us1 p (Exp.tSig r A B)) (Sound chkf dec encTy n D us1 p (Exp.tSig r A B))
    (Sound chkf dec encTy n (List.cons Exp B (List.cons Exp A D)) (List.cons U U.u1 (List.cons U r us2)) t (lift 2 0 C))
    (Sound chkf dec encTy n D (vadd us1 us2) (Exp.letp C p t) C)))
  (intro r) (cases r) (all_goals (intro hp ihp iht en k hk hs))
  (exact (F_letw chkf dec encTy n D (List.nil U) us1 us2 A B C p t hp hC hA hB ihp iht en k hk hs))
  (exact (F_let1 chkf dec encTy n D (List.nil U) us1 us2 A B C p t hp hC hA hB ihp iht en k hk hs))
  (exact (F_let0 chkf dec encTy n D (List.nil U) us1 us2 A B C p t hp hC hA hB ihp iht en k hk hs)))

;; --- usage-0 application and pairs --------------------------------------------------

;; App₀: u is type-level (Tl), so its skOf comes from skOf_tl; the Π₀ clause
;; of f's IH holds at every argument, in particular ⟦u⟧, at the same footprint.
(case! 'F_app0
  (into '[f :- Exp, u :- Exp, A :- Exp, B :- Exp,
          hu :- (Tl chkf Bool.false D u A), hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
        (into ['ihf :- (SND 'us 'f '(Exp.tPi U.u0 A B))] (conj ENV 'hs :- (ES 'us 'k))))
  (concl '(subst1 u B) '(Exp.app f u))
  '[(have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
    (have hBS (SkJ Bool.true (List.cons Sk (skel A) (skels D)) B Sk.unit) (lemma25_tl_type chkf (List.cons Exp A D) B hB))
    (have hclA (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
    (have huS (SkJ Bool.false (skels D) u (skel A)) (lemma25_tl_term chkf D u A hu))
    (have hsk (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A))) (skOf_tl chkf Bool.false D u A hu rfl hclA))
    (have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false (skels D) u (skel A) huS rfl))
    (rw [(skel_subst1 u B hU)])
    (rw [(den_app_some chkf dec encTy n f u (skels D) (skel A) (skel B) en hsk)])
    (refine' (Iff.mpr (V_subst1 chkf dec encTy n (skels D) (skel A) B hBS u huS hsk en k _) _))
    (have hvf (forall [a (Car (skel A))] (V chkf dec encTy n B (List.cons Sk (skel A) (skels D)) (Prod.mk a en) k (skel B)
                 ((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) en) a)))
      (ihf en k hk hs))
    (exact (hvf (den chkf dec encTy n u (skels D) (skel A) en)))])

;; Pair₀: the Σ₀ clause asks only for the second component, in V(B) at
;; (⟦x⟧, η); x is type-level.
(case! 'F_pair0
  (into '[A :- Exp, B :- Exp, x :- Exp, y :- Exp,
          hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit), hx :- (Tl chkf Bool.false D x A)]
        (into ['ihy :- (SND 'us 'y '(subst1 x B))] (conj ENV 'hs :- (ES 'us 'k))))
  (concl '(Exp.tSig U.u0 A B) '(Exp.pair (Exp.tSig U.u0 A B) x y))
  '[(have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
    (have hBS (SkJ Bool.true (List.cons Sk (skel A) (skels D)) B Sk.unit) (lemma25_tl_type chkf (List.cons Exp A D) B hB))
    (have hclA (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
    (have hxS (SkJ Bool.false (skels D) x (skel A)) (lemma25_tl_term chkf D x A hx))
    (have hxk (Eq (Option Sk) (skOf (skels D) x) (Option.some Sk (skel A))) (skOf_tl chkf Bool.false D x A hx rfl hclA))
    (have hxU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false (skels D) x (skel A) hxS rfl))
    (rw [(den_pair_at chkf dec encTy n (Exp.tSig U.u0 A B) x y (skels D) (skel (Exp.tSig U.u0 A B)) en)])
    (have vy0 (V chkf dec encTy n (subst1 x B) (skels D) en k (skel (subst1 x B)) (den chkf dec encTy n y (skels D) (skel (subst1 x B)) en))
      (ihy en k hk hs))
    (have vy1 (V chkf dec encTy n (subst1 x B) (skels D) en k (skel B) (den chkf dec encTy n y (skels D) (skel B) en))
      (sk_transport (fn [s :- Sk, v :- (Car s)] (V chkf dec encTy n (subst1 x B) (skels D) en k s v))
                    (fn [s :- Sk] (den chkf dec encTy n y (skels D) s en)) (skel (subst1 x B)) (skel B) (skel_subst1 x B hxU) vy0))
    (exact (Iff.mp (V_subst1 chkf dec encTy n (skels D) (skel A) B hBS x hxS hxk en k (den chkf dec encTy n y (skels D) (skel B) en)) vy1))])

;; --- natural-number recursion ----------------------------------------------------

;; RecN.  ⟦recN P z s n⟧ = Nat.rec ⟦z⟧ (i, acc ↦ ⟦s⟧(acc, (i, η))) ⟦n⟧, and the
;; goal V(P[n/x])η is V(P) at (⟦n⟧, η) (V_subst1).  By induction on the value
;; i (natrec_inv): the result at i is in V(P) at (i, η) with z's footprint k₂.
;; Base: z's IH at P[0].  Step: s's IH at the extended environment
;; (acc, (i, η)), whose context is (P, Nat, ω·Γ₃): acc at usage 1 with
;; footprint k₂, i at ω (V(Nat) is everything), and ω·Γ₃ at footprint 0 —
;; Lemma 3.5 (ii) and its converse at 0 (EnvSat_omega_back) — so the
;; footprint does not grow; its type stepTy P is read as V(P) at (i + 1, η)
;; by V_stepTy.  Finally raise k₂ to k.

(thm entry_omega_back [r :- U, P :- (=> Nat Prop)] (=> (EntryOK r 0 P) (EntryOK (umul U.uw r) 0 P))
  (cases r) (all_goals (intro h))
  (exact h)
  (have h1 (P 0) h) (constructor) (exact rfl) (exact h1)
  (exact rfl))

(thm add_le_zero_l [a :- Nat, b :- Nat, h :- (LE.le (+ a b) 0)] (Eq Nat a 0) (omega))
(thm add_le_zero_r [a :- Nat, b :- Nat, h :- (LE.le (+ a b) 0)] (Eq Nat b 0) (omega))

;; The converse of Lemma 3.5 (ii) at footprint 0: η ⊨₀ Γ ⟹ η ⊨₀ ωΓ.
(eval (list* 'lcert.formal.base/thm 'omega_back_cons
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
    A :- Exp, D :- (List Exp), r :- U, t :- (List U), en :- (HEnv (skels (List.cons Exp A D))),
    ih :- (forall [us (List U)] (forall [en (HEnv (skels D))] (=> (EnvSat chkf dec encTy n D us en 0) (EnvSat chkf dec encTy n D (vscale U.uw us) en 0)))),
    h :- (EnvSat chkf dec encTy n (List.cons Exp A D) (List.cons U r t) en 0)]
  '(EnvSat chkf dec encTy n (List.cons Exp A D) (vscale U.uw (List.cons U r t)) en 0)
  '[(have h2 (Exists (fn [j :- Nat] (Exists (fn [kk :- Nat] (And (Nat.le (+ j kk) 0) (And (EnvSat chkf dec encTy n D t (Prod.snd en) kk)
       (EntryOK r j (fn [jj :- Nat] (V chkf dec encTy n A (skels D) (Prod.snd en) jj (skel A) (Prod.fst en)))))))))) h)
    (refine' (exN _ _ h2 _)) (intro j hj) (refine' (exN _ _ hj _)) (intro kk hkk)
    (have p (And (Nat.le (+ j kk) 0) (And (EnvSat chkf dec encTy n D t (Prod.snd en) kk)
       (EntryOK r j (fn [jj :- Nat] (V chkf dec encTy n A (skels D) (Prod.snd en) jj (skel A) (Prod.fst en)))))) hkk)
    (have o (LE.le (+ j kk) 0) (And.left p))
    (have ej (Eq Nat j 0) (add_le_zero_l j kk o))
    (have ek (Eq Nat kk 0) (add_le_zero_r j kk o))
    (have ht (EnvSat chkf dec encTy n D t (Prod.snd en) 0)
      (Eq.mp (congrArg (fn [q :- Nat] (EnvSat chkf dec encTy n D t (Prod.snd en) q)) ek) (And.left (And.right p))))
    (have he (EntryOK r 0 (fn [jj :- Nat] (V chkf dec encTy n A (skels D) (Prod.snd en) jj (skel A) (Prod.fst en))))
      (Eq.mp (congrArg (fn [q :- Nat] (EntryOK r q (fn [jj :- Nat] (V chkf dec encTy n A (skels D) (Prod.snd en) jj (skel A) (Prod.fst en))))) ej)
             (And.right (And.right p))))
    (exact (EnvSat_cons chkf dec encTy n A D (umul U.uw r) (vscale U.uw t) (Prod.fst en) (Prod.snd en) 0 0
             (ih t (Prod.snd en) ht)
             (entry_omega_back r (fn [jj :- Nat] (V chkf dec encTy n A (skels D) (Prod.snd en) jj (skel A) (Prod.fst en))) he)))]))
(thm EnvSat_omega_back [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, D :- (List Exp)]
  (forall [us (List U)] (forall [en (HEnv (skels D))] (=> (EnvSat chkf dec encTy n D us en 0) (EnvSat chkf dec encTy n D (vscale U.uw us) en 0))))
  (induction D)
  (intro us en h) (exact True.intro)
  (intro us) (cases us) (intro en h) (exact (False.elim h))
  (intro en h) (exact (omega_back_cons _ _ _ _ _ _ _ _ _ ih_tail h)))


;; V(stepTy P) at (acc, (i, η)) is V(P) at (i + 1, η): V_lift_fam through the
;; lift (P[sSucc] is well-formed by skj_subst), then V_subst for sSucc, whose
;; environment is (i + 1, η) (envOf_sSucc).
(def ^:private Qs '(subst (fn [j :- Nat] (sSucc j)) P))
(def ^:private GN '(List.cons Sk Sk.nat G))
(eval (list 'lcert.formal.base/thm 'V_stepTy
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
    G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit), sk :- Sk, acc :- (Car sk), i :- Nat, en :- (HEnv G),
    K :- Nat, g :- (forall [s Sk] (Car s)),
    h :- (V chkf dec encTy n (stepTy P) (List.cons Sk sk (List.cons Sk Sk.nat G)) (Prod.mk acc (Prod.mk i en)) K (skel (stepTy P)) (g (skel (stepTy P))))]
  (list 'V 'chkf 'dec 'encTy 'n 'P GN '(Prod.mk (Nat.succ i) en) 'K '(skel P) '(g (skel P)))
  (list 'have 'hQ (list 'SkJ 'Bool.true GN Qs 'Sk.unit)
        (list 'skj_subst 'Bool.true GN 'P 'Sk.unit 'der GN '(fn [j :- Nat] (sSucc j)) '(subOK_sSucc G)))
  (list 'have 'h1 (list 'V 'chkf 'dec 'encTy 'n (list 'lift 1 0 Qs) (list 'List.cons 'Sk 'sk GN) '(Prod.mk acc (Prod.mk i en)) 'K
                        (list 'skel (list 'lift 1 0 Qs)) (list 'g (list 'skel (list 'lift 1 0 Qs)))) 'h)
  (list 'have 'h2 (list 'V 'chkf 'dec 'encTy 'n Qs GN '(Prod.mk i en) 'K (list 'skel Qs) (list 'g (list 'skel Qs)))
        (list 'Iff.mp (list 'V_lift_fam 'chkf 'dec 'encTy 'n GN Qs 'hQ 0 'sk '(Prod.mk i en) 'acc 'K 'g) 'h1))
  (list 'have 'h3 (list 'V 'chkf 'dec 'encTy 'n Qs GN '(Prod.mk i en) 'K '(skel P) '(g (skel P)))
        (list 'sk_transport (list 'fn '[s :- Sk, v :- (Car s)] (list 'V 'chkf 'dec 'encTy 'n Qs GN '(Prod.mk i en) 'K 's 'v)) 'g
              (list 'skel Qs) '(skel P) '(skel_subst P (fn [j :- Nat] (sSucc j)) sSucc_unit) 'h2))
  (list 'have 'h4 (list 'V 'chkf 'dec 'encTy 'n 'P GN (list 'envOf 'chkf 'dec 'encTy 'n GN '(fn [j :- Nat] (sSucc j)) GN '(Prod.mk i en)) 'K '(skel P) '(g (skel P)))
        (list 'Eq.mp (list 'V_subst 'chkf 'dec 'encTy 'n GN 'P 'der GN '(fn [j :- Nat] (sSucc j)) '(Prod.mk i en) '(subOK_sSucc G) 'K '(g (skel P))) 'h3))
  (list 'exact (list 'Eq.mp (list 'congrArg (list 'fn ['e :- (list 'HEnv GN)] (list 'V 'chkf 'dec 'encTy 'n 'P GN 'e 'K '(skel P) '(g (skel P))))
                                  '(envOf_sSucc chkf dec encTy n G i en)) 'h4))))

(thm den_zero_nat [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G)]
  (Eq Nat (den chkf dec encTy n Exp.zero G Sk.nat en) 0)
  (rw [(den_zero_at chkf dec encTy n G Sk.nat en)]))
;; the invariant, abstracted: a base value and a step preserving V(P) at the index
(thm natrec_inv [α :- Type, Q :- (=> Nat α Prop), b :- α, f :- (=> Nat α α),
                   hb :- (Q 0 b), hf :- (forall [i Nat] (forall [a α] (=> (Q i a) (Q (Nat.succ i) (f i a)))))]
  (forall [i Nat] (Q i (Nat.rec$1 (fn [_ :- Nat] α) b f i)))
  (intro i) (induction i) (exact hb) (exact (hf n (Nat.rec$1 (fn [_ :- Nat] α) b f n) ih_n)))

(def ^:private GNt '(List.cons Sk Sk.nat (skels D)))
(def ^:private G3n '(List.cons Sk (skel P) (List.cons Sk Sk.nat (skels D))))
(defn- VPn [iv kk v] (list 'V 'chkf 'dec 'encTy 'n 'P GNt (list 'Prod.mk iv 'en) kk '(skel P) v))
(def ^:private stepf '(fn [i :- Nat, acc :- (Car (skel P))] (den chkf dec encTy n st (List.cons Sk (skel P) (List.cons Sk Sk.nat (skels D))) (skel P) (Prod.mk acc (Prod.mk i en)))))
(def ^:private zv '(den chkf dec encTy n z (skels D) (skel P) en))
(eval (concat (list 'lcert.formal.base/thm 'F_recN
  (into P6 (into '[us1 :- (List U), us2 :- (List U), us3 :- (List U), P :- Exp, z :- Exp, st :- Exp, m :- Exp,
                   hm :- (Rt chkf D us1 m Exp.tNat), hP :- (Tl chkf Bool.true (List.cons Exp Exp.tNat D) P Exp.tUnit)]
                 (into ['ihz :- (SND 'us2 'z '(subst1 Exp.zero P))
                        'ihs :- '(Sound chkf dec encTy n (List.cons Exp P (List.cons Exp Exp.tNat D)) (List.cons U U.u1 (List.cons U U.uw (vscale U.uw us3))) st (stepTy P))]
                       (conj ENV 'hs :- (ES '(vadd us1 (vadd us2 (vscale U.uw us3))) 'k)))))
  (concl '(subst1 m P) '(Exp.recN P z st m)))
  (concat
    ['(have hPS (SkJ Bool.true (List.cons Sk Sk.nat (skels D)) P Sk.unit) (lemma25_tl_type chkf (List.cons Exp Exp.tNat D) P hP))
     '(have hmS (SkJ Bool.false (skels D) m Sk.nat) (lemma25_rt chkf D us1 m Exp.tNat hm))
     '(have hmU (Eq Sk (skel m) Sk.unit) (skj_term_unit Bool.false (skels D) m Sk.nat hmS rfl))
     '(have hmk (Eq (Option Sk) (skOf (skels D) m) (Option.some Sk Sk.nat)) (skOf_rt chkf D us1 m Exp.tNat hm rfl))
     '(rw [(skel_subst1 m P hmU)])
     '(rw [(den_recN_at chkf dec encTy n P z st m (skels D) (skel P) en)])
     '(refine' (Iff.mpr (V_subst1 chkf dec encTy n (skels D) Sk.nat P hPS m hmS hmk en k _) _))]
    (split-steps 'hs 'us1 '(vadd us2 (vscale U.uw us3)) 'k 'k1 'm1 'p1)
    (split-steps '(And.right (And.right p1)) 'us2 '(vscale U.uw us3) 'm1 'k2 'k3 'p2)
    ['(have hk2k (Nat.le k2 k) (Nat.le_trans (Nat.le_trans (Nat.le_add_right k2 k3) (And.left p2)) (Nat.le_trans (Nat.le_add_left m1 k1) (And.left p1))))
     '(have hk2 (Nat.le k2 n) (Nat.le_trans hk2k hk))
     '(have hs2 (EnvSat chkf dec encTy n D us2 en k2) (And.left (And.right p2)))
     '(have hs3 (EnvSat chkf dec encTy n D (vscale U.uw us3) en 0)
        (EnvSat_omega_back chkf dec encTy n D us3 en (EnvSat_omega chkf dec encTy n D us3 en k3 (And.right (And.right p2)))))
     ;; the base case: V(P) at (0, η)
     '(have vz0 (V chkf dec encTy n (subst1 Exp.zero P) (skels D) en k2 (skel (subst1 Exp.zero P)) (den chkf dec encTy n z (skels D) (skel (subst1 Exp.zero P)) en))
        (ihz en k2 hk2 hs2))
     '(have vz1 (V chkf dec encTy n (subst1 Exp.zero P) (skels D) en k2 (skel P) (den chkf dec encTy n z (skels D) (skel P) en))
        (sk_transport (fn [s :- Sk, v :- (Car s)] (V chkf dec encTy n (subst1 Exp.zero P) (skels D) en k2 s v))
                      (fn [s :- Sk] (den chkf dec encTy n z (skels D) s en)) (skel (subst1 Exp.zero P)) (skel P) (skel_subst1 Exp.zero P rfl) vz0))
     (list 'have 'vz2 (VPn '(den chkf dec encTy n Exp.zero (skels D) Sk.nat en) 'k2 zv)
        (list 'Iff.mp '(V_subst1 chkf dec encTy n (skels D) Sk.nat P hPS Exp.zero (SkJ.sZero (skels D)) rfl en k2 (den chkf dec encTy n z (skels D) (skel P) en)) 'vz1))
     (list 'have 'vz3 (VPn 0 'k2 zv)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Nat] (VPn 'q 'k2 zv)) '(den_zero_nat chkf dec encTy n (skels D) en)) 'vz2))
     ;; the step: V(P) at (i, η) for the accumulator gives V(P) at (i + 1, η)
     (list 'have 'vstep (list 'forall '[i Nat] (list 'forall '[acc (Car (skel P))] (list '=> (VPn 'i 'k2 'acc) (VPn '(Nat.succ i) 'k2 (list stepf 'i 'acc)))))
        (list 'fn '[i :- Nat, acc :- (Car (skel P)), ha :- (V chkf dec encTy n P (List.cons Sk Sk.nat (skels D)) (Prod.mk i en) k2 (skel P) acc)]
          (list 'V_stepTy 'chkf 'dec 'encTy 'n '(skels D) 'P 'hPS '(skel P) 'acc 'i 'en 'k2
                (list 'fn '[s :- Sk] (list 'den 'chkf 'dec 'encTy 'n 'st G3n 's '(Prod.mk acc (Prod.mk i en))))
                (list 'ihs '(Prod.mk acc (Prod.mk i en)) '(+ k2 (+ 0 0)) 'hk2
                      '(EnvSat_cons chkf dec encTy n P (List.cons Exp Exp.tNat D) U.u1 (List.cons U U.uw (vscale U.uw us3)) acc (Prod.mk i en) k2 (+ 0 0)
                         (EnvSat_cons chkf dec encTy n Exp.tNat D U.uw (vscale U.uw us3) i en 0 0 hs3 (And.intro rfl True.intro))
                         ha)))))
     (list 'have 'vall (list 'V 'chkf 'dec 'encTy 'n 'P GNt '(Prod.mk (den chkf dec encTy n m (skels D) Sk.nat en) en) 'k2 '(skel P)
                            (list 'Nat.rec$1 '(fn [_ :- Nat] (Car (skel P))) zv stepf '(den chkf dec encTy n m (skels D) Sk.nat en)))
        (list 'natrec_inv '(Car (skel P)) (list 'fn '[i :- Nat, v :- (Car (skel P))] (VPn 'i 'k2 'v)) zv stepf 'vz3 'vstep
              '(den chkf dec encTy n m (skels D) Sk.nat en)))
     (list 'exact (list 'V_mono 'chkf 'dec 'encTy 'n 'P GNt '(Prod.mk (den chkf dec encTy n m (skels D) Sk.nat en) en) 'k2 'k '(skel P)
                        (list 'Nat.rec$1 '(fn [_ :- Nat] (Car (skel P))) zv stepf '(den chkf dec encTy n m (skels D) Sk.nat en)) 'hk2k 'hk 'vall))])))
