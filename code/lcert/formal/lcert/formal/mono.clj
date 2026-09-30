(ns lcert.formal.mono
  "F3g — monotonicity of the semantic types (R4-metatheory.md Lemma 3.3,
  first clause): Vₖ(A) ⊆ Vₖ′(A) for k ≤ k′ ≤ n.

  By induction on the type A.  Raising the footprint shrinks the range of j
  in the Π₁ clause and enlarges every target; ◇ and R are monotone directly;
  every other clause does not mention the footprint, so the hypothesis is the
  goal.  The 41 cases are generated as tactic scripts (as in syntactic.clj),
  and checked by the kernel like any proof."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]))

(def ^:private ctors
  '[tEmpty tUnit tBool tNat tLbl tSyn tDia tR tT tPi tSig var star abort tt ff ite elimB zero succ recN lbl caseL
    bnil bcons sleaf snode recS leaf node itR prn lam app pair letp chk h1 refl insp tBrs])

(def ^:private intro-all '(intro G en fk fk2 s vv hk hkn hv))

(defn- ih-app
  "The induction hypothesis for B (or P), at an extended environment."
  [ihn G en fk fk2 s vv hk hkn hv]
  (list ihn G en fk fk2 s vv hk hkn hv))

(def ^:private scripts
  {'tDia ['(have h1 (LE.le 1 fk) hv) '(exact (Nat.le_trans h1 hk))]
   'tR   ['(cases s) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv)
          '(have h1 (LE.le (cnodes vv) fk) hv) '(exact (Nat.le_trans h1 hk)) '(exact hv) '(exact hv)]
   'tPi  ['(cases s) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv)
          '(cases r)
          ;; usage 0: every argument
          '(have h2 (forall [a (Car s)] (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) fk t (vv a))) hv)
          '(change (forall [a (Car s)] (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) fk2 t (vv a))))
          '(intro a)
          '(exact (ih_B (List.cons Sk s G) (Prod.mk a en) fk fk2 t (vv a) hk hkn (h2 a)))
          ;; usage 1: footprints j with fk + j ≤ n
          '(have h2 (forall [j Nat] (=> (LE.le (+ fk j) n) (forall [a (Car s)] (=> (V chkf dec encTy n A G en j s a) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) (+ fk j) t (vv a)))))) hv)
          '(change (forall [j Nat] (=> (LE.le (+ fk2 j) n) (forall [a (Car s)] (=> (V chkf dec encTy n A G en j s a) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) (+ fk2 j) t (vv a)))))))
          '(intro j hj a ha)
          '(have p1 (LE.le (Nat.add fk j) (Nat.add fk2 j)) (Nat.add_le_add_right hk j))
          '(have p2 (LE.le (Nat.add fk j) n) (Nat.le_trans p1 hj))
          '(have p3 (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) (Nat.add fk j) t (vv a)) (h2 j p2 a ha))
          '(have p4 (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) (Nat.add fk2 j) t (vv a))
                  (ih_B (List.cons Sk s G) (Prod.mk a en) (Nat.add fk j) (Nat.add fk2 j) t (vv a) p1 hj p3))
          '(exact p4)
          ;; usage ω: arguments in V₀
          '(have h2 (forall [a (Car s)] (=> (V chkf dec encTy n A G en 0 s a) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) fk t (vv a)))) hv)
          '(change (forall [a (Car s)] (=> (V chkf dec encTy n A G en 0 s a) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk a en) fk2 t (vv a)))))
          '(intro a ha)
          '(exact (ih_B (List.cons Sk s G) (Prod.mk a en) fk fk2 t (vv a) hk hkn (h2 a ha)))
          '(exact hv)]
   'tSig ['(cases s) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv)
          '(cases r)
          ;; usage 0: only the second component
          '(have h2 (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk t (Prod.snd vv)) hv)
          '(change (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk2 t (Prod.snd vv)))
          '(exact (ih_B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk fk2 t (Prod.snd vv) hk hkn h2))
          ;; usage 1: keep the split j; the second footprint is raised
          '(have h2 (Exists (fn [j :- Nat] (And (LE.le j fk) (And (V chkf dec encTy n A G en j s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk j) t (Prod.snd vv)))))) hv)
          '(change (Exists (fn [j :- Nat] (And (LE.le j fk2) (And (V chkf dec encTy n A G en j s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk2 j) t (Prod.snd vv)))))))
          '(exact (AT_Exists.rec Nat (fn [j :- Nat] (And (LE.le j fk) (And (V chkf dec encTy n A G en j s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk j) t (Prod.snd vv))))) (fn [_ :- (Exists (fn [j :- Nat] (And (LE.le j fk) (And (V chkf dec encTy n A G en j s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk j) t (Prod.snd vv))))))] (Exists (fn [j :- Nat] (And (LE.le j fk2) (And (V chkf dec encTy n A G en j s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk2 j) t (Prod.snd vv))))))) (fn [w :- Nat, hw :- (And (LE.le w fk) (And (V chkf dec encTy n A G en w s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk w) t (Prod.snd vv))))] (Exists.intro w (And.intro (Nat.le_trans (And.left hw) hk) (And.intro (And.left (And.right hw)) (ih_B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) (- fk w) (- fk2 w) t (Prod.snd vv) (Nat.sub_le_sub_right hk w) (Nat.le_trans (Nat.sub_le fk2 w) hkn) (And.right (And.right hw))))))) h2))
          ;; usage ω
          '(have h2 (And (V chkf dec encTy n A G en 0 s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk t (Prod.snd vv))) hv)
          '(change (And (V chkf dec encTy n A G en 0 s (Prod.fst vv)) (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk2 t (Prod.snd vv))))
          '(have p (V chkf dec encTy n B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk2 t (Prod.snd vv))
                  (ih_B (List.cons Sk s G) (Prod.mk (Prod.fst vv) en) fk fk2 t (Prod.snd vv) hk hkn (And.right h2)))
          '(constructor)
          '(exact (And.left h2))
          '(exact p)]
   'tBrs ['(cases s) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv) '(exact hv)
          '(have h2 (forall [l Nat] (=> (LE.le k l) (LT.lt l 100)
                    (V chkf dec encTy n P (List.cons Sk s G) (Prod.mk (coe Sk.lbl s l) en) fk t (vv (coe Sk.lbl s (- l k)))))) hv)
          '(change (forall [l Nat] (=> (LE.le k l) (LT.lt l 100)
                    (V chkf dec encTy n P (List.cons Sk s G) (Prod.mk (coe Sk.lbl s l) en) fk2 t (vv (coe Sk.lbl s (- l k)))))))
          '(intro l hl1 hl2)
          '(exact (ih_P (List.cons Sk s G) (Prod.mk (coe Sk.lbl s l) en) fk fk2 t (vv (coe Sk.lbl s (- l k))) hk hkn (h2 l hl1 hl2)))
          '(exact hv)]})

(defn- script [c]
  (into [intro-all] (get scripts c ['(exact hv)])))

(a/prove-theorem 'V_mono
  (lcert.formal.base/lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, A :- Exp])
  '(forall [G (List Sk)] (forall [en (HEnv G)] (forall [k Nat] (forall [k2 Nat] (forall [s Sk] (forall [v (Car s)]
     (=> (LE.le k k2) (LE.le k2 n) (V chkf dec encTy n A G en k s v) (V chkf dec encTy n A G en k2 s v))))))))
  (lcert.formal.base/lv (into ['(induction A)] (mapcat script ctors))))

;; --- Lemma 3.4: base data types across budgets ---------------------------------

;; V at R, raised from cap m and footprint m to cap n and footprint k ≥ m.  The
;; set is { v : ‖v‖ ≤ footprint } at the cert skeleton and empty elsewhere; it
;; never mentions the cap or the environment.
(thm base_R_mono [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  m :- Nat, n :- Nat, k :- Nat, G :- (List Sk), G2 :- (List Sk), en :- (HEnv G), en2 :- (HEnv G2), hmk :- (Nat.le m k)]
  (forall [s Sk] (forall [v (Car s)] (=> (V chkf dec encTy m Exp.tR G en m s v) (V chkf dec encTy n Exp.tR G2 en2 k s v))))
  (intro s) (cases s) (all_goals (intro v hv))
  ;; every skeleton but cert: the same False on both sides
  (all_goals (first (exact hv) (skip)))
  (have h2 (Nat.le (cnodes v) m) hv) (exact (Nat.le_trans h2 hmk)))

;; Lemma 3.4: for a base data type D (isBaseTy: 0, 1, Bool, Nat, Lbl, Syn, R)
;; and m ≤ k, Vᵐₘ(D) ⊆ Vⁿₖ(D) — at any caps and environments, since none of
;; these sets depends on them.  This is what lets reflect, which runs a program
;; at a smaller budget m, return its value at budget n.
(thm Lemma_3_4 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                D :- Exp, m :- Nat, n :- Nat, k :- Nat, G :- (List Sk), G2 :- (List Sk), en :- (HEnv G), en2 :- (HEnv G2),
                s :- Sk, v :- (Car s)]
  (=> (Eq Bool (isBaseTy D) Bool.true) (Nat.le m k) (V chkf dec encTy m D G en m s v) (V chkf dec encTy n D G2 en2 k s v))
  (cases D) (all_goals (intro hb hmk hv))
  ;; non-base constructors: isBaseTy is false; 0, 1, Bool, Nat, Lbl, Syn: the
  ;; same set on both sides
  (all_goals (first (exact (Bool.noConfusion hb)) (exact hv) (skip)))
  (exact (base_R_mono chkf dec encTy m n k G G2 en en2 hmk s v hv)))
