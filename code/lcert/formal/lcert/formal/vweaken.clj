(ns lcert.formal.vweaken
  "F3l — weakening for the semantic types (the renaming half of Lemma 3.3's
  substitution clause, for lifts).

    V_lift:  for G ⊢ A type,
      Vⁿₖ(lift 1 c A)(insE c x G η v) = Vⁿₖ(A) η   at the skeleton skel A,

  where insS c x G / insE c x G η v insert a skeleton x and a value v at
  position c of the context and environment (substitution.clj §1).  Rules
  whose types are lifted — Var (lift (i+1) 0 A), RecN (stepTy), Let (lift 2 0
  C), RecSyn, ItR, Inspect — read V through this.

  By induction on the formation derivation, exactly as V_subst_gen
  (substitution.clj §9) with the insertion in place of envOf:
  - base types: V does not mention the environment;
  - T(b): den_weaken (weakening for ⟦·⟧) for b;
  - Π, Σ: congruences of the three usage clauses, with the IH for B at
    c + 1.  No environment lemma is needed there: insS (c+1) x (s :: G) and
    insE (c+1) x (s :: G) (a, η) v are s :: insS c x G and (a, insE c x G η v)
    by definition."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.substitution]))

(def ^:private cong @#'lcert.formal.substitution/cong)
(def ^:private skj-rules @#'lcert.formal.substitution/skj-rules)

(def ^:private vparams '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat])
(defn- Vf [A G en k s v] (list 'V 'chkf 'dec 'encTy 'n A G en k s v))

;; The motive, for a formation derivation G ⊢ A type.
(defn- VW [G A]
  (list 'forall '[c Nat] (list 'forall '[x Sk] (list 'forall ['en (list 'HEnv G)] (list 'forall '[vx (Car x)]
    (list 'forall '[k Nat] (list 'forall ['v (list 'Car (list 'skel A))]
      (list '= (Vf (list 'lift 1 'c A) (list 'insS 'c 'x G) (list 'insE 'c 'x G 'en 'vx) 'k (list 'skel A) 'v)
               (Vf A G 'en 'k (list 'skel A) 'v)))))))))

(def ^:private GI '(insS c x G))
(def ^:private EI '(insE c x G en vx))

;; V of B under the binder at value a, and of A; left = lifted, right = not.
(defn- beq [a kk vv] (list 'ih_hB '(+ c 1) 'x (list 'Prod.mk a 'en) 'vx kk vv))
(defn- aeq [kk vv] (list 'ih_hA 'c 'x 'en 'vx kk vv))
(defn- vB-l [a kk vv] (Vf '(lift 1 (+ c 1) B) (list 'List.cons 'Sk '(skel A) GI) (list 'Prod.mk a EI) kk '(skel B) vv))
(defn- vB-r [a kk vv] (Vf 'B '(List.cons Sk (skel A) G) (list 'Prod.mk a 'en) kk '(skel B) vv))
(defn- vA-l [kk vv] (Vf '(lift 1 c A) GI EI kk '(skel A) vv))
(defn- vA-r [kk vv] (Vf 'A 'G 'en kk '(skel A) vv))

(defn- vw-thm! [nm params prop tactics]
  (a/prove-theorem nm (lv (into vparams params)) (lv prop) (lv tactics)))

(vw-thm! 'vw_T '[G :- (List Sk), b :- Exp, hb :- (SkJ Bool.false G b Sk.bool)]
  (VW 'G '(Exp.tT b))
  ['(intro c x en vx k v)
   '(exact (congrArg (fn [q :- Bool] (Eq Bool q Bool.true))
                     (den_weaken chkf dec encTy n Bool.false G b Sk.bool hb c x en vx)))])

(vw-thm! 'vw_Pi ['G :- '(List Sk), 'r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'ih_hA :- (VW 'G 'A), 'ih_hB :- (VW '(List.cons Sk (skel A) G) 'B)]
  (VW 'G '(Exp.tPi r A B))
  ['(intro c x en vx k f)
   (list 'exact
     (cong '(U.rec$1 (fn [_ :- U] Prop) q1 q2 q3 r)
       [;; usage 0: every argument
        ['Prop
         (list 'forall '[a (Car (skel A))] (vB-l 'a 'k '(f a)))
         (list 'forall '[a (Car (skel A))] (vB-r 'a 'k '(f a)))
         (list 'congrArg (list 'fn '[P :- (=> (Car (skel A)) Prop)] '(forall [a (Car (skel A))] (P a)))
               (list 'funext (list 'fn '[a :- (Car (skel A))] (beq 'a 'k '(f a)))))]
        ;; usage 1: footprints j with k + j ≤ n
        ['Prop
         (list 'forall '[j Nat] (list '=> '(Nat.le (+ k j) n) (list 'forall '[a (Car (skel A))] (list '=> (vA-l 'j 'a) (vB-l 'a '(+ k j) '(f a))))))
         (list 'forall '[j Nat] (list '=> '(Nat.le (+ k j) n) (list 'forall '[a (Car (skel A))] (list '=> (vA-r 'j 'a) (vB-r 'a '(+ k j) '(f a))))))
         (cong '(forall [j Nat] (=> (Nat.le (+ k j) n) (forall [a (Car (skel A))] (=> (q1 j a) (q2 j a)))))
               [['(=> Nat (Car (skel A)) Prop)
                 (list 'fn '[j :- Nat, a :- (Car (skel A))] (vA-l 'j 'a))
                 (list 'fn '[j :- Nat, a :- (Car (skel A))] (vA-r 'j 'a))
                 (list 'funext (list 'fn '[j :- Nat] (list 'funext (list 'fn '[a :- (Car (skel A))] (aeq 'j 'a)))))]
                ['(=> Nat (Car (skel A)) Prop)
                 (list 'fn '[j :- Nat, a :- (Car (skel A))] (vB-l 'a '(+ k j) '(f a)))
                 (list 'fn '[j :- Nat, a :- (Car (skel A))] (vB-r 'a '(+ k j) '(f a)))
                 (list 'funext (list 'fn '[j :- Nat] (list 'funext (list 'fn '[a :- (Car (skel A))] (beq 'a '(+ k j) '(f a))))))]])]
        ;; usage ω: arguments in V₀
        ['Prop
         (list 'forall '[a (Car (skel A))] (list '=> (vA-l 0 'a) (vB-l 'a 'k '(f a))))
         (list 'forall '[a (Car (skel A))] (list '=> (vA-r 0 'a) (vB-r 'a 'k '(f a))))
         (cong '(forall [a (Car (skel A))] (=> (q1 a) (q2 a)))
               [['(=> (Car (skel A)) Prop)
                 (list 'fn '[a :- (Car (skel A))] (vA-l 0 'a))
                 (list 'fn '[a :- (Car (skel A))] (vA-r 0 'a))
                 (list 'funext (list 'fn '[a :- (Car (skel A))] (aeq 0 'a)))]
                ['(=> (Car (skel A)) Prop)
                 (list 'fn '[a :- (Car (skel A))] (vB-l 'a 'k '(f a)))
                 (list 'fn '[a :- (Car (skel A))] (vB-r 'a 'k '(f a)))
                 (list 'funext (list 'fn '[a :- (Car (skel A))] (beq 'a 'k '(f a))))]])]]))])

(vw-thm! 'vw_Sig ['G :- '(List Sk), 'r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'ih_hA :- (VW 'G 'A), 'ih_hB :- (VW '(List.cons Sk (skel A) G) 'B)]
  (VW 'G '(Exp.tSig r A B))
  ['(intro c x en vx k p)
   (list 'exact
     (cong '(U.rec$1 (fn [_ :- U] Prop) q1 q2 q3 r)
       [;; usage 0: the second component
        ['Prop (vB-l '(Prod.fst p) 'k '(Prod.snd p)) (vB-r '(Prod.fst p) 'k '(Prod.snd p)) (beq '(Prod.fst p) 'k '(Prod.snd p))]
        ;; usage 1: a split j + (k − j) of the footprint
        ['Prop
         (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j k) (list 'And (vA-l 'j '(Prod.fst p)) (vB-l '(Prod.fst p) '(- k j) '(Prod.snd p))))))
         (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j k) (list 'And (vA-r 'j '(Prod.fst p)) (vB-r '(Prod.fst p) '(- k j) '(Prod.snd p))))))
         (list 'congrArg '(fn [P :- (=> Nat Prop)] (Exists P))
               (list 'funext (list 'fn '[j :- Nat]
                 (cong '(And (Nat.le j k) (And q1 q2))
                       [['Prop (vA-l 'j '(Prod.fst p)) (vA-r 'j '(Prod.fst p)) (aeq 'j '(Prod.fst p))]
                        ['Prop (vB-l '(Prod.fst p) '(- k j) '(Prod.snd p)) (vB-r '(Prod.fst p) '(- k j) '(Prod.snd p))
                         (beq '(Prod.fst p) '(- k j) '(Prod.snd p))]]))))]
        ;; usage ω: the first component in V₀
        ['Prop
         (list 'And (vA-l 0 '(Prod.fst p)) (vB-l '(Prod.fst p) 'k '(Prod.snd p)))
         (list 'And (vA-r 0 '(Prod.fst p)) (vB-r '(Prod.fst p) 'k '(Prod.snd p)))
         (cong '(And q1 q2)
               [['Prop (vA-l 0 '(Prod.fst p)) (vA-r 0 '(Prod.fst p)) (aeq 0 '(Prod.fst p))]
                ['Prop (vB-l '(Prod.fst p) 'k '(Prod.snd p)) (vB-r '(Prod.fst p) 'k '(Prod.snd p)) (beq '(Prod.fst p) 'k '(Prod.snd p))]])]]))])

(defn- vw-case [rule]
  (case rule
    (wEmpty wUnit wBool wNat wLbl wSyn wDia wR) '[(intro hw c x en vx k v) (rfl)]
    wT '[(intro hw) (exact (vw_T chkf dec encTy n G b hb))]
    wPi '[(intro hw) (exact (vw_Pi chkf dec encTy n G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))))]
    wSig '[(intro hw) (exact (vw_Sig chkf dec encTy n G r A B (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))))]
    '[(intro hw) (cases hw)]))

(a/prove-theorem 'V_lift_gen
  (lv (into vparams '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]))
  (lv (list '=> '(= w0 Bool.true) (VW 'G0 'e0)))
  (lv (into ['(induction der)] (mapcat vw-case skj-rules))))

;; Weakening for V, as an equation and as an Iff.
(thm V_lift [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
             G :- (List Sk), A :- Exp, der :- (SkJ Bool.true G A Sk.unit), c :- Nat, x :- Sk, en :- (HEnv G), vx :- (Car x),
             k :- Nat, v :- (Car (skel A))]
  (= (V chkf dec encTy n (lift 1 c A) (insS c x G) (insE c x G en vx) k (skel A) v) (V chkf dec encTy n A G en k (skel A) v))
  (exact (V_lift_gen chkf dec encTy n Bool.true G A Sk.unit der (Eq.refl$1 Bool.true) c x en vx k v)))

;; The same for a family of values g s, read at the lifted type's own
;; skeleton skel (lift 1 c A) (equal to skel A, but not definitionally), as
;; the fundamental lemma's hypotheses come.
(thm V_lift_fam [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                   G :- (List Sk), A :- Exp, der :- (SkJ Bool.true G A Sk.unit), c :- Nat, x :- Sk, en :- (HEnv G), vx :- (Car x),
                   k :- Nat, g :- (forall [s Sk] (Car s))]
  (Iff (V chkf dec encTy n (lift 1 c A) (insS c x G) (insE c x G en vx) k (skel (lift 1 c A)) (g (skel (lift 1 c A))))
       (V chkf dec encTy n A G en k (skel A) (g (skel A))))
  (rw [(skel_lift A 1 c)])
  (exact (Iff.of_eq (V_lift chkf dec encTy n G A der c x en vx k (g (skel A))))))

;; Two binders at once (Let's body, typed at lift 2 0 C under the pair's
;; components): V(lift 2 0 C) at (vb, (va, η)) is V(C) at η.
(thm V_lift2_fam [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    G :- (List Sk), C :- Exp, der :- (SkJ Bool.true G C Sk.unit), sa :- Sk, sb :- Sk, en :- (HEnv G), va :- (Car sa), vb :- (Car sb),
                    k :- Nat, g :- (forall [s Sk] (Car s)),
                    h :- (V chkf dec encTy n (lift 2 0 C) (List.cons Sk sb (List.cons Sk sa G)) (Prod.mk vb (Prod.mk va en)) k (skel (lift 2 0 C)) (g (skel (lift 2 0 C))))]
  (V chkf dec encTy n C G en k (skel C) (g (skel C)))
  (have h1 (V chkf dec encTy n (lift 1 0 (lift 1 0 C)) (List.cons Sk sb (List.cons Sk sa G)) (Prod.mk vb (Prod.mk va en)) k
              (skel (lift 1 0 (lift 1 0 C))) (g (skel (lift 1 0 (lift 1 0 C)))))
    (Eq.mp (congrArg (fn [E :- Exp] (V chkf dec encTy n E (List.cons Sk sb (List.cons Sk sa G)) (Prod.mk vb (Prod.mk va en)) k (skel E) (g (skel E))))
                     (Eq.symm (lift_comp C 1 1 0))) h))
  (have hW (SkJ Bool.true (List.cons Sk sa G) (lift 1 0 C) Sk.unit) (skj_weaken Bool.true G C Sk.unit der 0 sa))
  (have h2 (V chkf dec encTy n (lift 1 0 C) (List.cons Sk sa G) (Prod.mk va en) k (skel (lift 1 0 C)) (g (skel (lift 1 0 C))))
    (Iff.mp (V_lift_fam chkf dec encTy n (List.cons Sk sa G) (lift 1 0 C) hW 0 sb (Prod.mk va en) vb k g) h1))
  (exact (Iff.mp (V_lift_fam chkf dec encTy n G C der 0 sa en va k g) h2)))
