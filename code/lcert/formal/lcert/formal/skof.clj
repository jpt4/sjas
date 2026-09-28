(ns lcert.formal.skof
  "F3k — skeleton inference is complete on clean runtime types.

  ⟦app f u⟧ reads u's skeleton from skOf (carrier.clj), and the substitution
  lemmas (substitution.clj) assume skOf is faithful on the substituted terms
  (SubOK).  This namespace proves what the fundamental lemma needs to meet
  those assumptions:

    skOf_rt:  Rt D us t A  and  clean A  ⟹  skOf (skels D) t = some (skel A),
    skOf_tl:  the same for type-level terms (Tl false).

  clean A (judgment.clj) says A contains no branch-list pseudo-type tBrs.  A
  branch list has every skeleton Lbl → s, so skOf has none to give it; the
  formation premise of App₀/App (hence clean function types) keeps branch
  lists out of argument position, and every paper type is clean.

  Supporting facts:
  - clean_<ctor>: clean is true of a constructor whose Exp fields are clean
    (generated, one per constructor with Exp fields, tBrs excepted);
  - band_left/band_right: the parts of a true conjunction;
  - skj_clean: every skeleton-typed expression is clean (SkJ has no rule for
    tBrs as a type, and every other rule's fields are premises);
  - cv_wf_left: the left end of a conversion is skeleton-well-formed, so a
    converted term's original type is clean too."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.syntactic]
            [lcert.formal.skeletons]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)

;; --- clean, constructor by constructor --------------------------------------------

(defn- exp-names [fields] (for [[f ty] fields :when (= ty 'Exp)] f))

(defn- clean-intro!
  "clean_<ctor>: clean e₁ = true → … → clean (ctor …) = true."
  [[ctor fields]]
  (let [exps (exp-names fields)
        nm (symbol (str "clean_" ctor))
        hs (map #(symbol (str "h_" %)) exps)
        term (apply list (symbol (str "Exp." ctor)) (map first fields))
        conj (reduce (fn [acc e] (list 'Bool.and (list 'clean e) acc)) (list 'clean (last exps)) (reverse (butlast exps)))]
    (a/prove-theorem nm
      (lv (vec (mapcat (fn [[f ty]] [f :- ty]) fields)))
      (lv (reduce (fn [acc e] (list '=> (list 'Eq 'Bool (list 'clean e) 'Bool.true) acc))
                  (list 'Eq 'Bool (list 'clean term) 'Bool.true) (reverse exps)))
      (lv [(apply list 'intro hs) (list 'change (list 'Eq 'Bool conj 'Bool.true)) (list 'rw (vec hs))]))))

(def ^:private intro-ctors
  (filter (fn [[ctor fields]] (and (not= ctor 'tBrs) (seq (exp-names fields)))) exp-fields))

(doseq [c intro-ctors] (clean-intro! c))

(thm band_left [x :- Bool, y :- Bool, h :- (Eq Bool (Bool.and x y) Bool.true)] (Eq Bool x Bool.true)
  (cases x) (exact (Bool.noConfusion h)) (rfl))

(thm band_right [x :- Bool, y :- Bool, h :- (Eq Bool (Bool.and x y) Bool.true)] (Eq Bool y Bool.true)
  (cases x) (exact (Bool.noConfusion h)) (exact h))

;; --- skeleton-typed expressions are clean ---------------------------------------------

;; Each goal is clean (ctor …) = true with the fields' IHs in context: close
;; the leaves by rfl, apply the constructor's clean_<ctor>, then the IHs.
;; `apply clean_tPi` can also unify, through clean's unfolding, with another
;; binary constructor's goal and leave its unused usage argument open; any
;; usage closes it (clean ignores usages).
(eval
  (list 'lcert.formal.base/thm 'skj_clean '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
    '(Eq Bool (clean e0) Bool.true)
    '(induction der)
    (list 'all_goals (apply list 'first '(rfl) (map (fn [[c _]] (list 'apply (symbol (str "clean_" c)))) intro-ctors)))
    '(all_goals (first (assumption) (exact U.u0)))))

(thm cv_wf_left [chkf :- (=> Code Code Bool), G0 :- (List Sk), A0 :- Exp, B0 :- Exp, der :- (Cv chkf G0 A0 B0)]
  (SkJ Bool.true G0 A0 Sk.unit)
  (induction der)
  (exact h) (exact ih_hab) (exact ih_hab))

;; --- completeness ---------------------------------------------------------------

;; Constants: skOf reads the same skeleton constTyped assigns.  Pairs (t, A)
;; with constTyped t A = false are refuted by cases on the hypothesis.
(thm skOf_const [G :- (List Sk)]
  (forall [t Exp] (forall [A Exp] (=> (Eq Bool (constTyped t A) Bool.true) (Eq (Option Sk) (skOf G t) (Option.some Sk (skel A))))))
  (intro t) (cases t) (all_goals (intro A))
  (all_goals (first (and_then (intro hct) (exact (Bool.noConfusion hct))) (skip)))
  (all_goals (cases A))
  (all_goals (intro hct))
  (all_goals (first (rfl) (cases hct))))

;; skel u = Unit for a runtime-typed u (Lemma 2.5 and skj_term_unit): the
;; condition under which substituting u into a type keeps its skeleton.
(defn- term-unit [usX X T hX]
  (list 'skj_term_unit 'Bool.false '(skels D) X (list 'skel T) (list 'lemma25_rt 'chkf 'D usX X T hX) 'rfl))

(defn- some-cong [eq] (list 'congrArg '(fn [x :- Sk] (Option.some Sk x)) eq))

;; skOf_rt, by induction on the derivation.  Rules whose conclusion's skOf
;; is a constant or the annotation's skeleton close by rfl.  Of the rest:
;; - Var: nthS of the skeleton context, and skel (lift …) = skel;
;; - Lam, If, App₀, App: the IH (for App at the function, whose type is
;;   formed by the premise, hence clean), with skel (B[u/x]) = skel B;
;; - Conv: the IH at the original type, clean because it is well-formed
;;   (cv_wf_left, skj_clean), and conversion keeps skeletons;
;; - ElimBool, RecN, CaseL, RecSyn: skel (P[x/y]) = skel P;
;; - Bnil, Bcons: their type is not clean.
(eval
  (list 'thm 'skOf_rt '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)]
    '(=> (Eq Bool (clean A0) Bool.true) (Eq (Option Sk) (skOf (skels D0) t0) (Option.some Sk (skel A0))))
    '(induction der)
    '(all_goals (intro hcA))
    '(all_goals (try (rfl)))
    ;; Var
    (list 'exact (list 'Eq.trans '(skels_nth D i A hA) (some-cong '(Eq.symm (skel_lift A (+ i 1) 0)))))
    ;; Const
    '(exact (skOf_const (skels D) t A h))
    ;; Lam
    '(exact (skOf_lam (skels D) r A t (skel B) (ih_ht (band_right (clean A) (clean B) hcA))))
    ;; App₀
    (list 'exact (list 'Eq.trans '(skOf_app (skels D) f u (skel (Exp.tPi U.u0 A B)) (ih_hf (skj_clean Bool.true (skels D) (Exp.tPi U.u0 A B) Sk.unit (SkJ.wPi (skels D) U.u0 A B (lemma25_tl_type chkf D A hA) (lemma25_tl_type chkf (List.cons Exp A D) B hB)))))
           (some-cong (list 'Eq.symm (list 'skel_subst1 'u 'B '(skj_term_unit Bool.false (skels D) u (skel A) (lemma25_tl_term chkf D u A hu) rfl))))))
    ;; App
    (list 'exact (list 'Eq.trans '(skOf_app (skels D) f u (skel (Exp.tPi r A B)) (ih_hf (skj_clean Bool.true (skels D) (Exp.tPi r A B) Sk.unit (SkJ.wPi (skels D) r A B (lemma25_tl_type chkf D A hA) (lemma25_tl_type chkf (List.cons Exp A D) B hB)))))
           (some-cong (list 'Eq.symm (list 'skel_subst1 'u 'B (term-unit 'us2 'u 'A 'hu))))))
    ;; Conv
    (list 'exact (list 'Eq.trans '(ih_ht (skj_clean Bool.true (skels D) A Sk.unit (cv_wf_left chkf (skels D) A B hc)))
           (some-cong '(cv_skel chkf (skels D) A B hc))))
    ;; If
    '(exact (ih_ht hcA))
    ;; ElimBool, RecN, CaseL
    (list 'exact (some-cong (list 'Eq.symm (list 'skel_subst1 'b 'P (term-unit 'us1 'b 'Exp.tBool 'hb)))))
    (list 'exact (some-cong (list 'Eq.symm (list 'skel_subst1 'n 'P (term-unit 'us1 'n 'Exp.tNat 'hn)))))
    (list 'exact (some-cong (list 'Eq.symm (list 'skel_subst1 'x 'P (term-unit 'us1 'x 'Exp.tLbl 'hx)))))
    ;; Bnil, Bcons
    '(exact (Bool.noConfusion hcA))
    '(exact (Bool.noConfusion hcA))
    ;; RecSyn
    (list 'exact (some-cong (list 'Eq.symm (list 'skel_subst1 'c 'P (term-unit 'us1 'c 'Exp.tSyn 'hc)))))))

;; skOf_tl: the same for type-level terms (Tl at w = false), whose arguments
;; App₀ and Pair₀ substitute into types.  Formation derivations (w = true)
;; are excluded by the first hypothesis.
(defn- tu [X T hX] (list 'skj_term_unit 'Bool.false '(skels D) X (list 'skel T) (list 'lemma25_tl_term 'chkf 'D X T hX) 'rfl))
(defn- scg [eq] (list 'congrArg '(fn [x :- Sk] (Option.some Sk x)) eq))
(eval (list 'thm 'skOf_tl '[chkf :- (=> Code Code Bool), w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp, der :- (Tl chkf w0 D0 t0 A0)]
  '(=> (Eq Bool w0 Bool.false) (Eq Bool (clean A0) Bool.true) (Eq (Option Sk) (skOf (skels D0) t0) (Option.some Sk (skel A0))))
  '(induction der)
  '(all_goals (intro hw hcA))
  '(all_goals (first (exact (Bool.noConfusion hw)) (skip)))
  '(all_goals (try (rfl)))
  ;; RecSyn
  (list 'exact (scg (list 'Eq.symm (list 'skel_subst1 'c 'P (tu 'c 'Exp.tSyn 'hc)))))
  ;; Bcons, Bnil
  '(exact (Bool.noConfusion hcA))
  '(exact (Bool.noConfusion hcA))
  ;; CaseL, RecN, ElimBool
  (list 'exact (scg (list 'Eq.symm (list 'skel_subst1 'x 'P (tu 'x 'Exp.tLbl 'hx)))))
  (list 'exact (scg (list 'Eq.symm (list 'skel_subst1 'n 'P (tu 'n 'Exp.tNat 'hn)))))
  (list 'exact (scg (list 'Eq.symm (list 'skel_subst1 'b 'P (tu 'b 'Exp.tBool 'hb)))))
  ;; If
  '(exact (ih_ht rfl hcA))
  ;; App
  (list 'exact (list 'Eq.trans '(skOf_app (skels D) f u (skel (Exp.tPi r A B))
                                   (ih_hf rfl (skj_clean Bool.true (skels D) (Exp.tPi r A B) Sk.unit
                                                (SkJ.wPi (skels D) r A B (lemma25_tl_type chkf D A hA) (lemma25_tl_type chkf (List.cons Exp A D) B hB)))))
         (scg (list 'Eq.symm (list 'skel_subst1 'u 'B (tu 'u 'A 'hu))))))
  ;; Lam
  '(exact (skOf_lam (skels D) r A t (skel B) (ih_ht rfl (band_right (clean A) (clean B) hcA))))
  ;; Conv
  (list 'exact (list 'Eq.trans '(ih_ht rfl (skj_clean Bool.true (skels D) A Sk.unit (cv_wf_left chkf (skels D) A B hc)))
         (scg '(cv_skel chkf (skels D) A B hc))))
  ;; Const
  '(exact (skOf_const (skels D) t A h))
  ;; Var
  (list 'exact (list 'Eq.trans '(skels_nth D i A h) (scg '(Eq.symm (skel_lift A (+ i 1) 0)))))))
