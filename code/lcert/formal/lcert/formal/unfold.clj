(ns lcert.formal.unfold
  "F3i — one-step unfolding of the denotation, for every constructor.

  ⟦t⟧ⁿ is `den n t = denT n n t`, a row of the table of levels (den.clj).  For
  a symbolic n that table does not compute: `denT n n` is stuck on Nat.rec at
  n.  Every case of the fundamental lemma (3.6) needs one step of it, e.g.
  ⟦succ m⟧ⁿ = succ ⟦m⟧ⁿ.  So:

  - denPrev n: the level that decoded programs run through at cap n
    (den0 at n = 0, denT k at n = k + 1);
  - den_eq_denAt: den n t = denAt (denPrev n) n t, for every n (by cases on
    n: definitional at 0, denT_top at k + 1);
  - den_<ctor>_eq, one per constructor of Exp (and den_<ctor>_at, the same
    applied to G s η, for rewriting inside goals):
      den n (c x…) = den_c … (denPrev n) n x… (den n e…)
    with den n applied to each Exp field e.  The right side is the clause of
    den_gen.clj, so each case lemma sees exactly the paper's equation.

  The per-constructor statements are generated from syntactic.clj's field
  table; each is proved by rewriting every den with den_eq_denAt, after which
  the two sides agree definitionally (denAt is Exp.rec over the clauses)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.syntactic]))

(kdef denPrev
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat DenFn))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] DenFn) den0 (fn [k :- Nat, _ :- DenFn] (denT chkf dec encTy k)) n)))

(thm den_eq_denAt [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   n :- Nat, t :- Exp]
  (= (den chkf dec encTy n t) (denAt chkf dec encTy (denPrev chkf dec encTy n) n t))
  (cases n) (rfl) (exact (denT_top chkf dec encTy n t)))

;; Constructor fields [name type binder-depth], as in syntactic.clj.
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)

(def ^:private params
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat])

(defn- unfold-eq!
  "State and prove den_<ctor>_eq.  The cap is named `cap`: succ and recN
  have a field named n."
  [ctor fields]
  (let [nm (symbol (str "den_" ctor "_eq"))
        fs (map first fields)
        term (if (seq fs) (apply list (symbol (str "Exp." ctor)) fs) (symbol (str "Exp." ctor)))
        exps (for [[f ty] fields :when (= ty 'Exp)] f)
        rhs (concat (list (symbol (str "den_" ctor)) 'chkf 'dec 'encTy '(denPrev chkf dec encTy cap) 'cap)
                    fs
                    (for [e exps] (list 'den 'chkf 'dec 'encTy 'cap e)))
        rewrites (vec (cons (list 'den_eq_denAt 'chkf 'dec 'encTy 'cap term)
                            (for [e exps] (list 'den_eq_denAt 'chkf 'dec 'encTy 'cap e))))]
    (a/prove-theorem nm
      (lv (into params (mapcat (fn [[f ty]] [f :- ty]) fields)))
      (lv (list '= (list 'den 'chkf 'dec 'encTy 'cap term) (apply list rhs)))
      (lv [(list 'rw rewrites)]))))

;; The pointwise forms, den_<ctor>_at: the same equation applied to G, s, η.
;; Ansatz's rewrite matches whole applications only, so a goal mentioning
;; den n (c x…) G s η needs this form.  (The skeleton is named sk: h1 has a
;; field named s.)  Proved by congrArg with the function
;; f ↦ f G s η (congrFun's implicit arguments are not inferred here).
(defn- unfold-at!
  "State and prove den_<ctor>_at."
  [ctor fields]
  (let [nm (symbol (str "den_" ctor "_at"))
        fs (map first fields)
        term (if (seq fs) (apply list (symbol (str "Exp." ctor)) fs) (symbol (str "Exp." ctor)))
        exps (for [[f ty] fields :when (= ty 'Exp)] f)
        rhs (concat (list (symbol (str "den_" ctor)) 'chkf 'dec 'encTy '(denPrev chkf dec encTy cap) 'cap)
                    fs
                    (for [e exps] (list 'den 'chkf 'dec 'encTy 'cap e))
                    '[G sk en])]
    (a/prove-theorem nm
      (lv (into params (concat (mapcat (fn [[f ty]] [f :- ty]) fields) '[G :- (List Sk), sk :- Sk, en :- (HEnv G)])))
      (lv (list '= (list 'den 'chkf 'dec 'encTy 'cap term 'G 'sk 'en) (apply list rhs)))
      (lv [(list 'exact (list 'congrArg '(fn [f :- DenBody] (f G sk en))
                              (apply list (symbol (str "den_" ctor "_eq")) 'chkf 'dec 'encTy 'cap fs)))]))))

(doseq [[ctor fields] exp-fields] (unfold-eq! ctor fields) (unfold-at! ctor fields))
