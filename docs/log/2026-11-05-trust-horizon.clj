;; Trust horizon, kernel-checked in Ansatz: the port of
;; 2026-11-05-trust-horizon.lean to the environment λᶜᵉʳᵗ₀ is formalized in.
;; The note is 2026-11-05-trust-horizon.md.
;;
;; A possibly false contract `ax : ‘□’ C ‘→’ ‘⊥’` ("no certificate of C exists")
;; and `abort` are added to the guarded syntax. Reading ‘⊥’ at level n as blame,
;; a closed term of C smaller than n, gives `hblame` with no hypothesis on C:
;; every closed term of ‘⊥’ runs to a counterexample smaller than itself.
;; Tightness, the A- and E-analogues and consistency are corollaries.
;;
;; Differences from the Lean file, all forced by the surface language:
;; - `a/defn` cannot define a Type-valued or dependently typed function, so
;;   every definition is a `kdef` over an explicit recursor.
;; - Implicit arguments are written out: `(hsize C G A t)`.
;; - Ansatz's Init has no `Sum`, so `HSum` is declared here.
;; - Lean's `termination_by` on the level becomes `Nat.rec` over a bound
;;   (`hBelowAll`), so a run reduces definitionally: `hBlameRefute` is `rfl`.
;;
;; Run from code/lcert (ANSATZ_OFFLINE=1, under the usual caps):
;;   clojure -M:formal -i ../../docs/log/2026-11-05-trust-horizon.clj
;; A rejected declaration aborts the load; the last line printed is HORIZON-OK.
(ns trust-horizon
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as kenv]
            [ansatz.kernel.expr :as ke]
            [ansatz.kernel.name :as kname]
            [clojure.walk :as w]
            [lcert.formal.base :as b]))

;; Abbreviations, expanded before elaboration: Ø is the empty context and ⊥ is
;; the syntactic type of absurdity.
(def abbreviations {'Ø '(List.nil HTy), '⊥ 'HTy.bot,
                    ;; `Subtype.mk` with every argument explicit, at Type. The
                    ;; surface reader cannot spell this name, and the elaborator
                    ;; cannot infer the predicate from HHeld in a kdef.
                    'SubMk (symbol "@Subtype.mk.{1}"),
                    ;; `False.elim` at Type, for the same reason.
                    'FalseElim1 (symbol "@False.elim.{1}")})
(defn expand [form] (w/postwalk-replace abbreviations form))

(defmacro kd
  "A kernel-checked definition, with the abbreviations expanded."
  [nm ty body]
  `(b/kdef! '~nm '~(expand ty) '~(expand body)))

(defmacro th
  "A kernel-checked theorem, with the abbreviations expanded."
  [nm params prop & tactics]
  `(a/prove-theorem '~nm '~(b/lv (expand params)) '~(b/lv (expand prop))
                    '~(b/lv (expand (vec tactics)))))

;; ---------------------------------------------------------------------------
;; The syntax.

(a/inductive HTy [] (arr [a HTy] [b HTy]) (box [a HTy]) (top) (bot) (godel))

(a/inductive HVar [] :indices [G (List HTy), A HTy]
  (zero [G (List HTy)] [A HTy] :where [(List.cons HTy A G) A])
  (succ [G (List HTy)] [A HTy] [B HTy] [v (HVar G A)] :where [(List.cons HTy B G) A]))

;; The guarded syntax with the contract on C. `quo` costs the size of what it
;; quotes; `ax` is unchecked and may be false; `abort` certifies anything.
(a/inductive HTm [C HTy] :indices [G (List HTy), A HTy]
  (var [G (List HTy)] [A HTy] [v (HVar G A)] :where [G A])
  (lam [G (List HTy)] [A HTy] [B HTy] [b (HTm C (List.cons HTy A G) B)] :where [G (HTy.arr A B)])
  (app [G (List HTy)] [A HTy] [B HTy] [f (HTm C G (HTy.arr A B))] [x (HTm C G A)] :where [G B])
  (tt [G (List HTy)] :where [G HTy.top])
  (quo [G (List HTy)] [A HTy] [t (HTm C (List.nil HTy) A)] :where [G (HTy.box A)])
  (fold [G (List HTy)] :where [G (HTy.arr (HTy.arr (HTy.box HTy.godel) HTy.bot) HTy.godel)])
  (unfold [G (List HTy)] :where [G (HTy.arr HTy.godel (HTy.arr (HTy.box HTy.godel) HTy.bot))])
  (con [G (List HTy)] :where [G (HTy.arr (HTy.box HTy.bot) HTy.bot)])
  (ax [G (List HTy)] :where [G (HTy.arr (HTy.box C) HTy.bot)])
  (abort [G (List HTy)] [A HTy] :where [G (HTy.arr HTy.bot A)]))

(a/inductive HSum [X Type, Y Type] (inl [a X]) (inr [b Y]))

;; Syntax size: the measure a certificate's reach is bounded by.
(kd hsize (forall [C HTy] (forall [G (List HTy)] (forall [A HTy] (=> (HTm C G A) Nat))))
  (fn [C :- HTy, G :- (List HTy), A :- HTy, t :- (HTm C G A)]
    (HTm.rec$1 C (fn [G :- (List HTy), A :- HTy, _ :- (HTm C G A)] Nat)
      (fn [G :- (List HTy), A :- HTy, v :- (HVar G A)] 1)
      (fn [G :- (List HTy), A :- HTy, B :- HTy, b :- (HTm C (List.cons HTy A G) B), ib :- Nat] (Nat.succ ib))
      (fn [G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A), jf :- Nat, jx :- Nat]
        (Nat.succ (Nat.add jf jx)))
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy), A :- HTy, t :- (HTm C Ø A), it :- Nat] (Nat.succ it))
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy), A :- HTy] 1)
      G A t)))

;; ---------------------------------------------------------------------------
;; Types read at level n. Blame is a counterexample to the contract smaller
;; than n, and it is what ‘⊥’ means here. ‘□’ A is the syntax of A smaller than n.

(kd HHeld (=> HTy Nat HTy (Sort 1))
  (fn [C :- HTy, n :- Nat, A :- HTy]
    (Subtype (HTm C Ø A) (fn [t :- (HTm C Ø A)] (LT.lt (hsize C Ø A t) n)))))

(kd HBlame (=> HTy Nat (Sort 1)) (fn [C :- HTy, n :- Nat] (HHeld C n C)))

(kd hMk (forall [C HTy] (forall [n Nat] (forall [A HTy] (forall [t (HTm C Ø A)]
          (=> (LT.lt (hsize C Ø A t) n) (HHeld C n A))))))
  (fn [C :- HTy, n :- Nat, A :- HTy, t :- (HTm C Ø A), h :- (LT.lt (hsize C Ø A t) n)]
    (SubMk (HTm C Ø A) (fn [t :- (HTm C Ø A)] (LT.lt (hsize C Ø A t) n)) t h)))

(kd hVal (forall [C HTy] (forall [n Nat] (forall [A HTy] (=> (HHeld C n A) (HTm C Ø A)))))
  (fn [C :- HTy, n :- Nat, A :- HTy, x :- (HHeld C n A)] (Subtype.val x)))

(th hProp [C :- HTy, n :- Nat, A :- HTy, x :- (HHeld C n A)]
  (LT.lt (hsize C Ø A (hVal C n A x)) n)
  (exact (Subtype.property x)))

(kd HTyM (=> HTy Nat HTy (Sort 1))
  (fn [C :- HTy, n :- Nat, A :- HTy]
    (HTy.rec$2 (fn [_ :- HTy] (Sort 1))
      (fn [a :- HTy, b :- HTy, ia :- (Sort 1), ib :- (Sort 1)] (=> ia ib))
      (fn [a :- HTy, ia :- (Sort 1)] (HSum (HBlame C n) (HHeld C n a)))
      (HSum (HBlame C n) Unit)
      (HBlame C n)
      (=> (HSum (HBlame C n) (HHeld C n HTy.godel)) (HBlame C n))
      A)))

;; Blame propagates through every type: the `abort` clause.
(kd hErr (forall [C HTy] (forall [n Nat] (=> (HBlame C n) (forall [A HTy] (HTyM C n A)))))
  (fn [C :- HTy, n :- Nat, e :- (HBlame C n), A :- HTy]
    (HTy.rec$1 (fn [A :- HTy] (HTyM C n A))
      (fn [a :- HTy, b :- HTy, ia :- (HTyM C n a), ib :- (HTyM C n b)] (fn [_ :- (HTyM C n a)] ib))
      (fn [a :- HTy, ia :- (HTyM C n a)] (HSum.inl (HBlame C n) (HHeld C n a) e))
      (HSum.inl (HBlame C n) Unit e)
      e
      (fn [_ :- (HSum (HBlame C n) (HHeld C n HTy.godel))] e)
      A)))

(kd HEnvM (=> HTy Nat (List HTy) (Sort 1))
  (fn [C :- HTy, n :- Nat, G :- (List HTy)]
    (List.rec$2$0 HTy (fn [_ :- (List HTy)] (Sort 1)) Unit
      (fn [A :- HTy, rest :- (List HTy), h :- (Sort 1)] (Prod (HTyM C n A) h))
      G)))

(kd hLookup (forall [C HTy] (forall [n Nat] (forall [G (List HTy)] (forall [A HTy]
              (=> (HVar G A) (HEnvM C n G) (HTyM C n A))))))
  (fn [C :- HTy, n :- Nat, G :- (List HTy), A :- HTy, v :- (HVar G A)]
    (HVar.rec$1 (fn [G :- (List HTy), A :- HTy, _ :- (HVar G A)] (=> (HEnvM C n G) (HTyM C n A)))
      (fn [G :- (List HTy), A :- HTy]
        (fn [ρ :- (Prod (HTyM C n A) (HEnvM C n G))] (Prod.fst ρ)))
      (fn [G :- (List HTy), A :- HTy, B :- HTy, w :- (HVar G A), iw :- (=> (HEnvM C n G) (HTyM C n A))]
        (fn [ρ :- (Prod (HTyM C n B) (HEnvM C n G))] (iw (Prod.snd ρ))))
      G A v)))

;; ---------------------------------------------------------------------------
;; The size facts the interpreter's clauses need, each from the unfolded size.

;; Arithmetic is by explicit Nat lemmas wherever the goal is not False: Ansatz's
;; omega reaches Classical.choice through Classical.propDecidable on such goals,
;; and this argument does not need it.
(th hnatL [a :- Nat, b :- Nat, n :- Nat, h :- (LE.le (Nat.succ (Nat.add a b)) n)] (LE.le a n)
  (exact (Nat.le_trans (Nat.le_add_right a b) (Nat.le_of_succ_le h))))
(th hnatR [a :- Nat, b :- Nat, n :- Nat, h :- (LE.le (Nat.succ (Nat.add a b)) n)] (LE.le b n)
  (exact (Nat.le_trans (Nat.le_add_left b a) (Nat.le_of_succ_le h))))

(th hsLam [C :- HTy, G :- (List HTy), A :- HTy, B :- HTy, b :- (HTm C (List.cons HTy A G) B), n :- Nat,
           h :- (LE.le (hsize C G (HTy.arr A B) (HTm.lam C G A B b)) n)]
  (LE.le (hsize C (List.cons HTy A G) B b) n)
  (have h2 (LE.le (Nat.succ (hsize C (List.cons HTy A G) B b)) n) h)
  (exact (Nat.le_of_succ_le h2)))

(th hsAppF [C :- HTy, G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A), n :- Nat,
            h :- (LE.le (hsize C G B (HTm.app C G A B f x)) n)]
  (LE.le (hsize C G (HTy.arr A B) f) n)
  (exact (hnatL (hsize C G (HTy.arr A B) f) (hsize C G A x) n h)))

(th hsAppX [C :- HTy, G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A), n :- Nat,
            h :- (LE.le (hsize C G B (HTm.app C G A B f x)) n)]
  (LE.le (hsize C G A x) n)
  (exact (hnatR (hsize C G (HTy.arr A B) f) (hsize C G A x) n h)))

;; The guard: a quotation fits under the level only if what it holds is smaller.
(th hsQuo [C :- HTy, G :- (List HTy), A :- HTy, t :- (HTm C Ø A), n :- Nat,
           h :- (LE.le (hsize C G (HTy.box A) (HTm.quo C G A t)) n)]
  (LT.lt (hsize C Ø A t) n)
  (have h2 (LE.le (Nat.succ (hsize C Ø A t)) n) h)
  (exact (Nat.lt_of_succ_le h2)))

;; What the interpreter assumes at lower levels.
(kd HBelow (=> HTy Nat Prop)
  (fn [C :- HTy, n :- Nat] True))

;; Running a held refutation at its own level, weakened to the current level:
;; the `con` clause.
(kd hRunHeld (forall [C HTy] (forall [n Nat]
               (=> (forall [m Nat] (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m)))))
                   (HHeld C n ⊥)
                   (HBlame C n))))
  (fn [C :- HTy, n :- Nat,
       below :- (forall [m Nat] (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m))))),
       x :- (HHeld C n ⊥)]
    (hMk C n C
      (hVal C (hsize C Ø ⊥ (hVal C n ⊥ x)) C
        (below (hsize C Ø ⊥ (hVal C n ⊥ x)) (hProp C n ⊥ x) (hVal C n ⊥ x) (Nat.le_refl (hsize C Ø ⊥ (hVal C n ⊥ x)))))
      (Nat.lt_trans
        (hProp C (hsize C Ø ⊥ (hVal C n ⊥ x)) C
          (below (hsize C Ø ⊥ (hVal C n ⊥ x)) (hProp C n ⊥ x) (hVal C n ⊥ x) (Nat.le_refl (hsize C Ø ⊥ (hVal C n ⊥ x)))))
        (hProp C n ⊥ x)))))

;; The monitored interpreter at one level, given its result at lower levels.
;; The `ax` clause is the monitor: it returns the certificate it was handed,
;; which the level bounds. Neither clause needs the contract to be true.
(kd hInterp
  (forall [C HTy] (forall [n Nat]
    (=> (forall [m Nat] (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m)))))
        (forall [G (List HTy)] (forall [A HTy] (forall [t (HTm C G A)]
          (=> (LE.le (hsize C G A t) n) (HEnvM C n G) (HTyM C n A))))))))
  (fn [C :- HTy, n :- Nat,
       below :- (forall [m Nat] (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m))))),
       G :- (List HTy), A :- HTy, t :- (HTm C G A)]
    (HTm.rec$1 C
      (fn [G :- (List HTy), A :- HTy, t :- (HTm C G A)]
        (=> (LE.le (hsize C G A t) n) (HEnvM C n G) (HTyM C n A)))
      ;; var
      (fn [G :- (List HTy), A :- HTy, v :- (HVar G A),
           h :- (LE.le (hsize C G A (HTm.var C G A v)) n), ρ :- (HEnvM C n G)]
        (hLookup C n G A v ρ))
      ;; lam
      (fn [G :- (List HTy), A :- HTy, B :- HTy, b :- (HTm C (List.cons HTy A G) B),
           ib :- (=> (LE.le (hsize C (List.cons HTy A G) B b) n) (HEnvM C n (List.cons HTy A G)) (HTyM C n B)),
           h :- (LE.le (hsize C G (HTy.arr A B) (HTm.lam C G A B b)) n), ρ :- (HEnvM C n G)]
        (fn [a :- (HTyM C n A)]
          (ib (hsLam C G A B b n h) (Prod.mk (HTyM C n A) (HEnvM C n G) a ρ))))
      ;; app
      (fn [G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A),
           jf :- (=> (LE.le (hsize C G (HTy.arr A B) f) n) (HEnvM C n G) (HTyM C n (HTy.arr A B))),
           jx :- (=> (LE.le (hsize C G A x) n) (HEnvM C n G) (HTyM C n A)),
           h :- (LE.le (hsize C G B (HTm.app C G A B f x)) n), ρ :- (HEnvM C n G)]
        ((jf (hsAppF C G A B f x n h) ρ) (jx (hsAppX C G A B f x n h) ρ)))
      ;; tt
      (fn [G :- (List HTy), h :- (LE.le (hsize C G HTy.top (HTm.tt C G)) n), ρ :- (HEnvM C n G)]
        (HSum.inr (HBlame C n) Unit Unit.unit))
      ;; quo: the guard
      (fn [G :- (List HTy), A :- HTy, s :- (HTm C Ø A),
           is :- (=> (LE.le (hsize C Ø A s) n) (HEnvM C n Ø) (HTyM C n A)),
           h :- (LE.le (hsize C G (HTy.box A) (HTm.quo C G A s)) n), ρ :- (HEnvM C n G)]
        (HSum.inr (HBlame C n) (HHeld C n A) (hMk C n A s (hsQuo C G A s n h))))
      ;; fold
      (fn [G :- (List HTy), h :- (LE.le (hsize C G (HTy.arr (HTy.arr (HTy.box HTy.godel) ⊥) HTy.godel) (HTm.fold C G)) n),
           ρ :- (HEnvM C n G)]
        (fn [g :- (=> (HSum (HBlame C n) (HHeld C n HTy.godel)) (HBlame C n))] g))
      ;; unfold
      (fn [G :- (List HTy), h :- (LE.le (hsize C G (HTy.arr HTy.godel (HTy.arr (HTy.box HTy.godel) ⊥)) (HTm.unfold C G)) n),
           ρ :- (HEnvM C n G)]
        (fn [g :- (=> (HSum (HBlame C n) (HHeld C n HTy.godel)) (HBlame C n))] g))
      ;; con: run the held refutation, smaller than the level, at its own level
      (fn [G :- (List HTy), h :- (LE.le (hsize C G (HTy.arr (HTy.box ⊥) ⊥) (HTm.con C G)) n), ρ :- (HEnvM C n G)]
        (fn [x :- (HSum (HBlame C n) (HHeld C n ⊥))]
          (HSum.rec$1 (HBlame C n) (HHeld C n ⊥) (fn [_ :- (HSum (HBlame C n) (HHeld C n ⊥))] (HBlame C n))
            (fn [e :- (HBlame C n)] e)
            (fn [s :- (HHeld C n ⊥)] (hRunHeld C n below s))
            x)))
      ;; ax: the monitor returns the certificate it was handed
      (fn [G :- (List HTy), h :- (LE.le (hsize C G (HTy.arr (HTy.box C) ⊥) (HTm.ax C G)) n), ρ :- (HEnvM C n G)]
        (fn [x :- (HSum (HBlame C n) (HHeld C n C))]
          (HSum.rec$1 (HBlame C n) (HHeld C n C) (fn [_ :- (HSum (HBlame C n) (HHeld C n C))] (HBlame C n))
            (fn [e :- (HBlame C n)] e)
            (fn [c :- (HHeld C n C)] c)
            x)))
      ;; abort
      (fn [G :- (List HTy), A :- HTy, h :- (LE.le (hsize C G (HTy.arr ⊥ A) (HTm.abort C G A)) n), ρ :- (HEnvM C n G)]
        (fn [e :- (HBlame C n)] (hErr C n e A)))
      G A t)))

;; The interpreter's assumption, by induction on a bound on the level.
(kd hBelowAll (forall [C HTy] (forall [n Nat] (forall [m Nat]
                (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m)))))))
  (fn [C :- HTy, n :- Nat]
    (Nat.rec$1
      (fn [n :- Nat] (forall [m Nat] (=> (LT.lt m n) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m))))))
      (fn [m :- Nat, h :- (LT.lt m 0), s :- (HTm C Ø ⊥), hs :- (LE.le (hsize C Ø ⊥ s) m)]
        (FalseElim1 (HBlame C m) (Nat.not_lt_zero m h)))
      (fn [k :- Nat,
           ih :- (forall [m Nat] (=> (LT.lt m k) (forall [s (HTm C Ø ⊥)] (=> (LE.le (hsize C Ø ⊥ s) m) (HBlame C m))))),
           m :- Nat, hm :- (LT.lt m (Nat.succ k)), s :- (HTm C Ø ⊥), hs :- (LE.le (hsize C Ø ⊥ s) m)]
        (hInterp C m
          (fn [m2 :- Nat, hm2 :- (LT.lt m2 m), s2 :- (HTm C Ø ⊥), h2 :- (LE.le (hsize C Ø ⊥ s2) m2)]
            (ih m2 (Nat.lt_of_lt_of_le hm2 (Nat.le_of_lt_succ hm)) s2 h2))
          Ø ⊥ s hs Unit.unit))
      n)))

(kd hBlameAt (forall [C HTy] (forall [n Nat] (forall [s (HTm C Ø ⊥)]
               (=> (LE.le (hsize C Ø ⊥ s) n) (HBlame C n)))))
  (fn [C :- HTy, n :- Nat, s :- (HTm C Ø ⊥), h :- (LE.le (hsize C Ø ⊥ s) n)]
    (hBelowAll C (Nat.succ n) n (Nat.lt_succ_self n) s h)))

;; **No silent failure.** With no hypothesis on C: every closed term of ‘⊥’ runs
;; to a real counterexample to the contract, smaller than itself.
(kd hblame (forall [C HTy] (forall [s (HTm C Ø ⊥)] (HBlame C (hsize C Ø ⊥ s))))
  (fn [C :- HTy, s :- (HTm C Ø ⊥)]
    (hBlameAt C (hsize C Ø ⊥ s) s (Nat.le_refl (hsize C Ø ⊥ s)))))

(th hBlameSmaller [C :- HTy, s :- (HTm C Ø ⊥)]
  (LT.lt (hsize C Ø C (hVal C (hsize C Ø ⊥ s) C (hblame C s))) (hsize C Ø ⊥ s))
  (exact (hProp C (hsize C Ø ⊥ s) C (hblame C s))))

;; The contract holds on everything smaller than J. J is then a horizon.
(kd HTrueBelow (=> HTy Nat Prop)
  (fn [C :- HTy, J :- Nat]
    (forall [c (HTm C Ø C)] (=> (LT.lt (hsize C Ø C c) J) False))))

;; **Tightness.** No refutation fits under the horizon, though the contract may
;; be false at J.
(th hTightness [C :- HTy, J :- Nat, hθ :- (HTrueBelow C J), s :- (HTm C Ø ⊥), h :- (LE.le (hsize C Ø ⊥ s) J)]
  False
  (exact (hθ (hVal C (hsize C Ø ⊥ s) C (hblame C s)) (Nat.lt_of_lt_of_le (hBlameSmaller C s) h))))

;; A certificate of ‘□’ A runs to blame or to an output smaller than itself.
(kd hRun (forall [C HTy] (forall [A HTy] (forall [t (HTm C Ø (HTy.box A))]
           (HSum (HBlame C (hsize C Ø (HTy.box A) t)) (HHeld C (hsize C Ø (HTy.box A) t) A)))))
  (fn [C :- HTy, A :- HTy, t :- (HTm C Ø (HTy.box A))]
    (hInterp C (hsize C Ø (HTy.box A) t)
      (fn [m :- Nat, hm :- (LT.lt m (hsize C Ø (HTy.box A) t)), s :- (HTm C Ø ⊥), h :- (LE.le (hsize C Ø ⊥ s) m)]
        (hBlameAt C m s h))
      Ø (HTy.box A) t (Nat.le_refl (hsize C Ø (HTy.box A) t)) Unit.unit)))

;; **E-analogue.** Under the horizon the run returns a real output, bounded by
;; the certificate alone. Nothing about the bound depends on J.
(th hOutputBelow [C :- HTy, A :- HTy, J :- Nat, hθ :- (HTrueBelow C J),
                  t :- (HTm C Ø (HTy.box A)), h :- (LE.le (hsize C Ø (HTy.box A) t) J)]
  (Exists (fn [r :- (HTm C Ø A)] (LT.lt (hsize C Ø A r) (hsize C Ø (HTy.box A) t))))
  (exact (HSum.rec$0 (HBlame C (hsize C Ø (HTy.box A) t)) (HHeld C (hsize C Ø (HTy.box A) t) A)
           (fn [_ :- (HSum (HBlame C (hsize C Ø (HTy.box A) t)) (HHeld C (hsize C Ø (HTy.box A) t) A))]
             (Exists (fn [r :- (HTm C Ø A)] (LT.lt (hsize C Ø A r) (hsize C Ø (HTy.box A) t)))))
           (fn [e :- (HBlame C (hsize C Ø (HTy.box A) t))]
             (False.elim (hθ (hVal C (hsize C Ø (HTy.box A) t) C e)
                             (Nat.lt_of_lt_of_le (hProp C (hsize C Ø (HTy.box A) t) C e) h))))
           (fn [r :- (HHeld C (hsize C Ø (HTy.box A) t) A)]
             (Exists.intro (hVal C (hsize C Ø (HTy.box A) t) A r)
                           (hProp C (hsize C Ø (HTy.box A) t) A r)))
           (hRun C A t))))

;; Under the horizon there is no fake certificate of a refutation either.
(th hNoFakeBelow [C :- HTy, J :- Nat, hθ :- (HTrueBelow C J),
                  t :- (HTm C Ø (HTy.box ⊥)), hle :- (LE.le (hsize C Ø (HTy.box ⊥) t) J)]
  False
  (have he (Exists (fn [r :- (HTm C Ø ⊥)] (LT.lt (hsize C Ø ⊥ r) (hsize C Ø (HTy.box ⊥) t))))
    (hOutputBelow C ⊥ J hθ t hle))
  (cases he)
  (exact (hTightness C J hθ w (Nat.le_trans (Nat.le_of_lt h) hle))))

;; **A-analogue.** A certified universal claim, "no certificate of A exists", has
;; no counterexample under the horizon. Program and input each need only fit
;; under J: the level is one cap, not a budget they share.
(th hUniversalBelow [C :- HTy, A :- HTy, J :- Nat, hθ :- (HTrueBelow C J),
                     f :- (HTm C Ø (HTy.arr (HTy.box A) ⊥)), hf :- (LE.le (hsize C Ø (HTy.arr (HTy.box A) ⊥) f) J),
                     x :- (HTm C Ø A), hx :- (LT.lt (hsize C Ø A x) J)]
  False
  (have b (HBlame C J)
    ((hInterp C J
       (fn [m :- Nat, hm :- (LT.lt m J), s :- (HTm C Ø ⊥), h :- (LE.le (hsize C Ø ⊥ s) m)] (hBlameAt C m s h))
       Ø (HTy.arr (HTy.box A) ⊥) f hf Unit.unit)
     (HSum.inr (HBlame C J) (HHeld C J A) (hMk C J A x hx))))
  (exact (hθ (hVal C J C b) (hProp C J C b))))

;; **Self-justification is the special case C := ‘⊥’.** The contract is then the
;; consistency axiom, and a refutation's counterexample is a smaller refutation.
(th hConsAt [n :- Nat]
  (forall [s (HTm ⊥ Ø ⊥)] (=> (LE.le (hsize ⊥ Ø ⊥ s) n) False))
  (induction n)
  (intro s h)
  (exact (Nat.not_lt_zero (hsize ⊥ Ø ⊥ (hVal ⊥ (hsize ⊥ Ø ⊥ s) ⊥ (hblame ⊥ s)))
           (Nat.lt_of_lt_of_le (hBlameSmaller ⊥ s) h)))
  (intro s h)
  (exact (ih_n (hVal ⊥ (hsize ⊥ Ø ⊥ s) ⊥ (hblame ⊥ s))
           (Nat.le_of_lt_succ (Nat.lt_of_lt_of_le (hBlameSmaller ⊥ s) h)))))

(th hConsistentWhenContractIsConsistency [s :- (HTm ⊥ Ø ⊥)] False
  (exact (hConsAt (hsize ⊥ Ø ⊥ s) s (Nat.le_refl (hsize ⊥ Ø ⊥ s)))))

;; ---------------------------------------------------------------------------
;; The theorems are not vacuous: a false contract with a tunable horizon.

;; ‘□’ applied k times to ‘⊤’.
(kd hboxes (=> Nat HTy)
  (fn [k :- Nat] (Nat.rec$1 (fn [_ :- Nat] HTy) HTy.top (fn [j :- Nat, ih :- HTy] (HTy.box ih)) k)))

;; The smallest certificate of `hboxes k`, of size k + 1.
(kd hquos (forall [C HTy] (forall [k Nat] (HTm C Ø (hboxes k))))
  (fn [C :- HTy, k :- Nat]
    (Nat.rec$1 (fn [k :- Nat] (HTm C Ø (hboxes k)))
      (HTm.tt C Ø)
      (fn [j :- Nat, ih :- (HTm C Ø (hboxes j))] (HTm.quo C Ø (hboxes j) ih))
      k)))

(th hSizeQuos [C :- HTy, k :- Nat] (Eq Nat (hsize C Ø (hboxes k) (hquos C k)) (Nat.succ k))
  (induction k)
  (rfl)
  (have q (Eq Nat (Nat.succ (hsize C Ø (hboxes n) (hquos C n))) (Nat.succ (Nat.succ n)))
    (congrArg Nat.succ ih_n))
  (exact q))

(th hOneLeSucc [x :- Nat] (LE.le 1 (Nat.succ x))
  (exact (Nat.succ_le_succ (Nat.zero_le x))))

(th hSizePos [C :- HTy, G :- (List HTy), A :- HTy, t :- (HTm C G A)] (LE.le 1 (hsize C G A t))
  (exact (HTm.rec$0 C (fn [G :- (List HTy), A :- HTy, t :- (HTm C G A)] (LE.le 1 (hsize C G A t)))
           (fn [G :- (List HTy), A :- HTy, v :- (HVar G A)] (Nat.le_refl 1))
           (fn [G :- (List HTy), A :- HTy, B :- HTy, b :- (HTm C (List.cons HTy A G) B), ib :- (LE.le 1 (hsize C (List.cons HTy A G) B b))]
             (hOneLeSucc (hsize C (List.cons HTy A G) B b)))
           (fn [G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A),
                jf :- (LE.le 1 (hsize C G (HTy.arr A B) f)), jx :- (LE.le 1 (hsize C G A x))]
             (hOneLeSucc (Nat.add (hsize C G (HTy.arr A B) f) (hsize C G A x))))
           (fn [G :- (List HTy)] (Nat.le_refl 1))
           (fn [G :- (List HTy), A :- HTy, s :- (HTm C Ø A), is :- (LE.le 1 (hsize C Ø A s))]
             (hOneLeSucc (hsize C Ø A s)))
           (fn [G :- (List HTy)] (Nat.le_refl 1))
           (fn [G :- (List HTy)] (Nat.le_refl 1))
           (fn [G :- (List HTy)] (Nat.le_refl 1))
           (fn [G :- (List HTy)] (Nat.le_refl 1))
           (fn [G :- (List HTy), A :- HTy] (Nat.le_refl 1))
           G A t)))

(th hc1 [a :- Nat, b :- Nat, k :- Nat, h1 :- (LT.lt a b), h2 :- (LE.le b k), h3 :- (LE.le (Nat.succ k) a)] False (omega))
(th hc2 [a :- Nat, b :- Nat, n :- Nat, h1 :- (LT.lt a b), h2 :- (LE.le b (Nat.succ n))] (LE.le a n)
  (exact (Nat.le_of_lt_succ (Nat.lt_of_lt_of_le h1 h2))))
(th hc3 [a :- Nat, b :- Nat, k :- Nat, h1 :- (LT.lt a b), h2 :- (LE.le b k)] (LE.le a k)
  (exact (Nat.le_trans (Nat.le_of_lt h1) h2)))
(th hc4 [j :- Nat, a :- Nat, b :- Nat, h1 :- (LE.le (Nat.succ j) a), h2 :- (LT.lt a b)] (LE.le (Nat.succ (Nat.succ j)) b)
  (exact (Nat.succ_le_of_lt (Nat.lt_of_le_of_lt h1 h2))))
(th hc5 [a :- Nat, h1 :- (LE.le a 0), h2 :- (LE.le 1 a)] False (omega))

;; The induction hypothesis of `hMinCert`.
(kd HMinIH (=> Nat Nat Prop)
  (fn [k :- Nat, n :- Nat]
    (forall [j Nat] (forall [t (HTm (hboxes k) Ø (hboxes j))]
      (=> (LE.le (hsize (hboxes k) Ø (hboxes j) t) n)
          (LE.le (hsize (hboxes k) Ø (hboxes j) t) k)
          (LE.le (Nat.succ j) (hsize (hboxes k) Ø (hboxes j) t)))))))

;; One step of `hMinCert`, at ‘□’ (hboxes j): the term yields a smaller term of
;; the contract type, which the induction hypothesis rules out, or a smaller
;; term of hboxes j, which it bounds below.
(th hMinC [k :- Nat, n :- Nat, ih :- (HMinIH k n), j :- Nat, t :- (HTm (hboxes k) Ø (HTy.box (hboxes j))),
           h1 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (Nat.succ n)), h2 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k),
           c :- (HTm (hboxes k) Ø (hboxes k)), hc :- (LT.lt (hsize (hboxes k) Ø (hboxes k) c) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))]
  (LE.le (Nat.succ (Nat.succ j)) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))
  (have i1 (LE.le (hsize (hboxes k) Ø (hboxes k) c) n) (hc2 (hsize (hboxes k) Ø (hboxes k) c) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) n hc h1))
  (have i2 (LE.le (hsize (hboxes k) Ø (hboxes k) c) k) (hc3 (hsize (hboxes k) Ø (hboxes k) c) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k hc h2))
  (exact (False.elim (hc1 (hsize (hboxes k) Ø (hboxes k) c) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k hc h2 (ih k c i1 i2)))))

(th hMinR [k :- Nat, n :- Nat, ih :- (HMinIH k n), j :- Nat, t :- (HTm (hboxes k) Ø (HTy.box (hboxes j))),
           h1 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (Nat.succ n)), h2 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k),
           r :- (HTm (hboxes k) Ø (hboxes j)), hr :- (LT.lt (hsize (hboxes k) Ø (hboxes j) r) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))]
  (LE.le (Nat.succ (Nat.succ j)) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))
  (have i1 (LE.le (hsize (hboxes k) Ø (hboxes j) r) n) (hc2 (hsize (hboxes k) Ø (hboxes j) r) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) n hr h1))
  (have i2 (LE.le (hsize (hboxes k) Ø (hboxes j) r) k) (hc3 (hsize (hboxes k) Ø (hboxes j) r) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k hr h2))
  (exact (hc4 j (hsize (hboxes k) Ø (hboxes j) r) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (ih j r i1 i2) hr)))

(th hMinSucc [k :- Nat, n :- Nat, ih :- (HMinIH k n), j :- Nat, t :- (HTm (hboxes k) Ø (HTy.box (hboxes j))),
              h1 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (Nat.succ n)), h2 :- (LE.le (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) k)]
  (LE.le (Nat.succ (Nat.succ j)) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))
  (exact (HSum.rec$0
           (HBlame (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))
           (HHeld (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes j))
           (fn [_ :- (HSum (HBlame (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t)) (HHeld (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes j)))]
             (LE.le (Nat.succ (Nat.succ j)) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t)))
           (fn [e :- (HBlame (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t))]
             (hMinC k n ih j t h1 h2 (hVal (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes k) e) (hProp (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes k) e)))
           (fn [r :- (HHeld (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes j))]
             (hMinR k n ih j t h1 h2 (hVal (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes j) r) (hProp (hboxes k) (hsize (hboxes k) Ø (HTy.box (hboxes j)) t) (hboxes j) r)))
           (hRun (hboxes k) (hboxes j) t))))

;; Every closed term of `hboxes j` with size at most k has size at least j + 1,
;; in the syntax whose contract is on `hboxes k`.
(th hMinCert [k :- Nat, n :- Nat]
  (forall [j Nat] (forall [t (HTm (hboxes k) Ø (hboxes j))]
    (=> (LE.le (hsize (hboxes k) Ø (hboxes j) t) n)
        (LE.le (hsize (hboxes k) Ø (hboxes j) t) k)
        (LE.le (Nat.succ j) (hsize (hboxes k) Ø (hboxes j) t)))))
  (induction n)
  (intro j t h0 hk)
  (exact (False.elim (hc5 _ h0 (hSizePos (hboxes k) Ø (hboxes j) t))))
  (intro j)
  (cases j)
  (intro t h1 h2)
  (exact (hSizePos (hboxes k) Ø _ t))
  (intro t h1 h2)
  (exact (hMinSucc k _ ih_n _ t h1 h2)))

;; The contract on `hboxes k` is true below k + 1 ...
(th hContractTrueBelow [k :- Nat] (HTrueBelow (hboxes k) (Nat.succ k))
  (intro c hc)
  (exact (Nat.lt_irrefl (hsize (hboxes k) Ø (hboxes k) c)
           (Nat.lt_of_lt_of_le hc
             (hMinCert k (hsize (hboxes k) Ø (hboxes k) c) k c
               (Nat.le_refl (hsize (hboxes k) Ø (hboxes k) c))
               (Nat.le_of_lt_succ hc))))))

;; ... false at k + 1, where `hquos` is its counterexample ...
(th hContractFalseAt [k :- Nat]
  (Exists (fn [c :- (HTm (hboxes k) Ø (hboxes k))] (Eq Nat (hsize (hboxes k) Ø (hboxes k) c) (Nat.succ k))))
  (exact (Exists.intro (hquos (hboxes k) k) (hSizeQuos (hboxes k) k))))

;; ... and refuted a constant above that.
(kd hRefute (forall [k Nat] (HTm (hboxes k) Ø ⊥))
  (fn [k :- Nat]
    (HTm.app (hboxes k) Ø (HTy.box (hboxes k)) ⊥
      (HTm.ax (hboxes k) Ø)
      (HTm.quo (hboxes k) Ø (hboxes k) (hquos (hboxes k) k)))))

(th hs4 [a :- Nat, k :- Nat, e :- (Eq Nat a (Nat.succ k))]
  (Eq Nat (Nat.succ (Nat.add 1 (Nat.succ a))) (Nat.add k 4))
  (have h1 (Eq Nat (Nat.add 1 k) (Nat.succ k)) (Nat.add_comm 1 k))
  (have h2 (Eq Nat (Nat.succ (Nat.add 1 (Nat.succ (Nat.succ k)))) (Nat.add k 4))
    (congrArg (fn [x :- Nat] (Nat.succ (Nat.succ (Nat.succ x)))) h1))
  (exact (Eq.trans (congrArg (fn [x :- Nat] (Nat.succ (Nat.add 1 (Nat.succ x)))) e) h2)))

(th hSizeRefute [k :- Nat] (Eq Nat (hsize (hboxes k) Ø ⊥ (hRefute k)) (Nat.add k 4))
  (exact (hs4 (hsize (hboxes k) Ø (hboxes k) (hquos (hboxes k) k)) k (hSizeQuos (hboxes k) k))))

;; **The horizon is sharp to within 3.** With J = k + 1: no refutation of size at
;; most J, and one of size J + 3.
(th hHorizonSharp [k :- Nat]
  (And (forall [s (HTm (hboxes k) Ø ⊥)] (=> (LE.le (hsize (hboxes k) Ø ⊥ s) (Nat.succ k)) False))
       (Exists (fn [s :- (HTm (hboxes k) Ø ⊥)] (Eq Nat (hsize (hboxes k) Ø ⊥ s) (Nat.add k 4)))))
  (exact (And.intro
           (fn [s :- (HTm (hboxes k) Ø ⊥), h :- (LE.le (hsize (hboxes k) Ø ⊥ s) (Nat.succ k))]
             (hTightness (hboxes k) (Nat.succ k) (hContractTrueBelow k) s h))
           (Exists.intro (hRefute k) (hSizeRefute k)))))

;; Blame names the actual counterexample. Definitional here, since a run reduces.
(th hBlameRefute [k :- Nat]
  (Eq (HTm (hboxes k) Ø (hboxes k))
      (hVal (hboxes k) (hsize (hboxes k) Ø ⊥ (hRefute k)) (hboxes k) (hblame (hboxes k) (hRefute k)))
      (hquos (hboxes k) k))
  (rfl))

;; The worked example: the contract on `hboxes 3` first fails at size 4, and its
;; refutation has size 7.
(th hExample3 []
  (And (Eq Nat (hsize (hboxes 3) Ø (hboxes 3) (hquos (hboxes 3) 3)) 4)
       (Eq Nat (hsize (hboxes 3) Ø ⊥ (hRefute 3)) 7))
  (exact (And.intro (Eq.refl 4) (Eq.refl 7))))

;; Above the horizon `abort` certifies a false claim: a certificate of ‘⊥’.
(kd hFake (forall [k Nat] (HTm (hboxes k) Ø (HTy.box ⊥)))
  (fn [k :- Nat]
    (HTm.app (hboxes k) Ø ⊥ (HTy.box ⊥) (HTm.abort (hboxes k) Ø (HTy.box ⊥)) (hRefute k))))

(kd hIsInl (forall [X Type] (forall [Y Type] (=> (HSum X Y) Bool)))
  (fn [X :- Type, Y :- Type, s :- (HSum X Y)]
    (HSum.rec$1 X Y (fn [_ :- (HSum X Y)] Bool) (fn [_ :- X] Bool.true) (fn [_ :- Y] Bool.false) s)))

;; The monitor catches it: the run blames instead of returning an output.
(th hFakeIsBlamed [k :- Nat]
  (Eq Bool (hIsInl (HBlame (hboxes k) (hsize (hboxes k) Ø (HTy.box ⊥) (hFake k)))
                   (HHeld (hboxes k) (hsize (hboxes k) Ø (HTy.box ⊥) (hFake k)) ⊥)
                   (hRun (hboxes k) ⊥ (hFake k)))
      Bool.true)
  (rfl))

;; ---------------------------------------------------------------------------
;; The contrast: a contract on a sort whose values cost nothing to name.

(kd htower (=> Nat Nat)
  (fn [k :- Nat] (Nat.rec$1 (fn [_ :- Nat] Nat) 1 (fn [j :- Nat, ih :- Nat] (Nat.pow 2 ih)) k)))

;; A false contract on `Nat` falls to a constant-size term, however far out its
;; first violation is, and nothing computes `htower 5`. This is Ansatz's own
;; logic with the contract as a hypothesis, not λᶜᵉʳᵗ₀.
(th hDataContractRefuted []
  (=> (forall [n Nat] (=> (Eq Nat n (htower 5)) False)) False)
  (intro ax)
  (exact (ax (htower 5) (Eq.refl (htower 5)))))

;; ---------------------------------------------------------------------------
;; Negative checks: each statement must be refused.

(defn must-reject
  "Require that the statement is refused, and print the reason, so that a check
  cannot pass on an unrelated error such as a missing constant."
  [label params prop tactics]
  (let [reason (try (a/prove-theorem (gensym "neg_") (b/lv (expand params)) (b/lv (expand prop))
                                     (b/lv (expand (vec tactics))))
                    nil
                    (catch Throwable e (or (.getMessage e) (str e))))]
    (when-not reason (throw (ex-info (str "accepted, should be refused: " label) {})))
    (when (re-find #"Unknown constant" reason)
      (throw (ex-info (str "refused for an unrelated reason: " label " -- " reason) {})))
    (println "REFUSED" label "--" (subs reason 0 (min 170 (count reason))))))

;; A variant size in which quotation is free.
(kd hsizeFree (forall [C HTy] (forall [G (List HTy)] (forall [A HTy] (=> (HTm C G A) Nat))))
  (fn [C :- HTy, G :- (List HTy), A :- HTy, t :- (HTm C G A)]
    (HTm.rec$1 C (fn [G :- (List HTy), A :- HTy, _ :- (HTm C G A)] Nat)
      (fn [G :- (List HTy), A :- HTy, v :- (HVar G A)] 1)
      (fn [G :- (List HTy), A :- HTy, B :- HTy, b :- (HTm C (List.cons HTy A G) B), ib :- Nat] (Nat.succ ib))
      (fn [G :- (List HTy), A :- HTy, B :- HTy, f :- (HTm C G (HTy.arr A B)), x :- (HTm C G A), jf :- Nat, jx :- Nat]
        (Nat.succ (Nat.add jf jx)))
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy), A :- HTy, t :- (HTm C Ø A), it :- Nat] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy)] 1)
      (fn [G :- (List HTy), A :- HTy] 1)
      G A t)))

;; 1. With free quotation the guard's step does not go through.
(must-reject "guard with free quotation"
  '[C :- HTy, G :- (List HTy), A :- HTy, t :- (HTm C Ø A), n :- Nat,
    h :- (LE.le (hsizeFree C G (HTy.box A) (HTm.quo C G A t)) n)]
  '(LT.lt (hsizeFree C Ø A t) n)
  '[(have h2 (LE.le (Nat.succ (hsizeFree C Ø A t)) n) h) (exact (Nat.lt_of_succ_le h2))])

;; 2. Internal quotation (D3) needs size (quo s) < n from size s < n.
(must-reject "internal quotation under the level"
  '[C :- HTy, A :- HTy, s :- (HTm C Ø A), n :- Nat, h :- (LT.lt (hsize C Ø A s) n)]
  '(LT.lt (hsize C Ø (HTy.box A) (HTm.quo C Ø A s)) n)
  '[(exact h)])

;; 3. Tightness without its size hypothesis.
(must-reject "tightness with no size bound"
  '[C :- HTy, J :- Nat, hθ :- (HTrueBelow C J), s :- (HTm C Ø ⊥)]
  'False
  '[(exact (hθ (hVal C (hsize C Ø ⊥ s) C (hblame C s)) (hBlameSmaller C s)))])

;; 4. The refutation does not fit under the horizon.
(must-reject "refutation under the horizon"
  '[k :- Nat]
  '(LE.le (hsize (hboxes k) Ø ⊥ (hRefute k)) (Nat.succ k))
  '[(omega)])

;; 5. Blame is the actual counterexample, not another certificate of the same
;; type: at k = 1 it is `quo tt`, not `quo ((fn x => x) tt)`.
(must-reject "blame names another term"
  '[]
  '(Eq (HTm (hboxes 1) Ø (hboxes 1))
       (hVal (hboxes 1) (hsize (hboxes 1) Ø ⊥ (hRefute 1)) (hboxes 1) (hblame (hboxes 1) (hRefute 1)))
       (HTm.quo (hboxes 1) Ø HTy.top
         (HTm.app (hboxes 1) Ø HTy.top HTy.top
           (HTm.lam (hboxes 1) Ø HTy.top HTy.top
             (HTm.var (hboxes 1) (List.cons HTy HTy.top Ø) HTy.top (HVar.zero Ø HTy.top)))
           (HTm.tt (hboxes 1) Ø))))
  '[(rfl)])

;; ---------------------------------------------------------------------------
;; An axiom audit, standing in for Lean's `#print axioms`: every axiom reachable
;; from a declaration's type and value. A theorem's value is its proof.

(defn axioms-of [root]
  (let [seen-names (atom #{})
        seen-exprs (java.util.IdentityHashMap.)
        found (atom (sorted-set))
        visit-name (fn visit-name [nm]
                     (when-not (contains? @seen-names (str nm))
                       (swap! seen-names conj (str nm))
                       (let [ci (kenv/lookup @a/ansatz-env nm)]
                         (when (kenv/axiom? ci) (swap! found conj (str nm)))
                         nm)))
        queue (java.util.ArrayDeque.)
        push (fn [e] (when (and e (not (.containsKey seen-exprs e)))
                       (.put seen-exprs e true)
                       (.push queue e)))
        visit-const (fn [nm]
                      (when (visit-name nm)
                        (let [ci (kenv/lookup @a/ansatz-env nm)]
                          (push (kenv/ci-type ci))
                          (push (kenv/ci-value ci)))))]
    (visit-const (kname/from-string (str root)))
    (while (not (.isEmpty queue))
      (let [e (.pop queue)]
        (cond
          (ke/const? e) (visit-const (ke/const-name e))
          (ke/app? e) (do (push (ke/app-fn e)) (push (ke/app-arg e)))
          (ke/lam? e) (do (push (ke/lam-type e)) (push (ke/lam-body e)))
          (ke/forall? e) (do (push (ke/forall-type e)) (push (ke/forall-body e)))
          (ke/let? e) (do (push (ke/let-type e)) (push (ke/let-value e)) (push (ke/let-body e)))
          (ke/mdata? e) (push (ke/mdata-expr e))
          (ke/proj? e) (do (visit-const (ke/proj-type-name e)) (push (ke/proj-struct e))))))
    @found))

(doseq [nm '[hblame hTightness hOutputBelow hUniversalBelow hConsistentWhenContractIsConsistency
             hHorizonSharp hBlameRefute hFakeIsBlamed]]
  (println "AXIOMS" nm (vec (axioms-of nm))))

;; The audit's positive control, last so it can touch nothing above: a harmless
;; axiom, and a theorem that uses it, which the audit must report.
(swap! a/ansatz-env kenv/check-constant
       (kenv/mk-axiom (kname/from-string "hAuditAx") [] (ke/const' (kname/from-string "Nat") [])))
(th hAuditUse [] (Eq Nat hAuditAx hAuditAx) (exact (Eq.refl hAuditAx)))
(when-not (contains? (axioms-of 'hAuditUse) "hAuditAx")
  (throw (ex-info "the axiom audit missed a known axiom" {})))
(println "AUDIT-CONTROL ok" (vec (axioms-of 'hAuditUse)))

(println "HORIZON-OK")
