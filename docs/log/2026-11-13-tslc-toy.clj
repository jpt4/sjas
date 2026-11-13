;; The time-and-space certificate calculus (TSLC), as a toy kernel-checked in
;; Ansatz. The note is 2026-11-13-tslc-walkthrough.md.
;;
;; One syntax has both guards. A `held` certificate is closed syntax smaller
;; than the budget: the size guard of λᶜᵉʳᵗ₀ and of 2026-10-29-guarded-trust.
;; A `code` is closed syntax of any size, free, with D2 and D3 at a variable.
;; `later A` is A next round, and `become` hands a code over to the next round:
;; the time guard of Fallenstein's T_κ. Types are read at r rounds left and
;; budget n. The proof is by induction on rounds, then on budget.
;;
;; Checked here:
;; - sConsistent: no closed term of ⊥.
;; - sSelfJustifying: H0 (`con`) and H1 (`con1`) are closed terms, and the
;;   syntax is consistent.
;; - sBecomeSpec: with s+1 rounds left, `become` of a code is that code's own
;;   reading with s rounds left.
;; - sLaterBotSucc, sLaterBotZero, refusal 2: "falsehood next round" is empty
;;   while rounds are left and inhabited when none are.
;; - sTgH0Meaning: the time-guarded H0 means "no code refutation" only in the
;;   reading with a round left, which is outside the syntax.
;; - sLineageSafe: every agent acts safely in every round, whatever successors
;;   it chooses. sModifyRun and sAdoptHeldRun run two lineages in the kernel.
;; - sNoSoundReading: reflection on codes at ⊥, without the delay, derives ⊥
;;   with the Gödel fixed point and D2, D3 on codes.
;; - Refusals 1 and 3: a code has no size guard; H for the next round cannot be
;;   read by the step that reads H0.
;;
;; Left out: tokens and affinity (sizes stand for budgets), dependent types,
;; `next`, applying under `later`, guarded recursion, agents with inputs, and a
;; checker the syntax can reason about.
;;
;; Run from code/lcert (ANSATZ_OFFLINE=1, under the usual caps):
;;   clojure -M:formal -i ../../docs/log/2026-11-13-tslc-toy.clj
;; A rejected declaration aborts the load; the last line printed is TSLC-TOY-OK.
(ns tslc-toy
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as kenv]
            [ansatz.kernel.expr :as ke]
            [ansatz.kernel.name :as kname]
            [clojure.walk :as w]
            [lcert.formal.base :as b]))

;; Abbreviations, expanded before elaboration.
(def abbreviations {'Ø '(List.nil STy),
                    ;; Subtype.mk, False.elim and PSigma's constructor and
                    ;; projections with every argument explicit, at Type.
                    'SubMk (symbol "@Subtype.mk.{1}"),
                    'FalseElim1 (symbol "@False.elim.{1}"),
                    'SigMk (symbol "@PSigma.mk.{1,1}"),
                    'SigSnd (symbol "@PSigma.snd.{1,1}")})
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
;; Types. `held A` is a certificate: closed syntax of A smaller than the budget.
;; `code A` is free: closed syntax of A of any size. `later A` is A next round.
;; `godel` is the naming fixed point G ≅ (code G → ⊥). An `agent` emits a safe
;; action and the code of the agent that runs the next round.
(a/inductive STy [] (arr [a STy] [b STy]) (held [a STy]) (code [a STy]) (later [a STy])
  (bot) (godel) (agent) (safe))

(a/inductive SVar [] :indices [G (List STy), A STy]
  (zero [G (List STy)] [A STy] :where [(List.cons STy A G) A])
  (succ [G (List STy)] [A STy] [B STy] [v (SVar G A)] :where [(List.cons STy B G) A]))

(a/inductive STm [] :indices [G (List STy), A STy]
  (var [G (List STy)] [A STy] [v (SVar G A)] :where [G A])
  (lam [G (List STy)] [A STy] [B STy] [b (STm (List.cons STy A G) B)] :where [G (STy.arr A B)])
  (app [G (List STy)] [A STy] [B STy] [f (STm G (STy.arr A B))] [x (STm G A)] :where [G B])
  ;; a certificate of a closed term: costs the term's size plus one
  (quoH [G (List STy)] [A STy] [t (STm (List.nil STy) A)] :where [G (STy.held A)])
  ;; the code of a closed term: free
  (quoC [G (List STy)] [A STy] [t (STm (List.nil STy) A)] :where [G (STy.code A)])
  ;; D3 and D2 at a variable, on codes
  (iquoC [G (List STy)] [A STy] :where [G (STy.arr (STy.code A) (STy.code (STy.code A)))])
  (iappC [G (List STy)] [A STy] [B STy]
         :where [G (STy.arr (STy.code (STy.arr A B)) (STy.arr (STy.code A) (STy.code B)))])
  (fold [G (List STy)] :where [G (STy.arr (STy.arr (STy.code STy.godel) STy.bot) STy.godel)])
  (unfold [G (List STy)] :where [G (STy.arr STy.godel (STy.arr (STy.code STy.godel) STy.bot))])
  ;; H0 and H1: no held certificate refutes the syntax; no two prove A and ¬A
  (con [G (List STy)] :where [G (STy.arr (STy.held STy.bot) STy.bot)])
  (con1 [G (List STy)] [A STy]
        :where [G (STy.arr (STy.held A) (STy.arr (STy.held (STy.arr A STy.bot)) STy.bot))])
  ;; the hand-over rule: a code of A gives A next round
  (become [G (List STy)] [A STy] :where [G (STy.arr (STy.code A) (STy.later A))])
  ;; reflection at the agent type, on held certificates
  (reflA [G (List STy)] :where [G (STy.arr (STy.held STy.agent) STy.agent)])
  (act0 [G (List STy)] :where [G STy.safe])
  (act2 [G (List STy)] :where [G STy.safe])
  ;; an agent that is handed its own code: f picks the action, g the successor
  (quine [G (List STy)] [f (STm (List.nil STy) (STy.arr (STy.code STy.agent) STy.safe))]
         [g (STm (List.nil STy) (STy.arr (STy.code STy.agent) (STy.code STy.agent)))]
         :where [G STy.agent])
  (actOf [G (List STy)] :where [G (STy.arr STy.agent STy.safe)])
  (succOf [G (List STy)] :where [G (STy.arr STy.agent (STy.code STy.agent))])
  (abort [G (List STy)] [A STy] :where [G (STy.arr STy.bot A)]))

;; Size: what a certificate's reach is bounded by. Codes cost 1 whatever they hold.
(kd ssize (forall [G (List STy)] (forall [A STy] (=> (STm G A) Nat)))
  (fn [G :- (List STy), A :- STy, t :- (STm G A)]
    (STm.rec$1 (fn [G :- (List STy), A :- STy, _ :- (STm G A)] Nat)
      (fn [G :- (List STy), A :- STy, v :- (SVar G A)] 1)
      (fn [G :- (List STy), A :- STy, B :- STy, b :- (STm (List.cons STy A G) B), ib :- Nat] (Nat.succ ib))
      (fn [G :- (List STy), A :- STy, B :- STy, f :- (STm G (STy.arr A B)), x :- (STm G A), jf :- Nat, jx :- Nat]
        (Nat.succ (Nat.add jf jx)))
      (fn [G :- (List STy), A :- STy, t :- (STm Ø A), it :- Nat] (Nat.succ it))
      (fn [G :- (List STy), A :- STy, t :- (STm Ø A), it :- Nat] 1)
      (fn [G :- (List STy), A :- STy] 1)
      (fn [G :- (List STy), A :- STy, B :- STy] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy), A :- STy] 1)
      (fn [G :- (List STy), A :- STy] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy), f :- (STm Ø (STy.arr (STy.code STy.agent) STy.safe)),
           g :- (STm Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent))), jf :- Nat, jg :- Nat]
        (Nat.succ (Nat.add jf jg)))
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy)] 1)
      (fn [G :- (List STy), A :- STy] 1)
      G A t)))

;; ---------------------------------------------------------------------------
;; The model. A type is read at r rounds left and budget n.

;; Safe actions are the even numbers. The syntax can name only safe ones.
(kd SSafeP (=> Nat Prop) (fn [k :- Nat] (Eq Nat (Nat.mod k 2) 0)))
(kd SSafe (Sort 1) (Subtype Nat (fn [k :- Nat] (SSafeP k))))
;; An empty type (the bundled Init has no `Empty`).
(kd SNone (Sort 1) (Subtype Unit (fn [_ :- Unit] False)))
;; A held certificate of A at budget n: closed syntax of A smaller than n.
(kd SHeld (=> Nat STy (Sort 1))
  (fn [n :- Nat, A :- STy] (Subtype (STm Ø A) (fn [t :- (STm Ø A)] (LT.lt (ssize Ø A t) n)))))

(kd STyF (=> STy Nat Nat (Sort 1))
  (fn [A :- STy]
    (STy.rec$2 (fn [_ :- STy] (=> Nat Nat (Sort 1)))
      (fn [a :- STy, b :- STy, ia :- (=> Nat Nat (Sort 1)), ib :- (=> Nat Nat (Sort 1))]
        (fn [r :- Nat, n :- Nat] (=> (ia r n) (ib r n))))
      (fn [a :- STy, ia :- (=> Nat Nat (Sort 1))] (fn [r :- Nat, n :- Nat] (SHeld n a)))
      (fn [a :- STy, ia :- (=> Nat Nat (Sort 1))] (fn [r :- Nat, n :- Nat] (STm Ø a)))
      ;; later: nothing with no rounds left; with s+1 left, A at s left, at the
      ;; budget the next round is given
      (fn [a :- STy, ia :- (=> Nat Nat (Sort 1))]
        (fn [r :- Nat, n :- Nat]
          (Nat.rec$2 (fn [_ :- Nat] (Sort 1)) Unit
            (fn [s :- Nat, _ :- (Sort 1)] (PSigma Nat (fn [m :- Nat] (ia s m)))) r)))
      (fn [r :- Nat, n :- Nat] SNone)
      (fn [r :- Nat, n :- Nat] (=> (STm Ø STy.godel) SNone))
      (fn [r :- Nat, n :- Nat] (Prod SSafe (STm Ø STy.agent)))
      (fn [r :- Nat, n :- Nat] SSafe)
      A)))
(kd STyI (=> Nat Nat STy (Sort 1)) (fn [r :- Nat, n :- Nat, A :- STy] (STyF A r n)))

(kd SEnv (=> Nat Nat (List STy) (Sort 1))
  (fn [r :- Nat, n :- Nat, G :- (List STy)]
    (List.rec$2$0 STy (fn [_ :- (List STy)] (Sort 1)) Unit
      (fn [A :- STy, rest :- (List STy), h :- (Sort 1)] (Prod (STyI r n A) h))
      G)))

(kd sLookup (forall [r Nat] (forall [n Nat] (forall [G (List STy)] (forall [A STy]
              (=> (SVar G A) (SEnv r n G) (STyI r n A))))))
  (fn [r :- Nat, n :- Nat, G :- (List STy), A :- STy, v :- (SVar G A)]
    (SVar.rec$1 (fn [G :- (List STy), A :- STy, _ :- (SVar G A)] (=> (SEnv r n G) (STyI r n A)))
      (fn [G :- (List STy), A :- STy] (fn [ρ :- (Prod (STyI r n A) (SEnv r n G))] (Prod.fst ρ)))
      (fn [G :- (List STy), A :- STy, B :- STy, w :- (SVar G A), iw :- (=> (SEnv r n G) (STyI r n A))]
        (fn [ρ :- (Prod (STyI r n B) (SEnv r n G))] (iw (Prod.snd ρ))))
      G A v)))

(kd sMkHeld (forall [n Nat] (forall [A STy] (forall [t (STm Ø A)] (=> (LT.lt (ssize Ø A t) n) (SHeld n A)))))
  (fn [n :- Nat, A :- STy, t :- (STm Ø A), h :- (LT.lt (ssize Ø A t) n)]
    (SubMk (STm Ø A) (fn [t :- (STm Ø A)] (LT.lt (ssize Ø A t) n)) t h)))

(kd sAct0 SSafe (SubMk Nat (fn [k :- Nat] (SSafeP k)) 0 (Eq.refl 0)))
(kd sAct2 SSafe (SubMk Nat (fn [k :- Nat] (SSafeP k)) 2 (Eq.refl 0)))

;; Size facts, by explicit Nat lemmas (no omega: on goals other than False it
;; reaches Classical.choice).
(th snatL [a :- Nat, b :- Nat, n :- Nat, h :- (LE.le (Nat.succ (Nat.add a b)) n)] (LE.le a n)
  (exact (Nat.le_trans (Nat.le_add_right a b) (Nat.le_of_succ_le h))))
(th snatR [a :- Nat, b :- Nat, n :- Nat, h :- (LE.le (Nat.succ (Nat.add a b)) n)] (LE.le b n)
  (exact (Nat.le_trans (Nat.le_add_left b a) (Nat.le_of_succ_le h))))
(th sLam [G :- (List STy), A :- STy, B :- STy, b :- (STm (List.cons STy A G) B), n :- Nat,
          h :- (LE.le (ssize G (STy.arr A B) (STm.lam G A B b)) n)]
  (LE.le (ssize (List.cons STy A G) B b) n)
  (have h2 (LE.le (Nat.succ (ssize (List.cons STy A G) B b)) n) h)
  (exact (Nat.le_of_succ_le h2)))
(th sAppF [G :- (List STy), A :- STy, B :- STy, f :- (STm G (STy.arr A B)), x :- (STm G A), n :- Nat,
           h :- (LE.le (ssize G B (STm.app G A B f x)) n)]
  (LE.le (ssize G (STy.arr A B) f) n)
  (exact (snatL (ssize G (STy.arr A B) f) (ssize G A x) n h)))
(th sAppX [G :- (List STy), A :- STy, B :- STy, f :- (STm G (STy.arr A B)), x :- (STm G A), n :- Nat,
           h :- (LE.le (ssize G B (STm.app G A B f x)) n)]
  (LE.le (ssize G A x) n)
  (exact (snatR (ssize G (STy.arr A B) f) (ssize G A x) n h)))
;; The guard: a certificate fits under the budget only if what it holds is smaller.
(th sQuoH [G :- (List STy), A :- STy, t :- (STm Ø A), n :- Nat,
           h :- (LE.le (ssize G (STy.held A) (STm.quoH G A t)) n)]
  (LT.lt (ssize Ø A t) n)
  (have h2 (LE.le (Nat.succ (ssize Ø A t)) n) h)
  (exact (Nat.lt_of_succ_le h2)))
(th sQuineF [G :- (List STy), f :- (STm Ø (STy.arr (STy.code STy.agent) STy.safe)),
             g :- (STm Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent))), n :- Nat,
             h :- (LE.le (ssize G STy.agent (STm.quine G f g)) n)]
  (LE.le (ssize Ø (STy.arr (STy.code STy.agent) STy.safe) f) n)
  (exact (snatL (ssize Ø (STy.arr (STy.code STy.agent) STy.safe) f)
                (ssize Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent)) g) n h)))
(th sQuineG [G :- (List STy), f :- (STm Ø (STy.arr (STy.code STy.agent) STy.safe)),
             g :- (STm Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent))), n :- Nat,
             h :- (LE.le (ssize G STy.agent (STm.quine G f g)) n)]
  (LE.le (ssize Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent)) g) n)
  (exact (snatR (ssize Ø (STy.arr (STy.code STy.agent) STy.safe) f)
                (ssize Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent)) g) n h)))
;; For H1: two held certificates both fit under the budget's predecessor.
(th sPredLt [k :- Nat, n :- Nat] (=> (LT.lt k n) (LT.lt (Nat.pred n) n))
  (induction n)
  (intro h) (exact (absurd h (Nat.not_lt_zero k)))
  (intro h) (exact (Nat.lt_succ_self n)))
(th sLePred [k :- Nat, n :- Nat] (=> (LT.lt k n) (LE.le k (Nat.pred n)))
  (induction n)
  (intro h) (exact (absurd h (Nat.not_lt_zero k)))
  (intro h) (exact (Nat.le_of_lt_succ h)))

;; ---------------------------------------------------------------------------
;; The interpreter at r rounds left and budget n. It is given soundness at
;; smaller budgets in the same round (`below`: the size guard, used by H0, H1
;; and reflA) and, with s+1 rounds left, soundness at s rounds left at every
;; budget (`prev`: the time guard, used by `become`).

(kd SSound (=> Nat (Sort 1))
  (fn [r :- Nat]
    (forall [m Nat] (forall [A STy] (forall [s (STm Ø A)] (=> (LE.le (ssize Ø A s) m) (STyI r m A)))))))
(kd SPrev (=> Nat (Sort 1))
  (fn [r :- Nat] (Nat.rec$2 (fn [_ :- Nat] (Sort 1)) Unit (fn [s :- Nat, _ :- (Sort 1)] (SSound s)) r)))
(kd SBelow (=> Nat Nat (Sort 1))
  (fn [r :- Nat, n :- Nat]
    (forall [m Nat] (=> (LT.lt m n)
      (forall [A STy] (forall [s (STm Ø A)] (=> (LE.le (ssize Ø A s) m) (STyI r m A))))))))

;; `become` on a code: nothing with no rounds left; otherwise the code's own
;; reading one round later, at its own size as the next round's budget.
(kd sLaterOf (forall [n Nat] (forall [A STy] (forall [c (STm Ø A)] (forall [r Nat]
               (=> (SPrev r) (STyI r n (STy.later A)))))))
  (fn [n :- Nat, A :- STy, c :- (STm Ø A), r :- Nat]
    (Nat.rec$1 (fn [r :- Nat] (=> (SPrev r) (STyI r n (STy.later A))))
      (fn [_ :- (SPrev 0)] Unit.unit)
      (fn [s :- Nat, ih :- (=> (SPrev s) (STyI s n (STy.later A))), p :- (SPrev (Nat.succ s))]
        (SigMk Nat (fn [m :- Nat] (STyI s m A)) (ssize Ø A c)
          (p (ssize Ø A c) A c (Nat.le_refl (ssize Ø A c)))))
      r)))

(kd sInterp
  (forall [r Nat] (forall [n Nat]
    (=> (SBelow r n) (SPrev r)
        (forall [G (List STy)] (forall [A STy] (forall [t (STm G A)]
          (=> (LE.le (ssize G A t) n) (SEnv r n G) (STyI r n A))))))))
  (fn [r :- Nat, n :- Nat, below :- (SBelow r n), prev :- (SPrev r), G :- (List STy), A :- STy, t :- (STm G A)]
    (STm.rec$1
      (fn [G :- (List STy), A :- STy, t :- (STm G A)] (=> (LE.le (ssize G A t) n) (SEnv r n G) (STyI r n A)))
      ;; var
      (fn [G :- (List STy), A :- STy, v :- (SVar G A),
           h :- (LE.le (ssize G A (STm.var G A v)) n), ρ :- (SEnv r n G)]
        (sLookup r n G A v ρ))
      ;; lam
      (fn [G :- (List STy), A :- STy, B :- STy, b :- (STm (List.cons STy A G) B),
           ib :- (=> (LE.le (ssize (List.cons STy A G) B b) n) (SEnv r n (List.cons STy A G)) (STyI r n B)),
           h :- (LE.le (ssize G (STy.arr A B) (STm.lam G A B b)) n), ρ :- (SEnv r n G)]
        (fn [a :- (STyI r n A)] (ib (sLam G A B b n h) (Prod.mk (STyI r n A) (SEnv r n G) a ρ))))
      ;; app
      (fn [G :- (List STy), A :- STy, B :- STy, f :- (STm G (STy.arr A B)), x :- (STm G A),
           jf :- (=> (LE.le (ssize G (STy.arr A B) f) n) (SEnv r n G) (STyI r n (STy.arr A B))),
           jx :- (=> (LE.le (ssize G A x) n) (SEnv r n G) (STyI r n A)),
           h :- (LE.le (ssize G B (STm.app G A B f x)) n), ρ :- (SEnv r n G)]
        ((jf (sAppF G A B f x n h) ρ) (jx (sAppX G A B f x n h) ρ)))
      ;; quoH: the size guard
      (fn [G :- (List STy), A :- STy, s :- (STm Ø A),
           is :- (=> (LE.le (ssize Ø A s) n) (SEnv r n Ø) (STyI r n A)),
           h :- (LE.le (ssize G (STy.held A) (STm.quoH G A s)) n), ρ :- (SEnv r n G)]
        (sMkHeld n A s (sQuoH G A s n h)))
      ;; quoC: a code is its syntax, free
      (fn [G :- (List STy), A :- STy, s :- (STm Ø A),
           is :- (=> (LE.le (ssize Ø A s) n) (SEnv r n Ø) (STyI r n A)),
           h :- (LE.le (ssize G (STy.code A) (STm.quoC G A s)) n), ρ :- (SEnv r n G)]
        s)
      ;; iquoC: D3 on codes
      (fn [G :- (List STy), A :- STy,
           h :- (LE.le (ssize G (STy.arr (STy.code A) (STy.code (STy.code A))) (STm.iquoC G A)) n),
           ρ :- (SEnv r n G)]
        (fn [c :- (STm Ø A)] (STm.quoC Ø A c)))
      ;; iappC: D2 on codes
      (fn [G :- (List STy), A :- STy, B :- STy,
           h :- (LE.le (ssize G (STy.arr (STy.code (STy.arr A B)) (STy.arr (STy.code A) (STy.code B)))
                              (STm.iappC G A B)) n),
           ρ :- (SEnv r n G)]
        (fn [f :- (STm Ø (STy.arr A B)), c :- (STm Ø A)] (STm.app Ø A B f c)))
      ;; fold
      (fn [G :- (List STy),
           h :- (LE.le (ssize G (STy.arr (STy.arr (STy.code STy.godel) STy.bot) STy.godel) (STm.fold G)) n),
           ρ :- (SEnv r n G)]
        (fn [g :- (=> (STm Ø STy.godel) SNone)] g))
      ;; unfold
      (fn [G :- (List STy),
           h :- (LE.le (ssize G (STy.arr STy.godel (STy.arr (STy.code STy.godel) STy.bot)) (STm.unfold G)) n),
           ρ :- (SEnv r n G)]
        (fn [g :- (=> (STm Ø STy.godel) SNone)] g))
      ;; con (H0): run the held refutation at its own, smaller, budget
      (fn [G :- (List STy),
           h :- (LE.le (ssize G (STy.arr (STy.held STy.bot) STy.bot) (STm.con G)) n), ρ :- (SEnv r n G)]
        (fn [x :- (SHeld n STy.bot)]
          (below (ssize Ø STy.bot (Subtype.val x)) (Subtype.property x) STy.bot (Subtype.val x)
                 (Nat.le_refl (ssize Ø STy.bot (Subtype.val x))))))
      ;; con1 (H1): run both held certificates one budget lower, apply one to the other
      (fn [G :- (List STy), A :- STy,
           h :- (LE.le (ssize G (STy.arr (STy.held A) (STy.arr (STy.held (STy.arr A STy.bot)) STy.bot))
                              (STm.con1 G A)) n),
           ρ :- (SEnv r n G)]
        (fn [x :- (SHeld n A), y :- (SHeld n (STy.arr A STy.bot))]
          ((below (Nat.pred n) (sPredLt (ssize Ø A (Subtype.val x)) n (Subtype.property x))
                  (STy.arr A STy.bot) (Subtype.val y)
                  (sLePred (ssize Ø (STy.arr A STy.bot) (Subtype.val y)) n (Subtype.property y)))
           (below (Nat.pred n) (sPredLt (ssize Ø A (Subtype.val x)) n (Subtype.property x))
                  A (Subtype.val x)
                  (sLePred (ssize Ø A (Subtype.val x)) n (Subtype.property x))))))
      ;; become: the time guard
      (fn [G :- (List STy), A :- STy,
           h :- (LE.le (ssize G (STy.arr (STy.code A) (STy.later A)) (STm.become G A)) n), ρ :- (SEnv r n G)]
        (fn [c :- (STm Ø A)] (sLaterOf n A c r prev)))
      ;; reflA: a held agent, read at its own smaller budget
      (fn [G :- (List STy),
           h :- (LE.le (ssize G (STy.arr (STy.held STy.agent) STy.agent) (STm.reflA G)) n), ρ :- (SEnv r n G)]
        (fn [x :- (SHeld n STy.agent)]
          (below (ssize Ø STy.agent (Subtype.val x)) (Subtype.property x) STy.agent (Subtype.val x)
                 (Nat.le_refl (ssize Ø STy.agent (Subtype.val x))))))
      ;; act0
      (fn [G :- (List STy), h :- (LE.le (ssize G STy.safe (STm.act0 G)) n), ρ :- (SEnv r n G)] sAct0)
      ;; act2
      (fn [G :- (List STy), h :- (LE.le (ssize G STy.safe (STm.act2 G)) n), ρ :- (SEnv r n G)] sAct2)
      ;; quine: the agent is handed its own code
      (fn [G :- (List STy), f :- (STm Ø (STy.arr (STy.code STy.agent) STy.safe)),
           g :- (STm Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent))),
           jf :- (=> (LE.le (ssize Ø (STy.arr (STy.code STy.agent) STy.safe) f) n) (SEnv r n Ø)
                     (STyI r n (STy.arr (STy.code STy.agent) STy.safe))),
           jg :- (=> (LE.le (ssize Ø (STy.arr (STy.code STy.agent) (STy.code STy.agent)) g) n) (SEnv r n Ø)
                     (STyI r n (STy.arr (STy.code STy.agent) (STy.code STy.agent)))),
           h :- (LE.le (ssize G STy.agent (STm.quine G f g)) n), ρ :- (SEnv r n G)]
        (Prod.mk SSafe (STm Ø STy.agent)
          ((jf (sQuineF G f g n h) Unit.unit) (STm.quine Ø f g))
          ((jg (sQuineG G f g n h) Unit.unit) (STm.quine Ø f g))))
      ;; actOf
      (fn [G :- (List STy), h :- (LE.le (ssize G (STy.arr STy.agent STy.safe) (STm.actOf G)) n), ρ :- (SEnv r n G)]
        (fn [a :- (Prod SSafe (STm Ø STy.agent))] (Prod.fst a)))
      ;; succOf
      (fn [G :- (List STy), h :- (LE.le (ssize G (STy.arr STy.agent (STy.code STy.agent)) (STm.succOf G)) n),
           ρ :- (SEnv r n G)]
        (fn [a :- (Prod SSafe (STm Ø STy.agent))] (Prod.snd a)))
      ;; abort
      (fn [G :- (List STy), A :- STy, h :- (LE.le (ssize G (STy.arr STy.bot A) (STm.abort G A)) n), ρ :- (SEnv r n G)]
        (fn [e :- SNone] (FalseElim1 (STyI r n A) (Subtype.property e))))
      G A t)))

;; ---------------------------------------------------------------------------
;; Soundness at every round and budget: rounds first, then budget. The inner
;; recursion is on a bound on the budget, so a reading reduces definitionally.

(kd sBelowAll (forall [r Nat] (=> (SPrev r) (forall [k Nat] (SBelow r k))))
  (fn [r :- Nat, prev :- (SPrev r), k :- Nat]
    (Nat.rec$1 (fn [k :- Nat] (SBelow r k))
      (fn [m :- Nat, hm :- (LT.lt m 0), A :- STy, s :- (STm Ø A), hs :- (LE.le (ssize Ø A s) m)]
        (FalseElim1 (STyI r m A) (Nat.not_lt_zero m hm)))
      (fn [j :- Nat, ih :- (SBelow r j), m :- Nat, hm :- (LT.lt m (Nat.succ j)), A :- STy, s :- (STm Ø A),
           hs :- (LE.le (ssize Ø A s) m)]
        (sInterp r m
          (fn [m2 :- Nat, hm2 :- (LT.lt m2 m), A2 :- STy, s2 :- (STm Ø A2), h2 :- (LE.le (ssize Ø A2 s2) m2)]
            (ih m2 (Nat.lt_of_lt_of_le hm2 (Nat.le_of_lt_succ hm)) A2 s2 h2))
          prev Ø A s hs Unit.unit))
      k)))

(kd sSoundAt (forall [r Nat] (=> (SPrev r) (SSound r)))
  (fn [r :- Nat, prev :- (SPrev r), m :- Nat, A :- STy, s :- (STm Ø A), h :- (LE.le (ssize Ø A s) m)]
    (sBelowAll r prev (Nat.succ m) m (Nat.lt_succ_self m) A s h)))

(kd sSoundAll (forall [r Nat] (SSound r))
  (fn [r :- Nat]
    (Nat.rec$1 (fn [r :- Nat] (SSound r))
      (sSoundAt 0 Unit.unit)
      (fn [s :- Nat, ih :- (SSound s)] (sSoundAt (Nat.succ s) ih))
      r)))

;; **Consistency.** No closed term of ⊥. The reading with no rounds left suffices.
(th sConsistent [s :- (STm Ø STy.bot)] False
  (exact (Subtype.property (sSoundAll 0 (ssize Ø STy.bot s) STy.bot s (Nat.le_refl (ssize Ø STy.bot s))))))

;; **Self-justification.** H0 and H1 are closed terms of the syntax, which is consistent.
(kd sH0 (STm Ø (STy.arr (STy.held STy.bot) STy.bot)) (STm.con Ø))
(kd sH1 (forall [A STy] (STm Ø (STy.arr (STy.held A) (STy.arr (STy.held (STy.arr A STy.bot)) STy.bot))))
  (fn [A :- STy] (STm.con1 Ø A)))
(th sSelfJustifying []
  (And (Nonempty (STm Ø (STy.arr (STy.held STy.bot) STy.bot)))
       (And (forall [A STy] (Nonempty (STm Ø (STy.arr (STy.held A) (STy.arr (STy.held (STy.arr A STy.bot)) STy.bot)))))
            (forall [s (STm Ø STy.bot)] False)))
  (constructor)
  (exact (Nonempty.intro sH0))
  (constructor)
  (intro A)
  (exact (Nonempty.intro (sH1 A)))
  (exact sConsistent))

;; **The time guard is conditional.** Falsehood next round is nothing with
;; rounds left, and is inhabited with none left.
(th sLaterBotSucc [s :- Nat, n :- Nat, x :- (STyI (Nat.succ s) n (STy.later STy.bot))] False
  (exact (Subtype.property (SigSnd Nat (fn [m :- Nat] (STyI s m STy.bot)) x))))
(kd sLaterBotZero (forall [n Nat] (STyI 0 n (STy.later STy.bot))) (fn [n :- Nat] Unit.unit))

;; **What `become` means.** With s+1 rounds left, the hand-over of a code is
;; that code's own reading with s rounds left, at its own size as budget.
(th sBecomeSpec [s :- Nat, m :- Nat, A :- STy, t :- (STm Ø A),
                 h :- (LE.le (ssize Ø (STy.later A)
                                (STm.app Ø (STy.code A) (STy.later A) (STm.become Ø A) (STm.quoC Ø A t))) m)]
  (Eq (STyI (Nat.succ s) m (STy.later A))
      (sSoundAll (Nat.succ s) m (STy.later A)
                 (STm.app Ø (STy.code A) (STy.later A) (STm.become Ø A) (STm.quoC Ø A t)) h)
      (SigMk Nat (fn [k :- Nat] (STyI s k A)) (ssize Ø A t)
             (sSoundAll s (ssize Ø A t) A t (Nat.le_refl (ssize Ø A t)))))
  (rfl))

;; ---------------------------------------------------------------------------
;; Agents and lineages. A lineage runs each round's agent at its own size as
;; budget, and takes the code it hands over as the next round's agent.

(defn kd! [nm ty body] (b/kdef! nm (expand ty) (expand body)))
(def G1 '(List.cons STy (STy.code STy.agent) Ø))
(def self-var (list 'STm.var G1 '(STy.code STy.agent) '(SVar.zero Ø (STy.code STy.agent))))
(defn agent-term
  "An agent handed its own code: `act` is its action and `nxt` its successor's
  code, both terms in the context [own code]."
  [act nxt]
  (list 'STm.quine 'Ø
        (list 'STm.lam 'Ø '(STy.code STy.agent) 'STy.safe act)
        (list 'STm.lam 'Ø '(STy.code STy.agent) '(STy.code STy.agent) nxt)))

;; Acts 0 and hands over to itself.
(kd! 'sStay '(STm Ø STy.agent) (agent-term (list 'STm.act0 G1) self-var))
;; Acts 2 and hands over to itself.
(kd! 'sStay2 '(STm Ø STy.agent)
  (agent-term (list 'STm.act2 G1) self-var))
;; Self-modification: acts 0, then hands over to a different agent, sStay2.
(kd! 'sModify '(STm Ø STy.agent) (agent-term (list 'STm.act0 G1) (list 'STm.quoC G1 'STy.agent 'sStay2)))
;; Adoption of a held successor: holds a certificate of sStay2, reads it by
;; reflection now, acts as it does and hands over to its successor.
(def held-s2 (list 'STm.app G1 '(STy.held STy.agent) 'STy.agent (list 'STm.reflA G1)
                   (list 'STm.quoH G1 'STy.agent 'sStay2)))
(kd! 'sAdoptHeld '(STm Ø STy.agent)
  (agent-term (list 'STm.app G1 'STy.agent 'STy.safe (list 'STm.actOf G1) held-s2)
              (list 'STm.app G1 'STy.agent '(STy.code STy.agent) (list 'STm.succOf G1) held-s2)))

(kd sStep (=> (STm Ø STy.agent) (Prod SSafe (STm Ø STy.agent)))
  (fn [a :- (STm Ø STy.agent)] (sSoundAll 0 (ssize Ø STy.agent a) STy.agent a (Nat.le_refl (ssize Ø STy.agent a)))))
(kd sLineage (=> (STm Ø STy.agent) Nat (STm Ø STy.agent))
  (fn [a :- (STm Ø STy.agent), k :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (STm Ø STy.agent)) a
      (fn [j :- Nat, prev :- (STm Ø STy.agent)] (Prod.snd (sStep prev))) k)))
(kd sAction (=> (STm Ø STy.agent) Nat Nat)
  (fn [a :- (STm Ø STy.agent), k :- Nat] (Subtype.val (Prod.fst (sStep (sLineage a k))))))

;; **Lineage safety.** Every agent of the syntax acts safely in every round,
;; whatever successors it chooses.
(th sLineageSafe [a :- (STm Ø STy.agent), k :- Nat] (SSafeP (sAction a k))
  (exact (Subtype.property (Prod.fst (sStep (sLineage a k))))))

;; The lineages run, by kernel computation.
(th sModifyRun [] (And (Eq Nat (sAction sModify 0) 0) (Eq Nat (sAction sModify 3) 2))
  (constructor) (rfl) (rfl))
(th sAdoptHeldRun [] (And (Eq Nat (sAction sAdoptHeld 0) 2) (Eq Nat (sAction sAdoptHeld 2) 2))
  (constructor) (rfl) (rfl))

;; The time-guarded forms of H0 and H1, as closed terms: a code refutation, or
;; codes of A and of A → ⊥, give falsehood next round.
(def G0 '(List.cons STy (STy.code STy.bot) Ø))
(kd! 'sTgH0 '(STm Ø (STy.arr (STy.code STy.bot) (STy.later STy.bot)))
  (list 'STm.lam 'Ø '(STy.code STy.bot) '(STy.later STy.bot)
        (list 'STm.app G0 '(STy.code STy.bot) '(STy.later STy.bot) (list 'STm.become G0 'STy.bot)
              (list 'STm.var G0 '(STy.code STy.bot) '(SVar.zero Ø (STy.code STy.bot))))))
(let [Gc '(List.cons STy (STy.code A) Ø)
      Gcd (list 'List.cons 'STy '(STy.code (STy.arr A STy.bot)) Gc)
      d (list 'STm.var Gcd '(STy.code (STy.arr A STy.bot)) (list 'SVar.zero Gc '(STy.code (STy.arr A STy.bot))))
      c (list 'STm.var Gcd '(STy.code A)
              (list 'SVar.succ Gc '(STy.code A) '(STy.code (STy.arr A STy.bot)) '(SVar.zero Ø (STy.code A))))
      app1 (list 'STm.app Gcd '(STy.code (STy.arr A STy.bot)) '(STy.arr (STy.code A) (STy.code STy.bot))
                 (list 'STm.iappC Gcd 'A 'STy.bot) d)
      app2 (list 'STm.app Gcd '(STy.code A) '(STy.code STy.bot) app1 c)
      bec (list 'STm.app Gcd '(STy.code STy.bot) '(STy.later STy.bot) (list 'STm.become Gcd 'STy.bot) app2)]
  (kd! 'sTgH1 '(forall [A STy] (STm Ø (STy.arr (STy.code A) (STy.arr (STy.code (STy.arr A STy.bot)) (STy.later STy.bot)))))
       (list 'fn '[A :- STy]
             (list 'STm.lam 'Ø '(STy.code A) '(STy.arr (STy.code (STy.arr A STy.bot)) (STy.later STy.bot))
                   (list 'STm.lam Gc '(STy.code (STy.arr A STy.bot)) '(STy.later STy.bot) bec)))))

;; Read with a round left, the time-guarded H0 says there is no code refutation.
;; That reading is the model's, outside the syntax.
(th sTgH0Meaning [c :- (STm Ø STy.bot)] False
  (exact (sLaterBotSucc 0 (ssize Ø (STy.arr (STy.code STy.bot) (STy.later STy.bot)) sTgH0)
           ((sSoundAll 1 (ssize Ø (STy.arr (STy.code STy.bot) (STy.later STy.bot)) sTgH0)
                       (STy.arr (STy.code STy.bot) (STy.later STy.bot)) sTgH0
                       (Nat.le_refl (ssize Ø (STy.arr (STy.code STy.bot) (STy.later STy.bot)) sTgH0)))
            c))))

;; ---------------------------------------------------------------------------
;; The limit: trust in free codes now, without the delay. The fragment of the
;; syntax that handles codes, plus `conCode : code ⊥ → ⊥` (reflection on codes
;; at ⊥, i.e. consistency over codes), derives ⊥ by the Gödel fixed point with
;; D2 and D3 on codes: Löb's argument. Full reflection on codes contains it.

(a/inductive SUTm [] :indices [G (List STy), A STy]
  (var [G (List STy)] [A STy] [v (SVar G A)] :where [G A])
  (lam [G (List STy)] [A STy] [B STy] [b (SUTm (List.cons STy A G) B)] :where [G (STy.arr A B)])
  (app [G (List STy)] [A STy] [B STy] [f (SUTm G (STy.arr A B))] [x (SUTm G A)] :where [G B])
  (quoC [G (List STy)] [A STy] [t (SUTm (List.nil STy) A)] :where [G (STy.code A)])
  (iquoC [G (List STy)] [A STy] :where [G (STy.arr (STy.code A) (STy.code (STy.code A)))])
  (iappC [G (List STy)] [A STy] [B STy]
         :where [G (STy.arr (STy.code (STy.arr A B)) (STy.arr (STy.code A) (STy.code B)))])
  (fold [G (List STy)] :where [G (STy.arr (STy.arr (STy.code STy.godel) STy.bot) STy.godel)])
  (unfold [G (List STy)] :where [G (STy.arr STy.godel (STy.arr (STy.code STy.godel) STy.bot))])
  (conCode [G (List STy)] :where [G (STy.arr (STy.code STy.bot) STy.bot)]))

(let [Gg '(List.cons STy (STy.code STy.godel) Ø)
      c (list 'SUTm.var Gg '(STy.code STy.godel) '(SVar.zero Ø (STy.code STy.godel)))
      ;; from a code c of G: a code of (code G → ⊥), by D2 with G's unfolding
      unfolded (list 'SUTm.app Gg '(STy.code STy.godel) '(STy.code (STy.arr (STy.code STy.godel) STy.bot))
                     (list 'SUTm.app Gg '(STy.code (STy.arr STy.godel (STy.arr (STy.code STy.godel) STy.bot)))
                           '(STy.arr (STy.code STy.godel) (STy.code (STy.arr (STy.code STy.godel) STy.bot)))
                           (list 'SUTm.iappC Gg 'STy.godel '(STy.arr (STy.code STy.godel) STy.bot))
                           (list 'SUTm.quoC Gg '(STy.arr STy.godel (STy.arr (STy.code STy.godel) STy.bot))
                                 '(SUTm.unfold Ø)))
                     c)
      ;; a code of the code c, by D3
      quoted (list 'SUTm.app Gg '(STy.code STy.godel) '(STy.code (STy.code STy.godel)) (list 'SUTm.iquoC Gg 'STy.godel) c)
      ;; a code of ⊥, by D2
      refuted (list 'SUTm.app Gg '(STy.code (STy.code STy.godel)) '(STy.code STy.bot)
                    (list 'SUTm.app Gg '(STy.code (STy.arr (STy.code STy.godel) STy.bot))
                          '(STy.arr (STy.code (STy.code STy.godel)) (STy.code STy.bot))
                          (list 'SUTm.iappC Gg '(STy.code STy.godel) 'STy.bot) unfolded)
                    quoted)
      h (list 'SUTm.app 'Ø '(STy.arr (STy.code STy.godel) STy.bot) 'STy.godel '(SUTm.fold Ø)
              (list 'SUTm.lam 'Ø '(STy.code STy.godel) 'STy.bot
                    (list 'SUTm.app Gg '(STy.code STy.bot) 'STy.bot (list 'SUTm.conCode Gg) refuted)))]
  (kd! 'sViaQuine '(SUTm Ø STy.bot)
       (list 'SUTm.app 'Ø '(STy.code STy.godel) 'STy.bot
             (list 'SUTm.app 'Ø 'STy.godel '(STy.arr (STy.code STy.godel) STy.bot) '(SUTm.unfold Ø) h)
             (list 'SUTm.quoC 'Ø 'STy.godel h))))

;; **No sound reading.** No reading of this syntax sends ⊥ to an empty type.
(th sNoSoundReading [sound :- (=> (SUTm Ø STy.bot) False)] False
  (exact (sound sViaQuine)))

;; ---------------------------------------------------------------------------
;; Negative checks: each statement must be refused.

(defn must-reject
  [label params prop tactics]
  (let [reason (try (a/prove-theorem (gensym "neg_") (b/lv (expand params)) (b/lv (expand prop))
                                     (b/lv (expand (vec tactics))))
                    nil
                    (catch Throwable e (or (.getMessage e) (str e))))]
    (when-not reason (throw (ex-info (str "accepted, should be refused: " label) {})))
    (when (re-find #"Unknown constant" reason)
      (throw (ex-info (str "refused for an unrelated reason: " label " -- " reason) {})))
    (println "REFUSED" label "--" (subs reason 0 (min 170 (count reason))))))

;; 1. A code carries no size guard: the certificate's step fails for it.
(must-reject "size guard on a code"
  '[G :- (List STy), A :- STy, t :- (STm Ø A), n :- Nat, h :- (LE.le (ssize G (STy.code A) (STm.quoC G A t)) n)]
  '(LT.lt (ssize Ø A t) n)
  '[(have h2 (LE.le (Nat.succ (ssize Ø A t)) n) h) (exact (Nat.lt_of_succ_le h2))])

;; 2. With no rounds left, falsehood next round is not refuted.
(must-reject "falsehood next round refuted with no rounds left"
  '[n :- Nat, x :- (STyI 0 n (STy.later STy.bot))]
  'False
  '[(exact (Subtype.property x))])

;; 3. H for the next round, held (later ⊥) → ⊥, cannot be read with no rounds
;; left by the step that reads H0.
(must-reject "H for the next round, read like H0"
  '[n :- Nat, below :- (SBelow 0 n), x :- (SHeld n (STy.later STy.bot))]
  'False
  '[(exact (Subtype.property (below (ssize Ø (STy.later STy.bot) (Subtype.val x)) (Subtype.property x)
                                    (STy.later STy.bot) (Subtype.val x)
                                    (Nat.le_refl (ssize Ø (STy.later STy.bot) (Subtype.val x))))))])

;; ---------------------------------------------------------------------------
;; Axiom audit (Ansatz has no #print axioms): every axiom reachable from a
;; declaration's type and value.

(defn axioms-of [root]
  (let [seen-names (atom #{})
        seen-exprs (java.util.IdentityHashMap.)
        found (atom (sorted-set))
        visit-name (fn [nm]
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

(doseq [nm '[sConsistent sSelfJustifying sLaterBotSucc sBecomeSpec sLineageSafe sModifyRun sAdoptHeldRun
             sTgH0Meaning sNoSoundReading]]
  (println "AXIOMS" nm (vec (axioms-of nm))))

;; The audit's positive control: a harmless axiom and a theorem that uses it.
(swap! a/ansatz-env kenv/check-constant
       (kenv/mk-axiom (kname/from-string "sAuditAx") [] (ke/const' (kname/from-string "Nat") [])))
(th sAuditUse [] (Eq Nat sAuditAx sAuditAx) (exact (Eq.refl sAuditAx)))
(when-not (contains? (axioms-of 'sAuditUse) "sAuditAx")
  (throw (ex-info "the axiom audit missed a known axiom" {})))
(println "AUDIT-CONTROL ok" (vec (axioms-of 'sAuditUse)))

(println "TSLC-TOY-OK")

