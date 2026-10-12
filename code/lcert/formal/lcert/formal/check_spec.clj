(ns lcert.formal.check-spec
  "F7 step 5b — the concrete checker, and the trust base as theorems about it
  (ADR-0006, F7 plan).

  Check decD c d: does the certificate c prove a closed term of the type
  coded d? decD decodes c into a budget m, a term t, a type A and two
  derivation trees (t's typing at budget m, A's formation); the checker is
  parameterized by decD, so everything here holds for every decoder (step 5c
  supplies the real one, inverse to an encoding, with completeness).

  The circularity. CheckSpec asks that an accepted code decode to a
  derivation *at the checker itself*, and δ-steps inside a derivation consult
  the checker. So Check is defined by recursion on the certificate: chkN k is
  the checker with fuel k, and its body checks the trees at the checker
  restricted to codes smaller than c (restrC). Then
    - chkN_stable: with enough fuel the answer no longer depends on it, and
      check_fix: Check is a fixed point of the body;
    - check_full: an accepted code's trees are accepted at Check restricted
      below c, hence (dtCheck_agree, because the body also checks that every
      δ-code of the trees is below c) at Check itself, hence (dtCheck_sound)
      they are derivations at Check.

  The size facts — m < ‖c‖ (CheckSpec), 2f < ‖c‖ (TokSize), ‖⌜A⌝‖ < ‖c‖
  (TypeSize) — are tested by the body, so they hold of every accepted code by
  construction. (The paper derives them from its encoding, Lemmas 2.6–2.7;
  ADR-0006 records the deviation; completeness, step 5c, pads a certificate
  that is too small.)"
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.prop434]   ; TokSize, TypeSize
            [lcert.formal.check-agree]))

;; The decoded certificate: (m, t, A, typing tree, formation tree). Written
;; CD in the forms below and expanded here (an alias would need unfolding).
(def ^:private CD '(Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
(defn- x [form] (lv (walk/postwalk-replace {'CD CD} form)))
(defmacro ^:private kdef* [nm ty body] `(b/kdef! '~nm (x '~ty) (x '~body)))
(defmacro ^:private thm* [nm params goal & tactics]
  `(a/prove-theorem '~nm (x '~params) (x '~goal) (x '~(vec tactics))))

;; ---------------------------------------------------------------------------
;; The checker.

;; r, consulted only on codes smaller than c.
(kdef restrC (=> (=> Code Code Bool) Code (=> Code Code Bool))
  (fn [r :- (=> Code Code Bool), c :- Code]
    (fn [c2 :- Code, d2 :- Code] (Bool.and (Nat.ble (+ (cnodes c2) 1) (cnodes c)) (r c2 d2)))))

;; The checks on one decoded certificate, at checker r. Conjunct order
;; matters to the projections below (h/f7-and; it ends in Bool.true).
(def ^:private body-checks
  '[(dtCheck (restrC r c) T1 (DTJ.rt (thetaD m) (thetaU m) t A))
    (dtCheck (restrC r c) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit))
    (closedTy A)
    (codeEq (encE A) d)
    (Nat.ble (+ m 1) (cnodes c))
    (Nat.ble (+ (+ (cntU (maskUF (thetaU m) (freshF t))) (cntU (maskUF (thetaU m) (freshF t)))) 1) (cnodes c))
    (Nat.ble (+ (cnodes (encE A)) 1) (cnodes c))
    (Nat.ble (dtB T1) (cnodes c))
    (Nat.ble (dtB T2) (cnodes c))])
(defn- conj [r] (walk/postwalk-replace {'r r} (h/f7-and body-checks)))
(defn- proj [i hyp] (h/f7-projection body-checks i hyp))

(b/kdef! 'bodyD '(=> (=> Code Code Bool) Code Code Nat Exp Exp DT DT Bool)
  (list 'fn '[r :- (=> Code Code Bool), c :- Code, d :- Code, m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT]
    (conj 'r)))

;; The body on an optional certificate (none: reject), and on c's decoding.
(kdef* bodyO (=> (=> Code Code Bool) Code Code (Option CD) Bool)
  (fn [r :- (=> Code Code Bool), c :- Code, d :- Code, o :- (Option CD)]
    (Option.rec$1$0 CD (fn [_ :- (Option CD)] Bool) Bool.false
      (fn [y :- CD] (bodyD r c d (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                      (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))))
      o)))
(kdef* bodyC (=> (=> Code (Option CD)) (=> Code Code Bool) Code Code Bool)
  (fn [decD :- (=> Code (Option CD)), r :- (=> Code Code Bool), c :- Code, d :- Code]
    (bodyO r c d (decD c))))

;; Fuel: chkN decD 0 rejects; chkN decD (k+1) is the body at chkN decD k.
(kdef* chkN (=> (=> Code (Option CD)) Nat Code Code Bool)
  (fn [decD :- (=> Code (Option CD)), k :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (=> Code Code Bool))
      (fn [c :- Code, d :- Code] Bool.false)
      (fn [k2 :- Nat, r :- (=> Code Code Bool)] (fn [c :- Code, d :- Code] (bodyC decD r c d)))
      k)))

;; The checker: fuel one more than the certificate's size.
(kdef* Check (=> (=> Code (Option CD)) Code Code Bool)
  (fn [decD :- (=> Code (Option CD)), c :- Code, d :- Code] (chkN decD (+ (cnodes c) 1) c d)))

;; The decoder CheckSpec, TokSize and TypeSize read: budget, term, type.
(kdef* decOf (=> (=> Code (Option CD)) Code (Option (Prod Nat (Prod Exp Exp))))
  (fn [decD :- (=> Code (Option CD)), c :- Code]
    (Option.rec$1$0 CD (fn [_ :- (Option CD)] (Option (Prod Nat (Prod Exp Exp))))
      (Option.none (Prod Nat (Prod Exp Exp)))
      (fn [y :- CD] (Option.some (Prod Nat (Prod Exp Exp))
                      (Prod.mk Nat (Prod Exp Exp) (Prod.fst y)
                        (Prod.mk Exp Exp (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))))))
      (decD c))))

;; ---------------------------------------------------------------------------
;; Restriction and congruence.

;; Two checkers agreeing below c, restricted to below c, agree everywhere.
;; ‹lemma restr_agree: statement and proof elided, lines 107–115›

;; r restricted below c agrees with r below any n ≤ ‖c‖.
;; ‹lemma restr_self_agree: statement and proof elided, lines 118–124›

;; The body depends on the checker only below c.
;; ‹lemma bodyD_congr: statement and proof elided, lines 127–137›

(thm* bodyO_congr [r1 :- (=> Code Code Bool), r2 :- (=> Code Code Bool), c :- Code, d :- Code, o :- (Option CD),
                   hag :- (forall [c2 Code] (forall [d2 Code] (=> (Nat.lt (cnodes c2) (cnodes c)) (Eq Bool (r1 c2 d2) (r2 c2 d2)))))]
  (Eq Bool (bodyO r1 c d o) (bodyO r2 c d o))
  (cases o) (rfl)
  (exact (bodyD_congr r1 r2 c d _ _ _ _ _ hag)))

(thm* bodyC_congr [decD :- (=> Code (Option CD)), r1 :- (=> Code Code Bool), r2 :- (=> Code Code Bool), c :- Code, d :- Code,
                   hag :- (forall [c2 Code] (forall [d2 Code] (=> (Nat.lt (cnodes c2) (cnodes c)) (Eq Bool (r1 c2 d2) (r2 c2 d2)))))]
  (Eq Bool (bodyC decD r1 c d) (bodyC decD r2 c d))
  (exact (bodyO_congr r1 r2 c d (decD c) hag)))

;; ---------------------------------------------------------------------------
;; The fixed point.

;; With fuel k > ‖c‖, chkN decD k answers as Check does — for every k ≤ n, by
;; induction on n. At k = n+1 both chkN (n+1) and Check are the body at a
;; checker agreeing with Check below c (chkN n, and chkN ‖c‖), by the
;; hypothesis at n and at ‖c‖ ≤ n.
(thm* chkN_stable [decD :- (=> Code (Option CD)), n :- Nat]
  (forall [k Nat] (=> (LE.le k n) (forall [c Code] (forall [d Code]
    (=> (Nat.lt (cnodes c) k) (Eq Bool (chkN decD k c d) (Check decD c d)))))))
  (induction n)
  (intro k hk c d hc)
  (exact (absurd (Nat.lt_of_lt_of_le hc hk) (Nat.not_lt_zero (cnodes c))))
  (intro k hk c d hc)
  (refine' (Or.elim (Nat.eq_or_lt_of_le hk) _ _))
  (intro heq) (subst heq)
  (have stab_eq (Eq Bool (bodyC decD (chkN decD n) c d) (bodyC decD (chkN decD (cnodes c)) c d))
    (Eq.trans
      (bodyC_congr decD (chkN decD n) (Check decD) c d
        (fn [c2 :- Code, d2 :- Code, h2 :- (Nat.lt (cnodes c2) (cnodes c))]
          (ih_n n (Nat.le_refl n) c2 d2 (Nat.lt_of_lt_of_le h2 (Nat.le_of_lt_succ hc)))))
      (Eq.symm (bodyC_congr decD (chkN decD (cnodes c)) (Check decD) c d
        (fn [c2 :- Code, d2 :- Code, h2 :- (Nat.lt (cnodes c2) (cnodes c))]
          (ih_n (cnodes c) (Nat.le_of_lt_succ hc) c2 d2 h2))))))
  (exact stab_eq)
  (intro hlt)
  (exact (ih_n k (Nat.le_of_lt_succ hlt) c d hc)))

;; Check is the body at Check.
(thm* check_fix [decD :- (=> Code (Option CD)), c :- Code, d :- Code]
  (Eq Bool (Check decD c d) (bodyC decD (Check decD) c d))
  (have fix_eq (Eq Bool (bodyC decD (chkN decD (cnodes c)) c d) (bodyC decD (Check decD) c d))
    (bodyC_congr decD (chkN decD (cnodes c)) (Check decD) c d
      (fn [c2 :- Code, d2 :- Code, h2 :- (Nat.lt (cnodes c2) (cnodes c))]
        (chkN_stable decD (cnodes c) (cnodes c) (Nat.le_refl (cnodes c)) c2 d2 h2))))
  (exact fix_eq))

;; ---------------------------------------------------------------------------
;; Soundness: what an accepted code guarantees.

;; An accepted optional certificate is present, and its body holds.
(thm* bodyO_true [r :- (=> Code Code Bool), c :- Code, d :- Code, o :- (Option CD)]
  (=> (Eq Bool (bodyO r c d o) Bool.true)
      (Exists (fn [y :- CD] (And (Eq (Option CD) o (Option.some CD y))
        (Eq Bool (bodyD r c d (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                   (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))) Bool.true)))))
  (cases o)
  (intro hb) (exact (Bool.noConfusion hb))
  (intro hb) (constructor) (exact val) (exact (And.intro (Eq.refl (Option.some CD val)) hb)))

;; The body at Check: the trees are derivations at Check (accepted at Check
;; restricted below c; every δ-code is below c, so dtCheck_agree moves them
;; to Check; dtCheck_sound reads them), and the size facts hold.
(def ^:private body-at-check (walk/postwalk-replace {'r '(Check decD)} body-checks))
(defn- pr* [i] (h/f7-projection body-at-check i 'hb))
(defn- tree-sound [T J i-check i-bound]
  (list 'dtCheck_sound '(Check decD) T J
    (list 'Eq.trans
      (list 'Eq.symm (list 'dtCheck_agree '(restrC (Check decD) c) '(Check decD) T
                           (list 'restr_self_agree '(Check decD) 'c (list 'dtB T) (list 'Nat.le_of_ble_eq_true (pr* i-bound)))
                           J))
      (pr* i-check))))

(def ^:private sound-conj
  '(And (Rt (Check decD) (thetaD m) (thetaU m) t A)
   (And (Tl (Check decD) Bool.true (List.nil Exp) A Exp.tUnit)
   (And (Eq Bool (closedTy A) Bool.true)
   (And (Eq Code (encE A) d)
   (And (Nat.lt m (cnodes c))
   (And (LT.lt (+ (cntU (maskUF (thetaU m) (freshF t))) (cntU (maskUF (thetaU m) (freshF t)))) (cnodes c))
        (LT.lt (cnodes (encE A)) (cnodes c)))))))))

;; ‹lemma bodyD_sound: statement and proof elided, lines 222–233›

;; An accepted code decodes (decOf) to a budget, term and type with the
;; guarantees above.
;; ‹lemma check_full: statement and proof elided, lines 237–259›

;; ---------------------------------------------------------------------------
;; The trust base, as theorems about Check.

;; Two decodings of one code agree.
;; ‹lemma tup_eq: statement and proof elided, lines 265–276›

;; CheckSpec (Lemmas 2.6–2.7 and the encoding facts) holds of Check, with
;; decOf decD and the formal encoding encE, for every decoder decD.
(thm* check_spec [decD :- (=> Code (Option CD))]
  (CheckSpec (Check decD) (decOf decD) encE)
  (constructor)
  (intro c d h)
  (refine' (exT _ _ _ (check_full decD c d h) _)) (intro m hm)
  (refine' (exT _ _ _ hm _)) (intro t ht)
  (refine' (exT _ _ _ ht _)) (intro A hA)
  (constructor) (exact m) (constructor) (exact t) (constructor) (exact A)
  (exact (And.intro (And.left hA)
         (And.intro (And.left (And.right hA))
         (And.intro (And.left (And.right (And.right hA)))
         (And.intro (And.left (And.right (And.right (And.right hA))))
         (And.intro (And.left (And.right (And.right (And.right (And.right hA)))))
                    (And.left (And.right (And.right (And.right (And.right (And.right hA))))))))))))
  (constructor)
  (exact base_enc)
  (constructor)
  (intro A hA) (rfl)
  (intro A B hA hB h) (exact (encE_inj A B h)))

;; TokSize: an accepted code is more than twice its token count.
(thm* check_toksize [decD :- (=> Code (Option CD))]
  (TokSize (Check decD) (decOf decD))
  (intro c d m t A h hdec)
  (refine' (exT _ _ _ (check_full decD c d h) _)) (intro m2 hm)
  (refine' (exT _ _ _ hm _)) (intro t2 ht)
  (refine' (exT _ _ _ ht _)) (intro A2 hA)
  (have heq (And (Eq Nat m m2) (And (Eq Exp t t2) (Eq Exp A A2)))
    (tup_eq m t A m2 t2 A2 (Eq.trans (Eq.symm hdec) (And.left hA))))
  (have hm2 (Eq Nat m m2) (And.left heq))
  (have ht2 (Eq Exp t t2) (And.left (And.right heq)))
  (rw [hm2 ht2])
  (exact (And.left (And.right (And.right (And.right (And.right (And.right (And.right hA)))))))))

;; TypeSize: an accepted code is larger than its type's code.
(thm* check_typesize [decD :- (=> Code (Option CD))]
  (TypeSize (Check decD) (decOf decD) encE)
  (intro c d m t A h hdec)
  (refine' (exT _ _ _ (check_full decD c d h) _)) (intro m2 hm)
  (refine' (exT _ _ _ hm _)) (intro t2 ht)
  (refine' (exT _ _ _ ht _)) (intro A2 hA)
  (have heq (And (Eq Nat m m2) (And (Eq Exp t t2) (Eq Exp A A2)))
    (tup_eq m t A m2 t2 A2 (Eq.trans (Eq.symm hdec) (And.left hA))))
  (have hA2 (Eq Exp A A2) (And.right (And.right heq)))
  (rw [hA2])
  (exact (And.right (And.right (And.right (And.right (And.right (And.right (And.right hA)))))))))
