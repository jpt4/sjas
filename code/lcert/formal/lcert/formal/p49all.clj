(ns lcert.formal.p49all
  "Fuel induction for the all-depth Proposition 4.9 witness."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49generator]
            [lcert.formal.p49nodeapply]
            [lcert.formal.p49slotsunique]
            [lcert.formal.p49leafzero]
            [lcert.formal.p49terminal]
            [lcert.formal.p49teleformbridge]))

(kdef p49all_Claim (=> (=> Code Code Bool) Nat Prop)
  (fn [chkf :- (=> Code Code Bool), fuel :- Nat]
    (forall [K P49C]
      (forall [slots (List (Prod Nat Nat))]
        (forall [D (List Exp)]
          (forall [uv (List U)]
            (=> (Eq Bool (p49c_lblok K) Bool.true)
              (=> (p49su_unique slots)
                (=> (LE.le (p49pot slots) fuel)
                  (=> (Eq Nat (lenU uv) (lenE D))
                    (Rt chkf D uv (p49tau fuel K slots)
                      (p49tele
                        (fn [j :- Nat, c :- Exp] (p49dl j c))
                        K slots p49tl_rho0))))))))))))

;; ‹lemma p49all_nil: statement and proof elided, lines 26–54›

;; ‹lemma p49all_leaf_lblok: statement and proof elided, lines 56–63›

;; ‹lemma p49all_leaf_branch_len: statement and proof elided, lines 65–72›

;; ‹lemma p49all_head_type: statement and proof elided, lines 74–90›

;; ‹lemma p49all_zero_step: statement and proof elided, lines 92–180›

;; ‹lemma p49all_succ_step: statement and proof elided, lines 182–427›

;; ‹lemma p49all_slot_step: statement and proof elided, lines 429–452›

(thm p49all_fuel
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   fuel :- Nat]
  (p49all_Claim chkf fuel)
  ;; ‹proof elided: lines 460–472 of the full file; kernel-checked›
  )

;; ‹lemma p49all_prop49: statement and proof elided, lines 474–525›

;; ‹lemma p49all_prop49_concrete: statement and proof elided, lines 527–537›

(kdef t49 (=> Nat Exp)
  (fn [k :- Nat]
    (p49tau
      (p49pot
        (List.cons (Prod Nat Nat) (Prod.mk 0 k)
          (List.nil (Prod Nat Nat))))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))))))

(thm prop49
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   k :- Nat]
  (Rt chkf (List.nil Exp) (List.nil U) (t49 k) (P49 k))
  ;; ‹proof elided: lines 555–555 of the full file; kernel-checked›
  )

(thm prop49_concrete [k :- Nat]
  (Rt (Check decCert) (List.nil Exp) (List.nil U)
    (t49 k) (P49 k))
  ;; ‹proof elided: lines 560–560 of the full file; kernel-checked›
  )
