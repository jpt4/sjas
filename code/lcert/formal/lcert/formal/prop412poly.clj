(ns lcert.formal.prop412poly
  "A06: F7 size facts for the unary numeral in Proposition 4.12(1)."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.section4c]
            [lcert.formal.certcanon]))

;; F7 stores each successor under one Code.sn and zero under a Code.sl.
;; These equations concern the term field of a certificate; the derivation
;; tree and its remaining fields still need their own size proof.
(thm sh4_num_enc_step [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_num (Nat.succ n))))
          (+ 1 (cnodes (encE (sh4_num n)))))
  (rfl))

(thm sh4_num_enc_linear [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_num n))) n)
  (induction n)
  (rfl)
  (rw [(sh4_num_enc_step n)])
  (omega))
