(ns lcert.formal.p49telebridge
  "Bridge the generic singleton telescope to the depth-indexed proposition."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.p49tele]
            [lcert.formal.p49depth]))

(thm p49tele_P49 [k :- Nat]
  (Eq Exp
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat)))
      (fn [h :- Nat] (Exp.var h)))
    (P49 k))
  (rfl))

(thm p49tele_P49_closedenv [k :- Nat]
  (Eq Exp
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat)))
      (fn [_ :- Nat] (Exp.sleaf (Exp.lbl 0))))
    (P49 k))
  (rfl))
