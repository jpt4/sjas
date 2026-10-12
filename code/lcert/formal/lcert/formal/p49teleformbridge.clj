(ns lcert.formal.p49teleformbridge
  "Formation of the selected-hole unary family for Proposition 4.9."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49teleform]
            [lcert.formal.p49telebridgeaux]))

(kdef p49tf_rho0 (=> Nat Exp)
  (fn [_ :- Nat] (Exp.sleaf (Exp.lbl 0))))

(thm p49tf_rho0_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), h :- Nat]
  (Tl chkf Bool.false D (p49tf_rho0 h) Exp.tSyn)
  (exact (Tl.zSleaf chkf D (Exp.lbl 0)
    (Tl.zConst chkf D (Exp.lbl 0) Exp.tLbl rfl))))

(thm p49tf_selected_env_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp),
   rho :- (=> Nat Exp), j :- Nat, c :- Exp,
   hc :- (Tl chkf Bool.false D c Exp.tSyn),
   hrho :- (forall [h Nat]
     (Tl chkf Bool.false D (rho h) Exp.tSyn))]
  (forall [h Nat]
    (Tl chkf Bool.false D
      (if (Nat.beq h j) c (rho h)) Exp.tSyn))
  (intro h)
  (exact (p49tf_if_typed chkf D (Nat.beq h j)
    c (rho h) Exp.tSyn hc (hrho h))))

;; This is the expanded p49pd_Form interface for the concrete family.
;; The binder for c is introduced by the successor-depth peel, while all
;; other unbound holes receive the closed literal-leaf default.
(thm p49tf_F_closed_formed
  [chkf :- (=> Code Code Bool), K :- P49C,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   slots :- (List (Prod Nat Nat)), j :- Nat]
  (forall [D (List Exp)]
    (forall [c Exp]
      (=> (Tl chkf Bool.false D c Exp.tSyn)
        (Tl chkf Bool.true D
          (p49tba_F
            (fn [q :- Nat, x :- Exp] (p49dl q x))
            K slots j p49tf_rho0 c)
          Exp.tUnit))))
  (intro D c hc)
  (exact (p49tele_p49dl_formed chkf K hK slots D
    (fn [h :- Nat]
      (if (Nat.beq h j) c (p49tf_rho0 h)))
    (p49tf_selected_env_typed chkf D p49tf_rho0 j c hc
      (fn [h :- Nat] (p49tf_rho0_typed chkf D h))))))
