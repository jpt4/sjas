(ns lcert.formal.p49applyform
  "Discharge the formation premises for recursive telescope applications."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.p49applytyped]
            [lcert.formal.p49teleform]))

(thm p49af_first_env
  [rho :- (=> Nat Exp), h :- Nat, x :- Exp, i :- Nat]
  (Eq Exp
    (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
      (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))
    (if (Nat.beq i h) (lift 1 0 x) (lift 1 0 (rho i))))
  (exact (Bool.rec$0
    (fn [flag :- Bool]
      (Eq Exp
        (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
          (if flag (Exp.var 1) (lift 2 0 (rho i))))
        (if flag (lift 1 0 x) (lift 1 0 (rho i)))))
    (p49as_first_frame x (rho i))
    rfl (Nat.beq i h))))

(thm p49af_first_env_fun
  [rho :- (=> Nat Exp), h :- Nat, x :- Exp]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (fn [i :- Nat]
      (if (Nat.beq i h) (lift 1 0 x) (lift 1 0 (rho i)))))
  (exact (funext (fn [i :- Nat]
    (p49af_first_env rho h x i)))))

(thm p49af_form
  [chkf :- (=> Code Code Bool), D :- (List Exp), K :- P49C,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
   hrho :- (forall [i Nat]
     (Tl chkf Bool.false D (rho i) Exp.tSyn)),
   h :- Nat, j :- Nat, x :- Exp,
   hx :- (Tl chkf Bool.false D x Exp.tSyn)]
  (p49at_Form chkf D K slots rho h j x)
  (have hcode
    (Tl chkf Bool.false (consE Exp.tSyn D)
      (Exp.var 0) Exp.tSyn)
    (Tl.zVar chkf (consE Exp.tSyn D) 0 Exp.tSyn
      (nthE.eq_2 Exp.tSyn D)))
  (have hdom
    (Tl chkf Bool.true (consE Exp.tSyn D)
      (Exp.tT (p49dl j (Exp.var 0))) Exp.tUnit)
    (Tl.fT chkf (consE Exp.tSyn D)
      (p49dl j (Exp.var 0))
      (p49dl_typed chkf j (consE Exp.tSyn D)
        (Exp.var 0) hcode)))
  (have hbody
    (Tl chkf Bool.true (consE Exp.tSyn D)
      (Exp.tPi U.u1 (Exp.tT (p49dl j (Exp.var 0)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i)))))) Exp.tUnit)
    (Tl.fPi chkf (consE Exp.tSyn D) U.u1
      (Exp.tT (p49dl j (Exp.var 0)))
      (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
        (fn [i :- Nat]
          (if (Nat.beq i h) (Exp.var 1)
            (lift 2 0 (rho i)))))
      hdom
      (p49tele_p49dl_formed chkf K hK slots
        (consE (Exp.tT (p49dl j (Exp.var 0)))
          (consE Exp.tSyn D))
        (fn [i :- Nat]
          (if (Nat.beq i h) (Exp.var 1)
            (lift 2 0 (rho i))))
        (p49tf_update_typed chkf D rho hrho h
          (Exp.tT (p49dl j (Exp.var 0)))))))
  (have hA
    (Tl chkf Bool.true D
      (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) Exp.tUnit)
    (tl_cast3 chkf Bool.true D
      (Exp.tT (p49dl j x))
      (subst1 x (Exp.tT (p49dl j (Exp.var 0))))
      Exp.tUnit Exp.tUnit
      (Tl.fT chkf D (p49dl j x)
        (p49dl_typed chkf j D x hx))
      (Eq.symm (p49at_evidence_subst j x)) rfl))
  (have hstep
    (forall [i Nat]
      (Tl chkf Bool.false
        (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
        (if (Nat.beq i h) (lift 1 0 x)
          (lift 1 0 (rho i))) Exp.tSyn))
    (fn [i :- Nat]
      (p49tf_if_typed chkf
        (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
        (Nat.beq i h)
        (lift 1 0 x) (lift 1 0 (rho i)) Exp.tSyn
        (tl_weaken chkf Bool.false D x Exp.tSyn hx 0
          (subst1 x (Exp.tT (p49dl j (Exp.var 0)))))
        (tl_weaken chkf Bool.false D (rho i) Exp.tSyn
          (hrho i) 0
          (subst1 x (Exp.tT (p49dl j (Exp.var 0))))))))
  (have htail
    (Tl chkf Bool.true
      (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
      (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i)))))) Exp.tUnit)
    (tl_cast3 chkf Bool.true
      (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
      (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
        (fn [i :- Nat]
          (if (Nat.beq i h) (lift 1 0 x)
            (lift 1 0 (rho i)))))
      (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i))))))
      Exp.tUnit Exp.tUnit
      (p49tele_p49dl_formed chkf K hK slots
        (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
        (fn [i :- Nat]
          (if (Nat.beq i h) (lift 1 0 x)
            (lift 1 0 (rho i)))) hstep)
      (Eq.symm (Eq.trans
        (p49tl_subst_equiv slots
          (fn [q :- Nat, c :- Exp] (p49dl q c)) K
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i))))
          (upn 1 (fn [i :- Nat] (inst1 x i)))
          p49tl_p49dl_BSubst p49tl_frame_subst)
        (congrArg
          (fn [env :- (=> Nat Exp)]
            (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots env))
          (p49af_first_env_fun rho h x)))) rfl))
  (exact (And.intro hbody (And.intro hA htail))))
