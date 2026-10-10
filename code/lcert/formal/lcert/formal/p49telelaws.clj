(ns lcert.formal.p49telelaws
  "Equivariant telescope laws and their closed-environment base cases."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49telebridgeaux]
            [lcert.formal.p49peeldepth]
            [lcert.formal.derivations]))

;; The ambient environment must be transformed along with the selected
;; argument. These pointwise equations are valid without closedness.
(thm p49tl_choose_lift
  [j :- Nat, rho :- (=> Nat Exp), c :- Exp,
   n :- Nat, cut :- Nat]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (lift n cut (if (Nat.beq i j) c (rho i))))
    (fn [i :- Nat]
      (if (Nat.beq i j) (lift n cut c)
        (lift n cut (rho i)))))
  (exact (funext (fn [i :- Nat]
    (Bool.rec$0
      (fn [flag :- Bool]
        (Eq Exp
          (lift n cut (if flag c (rho i)))
          (if flag (lift n cut c) (lift n cut (rho i)))))
      rfl rfl (Nat.beq i j))))))

(thm p49tl_choose_subst
  [j :- Nat, rho :- (=> Nat Exp), c :- Exp,
   sg :- (=> Nat Exp)]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (subst sg (if (Nat.beq i j) c (rho i))))
    (fn [i :- Nat]
      (if (Nat.beq i j) (subst sg c)
        (subst sg (rho i)))))
  (exact (funext (fn [i :- Nat]
    (Bool.rec$0
      (fn [flag :- Bool]
        (Eq Exp
          (subst sg (if flag c (rho i)))
          (if flag (subst sg c) (subst sg (rho i)))))
      rfl rfl (Nat.beq i j))))))

;; With no remaining slot, B is absent. The telescope's sole checker
;; sees the transformed environment pointwise through p49plug.
(thm p49tl_lift_nil
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat,
   rho :- (=> Nat Exp), c :- Exp, n :- Nat, cut :- Nat]
  (Eq Exp
    (lift n cut
      (p49tba_F B K (List.nil (Prod Nat Nat)) j rho c))
    (p49tba_F B K (List.nil (Prod Nat Nat)) j
      (fn [i :- Nat] (lift n cut (rho i)))
      (lift n cut c)))
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (Eq.trans
      (p49plug_lift K n cut
        (fn [i :- Nat] (if (Nat.beq i j) c (rho i))))
      (congrArg (fn [env :- (=> Nat Exp)] (p49plug K env))
        (p49tl_choose_lift j rho c n cut))))))

(thm p49tl_subst_nil
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat,
   rho :- (=> Nat Exp), c :- Exp, sg :- (=> Nat Exp)]
  (Eq Exp
    (subst sg
      (p49tba_F B K (List.nil (Prod Nat Nat)) j rho c))
    (p49tba_F B K (List.nil (Prod Nat Nat)) j
      (fn [i :- Nat] (subst sg (rho i)))
      (subst sg c)))
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (Eq.trans
      (p49plug_subst K sg
        (fn [i :- Nat] (if (Nat.beq i j) c (rho i))))
      (congrArg (fn [env :- (=> Nat Exp)] (p49plug K env))
        (p49tl_choose_subst j rho c sg))))))

;; A canonical literal-leaf default has no free variables. It makes the
;; equivariant terminal laws into the fixed-family Lift/Subst interface.
(kdef p49tl_rho0 (=> Nat Exp)
  (fn [_ :- Nat] (Exp.sleaf (Exp.lbl 0))))

(thm p49tl_lift_nil_closed
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat]
  (p49tba_Lift
    (p49tba_F B K (List.nil (Prod Nat Nat)) j p49tl_rho0))
  (intro n cut c)
  (exact (p49tl_lift_nil B K j p49tl_rho0 c n cut)))

(thm p49tl_subst_nil_closed
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat]
  (p49tba_Subst
    (p49tba_F B K (List.nil (Prod Nat Nat)) j p49tl_rho0))
  (intro sg c)
  (exact (p49tl_subst_nil B K j p49tl_rho0 c sg)))

;; These conditions isolate the two issues at a telescope binder. B must
;; remain fixed under the substitution acting on its newly bound code.
;; Moving an old hole across the code/evidence pair requires the frame laws.
(kdef p49tl_BLift (=> (=> Nat Exp Exp) Prop)
  (fn [B :- (=> Nat Exp Exp)]
    (forall [q Nat] (forall [n Nat] (forall [cut Nat]
      (Eq Exp
        (lift n (+ cut 1) (B q (Exp.var 0)))
        (B q (Exp.var 0))))))))

(kdef p49tl_BSubst (=> (=> Nat Exp Exp) Prop)
  (fn [B :- (=> Nat Exp Exp)]
    (forall [q Nat] (forall [sg (=> Nat Exp)]
      (Eq Exp
        (subst (upn 1 sg) (B q (Exp.var 0)))
        (B q (Exp.var 0)))))))

(kdef p49tl_FrameLift Prop
  (forall [e Exp] (forall [n Nat] (forall [cut Nat]
    (Eq Exp
      (lift n (+ cut 2) (lift 2 0 e))
      (lift 2 0 (lift n cut e)))))))

(kdef p49tl_FrameSubst Prop
  (forall [e Exp] (forall [sg (=> Nat Exp)]
    (Eq Exp
      (subst (upn 2 sg) (lift 2 0 e))
      (lift 2 0 (subst sg e))))))

;; The generic shift algebra gives the two frame laws. No restriction on
;; the framed expression is needed.
(thm p49tl_frame_lift_n [n :- Nat]
  (forall [e Exp] (forall [cut Nat]
    (Eq Exp
      (lift n (+ cut 2) (lift 2 0 e))
      (lift 2 0 (lift n cut e)))))
  (induction n)
  (intro e cut)
  (exact (Eq.trans
    (lift_zero (lift 2 0 e) (+ cut 2))
    (Eq.symm (congrArg
      (fn [v :- Exp] (lift 2 0 v))
      (lift_zero e cut)))))
  (intro e cut)
  (exact (Eq.trans
    (Eq.symm (lift_comp (lift 2 0 e) 1 n (+ cut 2)))
    (Eq.trans
      (congrArg (fn [v :- Exp] (lift 1 (+ cut 2) v))
        (ih_n e cut))
      (Eq.trans
        (lift_lift_comm (lift n cut e) 2 cut 0)
        (congrArg (fn [v :- Exp] (lift 2 0 v))
          (lift_comp e 1 n cut)))))))

(thm p49tl_frame_lift [] p49tl_FrameLift
  (unfold p49tl_FrameLift)
  (intro e n cut)
  (exact (p49tl_frame_lift_n n e cut)))

(thm p49tl_lsc_up [sg :- (=> Nat Exp)]
  (LSC sg (upn 1 sg) 0 0)
  (unfold LSC)
  (intro i)
  (exact (Eq.symm
    (congrArg (fn [v :- Exp] (subst (upn 1 sg) v))
      (lift_var_above 1 0 i (Nat.zero_le i))))))

(thm p49tl_subst_up1 [e :- Exp, sg :- (=> Nat Exp)]
  (Eq Exp
    (subst (upn 1 sg) (lift 1 0 e))
    (lift 1 0 (subst sg e)))
  (exact (Eq.symm
    (lift_subst e sg (upn 1 sg) 0 0 (p49tl_lsc_up sg)))))

(thm p49tl_frame_subst [] p49tl_FrameSubst
  (unfold p49tl_FrameSubst)
  (intro e sg)
  (exact (Eq.trans
    (congrArg (fn [v :- Exp] (subst (upn 2 sg) v))
      (Eq.symm (lift_comp e 1 1 0)))
    (Eq.trans
      (p49tl_subst_up1 (lift 1 0 e) (upn 1 sg))
      (Eq.trans
        (congrArg (fn [v :- Exp] (lift 1 0 v))
          (p49tl_subst_up1 e sg))
        (lift_comp (subst sg e) 1 1 0))))))

(thm p49tl_bound1 [cut :- Nat]
  (LT.lt 1 (+ cut 2))
  (omega))

(thm p49tl_bound1_lift [n :- Nat, cut :- Nat]
  (Eq Exp (lift n (+ cut 2) (Exp.var 1)) (Exp.var 1))
  (exact (lift_var_below n (+ cut 2) 1 (p49tl_bound1 cut))))

(thm p49tl_step_lift
  [flag :- Bool, e :- Exp, n :- Nat, cut :- Nat,
   hframe :- p49tl_FrameLift]
  (Eq Exp
    (lift n (+ cut 2)
      (if flag (Exp.var 1) (lift 2 0 e)))
    (if flag (Exp.var 1) (lift 2 0 (lift n cut e))))
  (exact (Bool.rec$0
    (fn [b :- Bool]
      (Eq Exp
        (lift n (+ cut 2)
          (if b (Exp.var 1) (lift 2 0 e)))
        (if b (Exp.var 1) (lift 2 0 (lift n cut e)))))
    (hframe e n cut) (p49tl_bound1_lift n cut) flag)))

(thm p49tl_step_subst
  [flag :- Bool, e :- Exp, sg :- (=> Nat Exp),
   hframe :- p49tl_FrameSubst]
  (Eq Exp
    (subst (upn 2 sg)
      (if flag (Exp.var 1) (lift 2 0 e)))
    (if flag (Exp.var 1) (lift 2 0 (subst sg e))))
  (exact (Bool.rec$0
    (fn [b :- Bool]
      (Eq Exp
        (subst (upn 2 sg)
          (if b (Exp.var 1) (lift 2 0 e)))
        (if b (Exp.var 1) (lift 2 0 (subst sg e)))))
    (hframe e sg) rfl flag)))

(thm p49tl_step_lift_fun
  [h :- Nat, rho :- (=> Nat Exp), n :- Nat, cut :- Nat,
   hframe :- p49tl_FrameLift]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (lift n (+ cut 2)
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (fn [i :- Nat]
      (if (Nat.beq i h) (Exp.var 1)
        (lift 2 0 (lift n cut (rho i))))))
  (exact (funext (fn [i :- Nat]
    (p49tl_step_lift (Nat.beq i h) (rho i) n cut hframe)))))

(thm p49tl_step_subst_fun
  [h :- Nat, rho :- (=> Nat Exp), sg :- (=> Nat Exp),
   hframe :- p49tl_FrameSubst]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (subst (upn 2 sg)
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (fn [i :- Nat]
      (if (Nat.beq i h) (Exp.var 1)
        (lift 2 0 (subst sg (rho i))))))
  (exact (funext (fn [i :- Nat]
    (p49tl_step_subst (Nat.beq i h) (rho i) sg hframe)))))

;; Arbitrary-slot equivariance: the theorem transforms rho. The frame
;; laws and stability of B are stated openly as premises, not axioms.
(thm p49tl_lift_equiv [slots :- (List (Prod Nat Nat))]
  (forall [B (=> Nat Exp Exp)] (forall [K P49C]
    (forall [rho (=> Nat Exp)] (forall [n Nat] (forall [cut Nat]
      (=> (p49tl_BLift B) (=> p49tl_FrameLift
        (Eq Exp
          (lift n cut (p49tele B K slots rho))
          (p49tele B K slots
            (fn [i :- Nat] (lift n cut (rho i))))))))))))
  (induction slots)
  (intro B K rho n cut hB hframe)
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (p49plug_lift K n cut rho)))
  (intro B K rho n cut hB hframe)
  (have hbody (Eq Exp
    (lift n (+ cut 2)
      (p49tele B K tail
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))))
    (p49tele B K tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (lift n cut (rho i)))))))
    (Eq.trans
      (ih_tail B K
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))
        n (+ cut 2) hB hframe)
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K tail env))
        (p49tl_step_lift_fun (Prod.fst head) rho n cut hframe))))
  (exact (Eq.trans
    (congrArg
      (fn [A :- Exp]
        (Exp.tPi U.uw Exp.tSyn
          (Exp.tPi U.u1 (Exp.tT A)
            (lift n (+ cut 2)
              (p49tele B K tail
                (fn [i :- Nat]
                  (if (Nat.beq i (Prod.fst head))
                    (Exp.var 1) (lift 2 0 (rho i)))))))))
      (hB (Prod.snd head) n cut))
    (congrArg
      (fn [body :- Exp]
        (Exp.tPi U.uw Exp.tSyn
          (Exp.tPi U.u1
            (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
      hbody))))

(thm p49tl_subst_equiv [slots :- (List (Prod Nat Nat))]
  (forall [B (=> Nat Exp Exp)] (forall [K P49C]
    (forall [rho (=> Nat Exp)] (forall [sg (=> Nat Exp)]
      (=> (p49tl_BSubst B) (=> p49tl_FrameSubst
        (Eq Exp
          (subst sg (p49tele B K slots rho))
          (p49tele B K slots
            (fn [i :- Nat] (subst sg (rho i)))))))))))
  (induction slots)
  (intro B K rho sg hB hframe)
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (p49plug_subst K sg rho)))
  (intro B K rho sg hB hframe)
  (have hbody (Eq Exp
    (subst (upn 2 sg)
      (p49tele B K tail
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))))
    (p49tele B K tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (subst sg (rho i)))))))
    (Eq.trans
      (ih_tail B K
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))
        (upn 2 sg) hB hframe)
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K tail env))
        (p49tl_step_subst_fun (Prod.fst head) rho sg hframe))))
  (exact (Eq.trans
    (congrArg
      (fn [A :- Exp]
        (Exp.tPi U.uw Exp.tSyn
          (Exp.tPi U.u1 (Exp.tT A)
            (subst (upn 2 sg)
              (p49tele B K tail
                (fn [i :- Nat]
                  (if (Nat.beq i (Prod.fst head))
                    (Exp.var 1) (lift 2 0 (rho i)))))))))
      (hB (Prod.snd head) sg))
    (congrArg
      (fn [body :- Exp]
        (Exp.tPi U.uw Exp.tSyn
          (Exp.tPi U.u1
            (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
      hbody))))

;; The closed literal-leaf default cancels the environment transformation.
;; These are the precise arbitrary-slot fixed-family laws; their remaining
;; premises are B stability and the two general frame identities above.
(thm p49tl_lift_F_closed
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   hB :- (p49tl_BLift B)]
  (p49tba_Lift (p49tba_F B K slots j p49tl_rho0))
  (intro n cut c)
  (exact (Eq.trans
    (p49tl_lift_equiv slots B K
      (fn [i :- Nat]
        (if (Nat.beq i j) c (p49tl_rho0 i)))
      n cut hB p49tl_frame_lift)
    (congrArg
      (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
      (p49tl_choose_lift j p49tl_rho0 c n cut)))))

(thm p49tl_subst_F_closed
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   hB :- (p49tl_BSubst B)]
  (p49tba_Subst (p49tba_F B K slots j p49tl_rho0))
  (intro sg c)
  (exact (Eq.trans
    (p49tl_subst_equiv slots B K
      (fn [i :- Nat]
        (if (Nat.beq i j) c (p49tl_rho0 i)))
      sg hB p49tl_frame_subst)
    (congrArg
      (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
      (p49tl_choose_subst j p49tl_rho0 c sg)))))

;; For a general ambient environment, closedness is a pointwise
;; hypothesis. It cannot be omitted: unchanged free variables in rho
;; would make the fixed-family equations false.
(kdef p49tl_RhoLift (=> (=> Nat Exp) Prop)
  (fn [rho :- (=> Nat Exp)]
    (forall [i Nat] (forall [n Nat] (forall [cut Nat]
      (Eq Exp (lift n cut (rho i)) (rho i)))))))

(kdef p49tl_RhoSubst (=> (=> Nat Exp) Prop)
  (fn [rho :- (=> Nat Exp)]
    (forall [i Nat] (forall [sg (=> Nat Exp)]
      (Eq Exp (subst sg (rho i)) (rho i))))))

(thm p49tl_rho0_lift [] (p49tl_RhoLift p49tl_rho0)
  (intro i n cut)
  (rfl))

(thm p49tl_rho0_subst [] (p49tl_RhoSubst p49tl_rho0)
  (intro i sg)
  (rfl))

(thm p49tl_choose_stable_lift
  [j :- Nat, rho :- (=> Nat Exp), c :- Exp,
   n :- Nat, cut :- Nat, hRho :- (p49tl_RhoLift rho)]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (if (Nat.beq i j) (lift n cut c)
        (lift n cut (rho i))))
    (fn [i :- Nat]
      (if (Nat.beq i j) (lift n cut c) (rho i))))
  (exact (funext (fn [i :- Nat]
    (Bool.rec$0
      (fn [b :- Bool]
        (Eq Exp
          (if b (lift n cut c) (lift n cut (rho i)))
          (if b (lift n cut c) (rho i))))
      (hRho i n cut) rfl (Nat.beq i j))))))

(thm p49tl_choose_stable_subst
  [j :- Nat, rho :- (=> Nat Exp), c :- Exp,
   sg :- (=> Nat Exp), hRho :- (p49tl_RhoSubst rho)]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (if (Nat.beq i j) (subst sg c)
        (subst sg (rho i))))
    (fn [i :- Nat]
      (if (Nat.beq i j) (subst sg c) (rho i))))
  (exact (funext (fn [i :- Nat]
    (Bool.rec$0
      (fn [b :- Bool]
        (Eq Exp
          (if b (subst sg c) (subst sg (rho i)))
          (if b (subst sg c) (rho i))))
      (hRho i sg) rfl (Nat.beq i j))))))

(thm p49tl_lift_F_stable
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   rho :- (=> Nat Exp), hB :- (p49tl_BLift B),
   hRho :- (p49tl_RhoLift rho)]
  (p49tba_Lift (p49tba_F B K slots j rho))
  (intro n cut c)
  (exact (Eq.trans
    (p49tl_lift_equiv slots B K
      (fn [i :- Nat] (if (Nat.beq i j) c (rho i)))
      n cut hB p49tl_frame_lift)
    (Eq.trans
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
        (p49tl_choose_lift j rho c n cut))
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
        (p49tl_choose_stable_lift j rho c n cut hRho))))))

(thm p49tl_subst_F_stable
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   rho :- (=> Nat Exp), hB :- (p49tl_BSubst B),
   hRho :- (p49tl_RhoSubst rho)]
  (p49tba_Subst (p49tba_F B K slots j rho))
  (intro sg c)
  (exact (Eq.trans
    (p49tl_subst_equiv slots B K
      (fn [i :- Nat] (if (Nat.beq i j) c (rho i)))
      sg hB p49tl_frame_subst)
    (Eq.trans
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
        (p49tl_choose_subst j rho c sg))
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
        (p49tl_choose_stable_subst j rho c sg hRho))))))

;; The depth predicate is stable at the freshly bound code variable.
;; These discharge the abstract B premises above for the actual P49 family.
(thm p49tl_bound0 [cut :- Nat]
  (LT.lt 0 (+ cut 1))
  (omega))

(thm p49tl_p49dl_BLift []
  (p49tl_BLift (fn [q :- Nat, c :- Exp] (p49dl q c)))
  (intro q n cut)
  (exact (Eq.trans
    (p49pd_dl_lift q n (+ cut 1) (Exp.var 0))
    (congrArg (fn [e :- Exp] (p49dl q e))
      (lift_var_below n (+ cut 1) 0 (p49tl_bound0 cut))))))

(thm p49tl_p49dl_BSubst []
  (p49tl_BSubst (fn [q :- Nat, c :- Exp] (p49dl q c)))
  (intro q sg)
  (exact (p49dl_subst q (upn 1 sg) (Exp.var 0))))

;; p49pd_Lift/Subst are the successor-peel interfaces. The definitions
;; coincide with the bridge's unary laws, so this is a direct kernel bridge.
(thm p49tl_p49dl_Lift
  [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat]
  (p49pd_Lift
    (p49tba_F (fn [q :- Nat, c :- Exp] (p49dl q c))
      K slots j p49tl_rho0))
  (exact (p49tl_lift_F_closed
    (fn [q :- Nat, c :- Exp] (p49dl q c))
    K slots j p49tl_p49dl_BLift)))

(thm p49tl_p49dl_Subst
  [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat]
  (p49pd_Subst
    (p49tba_F (fn [q :- Nat, c :- Exp] (p49dl q c))
      K slots j p49tl_rho0))
  (exact (p49tl_subst_F_closed
    (fn [q :- Nat, c :- Exp] (p49dl q c))
    K slots j p49tl_p49dl_BSubst)))
