(ns lcert.formal.p49applytyped
  "Runtime typing for applying one recursive telescope code/evidence pair."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49applysubst]
            [lcert.formal.p49depth]))

(thm p49at_evidence_subst [j :- Nat, x :- Exp]
  (Eq Exp
    (subst1 x (Exp.tT (p49dl j (Exp.var 0))))
    (Exp.tT (p49dl j x)))
  (exact (congrArg (fn [e :- Exp] (Exp.tT e))
    (p49dl_subst j (fn [i :- Nat] (inst1 x i))
      (Exp.var 0)))))

;; The two formation premises are the codomains required by rApp. They
;; are kept explicit so this rule can be used with any ambient context and
;; any live child code/evidence pair.
(thm p49at_apply_pair
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
   h :- Nat, j :- Nat,
   D :- (List Exp), uf :- (List U), ux :- (List U), uv :- (List U),
   f :- Exp, x :- Exp, v :- Exp,
   hf :- (Rt chkf D uf f
     (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K
       (List.cons (Prod Nat Nat) (Prod.mk h j) slots) rho)),
   hx :- (Rt chkf D ux x Exp.tSyn),
   hv :- (Rt chkf D uv v (Exp.tT (p49dl j x))),
   hbody :- (Tl chkf Bool.true (consE Exp.tSyn D)
     (Exp.tPi U.u1 (Exp.tT (p49dl j (Exp.var 0)))
       (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
         (fn [i :- Nat]
           (if (Nat.beq i h) (Exp.var 1)
             (lift 2 0 (rho i)))))) Exp.tUnit),
   hA :- (Tl chkf Bool.true D
     (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) Exp.tUnit),
   htail :- (Tl chkf Bool.true
     (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
     (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
       (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
         (fn [i :- Nat]
           (if (Nat.beq i h) (Exp.var 1)
             (lift 2 0 (rho i)))))) Exp.tUnit)]
  (Rt chkf D
    (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
    (Exp.app (Exp.app f x) v)
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
      (fn [i :- Nat] (if (Nat.beq i h) x (rho i)))))
  (have hfirst
    (Rt chkf D (vadd uf (vscale U.uw ux))
      (Exp.app f x)
      (subst1 x
        (Exp.tPi U.u1 (Exp.tT (p49dl j (Exp.var 0)))
          (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
            (fn [i :- Nat]
              (if (Nat.beq i h) (Exp.var 1)
                (lift 2 0 (rho i))))))))
    (Rt.rApp chkf D uf ux U.uw f x Exp.tSyn
      (Exp.tPi U.u1 (Exp.tT (p49dl j (Exp.var 0)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i))))))
      rfl hf hx (Tl.fBase chkf D Exp.tSyn rfl) hbody))
  (have hsecond
    (Rt chkf D
      (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
      (Exp.app (Exp.app f x) v)
      (subst1 v
        (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
          (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
            (fn [i :- Nat]
              (if (Nat.beq i h) (Exp.var 1)
                (lift 2 0 (rho i))))))))
    (Rt.rApp chkf D
      (vadd uf (vscale U.uw ux)) uv U.u1
      (Exp.app f x) v
      (subst1 x (Exp.tT (p49dl j (Exp.var 0))))
      (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i))))))
      rfl hfirst
      (p49peel_cast chkf D uv uv v
        (Exp.tT (p49dl j x))
        (subst1 x (Exp.tT (p49dl j (Exp.var 0))))
        hv rfl (Eq.symm (p49at_evidence_subst j x)))
      hA htail))
  (exact (p49peel_cast chkf D
    (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
    (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
    (Exp.app (Exp.app f x) v)
    (subst1 v
      (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i)))))))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
      (fn [i :- Nat] (if (Nat.beq i h) x (rho i))))
    hsecond rfl
    (p49as_slot_body
      (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots rho h x v
      p49tl_p49dl_BSubst))))

(kdef p49at_env1 (=> (=> Nat Exp) Nat Exp (=> Nat Exp))
  (fn [rho :- (=> Nat Exp), h :- Nat, x :- Exp]
    (fn [i :- Nat] (if (Nat.beq i h) x (rho i)))))

;; These are precisely the three formation arguments of two rApp rules.
(kdef p49at_Form
  (=> (=> Code Code Bool) (List Exp) P49C
      (List (Prod Nat Nat)) (=> Nat Exp) Nat Nat Exp Prop)
  (fn [chkf :- (=> Code Code Bool), D :- (List Exp), K :- P49C,
       slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
       h :- Nat, j :- Nat, x :- Exp]
    (And
      (Tl chkf Bool.true (consE Exp.tSyn D)
        (Exp.tPi U.u1 (Exp.tT (p49dl j (Exp.var 0)))
          (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
            (fn [i :- Nat]
              (if (Nat.beq i h) (Exp.var 1)
                (lift 2 0 (rho i)))))) Exp.tUnit)
      (And
        (Tl chkf Bool.true D
          (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) Exp.tUnit)
        (Tl chkf Bool.true
          (consE (subst1 x (Exp.tT (p49dl j (Exp.var 0)))) D)
          (subst (upn 1 (fn [i :- Nat] (inst1 x i)))
            (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
              (fn [i :- Nat]
                (if (Nat.beq i h) (Exp.var 1)
                  (lift 2 0 (rho i)))))) Exp.tUnit)))))

;; The four runtime applications, in the order left code, left evidence,
;; right code, right evidence. No branch-typing premise is assumed.
(thm p49at_apply_four
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
   m :- Nat, j :- Nat, D :- (List Exp),
   uf :- (List U), ux :- (List U), uv :- (List U),
   uy :- (List U), uw :- (List U),
   f :- Exp, x :- Exp, v :- Exp, y :- Exp, w :- Exp,
   hf :- (Rt chkf D uf f
     (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K
       (List.cons (Prod Nat Nat) (Prod.mk m j)
         (List.cons (Prod Nat Nat) (Prod.mk (Nat.succ m) j) slots))
       rho)),
   hx :- (Rt chkf D ux x Exp.tSyn),
   hv :- (Rt chkf D uv v (Exp.tT (p49dl j x))),
   hy :- (Rt chkf D uy y Exp.tSyn),
   hw :- (Rt chkf D uw w (Exp.tT (p49dl j y))),
   hform1 :- (p49at_Form chkf D K
     (List.cons (Prod Nat Nat) (Prod.mk (Nat.succ m) j) slots)
     rho m j x),
   hform2 :- (p49at_Form chkf D K slots
     (p49at_env1 rho m x) (Nat.succ m) j y)]
  (Rt chkf D
    (vadd
      (vadd
        (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
        (vscale U.uw uy))
      (vscale U.u1 uw))
    (Exp.app (Exp.app (Exp.app (Exp.app f x) v) y) w)
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
      (p49sc_pair_env rho m x y)))
  (have hleft
    (Rt chkf D
      (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
      (Exp.app (Exp.app f x) v)
      (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K
        (List.cons (Prod Nat Nat) (Prod.mk (Nat.succ m) j) slots)
        (p49at_env1 rho m x)))
    (p49at_apply_pair chkf K
      (List.cons (Prod Nat Nat) (Prod.mk (Nat.succ m) j) slots)
      rho m j D uf ux uv f x v hf hx hv
      (And.left hform1)
      (And.left (And.right hform1))
      (And.right (And.right hform1))))
  (have hright
    (Rt chkf D
      (vadd
        (vadd
          (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
          (vscale U.uw uy))
        (vscale U.u1 uw))
      (Exp.app (Exp.app (Exp.app (Exp.app f x) v) y) w)
      (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
        (p49at_env1 (p49at_env1 rho m x) (Nat.succ m) y)))
    (p49at_apply_pair chkf K slots
      (p49at_env1 rho m x) (Nat.succ m) j
      D (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
      uy uw
      (Exp.app (Exp.app f x) v) y w hleft hy hw
      (And.left hform2)
      (And.left (And.right hform2))
      (And.right (And.right hform2))))
  (exact (p49peel_cast chkf D
    (vadd
      (vadd
        (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
        (vscale U.uw uy))
      (vscale U.u1 uw))
    (vadd
      (vadd
        (vadd (vadd uf (vscale U.uw ux)) (vscale U.u1 uv))
        (vscale U.uw uy))
      (vscale U.u1 uw))
    (Exp.app (Exp.app (Exp.app (Exp.app f x) v) y) w)
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
      (p49at_env1 (p49at_env1 rho m x) (Nat.succ m) y))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots
      (p49sc_pair_env rho m x y))
    hright rfl
    (congrArg
      (fn [env :- (=> Nat Exp)]
        (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c)) K slots env))
      (p49as_pair_env rho m x y)))))
