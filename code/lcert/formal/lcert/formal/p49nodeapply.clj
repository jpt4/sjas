(ns lcert.formal.p49nodeapply
  "Instantiate four recursive applications at the live child binders."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm]]
            [lcert.formal.p49applyform]
            [lcert.formal.p49nodelive]
            [lcert.formal.p49contextfacts]
            [lcert.formal.p49teleformbridge]))

(thm p49na_node_lblok
  [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat,
   l :- Nat,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
  (Eq Bool
    (p49c_lblok
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots))))))
    Bool.true)
  (have hR
    (Eq Bool
      (p49c_lblok
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      Bool.true)
    (Eq.mp
      (congrArg
        (fn [flag :- Bool]
          (Eq Bool (Bool.and flag Bool.true) Bool.true))
        (Eq.symm hl))
      (Eq.refl$1 Bool.true)))
  (exact (p49c_lblok_repl K j
    (P49C.node l
      (P49C.hole (p49fresh K slots))
      (P49C.hole (Nat.succ (p49fresh K slots))))
    hK hR)))

(def ^:private p49na-Eprefix
  ['(p49pd_X j) '(p49pd_X j)
   '(p49peel_nodeA (p49pd_A j))
   '(y2Ty (p49peel_motive (p49pd_A j) (F (Exp.var 0))))
   '(y1Ty (p49peel_motive (p49pd_A j) (F (Exp.var 0))))
   'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn])

(doseq [[name index]
        [['p49na_left_tl 6] ['p49na_right_tl 5]]]
  (a/prove-theorem name
    '[chkf :- (=> Code Code Bool), j :- Nat,
      F :- (=> Exp Exp), D :- (List Exp)]
    (list 'Tl 'chkf 'Bool.false '(p49nm_D j F D)
      (list 'Exp.var index) 'Exp.tSyn)
    [(list 'exact
       (list 'Tl.zVar 'chkf '(p49nm_D j F D)
         index 'Exp.tSyn
         (lcert.formal.p49nodemotive/p49nm-lookup
           'nthE p49na-Eprefix 'D index)))]))

(thm p49na_left_env_typed
  [chkf :- (=> Code Code Bool), j :- Nat,
   F :- (=> Exp Exp), D :- (List Exp),
   m :- Nat]
  (forall [i Nat]
    (Tl chkf Bool.false (p49nm_D j F D)
      (p49at_env1 p49tl_rho0 m (Exp.var 6) i) Exp.tSyn))
  (intro i)
  (exact (p49tf_if_typed chkf (p49nm_D j F D)
    (Nat.beq i m) (Exp.var 6) (p49tl_rho0 i) Exp.tSyn
    (p49na_left_tl chkf j F D)
    (p49tf_rho0_typed chkf (p49nm_D j F D) i))))

;; The two application steps have all their rApp formation premises in
;; the actual node context, including the live left-hole environment.
(thm p49na_forms
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), k :- Nat, h :- Nat, l :- Nat,
   D :- (List Exp),
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
  (And
    (p49at_Form chkf
      (p49nm_D k (p49nm_F K slots h) D)
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      (List.cons (Prod Nat Nat)
        (Prod.mk (Nat.succ (p49fresh K slots)) k) slots)
      p49tl_rho0 (p49fresh K slots) k (Exp.var 6))
    (p49at_Form chkf
      (p49nm_D k (p49nm_F K slots h) D)
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49at_env1 p49tl_rho0 (p49fresh K slots) (Exp.var 6))
      (Nat.succ (p49fresh K slots)) k (Exp.var 5)))
  (have hnode
    (Eq Bool
      (p49c_lblok
        ((p49crepl K) h
          (P49C.node l
            (P49C.hole (p49fresh K slots))
            (P49C.hole (Nat.succ (p49fresh K slots))))))
      Bool.true)
    (p49na_node_lblok K slots h l hK hl))
  (exact (And.intro
    (p49af_form chkf
      (p49nm_D k (p49nm_F K slots h) D)
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      hnode
      (List.cons (Prod Nat Nat)
        (Prod.mk (Nat.succ (p49fresh K slots)) k) slots)
      p49tl_rho0
      (fn [i :- Nat]
        (p49tf_rho0_typed chkf
          (p49nm_D k (p49nm_F K slots h) D) i))
      (p49fresh K slots) k (Exp.var 6)
      (p49na_left_tl chkf k (p49nm_F K slots h) D))
    (p49af_form chkf
      (p49nm_D k (p49nm_F K slots h) D)
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      hnode slots
      (p49at_env1 p49tl_rho0 (p49fresh K slots) (Exp.var 6))
      (p49na_left_env_typed chkf k (p49nm_F K slots h) D
        (p49fresh K slots))
      (Nat.succ (p49fresh K slots)) k (Exp.var 5)
      (p49na_right_tl chkf k (p49nm_F K slots h) D)))))

(defn- p49na-prefix [xs tail]
  (reduce (fn [acc x] (list 'consU x acc)) tail (reverse xs)))

(doseq [[name usage xs]
        [['p49na_scale_left 'p49nl_Uleft
          '[U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0]]
         ['p49na_scale_right 'p49nl_Uright
          '[U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0 U.u0]]]]
  (a/prove-theorem name '[uv :- (List U)]
    (list 'Eq '(List U)
      (list 'vscale 'U.uw (list usage 'uv))
      (list usage 'uv))
    [(list 'exact
       (list 'congrArg
         (list 'fn '[tail :- (List U)]
           (p49na-prefix xs 'tail))
         '(p49peel_zscale uv)))]))

(thm p49na_usage [uv :- (List U)]
  (Eq (List U)
    (vadd
      (vadd
        (vadd
          (vadd (p49nl_Ucarry uv)
            (vscale U.uw (p49nl_Uleft uv)))
          (vscale U.u1 (p49nl_UleftEv uv)))
        (vscale U.uw (p49nl_Uright uv)))
      (vscale U.u1 (p49nl_UrightEv uv)))
    (p49nl_Ubranch uv))
  (rw [(p49na_scale_left uv)
       (p49pd_scale1 (p49nl_UleftEv uv))
       (p49na_scale_right uv)
       (p49pd_scale1 (p49nl_UrightEv uv))])
  (exact (p49nl_branch_usage uv)))

(thm p49na_node_branch
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), k :- Nat, h :- Nat, l :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hl :- (Eq Bool (Nat.blt l 100) Bool.true),
   f :- Exp,
   hf :- (Rt chkf
     (p49nm_D k (p49nm_F K slots h) D)
     (p49nl_Ucarry uv) f
     (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c))
       ((p49crepl K) h
         (P49C.node l
           (P49C.hole (p49fresh K slots))
           (P49C.hole (Nat.succ (p49fresh K slots)))))
       (List.cons (Prod Nat Nat)
         (Prod.mk (p49fresh K slots) k)
         (List.cons (Prod Nat Nat)
           (Prod.mk (Nat.succ (p49fresh K slots)) k) slots))
       p49tl_rho0))]
  (Rt chkf
    (p49nm_D k (p49nm_F K slots h) D)
    (p49nl_Ubranch uv)
    (Exp.app (Exp.app (Exp.app (Exp.app f (Exp.var 6))
      (Exp.var 1)) (Exp.var 5)) (Exp.var 0))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
        (Exp.var 6) (Exp.var 5))))
  (have hforms
    (And
      (p49at_Form chkf
        (p49nm_D k (p49nm_F K slots h) D)
        ((p49crepl K) h
          (P49C.node l
            (P49C.hole (p49fresh K slots))
            (P49C.hole (Nat.succ (p49fresh K slots)))))
        (List.cons (Prod Nat Nat)
          (Prod.mk (Nat.succ (p49fresh K slots)) k) slots)
        p49tl_rho0 (p49fresh K slots) k (Exp.var 6))
      (p49at_Form chkf
        (p49nm_D k (p49nm_F K slots h) D)
        ((p49crepl K) h
          (P49C.node l
            (P49C.hole (p49fresh K slots))
            (P49C.hole (Nat.succ (p49fresh K slots)))))
        slots
        (p49at_env1 p49tl_rho0 (p49fresh K slots) (Exp.var 6))
        (Nat.succ (p49fresh K slots)) k (Exp.var 5)))
    (p49na_forms chkf K slots k h l D hK hl))
  (exact (p49peel_cast chkf
    (p49nm_D k (p49nm_F K slots h) D)
    (vadd
      (vadd
        (vadd
          (vadd (p49nl_Ucarry uv)
            (vscale U.uw (p49nl_Uleft uv)))
          (vscale U.u1 (p49nl_UleftEv uv)))
        (vscale U.uw (p49nl_Uright uv)))
      (vscale U.u1 (p49nl_UrightEv uv)))
    (p49nl_Ubranch uv)
    (Exp.app (Exp.app (Exp.app (Exp.app f (Exp.var 6))
      (Exp.var 1)) (Exp.var 5)) (Exp.var 0))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
        (Exp.var 6) (Exp.var 5)))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
        (Exp.var 6) (Exp.var 5)))
    (p49at_apply_four chkf
      ((p49crepl K) h
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots p49tl_rho0 (p49fresh K slots) k
      (p49nm_D k (p49nm_F K slots h) D)
      (p49nl_Ucarry uv)
      (p49nl_Uleft uv) (p49nl_UleftEv uv)
      (p49nl_Uright uv) (p49nl_UrightEv uv)
      f (Exp.var 6) (Exp.var 1) (Exp.var 5) (Exp.var 0)
      hf
      (p49nl_left_code chkf k (p49nm_F K slots h) D uv hlen)
      (p49nl_left_evidence chkf k (p49nm_F K slots h) D uv hlen)
      (p49nl_right_code chkf k (p49nm_F K slots h) D uv hlen)
      (p49nl_right_evidence chkf k (p49nm_F K slots h) D uv hlen)
      (And.left hforms) (And.right hforms))
    (p49na_usage uv) rfl)))
