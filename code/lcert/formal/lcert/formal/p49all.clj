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

(thm p49all_nil
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   fuel :- Nat, K :- P49C,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf D uv
    (p49tau fuel K (List.nil (Prod Nat Nat)))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.nil (Prod Nat Nat)) p49tl_rho0))
  (cases fuel)
  (exact (Eq.mp
    (congrArg
      (fn [t :- Exp]
        (Rt chkf D uv t
          (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
            K (List.nil (Prod Nat Nat)) p49tl_rho0)))
      (Eq.symm (p49tau_terminal_zero K)))
    (p49tc_closed chkf dec encTy hcs K hK D uv hlen)))
  (exact (Eq.mp
    (congrArg
      (fn [t :- Exp]
        (Rt chkf D uv t
          (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
            K (List.nil (Prod Nat Nat)) p49tl_rho0)))
      (Eq.symm (p49tau_terminal n K)))
    (p49tc_closed chkf dec encTy hcs K hK D uv hlen))))

(thm p49all_leaf_lblok
  [K :- P49C, j :- Nat, l :- Nat,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
  (Eq Bool
    (p49c_lblok ((p49crepl K) j (P49C.leaf l)))
    Bool.true)
  (exact (p49c_lblok_repl K j (P49C.leaf l) hK hl)))

(thm p49all_leaf_branch_len
  [j :- Nat, D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Eq Nat (lenU (p49lc_Ubranches uv))
    (lenE (p49lc_Dleaf j D)))
  (exact (Eq.trans
    (p49peel_zlen (p49lc_Uleaf uv))
    (p49lc_leaf_len j D uv hlen))))

(thm p49all_head_type
  [K :- P49C, tail :- (List (Prod Nat Nat)),
   hole :- Nat, depth :- Nat]
  (Eq Exp
    (Exp.tPi U.uw Exp.tSyn
      (p49peel_body
        (Exp.tT (p49dl depth (Exp.var 0)))
        (p49lc_F K tail hole (Exp.var 0))))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat) (Prod.mk hole depth) tail)
      p49tl_rho0))
  (exact (congrArg
    (fn [body :- Exp]
      (Exp.tPi U.uw Exp.tSyn
        (Exp.tPi U.u1
          (Exp.tT (p49dl depth (Exp.var 0))) body)))
    ((p49tl_p49dl_Lift K tail hole) 1 0 (Exp.var 0)))))

(thm p49all_zero_step
  [chkf :- (=> Code Code Bool), fuel :- Nat,
   ih :- (p49all_Claim chkf fuel),
   K :- P49C, tail :- (List (Prod Nat Nat)), hole :- Nat,
   D :- (List Exp), uv :- (List U),
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail)),
   hpot :- (LE.le
     (p49pot
       (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail))
     (Nat.succ fuel)),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf D uv
    (p49tau (Nat.succ fuel) K
      (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail)
      p49tl_rho0))
  (have hleaf
    (Rt chkf (p49lz_Dleaf D) (p49lc_Uleaf uv)
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      (p49peel_leafB (p49lc_F K tail hole (Exp.var 0))))
    (p49lz_leaf_cont chkf K tail hole D uv hlen
      (p49su_head (Prod.mk hole 0) tail hu)
      (p49tf_F_closed_formed chkf K hK tail hole)
      (p49tl_p49dl_Lift K tail hole)
      (p49tl_p49dl_Subst K tail hole)
      (fn [l :- Nat]
        (p49tau fuel
          ((p49crepl K) hole (P49C.leaf l)) tail))
      (fn [l :- Nat]
        (fn [hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
          (ih ((p49crepl K) hole (P49C.leaf l)) tail
            (p49lz_Dleaf D) (p49lc_Ubranches uv)
            (p49all_leaf_lblok K hole l hK hl)
            (p49su_tail (Prod.mk hole 0) tail hu)
            (p49su_leaf_fuel (Prod.mk hole 0) tail fuel hpot)
            (p49lz_branch_len D uv hlen))))))
  (change (Rt chkf D uv
    (p49pz_term
      (p49lc_F K tail hole (Exp.var 0))
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0)))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail)
      p49tl_rho0)))
  (exact (p49peel_cast chkf D uv uv
    (p49pz_term
      (p49lc_F K tail hole (Exp.var 0))
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0)))
    (Exp.tPi U.uw Exp.tSyn
      (p49peel_body p49pz_A
        (p49lc_F K tail hole (Exp.var 0))))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat) (Prod.mk hole 0) tail)
      p49tl_rho0)
    (p49pz_peel chkf D
      (p49lc_F K tail hole)
      (p49tf_F_closed_formed chkf K hK tail hole)
      (p49tl_p49dl_Lift K tail hole)
      (p49tl_p49dl_Subst K tail hole)
      uv hlen
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      hleaf)
    rfl (p49all_head_type K tail hole 0))))

(thm p49all_succ_step
  [chkf :- (=> Code Code Bool), fuel :- Nat,
   ih :- (p49all_Claim chkf fuel),
   K :- P49C, tail :- (List (Prod Nat Nat)),
   hole :- Nat, k :- Nat,
   D :- (List Exp), uv :- (List U),
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat)
       (Prod.mk hole (Nat.succ k)) tail)),
   hpot :- (LE.le
     (p49pot
       (List.cons (Prod Nat Nat)
         (Prod.mk hole (Nat.succ k)) tail))
     (Nat.succ fuel)),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf D uv
    (p49tau (Nat.succ fuel) K
      (List.cons (Prod Nat Nat)
        (Prod.mk hole (Nat.succ k)) tail))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat)
        (Prod.mk hole (Nat.succ k)) tail)
      p49tl_rho0))
  (have hleaf
    (Rt chkf (p49lc_Dleaf k D) (p49lc_Uleaf uv)
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      (p49peel_leafB (p49lc_F K tail hole (Exp.var 0))))
    (p49lc_leaf_cont chkf K tail hole k D uv hlen
      (p49su_head (Prod.mk hole (Nat.succ k)) tail hu)
      (p49tf_F_closed_formed chkf K hK tail hole)
      (p49tl_p49dl_Lift K tail hole)
      (p49tl_p49dl_Subst K tail hole)
      (fn [l :- Nat]
        (p49tau fuel
          ((p49crepl K) hole (P49C.leaf l)) tail))
      (fn [l :- Nat]
        (fn [hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
          (ih ((p49crepl K) hole (P49C.leaf l)) tail
            (p49lc_Dleaf k D) (p49lc_Ubranches uv)
            (p49all_leaf_lblok K hole l hK hl)
            (p49su_tail (Prod.mk hole (Nat.succ k)) tail hu)
            (p49su_leaf_fuel
              (Prod.mk hole (Nat.succ k)) tail fuel hpot)
            (p49all_leaf_branch_len k D uv hlen))))))
  (have hnode
    (Rt chkf
      (p49nm_D k (p49nm_F K tail hole) D)
      (p49nm_U uv)
      (Exp.caseL
        (p49nm_M (p49nm_F K tail hole))
        (Exp.var 7)
        (p49brs 100
          (fn [l :- Nat]
            (Exp.app
              (Exp.app
                (Exp.app
                  (Exp.app
                    (p49tau fuel
                      ((p49crepl K) hole
                        (P49C.node l
                          (P49C.hole (p49fresh K tail))
                          (P49C.hole (Nat.succ (p49fresh K tail)))))
                      (List.cons (Prod Nat Nat)
                        (Prod.mk (p49fresh K tail) k)
                        (List.cons (Prod Nat Nat)
                          (Prod.mk (Nat.succ (p49fresh K tail)) k)
                          tail)))
                    (Exp.var 6))
                  (Exp.var 1))
                (Exp.var 5))
              (Exp.var 0))) 0))
      (lift 2 0
        (p49peel_nodeB
          ((p49nm_F K tail hole) (Exp.var 0)))))
    (p49nl_node_cont chkf K tail k hole D uv hlen
      (p49su_head (Prod.mk hole (Nat.succ k)) tail hu)
      (p49tf_F_closed_formed chkf K hK tail hole)
      (fn [l :- Nat]
        (Exp.app
          (Exp.app
            (Exp.app
              (Exp.app
                (p49tau fuel
                  ((p49crepl K) hole
                    (P49C.node l
                      (P49C.hole (p49fresh K tail))
                      (P49C.hole (Nat.succ (p49fresh K tail)))))
                  (List.cons (Prod Nat Nat)
                    (Prod.mk (p49fresh K tail) k)
                    (List.cons (Prod Nat Nat)
                      (Prod.mk (Nat.succ (p49fresh K tail)) k)
                      tail)))
                (Exp.var 6))
              (Exp.var 1))
            (Exp.var 5))
          (Exp.var 0)))
      (fn [l :- Nat]
        (fn [hl :- (Eq Bool (Nat.blt l 100) Bool.true)]
          (p49na_node_branch chkf K tail k hole l D uv
            hlen hK hl
            (p49tau fuel
              ((p49crepl K) hole
                (P49C.node l
                  (P49C.hole (p49fresh K tail))
                  (P49C.hole (Nat.succ (p49fresh K tail)))))
              (List.cons (Prod Nat Nat)
                (Prod.mk (p49fresh K tail) k)
                (List.cons (Prod Nat Nat)
                  (Prod.mk (Nat.succ (p49fresh K tail)) k)
                  tail)))
            (ih
              ((p49crepl K) hole
                (P49C.node l
                  (P49C.hole (p49fresh K tail))
                  (P49C.hole (Nat.succ (p49fresh K tail)))))
              (List.cons (Prod Nat Nat)
                (Prod.mk (p49fresh K tail) k)
                (List.cons (Prod Nat Nat)
                  (Prod.mk (Nat.succ (p49fresh K tail)) k)
                  tail))
              (p49nm_D k (p49nm_F K tail hole) D)
              (p49nl_Ucarry uv)
              (p49na_node_lblok K tail hole l hK hl)
              (p49su_node K hole k tail hu)
              (p49su_node_fuel K (Prod.mk hole k) tail fuel hpot)
              (p49nm_len k (p49nm_F K tail hole) D uv hlen)))))))
  (change (Rt chkf D uv
    (p49pd_term k (p49lc_F K tail hole (Exp.var 0))
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      (Exp.caseL
        (p49nm_M (p49nm_F K tail hole))
        (Exp.var 7)
        (p49brs 100
          (fn [l :- Nat]
            (Exp.app
              (Exp.app
                (Exp.app
                  (Exp.app
                    (p49tau fuel
                      ((p49crepl K) hole
                        (P49C.node l
                          (P49C.hole (p49fresh K tail))
                          (P49C.hole (Nat.succ (p49fresh K tail)))))
                      (List.cons (Prod Nat Nat)
                        (Prod.mk (p49fresh K tail) k)
                        (List.cons (Prod Nat Nat)
                          (Prod.mk (Nat.succ (p49fresh K tail)) k)
                          tail)))
                    (Exp.var 6))
                  (Exp.var 1))
                (Exp.var 5))
              (Exp.var 0))) 0)))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat)
        (Prod.mk hole (Nat.succ k)) tail)
      p49tl_rho0)))
  (exact (p49peel_cast chkf D uv uv
    (p49pd_term k (p49lc_F K tail hole (Exp.var 0))
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      (Exp.caseL
        (p49nm_M (p49nm_F K tail hole))
        (Exp.var 7)
        (p49brs 100
          (fn [l :- Nat]
            (Exp.app
              (Exp.app
                (Exp.app
                  (Exp.app
                    (p49tau fuel
                      ((p49crepl K) hole
                        (P49C.node l
                          (P49C.hole (p49fresh K tail))
                          (P49C.hole (Nat.succ (p49fresh K tail)))))
                      (List.cons (Prod Nat Nat)
                        (Prod.mk (p49fresh K tail) k)
                        (List.cons (Prod Nat Nat)
                          (Prod.mk (Nat.succ (p49fresh K tail)) k)
                          tail)))
                    (Exp.var 6))
                  (Exp.var 1))
                (Exp.var 5))
              (Exp.var 0))) 0)))
    (Exp.tPi U.uw Exp.tSyn
      (p49peel_body (p49pd_A k)
        (p49lc_F K tail hole (Exp.var 0))))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat)
        (Prod.mk hole (Nat.succ k)) tail)
      p49tl_rho0)
    (p49pd_peel chkf k D
      (p49lc_F K tail hole)
      (p49tf_F_closed_formed chkf K hK tail hole)
      (p49tl_p49dl_Lift K tail hole)
      (p49tl_p49dl_Subst K tail hole)
      uv hlen
      (Exp.caseL
        (p49lc_F K tail hole (Exp.sleaf (Exp.var 0)))
        (Exp.var 1)
        (p49brs 100
          (fn [l :- Nat]
            (p49tau fuel
              ((p49crepl K) hole (P49C.leaf l)) tail)) 0))
      (Exp.caseL
        (p49nm_M (p49nm_F K tail hole))
        (Exp.var 7)
        (p49brs 100
          (fn [l :- Nat]
            (Exp.app
              (Exp.app
                (Exp.app
                  (Exp.app
                    (p49tau fuel
                      ((p49crepl K) hole
                        (P49C.node l
                          (P49C.hole (p49fresh K tail))
                          (P49C.hole (Nat.succ (p49fresh K tail)))))
                      (List.cons (Prod Nat Nat)
                        (Prod.mk (p49fresh K tail) k)
                        (List.cons (Prod Nat Nat)
                          (Prod.mk (Nat.succ (p49fresh K tail)) k)
                          tail)))
                    (Exp.var 6))
                  (Exp.var 1))
                (Exp.var 5))
              (Exp.var 0))) 0))
      hleaf hnode)
    rfl (p49all_head_type K tail hole (Nat.succ k)))))

(thm p49all_slot_step
  [chkf :- (=> Code Code Bool), fuel :- Nat,
   ih :- (p49all_Claim chkf fuel),
   K :- P49C, tail :- (List (Prod Nat Nat)),
   hole :- Nat, k :- Nat,
   D :- (List Exp), uv :- (List U),
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat) (Prod.mk hole k) tail)),
   hpot :- (LE.le
     (p49pot (List.cons (Prod Nat Nat) (Prod.mk hole k) tail))
     (Nat.succ fuel)),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf D uv
    (p49tau (Nat.succ fuel) K
      (List.cons (Prod Nat Nat) (Prod.mk hole k) tail))
    (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.cons (Prod Nat Nat) (Prod.mk hole k) tail)
      p49tl_rho0))
  (cases k)
  (exact (p49all_zero_step chkf fuel ih K tail hole
    D uv hK hu hpot hlen))
  (exact (p49all_succ_step chkf fuel ih K tail hole n
    D uv hK hu hpot hlen)))

(thm p49all_fuel
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   fuel :- Nat]
  (p49all_Claim chkf fuel)
  (induction fuel)
  (intro K slots D uv hK hu hpot hlen)
  (have hnil
    (Eq (List (Prod Nat Nat)) slots (List.nil (Prod Nat Nat)))
    (p49tau_zero_nil slots hpot))
  (subst hnil)
  (exact (p49all_nil chkf dec encTy hcs 0 K hK D uv hlen))
  (intro K slots D uv hK hu hpot hlen)
  (cases slots)
  (exact (p49all_nil chkf dec encTy hcs (Nat.succ n) K hK D uv hlen))
  (cases head)
  (exact (p49all_slot_step chkf n ih_n K tail fst snd
    D uv hK hu hpot hlen)))

(thm p49all_prop49
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   k :- Nat]
  (Rt chkf (List.nil Exp) (List.nil U)
    (p49tau (p49pot
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))))
    (P49 k))
  (have hcore
    (Rt chkf (List.nil Exp) (List.nil U)
      (p49tau (p49pot
        (List.cons (Prod Nat Nat) (Prod.mk 0 k)
          (List.nil (Prod Nat Nat))))
        (P49C.hole 0)
        (List.cons (Prod Nat Nat) (Prod.mk 0 k)
          (List.nil (Prod Nat Nat))))
      (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c))
        (P49C.hole 0)
        (List.cons (Prod Nat Nat) (Prod.mk 0 k)
          (List.nil (Prod Nat Nat))) p49tl_rho0))
    ((p49all_fuel chkf dec encTy hcs
       (p49pot
         (List.cons (Prod Nat Nat) (Prod.mk 0 k)
           (List.nil (Prod Nat Nat)))))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat)))
      (List.nil Exp) (List.nil U)
      (Eq.refl Bool.true)
      (p49su_one (Prod.mk 0 k))
      (Nat.le_refl (p49pot
        (List.cons (Prod Nat Nat) (Prod.mk 0 k)
          (List.nil (Prod Nat Nat)))))
      (Eq.refl 0)))
  (exact (Eq.mp
    (congrArg
      (fn [T :- Exp]
        (Rt chkf (List.nil Exp) (List.nil U)
          (p49tau (p49pot
            (List.cons (Prod Nat Nat) (Prod.mk 0 k)
              (List.nil (Prod Nat Nat))))
            (P49C.hole 0)
            (List.cons (Prod Nat Nat) (Prod.mk 0 k)
              (List.nil (Prod Nat Nat)))) T))
      (p49tele_one
        (fn [j :- Nat, c :- Exp] (p49dl j c)) k p49tl_rho0))
    hcore)))

(thm p49all_prop49_concrete [k :- Nat]
  (Rt (Check decCert) (List.nil Exp) (List.nil U)
    (p49tau (p49pot
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))))
      (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))))
    (P49 k))
  (exact (p49all_prop49
    (Check decCert) (decOf decCert) encE (check_spec decCert) k)))

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
  (exact (p49all_prop49 chkf dec encTy hcs k)))

(thm prop49_concrete [k :- Nat]
  (Rt (Check decCert) (List.nil Exp) (List.nil U)
    (t49 k) (P49 k))
  (exact (p49all_prop49_concrete k)))
