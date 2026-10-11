(ns lcert.formal.p49nodemotive
  "The dependent label motive and branch type for the node continuation."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49nodebranch]
            [lcert.formal.p49branches]
            [lcert.formal.p49peeldepth]))

;; In the pair continuation the two child codes are indices 6 and 5,
;; while the original label is index 7. The case motive binds a fresh
;; label at index 0 and shifts those child codes to 7 and 6.
(kdef p49nm_M (=> (=> Exp Exp) Exp)
  (fn [F :- (=> Exp Exp)]
    (F (Exp.snode (Exp.var 0) (Exp.var 7) (Exp.var 6)))))

(kdef p49nm_D (=> Nat (=> Exp Exp) (List Exp) (List Exp))
  (fn [j :- Nat, F :- (=> Exp Exp), D :- (List Exp)]
    (consE (p49pd_X j)
      (consE (p49pd_X j)
        (p49pd_De j (F (Exp.var 0)) D)))))

(kdef p49nm_F (=> P49C (List (Prod Nat Nat)) Nat (=> Exp Exp))
  (fn [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat]
    (fn [c :- Exp]
      (p49tba_F (fn [q :- Nat, x :- Exp] (p49dl q x))
        K slots j p49tl_rho0 c))))

(kdef p49nm_U (=> (List U) (List U))
  (fn [uv :- (List U)]
    (consU U.u1 (consU U.u1 (p49peel_Uk uv)))))

(kdef p49nm_Ubr (=> (List U) (List U))
  (fn [uv :- (List U)] (p49peel_z (p49nm_U uv))))

(thm p49nm_len
  [j :- Nat, F :- (=> Exp Exp), D :- (List Exp),
   uv :- (List U), hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Eq Nat (lenU (p49nm_U uv)) (lenE (p49nm_D j F D)))
  (exact (congrArg Nat.succ
    (congrArg Nat.succ
      (congrArg Nat.succ
        (congrArg Nat.succ
          (congrArg Nat.succ
            (congrArg Nat.succ
              (congrArg Nat.succ
                (congrArg Nat.succ
                  (congrArg Nat.succ
                    (Eq.trans (p49peel_zlen uv) hlen))))))))))))

;; nthE/nthU are inductive list lookups. An explicit chain also fixes the
;; Option universe that an unannotated `rfl` leaves ambiguous in rVar.
(defn p49nm-lookup [kind prefix tail i]
  (let [ctor (if (= kind 'nthE) 'consE 'consU)
        eq2 (symbol (str kind ".eq_2"))
        eq3 (symbol (str kind ".eq_3"))
        [head & rest] prefix
        suffix (reduce (fn [acc x] (list ctor x acc)) tail (reverse rest))]
    (if (zero? i)
      (list eq2 head suffix)
      (list 'Eq.trans (list eq3 head suffix (dec i))
        (p49nm-lookup kind (vec rest) tail (dec i))))))

(def ^:private p49nm-M
  '(p49peel_motive (p49pd_A j) (F (Exp.var 0))))
(def ^:private p49nm-Eprefix
  ['(p49pd_X j) '(p49pd_X j)
   '(p49peel_nodeA (p49pd_A j))
   (list 'y2Ty p49nm-M) (list 'y1Ty p49nm-M)
   'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn])
(def ^:private p49nm-Uprefix
  '[U.u1 U.u1 U.u0 U.u1 U.u1 U.uw U.uw U.uw U.u0])

(a/prove-theorem 'p49nm_label_at
  '[chkf :- (=> Code Code Bool), j :- Nat,
    F :- (=> Exp Exp), D :- (List Exp), us :- (List U),
    hlen :- (Eq Nat (lenU us) (lenE (p49nm_D j F D))),
    hu :- (Eq (Option U) (nthU us 7) (Option.some U U.uw))]
  '(Rt chkf (p49nm_D j F D) us (Exp.var 7) Exp.tLbl)
  [(list 'exact
     (list 'Rt.rVar 'chkf '(p49nm_D j F D) 'us
       7 'Exp.tLbl 'U.uw 'hlen
       (p49nm-lookup 'nthE p49nm-Eprefix 'D 7)
       'hu 'rfl))])

(a/prove-theorem 'p49nm_label
  '[chkf :- (=> Code Code Bool), j :- Nat,
    F :- (=> Exp Exp), D :- (List Exp), uv :- (List U),
    hlen :- (Eq Nat (lenU uv) (lenE D))]
  '(Rt chkf (p49nm_D j F D) (p49nm_U uv)
     (Exp.var 7) Exp.tLbl)
  [(list 'exact
     (list 'p49nm_label_at 'chkf 'j 'F 'D '(p49nm_U uv)
       '(p49nm_len j F D uv hlen)
       (p49nm-lookup 'nthU p49nm-Uprefix '(p49peel_z uv) 7)))])

(let [ctx '(consE Exp.tLbl (p49nm_D j F D))
      prefix (into ['Exp.tLbl] p49nm-Eprefix)
      v (fn [i A]
          (list 'Tl.zVar 'chkf ctx i A
            (p49nm-lookup 'nthE prefix 'D i)))]
  (a/prove-theorem 'p49nm_motive_formed
    '[chkf :- (=> Code Code Bool), j :- Nat,
      D :- (List Exp), F :- (=> Exp Exp),
      hF :- (p49pd_Form chkf F)]
    '(Tl chkf Bool.true
       (consE Exp.tLbl (p49nm_D j F D))
       (p49nm_M F) Exp.tUnit)
    [(list 'exact
       (list 'hF ctx
         '(Exp.snode (Exp.var 0) (Exp.var 7) (Exp.var 6))
         (list 'Tl.zSnode 'chkf ctx
           '(Exp.var 0) '(Exp.var 7) '(Exp.var 6)
           (v 0 'Exp.tLbl)
           (v 7 'Exp.tSyn)
           (v 6 'Exp.tSyn))))]))

(thm p49nm_subst
  [F :- (=> Exp Exp), hS :- (p49tba_Subst F), v :- Exp]
  (Eq Exp
    (subst1 v (p49nm_M F))
    (F (Exp.snode v (Exp.var 6) (Exp.var 5))))
  (exact (hS (fn [i :- Nat] (inst1 v i))
    (Exp.snode (Exp.var 0) (Exp.var 7) (Exp.var 6)))))

(thm p49nm_case_result
  [F :- (=> Exp Exp), hS :- (p49tba_Subst F)]
  (Eq Exp
    (subst1 (Exp.var 7) (p49nm_M F))
    (F (Exp.snode (Exp.var 7) (Exp.var 6) (Exp.var 5))))
  (exact (p49nm_subst F hS (Exp.var 7))))

(thm p49nm_peel_result
  [F :- (=> Exp Exp),
   hL :- (p49tba_Lift F), hS :- (p49tba_Subst F)]
  (Eq Exp
    (subst1 (Exp.var 7) (p49nm_M F))
    (lift 2 0 (p49peel_nodeB (F (Exp.var 0)))))
  (exact (Eq.trans
    (p49nm_case_result F hS)
    (Eq.symm (Eq.trans
      (congrArg (fn [e :- Exp] (lift 2 0 e))
        (p49pd_NB_eq F hL hS))
      (hL 2 0
        (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))))))

(thm p49nm_branch_tail
  [K :- P49C, slots :- (List (Prod Nat Nat)),
   j :- Nat, l :- Nat,
   hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (subst1 (Exp.lbl l)
      (p49nm_M
        (p49tba_F
          (fn [q :- Nat, c :- Exp] (p49dl q c))
          K slots j p49tl_rho0)))
    (p49tele
      (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
        (Exp.var 6) (Exp.var 5))))
  (exact (Eq.trans
    (p49nm_subst
      (p49tba_F
        (fn [q :- Nat, c :- Exp] (p49dl q c))
        K slots j p49tl_rho0)
      (p49tl_p49dl_Subst K slots j)
      (Exp.lbl l))
    (p49nb_branch_concrete K slots j l (Exp.var 6) (Exp.var 5) hj))))

;; The node continuation is a genuine Rt term, conditional only on the
;; smaller-potential branch witnesses supplied by the fuel induction.
(thm p49nm_node_cont
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hj :- (p49tr_avoids slots j),
   hF :- (p49pd_Form chkf (p49nm_F K slots j)),
   f :- (=> Nat Exp),
   hf :- (forall [l Nat]
     (=> (Eq Bool (Nat.blt l 100) Bool.true)
       (Rt chkf
         (p49nm_D j (p49nm_F K slots j) D)
         (p49nm_Ubr uv) (f l)
         (p49tele
           (fn [q :- Nat, c :- Exp] (p49dl q c))
           ((p49crepl K) j
             (P49C.node l
               (P49C.hole (p49fresh K slots))
               (P49C.hole (Nat.succ (p49fresh K slots)))))
           slots
           (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
             (Exp.var 6) (Exp.var 5))))))]
  (Rt chkf
    (p49nm_D j (p49nm_F K slots j) D)
    (p49nm_U uv)
    (Exp.caseL
      (p49nm_M (p49nm_F K slots j))
      (Exp.var 7) (p49brs 100 f 0))
    (lift 2 0
      (p49peel_nodeB ((p49nm_F K slots j) (Exp.var 0)))))
  (have hP
    (Tl chkf Bool.true
      (consE Exp.tLbl
        (p49nm_D j (p49nm_F K slots j) D))
      (p49nm_M (p49nm_F K slots j)) Exp.tUnit)
    (p49nm_motive_formed chkf j D
      (p49nm_F K slots j) hF))
  (have hbr
    (forall [l Nat]
      (=> (Eq Bool (Nat.blt l 100) Bool.true)
        (Rt chkf
          (p49nm_D j (p49nm_F K slots j) D)
          (p49nm_Ubr uv) (f l)
          (subst1 (Exp.lbl l)
            (p49nm_M (p49nm_F K slots j))))))
    (fn [l :- Nat]
      (fn [hlt :- (Eq Bool (Nat.blt l 100) Bool.true)]
        (Eq.mp
          (congrArg
            (fn [A :- Exp]
              (Rt chkf
                (p49nm_D j (p49nm_F K slots j) D)
                (p49nm_Ubr uv) (f l) A))
            (Eq.symm (p49nm_branch_tail K slots j l hj)))
          (hf l hlt)))))
  (exact (p49peel_cast chkf
    (p49nm_D j (p49nm_F K slots j) D)
    (vadd (p49nm_U uv) (p49nm_Ubr uv))
    (p49nm_U uv)
    (Exp.caseL
      (p49nm_M (p49nm_F K slots j))
      (Exp.var 7) (p49brs 100 f 0))
    (subst1 (Exp.var 7)
      (p49nm_M (p49nm_F K slots j)))
    (lift 2 0
      (p49peel_nodeB ((p49nm_F K slots j) (Exp.var 0))))
    (p49case_typed chkf
      (p49nm_D j (p49nm_F K slots j) D)
      (p49nm_U uv) (p49nm_Ubr uv)
      (p49nm_M (p49nm_F K slots j))
      (Exp.var 7) f
      (Eq.trans (p49peel_zlen (p49nm_U uv))
        (p49nm_len j (p49nm_F K slots j) D uv hlen))
      hP
      (p49nm_label chkf j (p49nm_F K slots j) D uv hlen)
      hbr)
    (p49peel_addz (p49nm_U uv))
    (p49nm_peel_result
      (p49nm_F K slots j)
      (p49tl_p49dl_Lift K slots j)
      (p49tl_p49dl_Subst K slots j)))))
