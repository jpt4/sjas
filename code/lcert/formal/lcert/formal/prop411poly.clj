(ns lcert.formal.prop411poly
  "A10: checked recursive certificates and their size for the sharing chain."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.check-der :as cd]
            [lcert.formal.check-hd :as hd]
            [lcert.formal.section4c]
            [lcert.formal.certcanon]
            [lcert.formal.prop411cert]))

;; Emit the checker's exact constructor equation from the same rule table
;; used to define dtCheck.  The equation is verified by reduction in Ansatz.
(doseq [nm '[fBase rVar rConst rLam rSleaf rSnode rApp]]
  (let [[family rule] (first (filter (fn [[_ r]] (= nm (first r))) dt/dt-rules))
        fields ((deref #'dt/dt-fields) rule)
        checks ((deref #'cd/checks-of) family rule
                 (fn [x j] (list 'dtCheck 'chkf x j)))
        tree (apply list (symbol (str "DT." nm)) (map first fields))]
    (a/prove-theorem (symbol (str "dtCheck_" nm "_eq"))
      (lv (hd/f7-params (concat [['chkf '(=> Code Code Bool)]] fields [['JJ 'DTJ]])))
      (lv (list 'Eq 'Bool (list 'dtCheck 'chkf tree 'JJ)
            (hd/f7-and (map first checks))))
      (lv '[(rfl)]))))

(thm sh4_listE_refl [D :- (List Exp)]
  (Eq Bool (listEqE D D) Bool.true)
  (induction D)
  (rfl)
  (change (Eq Bool (Bool.and (expEq head head) (listEqE tail tail)) Bool.true))
  (rw [(expEq_refl head) ih_tail]))

(thm sh4_uEq_refl [u :- U]
  (Eq Bool (f7UEq u u) Bool.true)
  (cases u) (rfl) (rfl) (rfl))
(thm sh4_listU_refl [us :- (List U)]
  (Eq Bool (listEqU us us) Bool.true)
  (induction us)
  (rfl)
  (change (Eq Bool (Bool.and (f7UEq head head) (listEqU tail tail)) Bool.true))
  (rw [(sh4_uEq_refl head) ih_tail]))

(thm sh4_dj_rt_refl [D :- (List Exp), us :- (List U), t :- Exp, A :- Exp]
  (Eq Bool (djEq (DTJ.rt D us t A) (DTJ.rt D us t A)) Bool.true)
  (change (Eq Bool (Bool.and (listEqE D D)
    (Bool.and (listEqU us us) (Bool.and (expEq t t)
      (Bool.and (expEq A A) Bool.true)))) Bool.true))
  (rw [(sh4_listE_refl D) (sh4_listU_refl us) (expEq_refl t) (expEq_refl A)]))

(thm sh4_dj_tl_refl [D :- (List Exp), t :- Exp, A :- Exp]
  (Eq Bool (djEq (DTJ.tl Bool.true D t A) (DTJ.tl Bool.true D t A)) Bool.true)
  (change (Eq Bool (Bool.and (f7BoolEq Bool.true Bool.true)
    (Bool.and (listEqE D D) (Bool.and (expEq t t)
      (Bool.and (expEq A A) Bool.true)))) Bool.true))
  (rw [(sh4_listE_refl D) (expEq_refl t) (expEq_refl A)]))

(thm sh4_app_usage [D :- (List Exp)]
  (Eq (List U)
    (vadd (vzero (lenE (List.cons Exp Exp.tSyn D)))
      (vscale U.uw (List.cons U U.uw (vzero (lenE D)))))
    (List.cons U U.uw (vzero (lenE D))))
  (exact (sh4_vadd_app (lenE D))))
(thm sh4_node_usage [D :- (List Exp)]
  (Eq (List U)
    (vadd (vzero (lenE (List.cons Exp Exp.tSyn D)))
      (vadd (List.cons U U.uw (vzero (lenE D)))
            (List.cons U U.uw (vzero (lenE D)))))
    (List.cons U U.uw (vzero (lenE D))))
  (exact (sh4_vadd_node (lenE D))))

(thm sh4_open_tree_zero_check [D :- (List Exp)]
  (Eq Bool
    (dtCheck (Check decCert) (sh4_open_tree 0 D)
      (DTJ.rt (List.cons Exp Exp.tSyn D)
        (List.cons U U.uw (vzero (lenE D))) (sh4_open 0) Exp.tSyn))
    Bool.true)
  (have hlen (Eq Nat (lenU (List.cons U U.uw (vzero (lenE D))))
                      (lenE (List.cons Exp Exp.tSyn D)))
    (sh4_len_head U.uw D))
  (unfold dtCheck)
  (rw [(sh4_dj_rt_refl (List.cons Exp Exp.tSyn D)
        (List.cons U U.uw (vzero (lenE D))) (Exp.var 0) Exp.tSyn)])
  (rw [hlen])
  (rw [(nbeq_refl (lenE (List.cons Exp Exp.tSyn D)))])
  (rw [(nthE.eq_2 Exp.tSyn D) (nthU.eq_2 U.uw (vzero (lenE D)))]))

;; The recursor's successor equation, generated from the same constructor
;; expression used by sh4_open_tree; rfl checks the copy against the kernel.
(thm sh4_open_tree_succ [n :- Nat, D :- (List Exp)]
  (Eq DT (sh4_open_tree (Nat.succ n) D)
(DT.rApp (List.cons Exp Exp.tSyn D)
        (vzero (lenE (List.cons Exp Exp.tSyn D)))
        (List.cons U U.uw (vzero (lenE D))) U.uw
        (Exp.lam U.uw Exp.tSyn (sh4_open n))
        (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
        Exp.tSyn Exp.tSyn
        (DT.rLam (List.cons Exp Exp.tSyn D)
          (vzero (lenE (List.cons Exp Exp.tSyn D))) U.uw Exp.tSyn
          (sh4_open n) Exp.tSyn
          (DT.fBase (List.cons Exp Exp.tSyn D) Exp.tSyn)
          (sh4_open_tree n (List.cons Exp Exp.tSyn D)))
        (DT.rSnode (List.cons Exp Exp.tSyn D)
          (vzero (lenE (List.cons Exp Exp.tSyn D)))
          (List.cons U U.uw (vzero (lenE D)))
          (List.cons U U.uw (vzero (lenE D)))
          (Exp.lbl 0) (Exp.var 0) (Exp.var 0)
          (DT.rConst (List.cons Exp Exp.tSyn D)
            (vzero (lenE (List.cons Exp Exp.tSyn D)))
            (Exp.lbl 0) Exp.tLbl)
          (DT.rVar (List.cons Exp Exp.tSyn D)
            (List.cons U U.uw (vzero (lenE D))) 0 Exp.tSyn U.uw)
          (DT.rVar (List.cons Exp Exp.tSyn D)
            (List.cons U U.uw (vzero (lenE D))) 0 Exp.tSyn U.uw))
        (DT.fBase (List.cons Exp Exp.tSyn D) Exp.tSyn)
        (DT.fBase (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn D))
          Exp.tSyn)))
  (rfl))

(thm sh4_open_tree_step_check
  [n :- Nat, D :- (List Exp),
   ih :- (Eq Bool
     (dtCheck (Check decCert)
       (sh4_open_tree n (List.cons Exp Exp.tSyn D))
       (DTJ.rt (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn D))
         (List.cons U U.uw
           (vzero (lenE (List.cons Exp Exp.tSyn D))))
         (sh4_open n) Exp.tSyn)) Bool.true)]
  (Eq Bool
    (dtCheck (Check decCert) (sh4_open_tree (Nat.succ n) D)
      (DTJ.rt (List.cons Exp Exp.tSyn D)
        (List.cons U U.uw (vzero (lenE D)))
        (sh4_open (Nat.succ n)) Exp.tSyn)) Bool.true)
  (rw [(sh4_open_tree_succ n D)])
  (simp [dtCheck_rApp_eq dtCheck_rLam_eq dtCheck_rSnode_eq
         dtCheck_rVar_eq dtCheck_rConst_eq dtCheck_fBase_eq
         ih sh4_dj_rt_refl sh4_dj_tl_refl
         sh4_app_usage sh4_node_usage sh4_listE_refl sh4_listU_refl
         sh4_len_head nbeq_refl expEq_refl])
  (constructor)
  (exact ih)
  (constructor)
  (exact (sh4_lenU_vzero (lenE (List.cons Exp Exp.tSyn D))))
  (constructor)
  (rw [(nthE.eq_2 Exp.tSyn D)])
  (rw [(nthU.eq_2 U.uw (vzero (lenE D)))])
  (rfl))

(thm sh4_open_tree_check [n :- Nat]
  (forall [D (List Exp)]
    (Eq Bool (dtCheck (Check decCert) (sh4_open_tree n D)
      (DTJ.rt (List.cons Exp Exp.tSyn D)
        (List.cons U U.uw (vzero (lenE D))) (sh4_open n) Exp.tSyn))
      Bool.true))
  (induction n)
  (intro D)
  (exact (sh4_open_tree_zero_check D))
  (intro D)
  (exact (sh4_open_tree_step_check n D
    (ih_n (List.cons Exp Exp.tSyn D)))))

(thm sh4_cn_tree_check [n :- Nat]
  (Eq Bool (dtCheck (Check decCert) (sh4_cn_tree n)
    (DTJ.rt (List.nil Exp) (List.nil U) (sh4_cn n) Exp.tSyn))
    Bool.true)
  (unfold sh4_cn_tree)
  (simp [dtCheck_rApp_eq dtCheck_rLam_eq dtCheck_rSleaf_eq
         dtCheck_rConst_eq dtCheck_fBase_eq sh4_open_tree_check
         sh4_dj_rt_refl sh4_dj_tl_refl sh4_listE_refl sh4_listU_refl
         expEq_refl])
  (exact (sh4_open_tree_check n (List.nil Exp))))

(thm prop411_family_cert [n :- Nat]
  (Exists (fn [c :- Code]
    (Eq Bool (Check decCert c (encE Exp.tSyn)) Bool.true)))
  (exact (check_complete 0 (sh4_cn n) Exp.tSyn
    (sh4_cn_tree n) sh4_c0_form (sh4_cn_tree_check n) rfl rfl)))

;; Name the witness so its encoded size can be studied directly.  The
;; certificate is built by F7's canonical encoder from the checked trees.
(lcert.formal.base/kdef! 'sh4_cn_cert '(=> Nat Code)
  '(fn [n :- Nat]
     (encCert 0 (sh4_cn n) Exp.tSyn (sh4_cn_tree n) sh4_c0_form)))

(thm prop411_explicit_cert [n :- Nat]
  (Eq Bool (Check decCert (sh4_cn_cert n) (encE Exp.tSyn)) Bool.true)
  (exact (check_cert_ok 0 (sh4_cn n) Exp.tSyn
    (sh4_cn_tree n) sh4_c0_form (sh4_cn_tree_check n) rfl rfl)))
