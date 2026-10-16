(ns lcert.formal.arrfamily
  "The paper's witness family for §4.4, A_j := 1 ⊸ ⋯ ⊸ 1 ⊸ 1 with j arrows
  (R4 §4.4), and its certificates.

  prop434 and cor46 used ¬ʲ1 (negN) in its place.  ¬ʲ1 has no certificate
  for odd j: 1 ⊸ 0 would refute 1, against T1.  So their certificate
  hypotheses cannot be met at half the budgets, and the theorems are vacuous
  there.  A_j is certifiable at every j, by λ(x₁ :₁ 1). ⋯ λ(x_j :₁ 1). ⋆, as
  the paper says.  This namespace defines the family and proves:
  - arrN_closedF: A_j is closed at every binder depth;
  - arrN_size: F7's code of A_j has at least j nodes (E5's role for ¬ʲ1);
  - arrN_certifiable: A_j has a label-safe accepted certificate at Check,
    from an explicit derivation tree checked by dtCheck (check_complete_lbl)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.check-der :as cd]
            [lcert.formal.check-hd :as hd]
            [lcert.formal.certcanon]
            [lcert.formal.enclabels]
            [lcert.formal.thm46f7]
            [lcert.formal.sizegen :as sg]
            [lcert.formal.prop411poly]))

;; A_j and its canonical inhabitant.
(kdef arrN (=> Nat Exp)
  (fn [j :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] Exp) Exp.tUnit
      (fn [i :- Nat, A :- Exp] (Exp.tPi U.u1 Exp.tUnit A)) j)))
(kdef lamN (=> Nat Exp)
  (fn [j :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] Exp) Exp.star
      (fn [i :- Nat, t :- Exp] (Exp.lam U.u1 Exp.tUnit t)) j)))

(thm arrN_closedF [j :- Nat]
  (forall [d Nat] (Eq Bool ((closedF (arrN j)) d) Bool.true))
  (induction j)
  (intro d) (rfl)
  (intro d)
  (change (Eq Bool (Bool.and ((closedF Exp.tUnit) d) ((closedF (arrN n)) (Nat.succ d))) Bool.true))
  (rw [(ih_n (Nat.succ d))]))

(thm arrN_closed [j :- Nat] (Eq Bool (closedTy (arrN j)) Bool.true)
  (exact (arrN_closedF j 0)))

;; F7 spends one node on each ⊸ (label 25, as in E5), over the code of 1
;; (the leaf 16).
(let [code '(Code.sn 25 (Code.sl 16) (encE (arrN j)))]
  (a/prove-theorem 'arrN_size_step (lv '[j :- Nat])
    (lv (list 'Eq 'Nat '(cnodes (encE (arrN (Nat.succ j)))) (sg/nodes-of code)))
    (lv [(list 'have 'q (list 'Eq 'Code '(encE (arrN (Nat.succ j))) code) '(rfl))
         (list 'exact (list 'Eq.trans '(congrArg cnodes q) (sg/cnodes-eq code)))])))

(thm arrN_size [j :- Nat] (LE.le j (cnodes (encE (arrN j))))
  (induction j)
  (exact (Nat.zero_le _))
  (have h (Eq Nat (cnodes (encE (arrN (Nat.succ n)))) (+ 1 (+ 0 (cnodes (encE (arrN n))))))
    (arrN_size_step n))
  (omega))

;; --- Certificates of A_j ----------------------------------------------------
;; Contexts of d entries of 1 at usage 1, as the λs of λ(x :₁ 1). … build.
(kdef unitsD (=> Nat (List Exp))
  (fn [d :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (List Exp)) (List.nil Exp)
      (fn [_ :- Nat, r :- (List Exp)] (List.cons Exp Exp.tUnit r)) d)))
(kdef unitsU (=> Nat (List U))
  (fn [d :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (List U)) (List.nil U)
      (fn [_ :- Nat, r :- (List U)] (List.cons U U.u1 r)) d)))

;; The typing tree of Θ-free ⊢ lamN j :¹ arrN j, under d older binders:
;; j λ-rules (each with the formation of 1), then the constant ⋆.
(kdef lamTree (=> Nat Nat DT)
  (fn [j :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (=> Nat DT))
      (fn [d :- Nat] (DT.rConst (unitsD d) (unitsU d) Exp.star Exp.tUnit))
      (fn [i :- Nat, rec :- (=> Nat DT)]
        (fn [d :- Nat]
          (DT.rLam (unitsD d) (unitsU d) U.u1 Exp.tUnit (lamN i) (arrN i)
            (DT.fBase (unitsD d) Exp.tUnit)
            (rec (Nat.succ d)))))
      j)))

;; The formation tree of arrN j, under d binders of 1.
(kdef arrForm (=> Nat Nat DT)
  (fn [j :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (=> Nat DT))
      (fn [d :- Nat] (DT.fBase (unitsD d) Exp.tUnit))
      (fn [i :- Nat, rec :- (=> Nat DT)]
        (fn [d :- Nat]
          (DT.fPi (unitsD d) U.u1 Exp.tUnit (arrN i)
            (DT.fBase (unitsD d) Exp.tUnit)
            (rec (Nat.succ d)))))
      j)))

;; The checker's exact equation for Π formation, generated from the rule
;; table that defines dtCheck, as prop411poly does for the other rules.
(doseq [nm '[fPi]]
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

(thm units_len [d :- Nat] (Eq Nat (lenU (unitsU d)) (lenE (unitsD d)))
  (induction d)
  (rfl)
  (change (Eq Nat (Nat.succ (lenU (unitsU n))) (Nat.succ (lenE (unitsD n)))))
  (rw [ih_n]))

;; dtCheck accepts both trees, at every depth d of older binders.  Each case
;; is its own lemma: with two goals open, Ansatz's rw moves the goal it
;; rewrites behind the other one.
(thm lamTree_check_zero [d :- Nat]
  (Eq Bool (dtCheck (Check decCert) (lamTree 0 d)
             (DTJ.rt (unitsD d) (unitsU d) (lamN 0) (arrN 0)))
           Bool.true)
  (change (Eq Bool (dtCheck (Check decCert)
                     (DT.rConst (unitsD d) (unitsU d) Exp.star Exp.tUnit)
                     (DTJ.rt (unitsD d) (unitsU d) Exp.star Exp.tUnit))
                   Bool.true))
  (rw [(dtCheck_rConst_eq (Check decCert) (unitsD d) (unitsU d) Exp.star Exp.tUnit
         (DTJ.rt (unitsD d) (unitsU d) Exp.star Exp.tUnit))
       (sh4_dj_rt_refl (unitsD d) (unitsU d) Exp.star Exp.tUnit)
       (units_len d)
       (nbeq_refl (lenE (unitsD d)))]))

(thm lamTree_check_succ
  [n :- Nat, d :- Nat,
   ih :- (Eq Bool (dtCheck (Check decCert) (lamTree n (Nat.succ d))
                    (DTJ.rt (consE Exp.tUnit (unitsD d)) (consU U.u1 (unitsU d))
                      (lamN n) (arrN n)))
                  Bool.true)]
  (Eq Bool (dtCheck (Check decCert)
             (DT.rLam (unitsD d) (unitsU d) U.u1 Exp.tUnit (lamN n) (arrN n)
               (DT.fBase (unitsD d) Exp.tUnit) (lamTree n (Nat.succ d)))
             (DTJ.rt (unitsD d) (unitsU d) (Exp.lam U.u1 Exp.tUnit (lamN n))
               (Exp.tPi U.u1 Exp.tUnit (arrN n))))
           Bool.true)
  (rw [(dtCheck_rLam_eq (Check decCert) (unitsD d) (unitsU d) U.u1 Exp.tUnit (lamN n) (arrN n)
         (DT.fBase (unitsD d) Exp.tUnit) (lamTree n (Nat.succ d))
         (DTJ.rt (unitsD d) (unitsU d) (Exp.lam U.u1 Exp.tUnit (lamN n))
           (Exp.tPi U.u1 Exp.tUnit (arrN n))))
       (sh4_dj_rt_refl (unitsD d) (unitsU d) (Exp.lam U.u1 Exp.tUnit (lamN n))
         (Exp.tPi U.u1 Exp.tUnit (arrN n)))
       (dtCheck_fBase_eq (Check decCert) (unitsD d) Exp.tUnit
         (DTJ.tl Bool.true (unitsD d) Exp.tUnit Exp.tUnit))
       (sh4_dj_tl_refl (unitsD d) Exp.tUnit Exp.tUnit)
       ih]))

(thm lamTree_check [j :- Nat]
  (forall [d Nat]
    (Eq Bool (dtCheck (Check decCert) (lamTree j d)
               (DTJ.rt (unitsD d) (unitsU d) (lamN j) (arrN j)))
             Bool.true))
  (induction j)
  (intro d)
  (exact (lamTree_check_zero d))
  (intro d)
  (exact (lamTree_check_succ n d (ih_n (Nat.succ d)))))

(thm arrForm_check_zero [d :- Nat]
  (Eq Bool (dtCheck (Check decCert) (arrForm 0 d)
             (DTJ.tl Bool.true (unitsD d) (arrN 0) Exp.tUnit))
           Bool.true)
  (change (Eq Bool (dtCheck (Check decCert) (DT.fBase (unitsD d) Exp.tUnit)
                     (DTJ.tl Bool.true (unitsD d) Exp.tUnit Exp.tUnit))
                   Bool.true))
  (rw [(dtCheck_fBase_eq (Check decCert) (unitsD d) Exp.tUnit
         (DTJ.tl Bool.true (unitsD d) Exp.tUnit Exp.tUnit))
       (sh4_dj_tl_refl (unitsD d) Exp.tUnit Exp.tUnit)]))

(thm arrForm_check_succ
  [n :- Nat, d :- Nat,
   ih :- (Eq Bool (dtCheck (Check decCert) (arrForm n (Nat.succ d))
                    (DTJ.tl Bool.true (List.cons Exp Exp.tUnit (unitsD d)) (arrN n) Exp.tUnit))
                  Bool.true)]
  (Eq Bool (dtCheck (Check decCert)
             (DT.fPi (unitsD d) U.u1 Exp.tUnit (arrN n)
               (DT.fBase (unitsD d) Exp.tUnit) (arrForm n (Nat.succ d)))
             (DTJ.tl Bool.true (unitsD d) (Exp.tPi U.u1 Exp.tUnit (arrN n)) Exp.tUnit))
           Bool.true)
  (rw [(dtCheck_fPi_eq (Check decCert) (unitsD d) U.u1 Exp.tUnit (arrN n)
         (DT.fBase (unitsD d) Exp.tUnit) (arrForm n (Nat.succ d))
         (DTJ.tl Bool.true (unitsD d) (Exp.tPi U.u1 Exp.tUnit (arrN n)) Exp.tUnit))
       (sh4_dj_tl_refl (unitsD d) (Exp.tPi U.u1 Exp.tUnit (arrN n)) Exp.tUnit)
       (dtCheck_fBase_eq (Check decCert) (unitsD d) Exp.tUnit
         (DTJ.tl Bool.true (unitsD d) Exp.tUnit Exp.tUnit))
       (sh4_dj_tl_refl (unitsD d) Exp.tUnit Exp.tUnit)
       ih]))

(thm arrForm_check [j :- Nat]
  (forall [d Nat]
    (Eq Bool (dtCheck (Check decCert) (arrForm j d)
               (DTJ.tl Bool.true (unitsD d) (arrN j) Exp.tUnit))
             Bool.true))
  (induction j)
  (intro d)
  (exact (arrForm_check_zero d))
  (intro d)
  (exact (arrForm_check_succ n d (ih_n (Nat.succ d)))))

;; The label and data side conditions of check_complete_lbl: no term or type
;; here has a label constant, and every list holds only 1 or usage 1.
(thm unitsD_ok [d :- Nat] (Eq Bool (dataOkLE 100 (unitsD d)) Bool.true)
  (induction d)
  (rfl)
  (have e (Eq Bool (dataOkLE 100 (unitsD (Nat.succ n))) (dataOkLE 100 (unitsD n))) (rfl))
  (exact (Eq.trans e ih_n)))
(thm unitsU_ok [d :- Nat] (Eq Bool (dataOkLU 100 (unitsU d)) Bool.true)
  (induction d)
  (rfl)
  (have e (Eq Bool (dataOkLU 100 (unitsU (Nat.succ n))) (dataOkLU 100 (unitsU n))) (rfl))
  (exact (Eq.trans e ih_n)))
(thm lbls_lamN_succ [n :- Nat, ih :- (Eq Bool (lblsE 100 (lamN n)) Bool.true)]
  (Eq Bool (lblsE 100 (lamN (Nat.succ n))) Bool.true)
  (have e (Eq Bool (lblsE 100 (lamN (Nat.succ n))) (Bool.and (lblsE 100 (lamN n)) Bool.true)) (rfl))
  (rw [e ih]))
(thm lbls_lamN [j :- Nat] (Eq Bool (lblsE 100 (lamN j)) Bool.true)
  (induction j)
  (rfl)
  (exact (lbls_lamN_succ n ih_n)))
(thm lbls_arrN_succ [n :- Nat, ih :- (Eq Bool (lblsE 100 (arrN n)) Bool.true)]
  (Eq Bool (lblsE 100 (arrN (Nat.succ n))) Bool.true)
  (have e (Eq Bool (lblsE 100 (arrN (Nat.succ n))) (Bool.and (lblsE 100 (arrN n)) Bool.true)) (rfl))
  (rw [e ih]))
(thm lbls_arrN [j :- Nat] (Eq Bool (lblsE 100 (arrN j)) Bool.true)
  (induction j)
  (rfl)
  (exact (lbls_arrN_succ n ih_n)))
(thm fBase_ok [d :- Nat] (Eq Bool (dataOkDT 100 (DT.fBase (unitsD d) Exp.tUnit)) Bool.true)
  (change (Eq Bool (Bool.and (dataOkLE 100 (unitsD d)) (Bool.and (lblsE 100 Exp.tUnit) Bool.true)) Bool.true))
  (rw [(unitsD_ok d)]))

(thm dataOk_lamTree_zero [d :- Nat] (Eq Bool (dataOkDT 100 (lamTree 0 d)) Bool.true)
  (change (Eq Bool (Bool.and (dataOkLE 100 (unitsD d)) (Bool.and (dataOkLU 100 (unitsU d))
                     (Bool.and (lblsE 100 Exp.star) (Bool.and (lblsE 100 Exp.tUnit) Bool.true))))
                   Bool.true))
  (rw [(unitsD_ok d) (unitsU_ok d)]))
(thm dataOk_lamTree_succ [n :- Nat, d :- Nat,
                          ih :- (Eq Bool (dataOkDT 100 (lamTree n (Nat.succ d))) Bool.true)]
  (Eq Bool (dataOkDT 100 (lamTree (Nat.succ n) d)) Bool.true)
  (change (Eq Bool
    (Bool.and (dataOkLE 100 (unitsD d)) (Bool.and (dataOkLU 100 (unitsU d))
      (Bool.and (dataOkU 100 U.u1) (Bool.and (lblsE 100 Exp.tUnit)
        (Bool.and (lblsE 100 (lamN n)) (Bool.and (lblsE 100 (arrN n))
          (Bool.and (dataOkDT 100 (DT.fBase (unitsD d) Exp.tUnit))
            (Bool.and (dataOkDT 100 (lamTree n (Nat.succ d))) Bool.true))))))))
    Bool.true))
  (rw [(unitsD_ok d) (unitsU_ok d) (lbls_lamN n) (lbls_arrN n) (fBase_ok d) ih]))
(thm dataOk_lamTree [j :- Nat]
  (forall [d Nat] (Eq Bool (dataOkDT 100 (lamTree j d)) Bool.true))
  (induction j)
  (intro d)
  (exact (dataOk_lamTree_zero d))
  (intro d)
  (exact (dataOk_lamTree_succ n d (ih_n (Nat.succ d)))))

(thm dataOk_arrForm_succ [n :- Nat, d :- Nat,
                          ih :- (Eq Bool (dataOkDT 100 (arrForm n (Nat.succ d))) Bool.true)]
  (Eq Bool (dataOkDT 100 (arrForm (Nat.succ n) d)) Bool.true)
  (change (Eq Bool
    (Bool.and (dataOkLE 100 (unitsD d)) (Bool.and (dataOkU 100 U.u1)
      (Bool.and (lblsE 100 Exp.tUnit) (Bool.and (lblsE 100 (arrN n))
        (Bool.and (dataOkDT 100 (DT.fBase (unitsD d) Exp.tUnit))
          (Bool.and (dataOkDT 100 (arrForm n (Nat.succ d))) Bool.true))))))
    Bool.true))
  (rw [(unitsD_ok d) (lbls_arrN n) (fBase_ok d) ih]))
(thm dataOk_arrForm [j :- Nat]
  (forall [d Nat] (Eq Bool (dataOkDT 100 (arrForm j d)) Bool.true))
  (induction j)
  (intro d)
  (exact (fBase_ok d))
  (intro d)
  (exact (dataOk_arrForm_succ n d (ih_n (Nat.succ d)))))

;; A_j has a label-safe certificate at Check: F7's canonical certificate of
;; the explicit derivation of λ(x₁ :₁ 1). ⋯ λ(x_j :₁ 1). ⋆, at budget 0.
(thm arrN_certifiable [j :- Nat] (Certifiable (arrN j))
  (exact (check_complete_lbl 0 (lamN j) (arrN j) (lamTree j 0) (arrForm j 0)
           (lamTree_check j 0) (arrForm_check j 0) (arrN_closed j)
           (lbls_lamN j) (lbls_arrN j) (dataOk_lamTree j 0) (dataOk_arrForm j 0))))
