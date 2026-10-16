(ns lcert.formal.prop411poly
  "A10: checked recursive certificates and their size for the sharing chain."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
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

;; Generate the exact size equation of each derivation-tree constructor used
;; by the sharing chain.  The generator reads F7's constructor and encoder
;; tables; its equations and proof terms are checked by the kernel.
(let [wanted '#{rVar rApp rLam rSnode rSleaf rConst fBase}
      ctors (deref #'lcert.formal.certenc/dt-ctors)
      info @(deref #'lcert.formal.certenc/ty-info)
      finfo @(deref #'lcert.formal.certenc/fuel-info)
      chain (deref #'lcert.formal.certenc/chain-enc)
      nodes (deref #'lcert.formal.certcanon/nodes-of)
      proof (deref #'lcert.formal.certcanon/cnodes-eq)]
  (doseq [[i [nm fields]] (map-indexed vector ctors) :when (wanted nm)]
    (let [ctor (apply list (symbol (str "DT." nm)) (map first fields))
          code (chain i (for [[x T] fields]
                          (list (if (= T 'DT) 'encDT
                                  (first (or (info T) (finfo T)))) x)))
          params (hd/f7-params fields)]
      (a/prove-theorem (symbol (str "sh4_encDT_" nm "_nodes"))
        (lv params)
        (lv (list 'Eq 'Nat (list 'cnodes (list 'encDT ctor)) (nodes code)))
        (lv [(list 'have 'q_e (list 'Eq 'Code (list 'encDT ctor) code) '(rfl))
             (list 'exact (list 'Eq.trans '(congrArg cnodes q_e) (proof code)))])))))

;; E4's expression encoding has seven additional internal nodes for each
;; sharing let.  The arithmetic form below uses repeated addition so omega
;; can check the induction without nonlinear arithmetic.
(let [nodes (deref #'lcert.formal.certcanon/nodes-of)
      proof (deref #'lcert.formal.certcanon/cnodes-eq)
      var0 '(Code.sn 14 (encNat 0) (Code.sl 0))
      lbl0 '(Code.sn 39 (Code.sl 0) (Code.sl 0))
      doubled (list 'Code.sn 42 lbl0 (list 'Code.sn 12 var0 var0))
      open-next (list 'Code.sn 51
                      '(Code.sn 50 (Code.sl 20) (encE (sh4_open n))) doubled)
      cn-code (list 'Code.sn 51
                    '(Code.sn 50 (Code.sl 20) (encE (sh4_open n)))
                    (list 'Code.sn 41 lbl0 '(Code.sl 0)))]
  (a/prove-theorem 'sh4_open_enc_step (lv '[n :- Nat])
    (lv '(Eq Nat (cnodes (encE (sh4_open (Nat.succ n))))
                 (+ 7 (cnodes (encE (sh4_open n))))))
    (lv [(list 'have 'q_e (list 'Eq 'Code '(encE (sh4_open (Nat.succ n))) open-next) '(rfl))
         (list 'have 'q_n (list 'Eq 'Nat '(cnodes (encE (sh4_open (Nat.succ n)))) (nodes open-next))
               (list 'Eq.trans '(congrArg cnodes q_e) (proof open-next)))
         '(omega)]))
  (a/prove-theorem 'sh4_cn_enc_parts (lv '[n :- Nat])
    (lv '(Eq Nat (cnodes (encE (sh4_cn n)))
                 (+ 4 (cnodes (encE (sh4_open n))))))
    (lv [(list 'have 'q_e (list 'Eq 'Code '(encE (sh4_cn n)) cn-code) '(rfl))
         (list 'have 'q_n (list 'Eq 'Nat '(cnodes (encE (sh4_cn n))) (nodes cn-code))
               (list 'Eq.trans '(congrArg cnodes q_e) (proof cn-code)))
         '(omega)])))

(thm sh4_open_enc_linear [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_open n)))
          (+ 1 (+ n (+ n (+ n (+ n (+ n (+ n n))))))))
  (induction n)
  (rfl)
  (rw [(sh4_open_enc_step n)])
  (omega))

(thm sh4_cn_enc_linear [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_cn n)))
          (+ 5 (+ n (+ n (+ n (+ n (+ n (+ n n))))))))
  (have h (Eq Nat (cnodes (encE (sh4_open n)))
                  (+ 1 (+ n (+ n (+ n (+ n (+ n (+ n n))))))))
    (sh4_open_enc_linear n))
  (have hc (Eq Nat (cnodes (encE (sh4_cn n)))
                   (+ 4 (cnodes (encE (sh4_open n)))))
    (sh4_cn_enc_parts n))
  (omega))

;; Every older let binder has type Syn; the certificate records these
;; contexts and the matching zero-usage tails explicitly.
(lcert.formal.base/kdef! 'sh4_syns '(=> Nat (List Exp))
  '(fn [d :- Nat]
     (Nat.rec$1 (fn [_ :- Nat] (List Exp)) (List.nil Exp)
       (fn [_ :- Nat, rec :- (List Exp)] (List.cons Exp Exp.tSyn rec)) d)))

(thm sh4_encLE_syns [d :- Nat]
  (Eq Nat (cnodes (encLE (sh4_syns d))) d)
  (induction d)
  (rfl)
  (have e (Eq Nat (cnodes (encLE (sh4_syns (Nat.succ n))))
                  (+ 1 (+ 0 (cnodes (encLE (sh4_syns n)))))) (rfl))
  (omega))

(thm sh4_encLU_vzero [d :- Nat]
  (Eq Nat (cnodes (encLU (vzero d))) d)
  (induction d)
  (rfl)
  (have e (Eq Nat (cnodes (encLU (vzero (Nat.succ n))))
                  (+ 1 (+ 0 (cnodes (encLU (vzero n)))))) (rfl))
  (omega))

(thm sh4_encLU_head [d :- Nat]
  (Eq Nat (cnodes (encLU (List.cons U U.uw (vzero d))))
          (Nat.succ d))
  (have e (Eq Nat (cnodes (encLU (List.cons U U.uw (vzero d))))
                  (+ 1 (+ 0 (cnodes (encLU (vzero d)))))) (rfl))
  (have h (Eq Nat (cnodes (encLU (vzero d))) d) (sh4_encLU_vzero d))
  (omega))

(thm sh4_sq_succ [n :- Nat]
  (Eq Nat (Nat.mul (Nat.succ n) (Nat.succ n))
          (+ (Nat.mul n n) (+ n (+ n 1))))
  (rw [(Nat.mul_succ (Nat.succ n) n)])
  (rw [(Nat.succ_mul n n)])
  (omega))

(thm sh4_syns_succ [d :- Nat]
  (Eq (List Exp) (sh4_syns (Nat.succ d))
                 (List.cons Exp Exp.tSyn (sh4_syns d)))
  (rfl))

(thm sh4_syns_len [d :- Nat]
  (Eq Nat (lenE (sh4_syns d)) d)
  (induction d)
  (rfl)
  (have e (Eq Nat (lenE (sh4_syns (Nat.succ n)))
                  (Nat.succ (lenE (sh4_syns n)))) (rfl))
  (omega))

(thm sh4_encLE_head [d :- Nat]
  (Eq Nat (cnodes (encLE (List.cons Exp Exp.tSyn (sh4_syns d))))
          (Nat.succ d))
  (have e (Eq Nat (cnodes (encLE (List.cons Exp Exp.tSyn (sh4_syns d))))
                  (+ 1 (+ 0 (cnodes (encLE (sh4_syns d)))))) (rfl))
  (have h (Eq Nat (cnodes (encLE (sh4_syns d))) d)
    (sh4_encLE_syns d))
  (omega))

(thm sh4_open_tree_zero [D :- (List Exp)]
  (Eq DT (sh4_open_tree 0 D)
    (DT.rVar (List.cons Exp Exp.tSyn D)
      (List.cons U U.uw (vzero (lenE D))) 0 Exp.tSyn U.uw))
  (rfl))

(thm sh4_open_tree_nodes_zero [d :- Nat]
  (Eq Nat (cnodes (encDT (sh4_open_tree 0 (sh4_syns d))))
          (+ (Nat.mul d 2) 7))
  (rw [(sh4_open_tree_zero (sh4_syns d))])
  (simp [sh4_encDT_rVar_nodes sh4_encLE_head sh4_encLU_head
         sh4_syns_len cnodes_encNat])
  (omega))

;; The normal form's certificate.  The literal codeTerm c has one
;; conversion-free derivation: Snode at each internal node, Sleaf at each
;; leaf, Const for each label.  codeTree c is that derivation's tree, and its
;; F7 certificate is the one R4's table counts.
(lcert.formal.base/kdef! 'codeTree '(=> Code DT)
  '(fn [c :- Code]
     (Code.rec$1 (fn [_ :- Code] DT)
       (fn [l :- Nat]
         (DT.rSleaf (List.nil Exp) (List.nil U) (Exp.lbl l)
           (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl l) Exp.tLbl)))
       (fn [l :- Nat, a :- Code, b :- Code, ia :- DT, ib :- DT]
         (DT.rSnode (List.nil Exp) (List.nil U) (List.nil U) (List.nil U)
           (Exp.lbl l) (codeTerm a) (codeTerm b)
           (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl l) Exp.tLbl)
           ia ib))
       c)))

