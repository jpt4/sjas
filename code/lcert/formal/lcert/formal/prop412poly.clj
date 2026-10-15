(ns lcert.formal.prop412poly
  "A06: F7 size facts for the unary numeral in Proposition 4.12(1)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm lv]]
            [lcert.formal.dtgen :as g]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.check-der :as cd]
            [lcert.formal.check-hd :as hd]
            [lcert.formal.section4c]
            [lcert.formal.prop411poly]
            [lcert.formal.certcanon]))

;; Generate checker equations from the same constructor table that defines
;; dtCheck.  The kernel verifies each generated equation by reduction.
(doseq [nm '[rSucc rRecN]]
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

;; F7 stores each successor under one Code.sn and zero under a Code.sl.
;; These equations concern the term field of a certificate; the derivation
;; tree and its remaining fields still need their own size proof.
(thm sh4_num_enc_step [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_num (Nat.succ n))))
          (+ 1 (cnodes (encE (sh4_num n)))))
  (rfl))

(thm sh4_num_enc_linear [n :- Nat]
  (Eq Nat (cnodes (encE (sh4_num n))) n)
  (induction n)
  (rfl)
  (rw [(sh4_num_enc_step n)])
  (omega))

;; The generator supplies the fixed recursor body from the paper's term.
;; The only variable part is the numeral and its recursive derivation tree.
;; Both generated trees are independently checked below by dtCheck.
(b/kdef! 'sh4_num_tree '(=> Nat DT)
  '(fn [n :- Nat]
     (Nat.rec$1 (fn [_ :- Nat] DT)
       (DT.rConst (List.nil Exp) (List.nil U) Exp.zero Exp.tNat)
       (fn [k :- Nat, rec :- DT]
         (DT.rSucc (List.nil Exp) (List.nil U) (sh4_num k) rec)) n)))

(thm sh4_num_tree_succ [n :- Nat]
  (Eq DT (sh4_num_tree (Nat.succ n))
    (DT.rSucc (List.nil Exp) (List.nil U) (sh4_num n) (sh4_num_tree n)))
  (rfl))

(thm sh4_num_tree_check [n :- Nat]
  (Eq Bool (dtCheck (Check decCert) (sh4_num_tree n)
             (DTJ.rt (List.nil Exp) (List.nil U) (sh4_num n) Exp.tNat))
           Bool.true)
  (induction n)
  (rfl)
  (rw [(sh4_num_tree_succ n)])
  (simp [dtCheck_rSucc_eq ih_n])
  (constructor)
  (exact (sh4_dj_rt_refl (List.nil Exp) (List.nil U)
           (Exp.succ (sh4_num n)) Exp.tNat))
  (exact ih_n))

(def ^:private motive [:tSig :uw [:tSyn] [:tUnit]])
(def ^:private doubler-body
  [:pair motive [:snode [:lbl 0] [:var 1] [:var 1]] [:star]])
(def ^:private doubler-step [:letp motive [:var 0] doubler-body])
(def ^:private doubler-base [:pair motive [:sleaf [:lbl 0]] [:star]])
(def ^:private zero-doubler
  [:recN motive doubler-base doubler-step [:zero]])

;; The tree for zero-doubler, generated once by dtgen/rt and stored literally
;; (Claude, 2026-10-15).  dtgen's rt has no synthesis rules for sleaf, snode
;; or label constants, so the earlier (let [tree (g/rt …)] …) failed on a
;; fresh load: it had only worked in a server whose dtgen was extended in the
;; REPL.  This copy was generated with a REPL-local extension of rt-syn for
;; exactly those three rules (rSleaf; rSnode, its usages split by alloc;
;; rConst at Lbl).  Stored, the namespace loads from scratch without
;; reloading dtgen's dependents, as prop411cert does with its tree, and
;; sh4_doubler_tree_check below checks it with dtCheck in the kernel.
(def doubler-tree-form
  '
  (DT.rRecN
   (List.nil Exp)
   (List.nil U)
   (List.nil U)
   (List.nil U)
   (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
   (Exp.pair
    (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
    (Exp.sleaf (Exp.lbl 0))
    Exp.star)
   (Exp.letp
    (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
    (Exp.var 0)
    (Exp.pair
     (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
     (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1))
     Exp.star))
   Exp.zero
   (DT.rConst (List.nil Exp) (List.nil U) Exp.zero Exp.tNat)
   (DT.fSig
    (List.cons Exp Exp.tNat (List.nil Exp))
    U.uw
    Exp.tSyn
    Exp.tUnit
    (DT.fBase (List.cons Exp Exp.tNat (List.nil Exp)) Exp.tSyn)
    (DT.fBase
     (List.cons Exp Exp.tSyn (List.cons Exp Exp.tNat (List.nil Exp)))
     Exp.tUnit))
   (DT.rPair
    (List.nil Exp)
    (List.nil U)
    (List.nil U)
    U.uw
    Exp.tSyn
    Exp.tUnit
    (Exp.sleaf (Exp.lbl 0))
    Exp.star
    (DT.fBase (List.nil Exp) Exp.tSyn)
    (DT.fBase (List.cons Exp Exp.tSyn (List.nil Exp)) Exp.tUnit)
    (DT.rSleaf
     (List.nil Exp)
     (List.nil U)
     (Exp.lbl 0)
     (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl 0) Exp.tLbl))
    (DT.rConst (List.nil Exp) (List.nil U) Exp.star Exp.tUnit))
   (DT.rLet
    (List.cons
     Exp
     (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
     (List.cons Exp Exp.tNat (List.nil Exp)))
    (List.cons U U.u1 (List.cons U U.uw (List.nil U)))
    (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))
    U.uw
    Exp.tSyn
    Exp.tUnit
    (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
    (Exp.var 0)
    (Exp.pair
     (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
     (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1))
     Exp.star)
    (DT.rVar
     (List.cons
      Exp
      (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
      (List.cons Exp Exp.tNat (List.nil Exp)))
     (List.cons U U.u1 (List.cons U U.uw (List.nil U)))
     0
     (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
     U.u1)
    (DT.fSig
     (List.cons
      Exp
      (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
      (List.cons Exp Exp.tNat (List.nil Exp)))
     U.uw
     Exp.tSyn
     Exp.tUnit
     (DT.fBase
      (List.cons
       Exp
       (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
       (List.cons Exp Exp.tNat (List.nil Exp)))
      Exp.tSyn)
     (DT.fBase
      (List.cons
       Exp
       Exp.tSyn
       (List.cons
        Exp
        (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
        (List.cons Exp Exp.tNat (List.nil Exp))))
      Exp.tUnit))
    (DT.fBase
     (List.cons
      Exp
      (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
      (List.cons Exp Exp.tNat (List.nil Exp)))
     Exp.tSyn)
    (DT.fBase
     (List.cons
      Exp
      Exp.tSyn
      (List.cons
       Exp
       (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
       (List.cons Exp Exp.tNat (List.nil Exp))))
     Exp.tUnit)
    (DT.rPair
     (List.cons
      Exp
      Exp.tUnit
      (List.cons
       Exp
       Exp.tSyn
       (List.cons
        Exp
        (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
        (List.cons Exp Exp.tNat (List.nil Exp)))))
     (List.cons
      U
      U.u0
      (List.cons
       U
       U.uw
       (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
     (List.cons
      U
      U.u1
      (List.cons
       U
       U.u0
       (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
     U.uw
     Exp.tSyn
     Exp.tUnit
     (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1))
     Exp.star
     (DT.fBase
      (List.cons
       Exp
       Exp.tUnit
       (List.cons
        Exp
        Exp.tSyn
        (List.cons
         Exp
         (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
         (List.cons Exp Exp.tNat (List.nil Exp)))))
      Exp.tSyn)
     (DT.fBase
      (List.cons
       Exp
       Exp.tSyn
       (List.cons
        Exp
        Exp.tUnit
        (List.cons
         Exp
         Exp.tSyn
         (List.cons
          Exp
          (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
          (List.cons Exp Exp.tNat (List.nil Exp))))))
      Exp.tUnit)
     (DT.rSnode
      (List.cons
       Exp
       Exp.tUnit
       (List.cons
        Exp
        Exp.tSyn
        (List.cons
         Exp
         (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
         (List.cons Exp Exp.tNat (List.nil Exp)))))
      (List.cons
       U
       U.u0
       (List.cons
        U
        U.u0
        (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
      (List.cons
       U
       U.u0
       (List.cons
        U
        U.uw
        (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
      (List.cons
       U
       U.u0
       (List.cons
        U
        U.uw
        (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
      (Exp.lbl 0)
      (Exp.var 1)
      (Exp.var 1)
      (DT.rConst
       (List.cons
        Exp
        Exp.tUnit
        (List.cons
         Exp
         Exp.tSyn
         (List.cons
          Exp
          (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
          (List.cons Exp Exp.tNat (List.nil Exp)))))
       (List.cons
        U
        U.u0
        (List.cons
         U
         U.u0
         (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
       (Exp.lbl 0)
       Exp.tLbl)
      (DT.rVar
       (List.cons
        Exp
        Exp.tUnit
        (List.cons
         Exp
         Exp.tSyn
         (List.cons
          Exp
          (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
          (List.cons Exp Exp.tNat (List.nil Exp)))))
       (List.cons
        U
        U.u0
        (List.cons
         U
         U.uw
         (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
       1
       Exp.tSyn
       U.uw)
      (DT.rVar
       (List.cons
        Exp
        Exp.tUnit
        (List.cons
         Exp
         Exp.tSyn
         (List.cons
          Exp
          (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
          (List.cons Exp Exp.tNat (List.nil Exp)))))
       (List.cons
        U
        U.u0
        (List.cons
         U
         U.uw
         (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
       1
       Exp.tSyn
       U.uw))
     (DT.rConst
      (List.cons
       Exp
       Exp.tUnit
       (List.cons
        Exp
        Exp.tSyn
        (List.cons
         Exp
         (Exp.tSig U.uw Exp.tSyn Exp.tUnit)
         (List.cons Exp Exp.tNat (List.nil Exp)))))
      (List.cons
       U
       U.u1
       (List.cons
        U
        U.u0
        (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))
      Exp.star
      Exp.tUnit)))))

(let [tree (vec doubler-tree-form)
      ;; rRecN's final fields are n, hn, hP, hz, hs.  Assert the generated
      ;; shape before replacing precisely the two varying fields.
      _ (assert (= (first tree) 'DT.rRecN))
      _ (assert (= (nth tree 8) 'Exp.zero))
      _ (assert (= (first (nth tree 9)) 'DT.rConst))
      body (apply list (assoc tree 8 '(sh4_num n) 9 '(sh4_num_tree n)))]
  (b/kdef! 'sh4_doubler_tree '(=> Nat DT)
    (lv (list 'fn '[n :- Nat] body))))

(b/kdef! 'sh4_doubler_form_tree 'DT (lv (g/tl-form [] motive)))

(thm sh4_doubler_tree_check [n :- Nat]
  (Eq Bool (dtCheck (Check decCert) (sh4_doubler_tree n)
             (DTJ.rt (List.nil Exp) (List.nil U)
               (sh4_doubler (sh4_num n)) (sh4_M)))
           Bool.true)
  (unfold sh4_doubler_tree)
  (simp [dtCheck_rRecN_eq sh4_num_tree_check
         sh4_dj_rt_refl sh4_dj_tl_refl sh4_listE_refl sh4_listU_refl
         expEq_refl]))

(b/kdef! 'sh4_doubler_cert '(=> Nat Code)
  '(fn [n :- Nat]
     (encCert 0 (sh4_doubler (sh4_num n)) (sh4_M)
       (sh4_doubler_tree n) sh4_doubler_form_tree)))

(thm prop412_explicit_cert [n :- Nat]
  (Eq Bool (Check decCert (sh4_doubler_cert n) (encE (sh4_M))) Bool.true)
  (exact (check_cert_ok 0 (sh4_doubler (sh4_num n)) (sh4_M)
    (sh4_doubler_tree n) sh4_doubler_form_tree
    (sh4_doubler_tree_check n) rfl rfl)))

;; The successor tree stores the current numeral once and the preceding
;; derivation once.  Generate its exact F7 equation from the encoder table.
(let [ctors (deref #'lcert.formal.certenc/dt-ctors)
      info @(deref #'lcert.formal.certenc/ty-info)
      finfo @(deref #'lcert.formal.certenc/fuel-info)
      chain (deref #'lcert.formal.certenc/chain-enc)
      nodes (deref #'lcert.formal.certcanon/nodes-of)
      proof (deref #'lcert.formal.certcanon/cnodes-eq)
      [i [_ fields]] (first (filter (fn [[_ [nm _]]] (= nm 'rSucc))
                            (map-indexed vector ctors)))
      ctor (apply list 'DT.rSucc (map first fields))
      code (chain i (for [[x T] fields]
                      (list (if (= T 'DT) 'encDT
                              (first (or (info T) (finfo T)))) x)))]
  (a/prove-theorem 'sh4_encDT_rSucc_nodes
    (lv (hd/f7-params fields))
    (lv (list 'Eq 'Nat (list 'cnodes (list 'encDT ctor)) (nodes code)))
    (lv [(list 'have 'q_e (list 'Eq 'Code (list 'encDT ctor) code) '(rfl))
         (list 'exact (list 'Eq.trans '(congrArg cnodes q_e) (proof code)))])))

(thm sh4_num_tree_size_zero []
  (Eq Nat (cnodes (encDT (sh4_num_tree 0))) 4)
  (rfl))

(thm sh4_num_tree_size_step [n :- Nat]
  (Eq Nat (cnodes (encDT (sh4_num_tree (Nat.succ n))))
          (+ (cnodes (encDT (sh4_num_tree n))) (+ n 4)))
  (rw [(sh4_num_tree_succ n)])
  (simp [sh4_encDT_rSucc_nodes sh4_num_enc_linear])
  (omega))

(thm sh4_num_tree_bound [n :- Nat]
  (LE.le (cnodes (encDT (sh4_num_tree n)))
         (+ (Nat.mul n n) (+ n (+ n (+ n (+ n 4))))))
  (induction n)
  (exact (Nat.le_refl 4))
  (have hs (Eq Nat (cnodes (encDT (sh4_num_tree (Nat.succ n))))
               (+ (cnodes (encDT (sh4_num_tree n))) (+ n 4)))
    (sh4_num_tree_size_step n))
  (have hsq (Eq Nat (Nat.mul (Nat.succ n) (Nat.succ n))
                 (+ (Nat.mul n n) (+ n (+ n 1))))
    (sh4_sq_succ n))
  (omega))

(thm sh4_num_tree_ht [n :- Nat]
  (Eq Nat (htDT (sh4_num_tree n)) (+ n 1))
  (induction n)
  (rfl)
  (rw [(sh4_num_tree_succ n)])
  (simp [ih_n])
  (omega))

(let [nodes (deref #'lcert.formal.certcanon/nodes-of)
      proof (deref #'lcert.formal.certcanon/cnodes-eq)
      code (fn [arg]
             (list 'Code.sn 38 '(encE (sh4_M))
               (list 'Code.sn 12 '(encE (sh4_dbl_base))
                 (list 'Code.sn 12 '(encE (sh4_dbl_step)) arg))))
      succ-code (code '(encE (sh4_num (Nat.succ n))))
      prev-code (code '(encE (sh4_num n)))]
  (a/prove-theorem 'sh4_doubler_term_size_step (lv '[n :- Nat])
    (lv '(Eq Nat (cnodes (encE (sh4_doubler (sh4_num (Nat.succ n)))))
          (+ (cnodes (encE (sh4_doubler (sh4_num n)))) 1)))
    (lv [(list 'have 'qs (list 'Eq 'Code
                 '(encE (sh4_doubler (sh4_num (Nat.succ n)))) succ-code) '(rfl))
         (list 'have 'qp (list 'Eq 'Code
                 '(encE (sh4_doubler (sh4_num n))) prev-code) '(rfl))
         (list 'have 'ns (list 'Eq 'Nat
                 '(cnodes (encE (sh4_doubler (sh4_num (Nat.succ n)))))
                 (nodes succ-code))
               (list 'Eq.trans '(congrArg cnodes qs) (proof succ-code)))
         (list 'have 'np (list 'Eq 'Nat
                 '(cnodes (encE (sh4_doubler (sh4_num n))))
                 (nodes prev-code))
               (list 'Eq.trans '(congrArg cnodes qp) (proof prev-code)))
         '(have hn (Eq Nat (cnodes (encE (sh4_num (Nat.succ n))))
                       (+ 1 (cnodes (encE (sh4_num n)))))
            (sh4_num_enc_step n))
         '(omega)])))

(thm sh4_doubler_tree_ht_step [n :- Nat]
  (Eq Nat (htDT (sh4_doubler_tree (Nat.succ n)))
    (+ (htDT (sh4_doubler_tree n)) 1))
  (unfold sh4_doubler_tree)
  (simp [sh4_num_tree_ht]))

;; The outer recursor node has four fixed proof subtrees.  Only its numeral
;; expression and numeral proof subtree change with n.
