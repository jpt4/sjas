(ns lcert.formal.p49nodelive
  "Node label split with live child variables assigned to each branch."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.p49nodemotive]))

;; The label scrutinee needs only index 7. The selected branch receives
;; both pair witnesses, both child codes, and the recSyn result usages.
(kdef p49nl_Ulabel (=> (List U) (List U))
  (fn [uv :- (List U)]
    (consU U.u0 (consU U.u0 (consU U.u0
      (consU U.u0 (consU U.u0 (consU U.u0
        (consU U.u0 (consU U.uw (consU U.u0
          (p49peel_z uv))))))))))))

(kdef p49nl_Ubranch (=> (List U) (List U))
  (fn [uv :- (List U)]
    (consU U.u1 (consU U.u1 (consU U.u0
      (consU U.u1 (consU U.u1 (consU U.uw
        (consU U.uw (consU U.u0 (consU U.u0
          (p49peel_z uv))))))))))))

(thm p49nl_usage [uv :- (List U)]
  (Eq (List U)
    (vadd (p49nl_Ulabel uv) (p49nl_Ubranch uv))
    (p49nm_U uv))
  (exact (congrArg
    (fn [v :- (List U)]
      (consU U.u1 (consU U.u1 (consU U.u0
        (consU U.u1 (consU U.u1 (consU U.uw
          (consU U.uw (consU U.uw (consU U.u0 v))))))))))
    (p49peel_zadd uv))))

(a/prove-theorem 'p49nl_label
  '[chkf :- (=> Code Code Bool), j :- Nat,
    F :- (=> Exp Exp), D :- (List Exp), uv :- (List U),
    hlen :- (Eq Nat (lenU uv) (lenE D))]
  '(Rt chkf (p49nm_D j F D) (p49nl_Ulabel uv)
     (Exp.var 7) Exp.tLbl)
  [(list 'exact
     (list 'p49nm_label_at 'chkf 'j 'F 'D '(p49nl_Ulabel uv)
       '(p49nm_len j F D uv hlen)
       (lcert.formal.p49nodemotive/p49nm-lookup
         'nthU
         '[U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0]
         '(p49peel_z uv) 7)))])

(thm p49nl_node_cont
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), k :- Nat, h :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hh :- (p49tr_avoids slots h),
   hF :- (p49pd_Form chkf (p49nm_F K slots h)),
   f :- (=> Nat Exp),
   hf :- (forall [l Nat]
     (=> (Eq Bool (Nat.blt l 100) Bool.true)
       (Rt chkf
         (p49nm_D k (p49nm_F K slots h) D)
         (p49nl_Ubranch uv) (f l)
         (p49tele
           (fn [q :- Nat, c :- Exp] (p49dl q c))
           ((p49crepl K) h
             (P49C.node l
               (P49C.hole (p49fresh K slots))
               (P49C.hole (Nat.succ (p49fresh K slots)))))
           slots
           (p49sc_pair_env p49tl_rho0 (p49fresh K slots)
             (Exp.var 6) (Exp.var 5))))))]
  (Rt chkf
    (p49nm_D k (p49nm_F K slots h) D)
    (p49nm_U uv)
    (Exp.caseL
      (p49nm_M (p49nm_F K slots h))
      (Exp.var 7) (p49brs 100 f 0))
    (lift 2 0
      (p49peel_nodeB ((p49nm_F K slots h) (Exp.var 0)))))
  (have hP
    (Tl chkf Bool.true
      (consE Exp.tLbl
        (p49nm_D k (p49nm_F K slots h) D))
      (p49nm_M (p49nm_F K slots h)) Exp.tUnit)
    (p49nm_motive_formed chkf k D
      (p49nm_F K slots h) hF))
  (have hbr
    (forall [l Nat]
      (=> (Eq Bool (Nat.blt l 100) Bool.true)
        (Rt chkf
          (p49nm_D k (p49nm_F K slots h) D)
          (p49nl_Ubranch uv) (f l)
          (subst1 (Exp.lbl l)
            (p49nm_M (p49nm_F K slots h))))))
    (fn [l :- Nat]
      (fn [hlt :- (Eq Bool (Nat.blt l 100) Bool.true)]
        (Eq.mp
          (congrArg
            (fn [A :- Exp]
              (Rt chkf
                (p49nm_D k (p49nm_F K slots h) D)
                (p49nl_Ubranch uv) (f l) A))
            (Eq.symm (p49nm_branch_tail K slots h l hh)))
          (hf l hlt)))))
  (exact (p49peel_cast chkf
    (p49nm_D k (p49nm_F K slots h) D)
    (vadd (p49nl_Ulabel uv) (p49nl_Ubranch uv))
    (p49nm_U uv)
    (Exp.caseL
      (p49nm_M (p49nm_F K slots h))
      (Exp.var 7) (p49brs 100 f 0))
    (subst1 (Exp.var 7)
      (p49nm_M (p49nm_F K slots h)))
    (lift 2 0
      (p49peel_nodeB ((p49nm_F K slots h) (Exp.var 0))))
    (p49case_typed chkf
      (p49nm_D k (p49nm_F K slots h) D)
      (p49nl_Ulabel uv) (p49nl_Ubranch uv)
      (p49nm_M (p49nm_F K slots h))
      (Exp.var 7) f
      (p49nm_len k (p49nm_F K slots h) D uv hlen)
      hP
      (p49nl_label chkf k (p49nm_F K slots h) D uv hlen)
      hbr)
    (p49nl_usage uv)
    (p49nm_peel_result
      (p49nm_F K slots h)
      (p49tl_p49dl_Lift K slots h)
      (p49tl_p49dl_Subst K slots h)))))

;; The induction term carries the two recSyn result usages. The four
;; arguments supply, in order, left code, left evidence, right code, and
;; right evidence. Their sum is exactly the branch budget above.
(defn- p49nl-vec [xs]
  (reduce (fn [tail x] (list 'consU x tail))
    '(p49peel_z uv) (reverse xs)))
(doseq [[name xs]
        [['p49nl_Ucarry '[U.u0 U.u0 U.u0 U.u1 U.u1 U.u0 U.u0 U.u0 U.u0]]
         ['p49nl_Uleft '[U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0]]
         ['p49nl_UleftEv '[U.u0 U.u1 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0]]
         ['p49nl_Uright '[U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0 U.u0]]
         ['p49nl_UrightEv '[U.u1 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0]]]]
  (b/kdef! name '(=> (List U) (List U))
    (list 'fn '[uv :- (List U)] (p49nl-vec xs))))

(thm p49nl_zsum [uv :- (List U)]
  (Eq (List U)
    (vadd
      (vadd
        (vadd
          (vadd (p49peel_z uv) (p49peel_z uv))
          (p49peel_z uv))
        (p49peel_z uv))
      (p49peel_z uv))
    (p49peel_z uv))
  (exact (Eq.trans
    (congrArg (fn [v :- (List U)]
      (vadd (vadd (vadd v (p49peel_z uv)) (p49peel_z uv))
        (p49peel_z uv))) (p49peel_zadd uv))
    (Eq.trans
      (congrArg (fn [v :- (List U)]
        (vadd (vadd v (p49peel_z uv)) (p49peel_z uv)))
        (p49peel_zadd uv))
      (Eq.trans
        (congrArg (fn [v :- (List U)]
          (vadd v (p49peel_z uv))) (p49peel_zadd uv))
        (p49peel_zadd uv))))))

(thm p49nl_branch_usage [uv :- (List U)]
  (Eq (List U)
    (vadd
      (vadd
        (vadd
          (vadd (p49nl_Ucarry uv) (p49nl_Uleft uv))
          (p49nl_UleftEv uv))
        (p49nl_Uright uv))
      (p49nl_UrightEv uv))
    (p49nl_Ubranch uv))
  (exact (congrArg
    (fn [tail :- (List U)]
      (consU U.u1 (consU U.u1 (consU U.u0
        (consU U.u1 (consU U.u1 (consU U.uw
          (consU U.uw (consU U.u0 (consU U.u0 tail))))))))))
    (p49nl_zsum uv))))

;; The two code arguments are genuine runtime variables, each at its own
;; omega singleton budget. Their evidence arguments are treated next.
(def ^:private p49nl-Eprefix
  ['(p49pd_X j) '(p49pd_X j)
   '(p49peel_nodeA (p49pd_A j))
   '(y2Ty (p49peel_motive (p49pd_A j) (F (Exp.var 0))))
   '(y1Ty (p49peel_motive (p49pd_A j) (F (Exp.var 0))))
   'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn])
(doseq [[name usage index uprefix]
        [['p49nl_left_code 'p49nl_Uleft 6
          '[U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0]]
         ['p49nl_right_code 'p49nl_Uright 5
          '[U.u0 U.u0 U.u0 U.u0 U.u0 U.uw U.u0 U.u0 U.u0]]]]
  (a/prove-theorem name
    '[chkf :- (=> Code Code Bool), j :- Nat,
      F :- (=> Exp Exp), D :- (List Exp), uv :- (List U),
      hlen :- (Eq Nat (lenU uv) (lenE D))]
    (list 'Rt 'chkf '(p49nm_D j F D)
      (list usage 'uv) (list 'Exp.var index) 'Exp.tSyn)
    [(list 'exact
       (list 'Rt.rVar 'chkf '(p49nm_D j F D)
         (list usage 'uv) index 'Exp.tSyn 'U.uw
         '(p49nm_len j F D uv hlen)
         (lcert.formal.p49nodemotive/p49nm-lookup
           'nthE p49nl-Eprefix 'D index)
         (lcert.formal.p49nodemotive/p49nm-lookup
           'nthU uprefix '(p49peel_z uv) index)
         'rfl))]))

(thm p49nl_left_evidence_type [j :- Nat]
  (Eq Exp
    (lift 2 0 (p49pd_X j))
    (Exp.tT (p49dl j (Exp.var 6))))
  (exact (congrArg (fn [e :- Exp] (Exp.tT e))
    (p49pd_dl_lift j 2 0 (Exp.var 4)))))

(thm p49nl_right_evidence_type [j :- Nat]
  (Eq Exp
    (lift 1 0 (p49pd_X j))
    (Exp.tT (p49dl j (Exp.var 5))))
  (exact (congrArg (fn [e :- Exp] (Exp.tT e))
    (p49pd_dl_lift j 1 0 (Exp.var 4)))))

(doseq [[name usage index uprefix target eqname]
        [['p49nl_left_evidence 'p49nl_UleftEv 1
          '[U.u0 U.u1 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0]
          '(Exp.tT (p49dl j (Exp.var 6)))
          'p49nl_left_evidence_type]
         ['p49nl_right_evidence 'p49nl_UrightEv 0
          '[U.u1 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0 U.u0]
          '(Exp.tT (p49dl j (Exp.var 5)))
          'p49nl_right_evidence_type]]]
  (let [ctx '(p49nm_D j F D)
        us (list usage 'uv)
        var (list 'Exp.var index)
        source (list 'lift (inc index) 0 '(p49pd_X j))]
    (a/prove-theorem name
      '[chkf :- (=> Code Code Bool), j :- Nat,
        F :- (=> Exp Exp), D :- (List Exp), uv :- (List U),
        hlen :- (Eq Nat (lenU uv) (lenE D))]
      (list 'Rt 'chkf ctx us var target)
      [(list 'exact
         (list 'p49peel_cast 'chkf ctx us us var source target
           (list 'Rt.rVar 'chkf ctx us index '(p49pd_X j) 'U.u1
             '(p49nm_len j F D uv hlen)
             (lcert.formal.p49nodemotive/p49nm-lookup
               'nthE p49nl-Eprefix 'D index)
             (lcert.formal.p49nodemotive/p49nm-lookup
               'nthU uprefix '(p49peel_z uv) index)
             'rfl)
           'rfl (list eqname 'j)))])))
