(ns lcert.formal.s52inspect
  "Theorem 5.2: Inspect and its truthful branch evidence (R4 §5)."
  (:require [clojure.walk :as walk]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]
            [lcert.formal.s52data]))

;; Inspect's evidence mentions the bound certificate and the lifted code.
;; Its denotation is the same checker call that selects the runtime branch.
(def F1 '(chkT (Exp.var 0) (lift 1 0 c)))
(def F2 '(Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))))
(doseq [[nm truth formula pf]
        '[[s52_insp_true Bool.true (chkT (Exp.var 0) (lift 1 0 c))
           (Eq.trans (insp_chk chkf dec encTy cap G c hc cr en) hb)]
          [s52_insp_false Bool.false (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))
           (insp_not chkf dec encTy cap G c hc cr en hb)]]]
  (prove! nm
    (concat ctx '[c :- Exp, cr :- Code, hc :- (SkJ Bool.false G c Sk.syn)]
      ['hb :- (list 'Eq 'Bool (list 'chkf 'cr (den 'c 'Sk.syn)) truth)])
    (apply list 'S52 (concat ps [formula '(List.cons Sk Sk.cert G) '(Prod.mk cr en) 'Sk.unit 'RV.star 'Unit.unit]))
    ['(constructor) '(rfl) (list 'exact pf)]))

(def CR (den 'r 'Sk.cert))
(def CD (den 'c 'Sk.syn))
(def GC '(List.cons Sk Sk.unit (List.cons Sk Sk.cert G)))
(def EC (list 'Prod.mk 'Unit.unit (list 'Prod.mk CR 'en)))
(def RC (list 'List.cons 'RV 'RV.star (list 'List.cons 'RV (list 'RV.cert CR) 'rho)))
(defn- br-result [t te]
  (walk/postwalk-replace {'G GC 'en EC 'rho RC} (result t '(lift 2 0 X) te)))
(def insp-params
  '[X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp,
    re :- Exp, ce :- Exp, t1e :- Exp, t2e :- Exp,
    hX :- (SkJ Bool.true G X Sk.unit)])

;; Only the taken branch needs a result. It is transported from lift 2 X
;; back to X, without weakening or forgetting any of its evaluation trace.
(doseq [[nm truth t te rule]
        '[[s52_insp_pick_false Bool.false t2 t2e eInspF]
          [s52_insp_pick_true Bool.true t1 t1e eInspT]]]
  (prove! nm
    (concat ctx insp-params
      ['hb :- (list 'Eq 'Bool (list 'chkf CR CD) truth)
       'ihr :- (result 'r 'Exp.tR 're) 'ihc :- (result 'c 'Exp.tSyn 'ce)
       'iht :- (br-result t te)])
    (result '(Exp.insp X r c t1 t2) 'X '(Exp.insp X re ce t1e t2e))
    ['(rw [(den_insp_val chkf dec encTy cap X r c t1 t2 G (skel X) en)])
     '(rw [hb])
     '(refine' (exT RV _ _ iht _)) '(intro v hv)
     '(constructor) '(exact v) '(constructor)
     (list 'exact (apply list (symbol (str "trace52_" rule))
       (concat '[chkf dec encTy erasing cap rho X re ce t1e t2e]
         [CR CD 'v '(s52_value_cert chkf dec encTy cap erasing G en rho r re ihr)
          '(s52_value_syn chkf dec encTy cap erasing G en rho c ce ihc) 'hb '(And.left hv)])))
     (list 'exact (list 'Eq.mp
       (apply list 's52_lift2_fam (concat ps ['G 'X 'hX 'Sk.cert 'Sk.unit 'en CR 'Unit.unit 'v
         (list 'fn '[s :- Sk] (list 'den 'chkf 'dec 'encTy 'cap t GC 's EC))])) '(And.right hv)))]))

;; The derivation induction supplies branch results conditional on the
;; actual checker answer. s52_insp_true/false establish their new evidence
;; entries when the environments are extended in that induction.
(prove! 's52_insp
  (concat ctx insp-params
    ['ihr :- (result 'r 'Exp.tR 're) 'ihc :- (result 'c 'Exp.tSyn 'ce)
     'ih1 :- (list '=> (list 'Eq 'Bool (list 'chkf CR CD) 'Bool.true) (br-result 't1 't1e))
     'ih2 :- (list '=> (list 'Eq 'Bool (list 'chkf CR CD) 'Bool.false) (br-result 't2 't2e))])
  (result '(Exp.insp X r c t1 t2) 'X '(Exp.insp X re ce t1e t2e))
  ['(by_cases (chkf (den chkf dec encTy cap r G Sk.cert en) (den chkf dec encTy cap c G Sk.syn en)))
   '(exact (s52_insp_pick_false chkf dec encTy cap erasing G en rho X r c t1 t2 re ce t1e t2e hX hc ihr ihc (ih2 hc)))
   '(exact (s52_insp_pick_true chkf dec encTy cap erasing G en rho X r c t1 t2 re ce t1e t2e hX hc ihr ihc (ih1 hc)))])
