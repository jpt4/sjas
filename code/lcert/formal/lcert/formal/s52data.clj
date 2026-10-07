(ns lcert.formal.s52data
  "Theorem 5.2: data constructors, print and the checker (R4 §5).

  These cases compose safe traces at base types. S requires finite labels
  for Lbl, Syn and R, just as the formalization's existing carriers do.
  Certificates have no node-count bound in S. The same proofs cover both
  evaluators and both typing modes; original and evaluated operands are
  separate parameters, to allow the latter to be erasures."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]))

;; Read a base-type result as its canonical runtime value and, separately,
;; recover the finite-label invariant. This is exactly S's base clause.
(def base-cases
  '[[bool Exp.tBool Sk.bool RV.bool nil]
    [nat Exp.tNat Sk.nat RV.nat nil]
    [lbl Exp.tLbl Sk.lbl RV.lbl (LT.lt (den chkf dec encTy cap t G Sk.lbl en) (NL))]
    [syn Exp.tSyn Sk.syn RV.code (Eq Bool (lblOk (den chkf dec encTy cap t G Sk.syn en)) Bool.true)]
    [cert Exp.tR Sk.cert RV.cert (Eq Bool (lblOk (den chkf dec encTy cap t G Sk.cert en)) Bool.true)]])

(doseq [[nm A sk ctor valid] base-cases]
  (prove! (symbol (str "s52_value_" nm))
    (concat ctx '[t :- Exp, e :- Exp] ['ih :- (result 't A 'e)])
    (trace 'e (list ctor (den 't sk)))
    ['(refine' (exT RV _ _ ih _)) '(intro v hv)
     (list 'exact (list 's52_trace_cast 'chkf 'dec 'encTy 'cap 'erasing
       '(EvSrc.tm rho e) 'v (list ctor (den 't sk)) '(And.left hv)
       (if valid '(And.left (And.right hv)) '(And.right hv))))])
  (when valid
    (prove! (symbol (str "s52_valid_" nm))
      (concat ctx '[t :- Exp, e :- Exp] ['ih :- (result 't A 'e)]) valid
      '[(refine' (exT RV _ _ ih _)) (intro v hv) (exact (And.right (And.right hv)))])))

(defn- valpf [nm t e ih]
  (list (symbol (str "s52_value_" nm)) 'chkf 'dec 'encTy 'cap 'erasing 'G 'en 'rho t e ih))
(defn- validpf [nm t e ih]
  (list (symbol (str "s52_valid_" nm)) 'chkf 'dec 'encTy 'cap 'erasing 'G 'en 'rho t e ih))
(defn- runrule [rule & args]
  (apply list (symbol (str "trace52_" rule)) 'chkf 'dec 'encTy 'erasing 'cap 'rho args))

(prove! 's52_succ (concat ctx '[t :- Exp, e :- Exp] ['ih :- (result 't 'Exp.tNat 'e)])
  (result '(Exp.succ t) 'Exp.tNat '(Exp.succ e))
  ['(rw [(den_succ_at chkf dec encTy cap t G Sk.nat en)])
   '(constructor) (list 'exact (list 'RV.nat (list 'Nat.succ (den 't 'Sk.nat))))
   '(constructor) (list 'exact (runrule 'eSucc 'e (den 't 'Sk.nat) (valpf 'nat 't 'e 'ih)))
   '(rfl)])

;; Code/certificate leaves differ only in the runtime tag and carrier.
(doseq [[nm ctor A sk rv rule eq] '[[sleaf Exp.sleaf Exp.tSyn Sk.syn RV.code eSleaf den_sleaf_at]
                                    [leaf Exp.leaf Exp.tR Sk.cert RV.cert eLeaf den_leaf_at]]]
  (prove! (symbol (str "s52_" nm))
    (concat ctx '[a :- Exp, ae :- Exp] ['ih :- (result 'a 'Exp.tLbl 'ae)])
    (result (list ctor 'a) A (list ctor 'ae))
    [(list 'rw [(list eq 'chkf 'dec 'encTy 'cap 'a 'G sk 'en)])
     '(constructor) (list 'exact (list rv (list 'Code.sl (den 'a 'Sk.lbl))))
     '(constructor) (list 'exact (runrule rule 'ae (den 'a 'Sk.lbl) (valpf 'lbl 'a 'ae 'ih)))
     '(constructor) '(rfl)
     (list 'exact (list 'Eq.mpr (list 'Nat.blt_eq (den 'a 'Sk.lbl) 100) (validpf 'lbl 'a 'ae 'ih)))]))

(prove! 's52_snode
  (concat ctx '[a :- Exp, x :- Exp, y :- Exp, ae :- Exp, xe :- Exp, ye :- Exp]
    ['iha :- (result 'a 'Exp.tLbl 'ae) 'ihx :- (result 'x 'Exp.tSyn 'xe) 'ihy :- (result 'y 'Exp.tSyn 'ye)])
  (result '(Exp.snode a x y) 'Exp.tSyn '(Exp.snode ae xe ye))
  ['(rw [(den_snode_at chkf dec encTy cap a x y G Sk.syn en)])
   '(constructor) (list 'exact (list 'RV.code (list 'Code.sn (den 'a 'Sk.lbl) (den 'x 'Sk.syn) (den 'y 'Sk.syn))))
   '(constructor) (list 'exact (runrule 'eSnode 'ae 'xe 'ye (den 'a 'Sk.lbl) (den 'x 'Sk.syn) (den 'y 'Sk.syn)
     (valpf 'lbl 'a 'ae 'iha) (valpf 'syn 'x 'xe 'ihx) (valpf 'syn 'y 'ye 'ihy)))
   '(constructor) '(rfl)
   (list 'exact (list 'band3 (list 'Nat.blt (den 'a 'Sk.lbl) 100)
     (list 'lblOk (den 'x 'Sk.syn)) (list 'lblOk (den 'y 'Sk.syn))
     (list 'Eq.mpr (list 'Nat.blt_eq (den 'a 'Sk.lbl) 100) (validpf 'lbl 'a 'ae 'iha))
     (validpf 'syn 'x 'xe 'ihx) (validpf 'syn 'y 'ye 'ihy)))])

(prove! 's52_node
  (concat ctx '[d :- Exp, a :- Exp, x :- Exp, y :- Exp, de :- Exp, ae :- Exp, xe :- Exp, ye :- Exp]
    ['ihd :- (result 'd 'Exp.tDia 'de) 'iha :- (result 'a 'Exp.tLbl 'ae)
     'ihx :- (result 'x 'Exp.tR 'xe) 'ihy :- (result 'y 'Exp.tR 'ye)])
  (result '(Exp.node d a x y) 'Exp.tR '(Exp.node de ae xe ye))
  ['(rw [(den_node_at chkf dec encTy cap d a x y G Sk.cert en)])
   '(refine' (exT RV _ _ ihd _)) '(intro vd hd)
   '(constructor) (list 'exact (list 'RV.cert (list 'Code.sn (den 'a 'Sk.lbl) (den 'x 'Sk.cert) (den 'y 'Sk.cert))))
   '(constructor) (list 'exact (runrule 'eNode 'de 'ae 'xe 'ye 'vd (den 'a 'Sk.lbl) (den 'x 'Sk.cert) (den 'y 'Sk.cert)
     '(And.left hd) (valpf 'lbl 'a 'ae 'iha) (valpf 'cert 'x 'xe 'ihx) (valpf 'cert 'y 'ye 'ihy)))
   '(constructor) '(rfl)
   (list 'exact (list 'band3 (list 'Nat.blt (den 'a 'Sk.lbl) 100)
     (list 'lblOk (den 'x 'Sk.cert)) (list 'lblOk (den 'y 'Sk.cert))
     (list 'Eq.mpr (list 'Nat.blt_eq (den 'a 'Sk.lbl) 100) (validpf 'lbl 'a 'ae 'iha))
     (validpf 'cert 'x 'xe 'ihx) (validpf 'cert 'y 'ye 'ihy)))])

(prove! 's52_prn (concat ctx '[r :- Exp, re :- Exp] ['ih :- (result 'r 'Exp.tR 're)])
  (result '(Exp.prn r) 'Exp.tSyn '(Exp.prn re))
  ['(rw [(den_prn_val chkf dec encTy cap r G en)])
   '(constructor) (list 'exact (list 'RV.code (den 'r 'Sk.cert)))
   '(constructor) (list 'exact (runrule 'ePrn 're (den 'r 'Sk.cert) (valpf 'cert 'r 're 'ih)))
   '(constructor) '(rfl) (list 'exact (validpf 'cert 'r 're 'ih))])

(prove! 's52_chk
  (concat ctx '[c :- Exp, d :- Exp, ce :- Exp, de :- Exp]
    ['ihc :- (result 'c 'Exp.tSyn 'ce) 'ihd :- (result 'd 'Exp.tSyn 'de)])
  (result '(Exp.chk c d) 'Exp.tBool '(Exp.chk ce de))
  ['(rw [(den_chk_val chkf dec encTy cap c d G en)])
   '(constructor) (list 'exact (list 'RV.bool (list 'chkf (den 'c 'Sk.syn) (den 'd 'Sk.syn))))
   '(constructor) (list 'exact (runrule 'eChk 'ce 'de (den 'c 'Sk.syn) (den 'd 'Sk.syn)
     (valpf 'syn 'c 'ce 'ihc) (valpf 'syn 'd 'de 'ihd)))
   '(rfl)])
