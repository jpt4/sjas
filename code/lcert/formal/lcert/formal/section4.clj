(ns lcert.formal.section4
  "F4 — §4 of R4-metatheory.md, the draft's §§4–5 settled.

  Proved here, from Lemma 3.6 (lemma36.clj; CheckSpec and the Conv case as
  hypotheses, as there):
  - Proposition 4.1: no token-free certificates (prop41_closed, prop41_fun);
  - Proposition 4.7: Con′ ⊸ H° is derivable (prop47).

  The rest of §4 is recorded in ADR-0006's statement index with what each
  result needs: conversion steps on concrete codes (4.2, 4.5, 4.10), the
  encoding facts E2–E6 of the concrete Check (4.3, 4.4, 4.6, 4.9), second
  models (Lemma 4.6a's phantom certificates, 4.10′'s truth predicate), or P5
  (4.8)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.lemma36 :refer :all]))

;; A tree with no internal node is a leaf.
(thm sn_not_le0 [l :- Nat, a :- Code, b :- Code, h :- (LE.le (cnodes (Code.sn l a b)) 0)] False
  (have h2 (LE.le (+ 1 (+ (cnodes a) (cnodes b))) 0) (Eq.mp (congrArg (fn [q :- Nat] (LE.le q 0)) (cnodes_sn l a b)) h))
  (omega))
(thm leaf_of_cnodes [v :- Code, h :- (LE.le (cnodes v) 0)] (Exists (fn [l :- Nat] (Eq Code v (Code.sl l))))
  (cases v) (exact (Exists.intro l (Eq.refl$1 (Code.sl l)))) (refine' (False.elim (sn_not_le0 _ _ _ h))))
;; Proposition 4.1 (i): a closed runtime term of type R denotes a leaf (T3 at
;; the empty context, footprint 0).
(thm prop41_closed [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                      hcs :- (CheckSpec chkf dec encTy), hconv :- (forall [n Nat] (ConvCase chkf dec encTy n)),
                      n :- Nat, t :- Exp, der :- (Rt chkf (List.nil Exp) (List.nil U) t Exp.tR)]
  (Exists (fn [l :- Nat] (Eq Code (den chkf dec encTy n t (List.nil Sk) Sk.cert Unit.unit) (Code.sl l))))
  (exact (leaf_of_cnodes (den chkf dec encTy n t (List.nil Sk) Sk.cert Unit.unit)
           (theorem3 chkf dec encTy hcs hconv n (List.nil Exp) (List.nil U) t der True.intro Unit.unit 0 (Nat.zero_le n) True.intro))))
;; Proposition 4.1 (ii): a closed f : Syn ⊸ R maps every code to a leaf — the
;; Π₁ clause at j = 0, since V₀(Syn) is all of V(Syn) (codes with labels
;; below NL, the paper's codes).
(thm prop41_fun [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   hcs :- (CheckSpec chkf dec encTy), hconv :- (forall [n Nat] (ConvCase chkf dec encTy n)),
                   n :- Nat, f :- Exp, der :- (Rt chkf (List.nil Exp) (List.nil U) f (Exp.tPi U.u1 Exp.tSyn Exp.tR)),
                   c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)]
  (Exists (fn [l :- Nat] (Eq Code ((den chkf dec encTy n f (List.nil Sk) (Sk.arr Sk.syn Sk.cert) Unit.unit) c) (Code.sl l))))
  (have hv (forall [j Nat] (=> (Nat.le (+ 0 j) n) (forall [a Code] (=> (Eq Bool (lblOk a) Bool.true)
             (And (Nat.le (cnodes ((den chkf dec encTy n f (List.nil Sk) (Sk.arr Sk.syn Sk.cert) Unit.unit) a)) (+ 0 j))
                  (Eq Bool (lblOk ((den chkf dec encTy n f (List.nil Sk) (Sk.arr Sk.syn Sk.cert) Unit.unit) a)) Bool.true))))))
    (lemma36 chkf dec encTy hcs hconv n (List.nil Exp) (List.nil U) f (Exp.tPi U.u1 Exp.tSyn Exp.tR) der True.intro Unit.unit 0 (Nat.zero_le n) True.intro))
  (exact (leaf_of_cnodes ((den chkf dec encTy n f (List.nil Sk) (Sk.arr Sk.syn Sk.cert) Unit.unit) c)
           (And.left (hv 0 (Nat.zero_le n) c hc)))))

;; --- §4.6: the two consistency statements --------------------------------------------

;; Con′ = Π(c :₁ Syn). T(chk′ c c⊥) ⊸ 0, the code form of consistency.
;; Proposition 4.7: λ(f :₁ Con′). λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)).
;; f (print r) e has type Con′ ⊸ H°, closed, at budget 0 — App twice: print r
;; uses r once, and e's type is syntactically the domain of f (print r)
;; (subst1 of print r into Con′'s body).
(a/defn ConP [] Exp (Exp.tPi U.u1 Exp.tSyn (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)))
(a/defn p47term [] Exp
  (Exp.lam U.u1 (ConP) (Exp.lam U.u1 Exp.tR (Exp.lam U.u1 (chkT (Exp.var 0) (cbot))
    (Exp.app (Exp.app (Exp.var 2) (Exp.prn (Exp.var 1))) (Exp.var 0))))))
(let [D0 '(List.nil Exp) D1 (list 'consE '(ConP) D0) D2 (list 'consE 'Exp.tR D1)
      A3 '(chkT (Exp.var 0) (cbot)) D3 (list 'consE A3 D2)
      vec3 (fn [a b c] (list 'consU a (list 'consU b (list 'consU c '(List.nil U)))))
      Vf (vec3 'U.u0 'U.u0 'U.u1) Vr (vec3 'U.u0 'U.u1 'U.u0) Ve (vec3 'U.u1 'U.u0 'U.u0)
      Bc '(Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)
      zcb (fn [D] (list 'Tl.zSleaf 'chkf D '(Exp.lbl 15) (list 'Tl.zConst 'chkf D '(Exp.lbl 15) 'Exp.tLbl 'rfl)))
      SD3 (list 'List.cons 'Exp 'Exp.tSyn D3)
      hBi (list 'Tl.fPi 'chkf SD3 'U.u1 '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) 'Exp.tEmpty
                (list 'Tl.fT 'chkf SD3 '(Exp.chk (Exp.var 0) (cbot))
                      (list 'Tl.zChk 'chkf SD3 '(Exp.var 0) '(cbot) (list 'Tl.zVar 'chkf SD3 0 'Exp.tSyn 'rfl) (zcb SD3)))
                (list 'Tl.fBase 'chkf (list 'List.cons 'Exp '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) SD3) 'Exp.tEmpty 'rfl))
      inner (list 'Rt.rApp 'chkf D3 Vf Vr 'U.u1 '(Exp.var 2) '(Exp.prn (Exp.var 1)) 'Exp.tSyn Bc '(Eq.refl$1 Bool.true)
                  (list 'Rt.rVar 'chkf D3 Vf 2 '(ConP) 'U.u1 'rfl 'rfl 'rfl 'rfl)
                  (list 'Rt.rPrn 'chkf D3 Vr '(Exp.var 1) (list 'Rt.rVar 'chkf D3 Vr 1 'Exp.tR 'U.u1 'rfl 'rfl 'rfl 'rfl))
                  (list 'Tl.fBase 'chkf D3 'Exp.tSyn 'rfl) hBi)
      A2 '(chkT (Exp.var 1) (cbot))
      hA2 (list 'Tl.fT 'chkf D3 '(Exp.chk (Exp.prn (Exp.var 1)) (cbot))
                (list 'Tl.zChk 'chkf D3 '(Exp.prn (Exp.var 1)) '(cbot)
                      (list 'Tl.zPrn 'chkf D3 '(Exp.var 1) (list 'Tl.zVar 'chkf D3 1 'Exp.tR 'rfl)) (zcb D3)))
      body (list 'Rt.rApp 'chkf D3 (list 'vadd Vf (list 'vscale 'U.u1 Vr)) Ve 'U.u1 '(Exp.app (Exp.var 2) (Exp.prn (Exp.var 1))) '(Exp.var 0)
                 A2 'Exp.tEmpty '(Eq.refl$1 Bool.true) inner
                 (list 'Rt.rVar 'chkf D3 Ve 0 A3 'U.u1 'rfl 'rfl 'rfl 'rfl)
                 hA2 (list 'Tl.fBase 'chkf (list 'List.cons 'Exp A2 D3) 'Exp.tEmpty 'rfl))
      F3 (list 'Tl.fT 'chkf D2 '(Exp.chk (Exp.prn (Exp.var 0)) (cbot))
               (list 'Tl.zChk 'chkf D2 '(Exp.prn (Exp.var 0)) '(cbot)
                     (list 'Tl.zPrn 'chkf D2 '(Exp.var 0) (list 'Tl.zVar 'chkf D2 0 'Exp.tR 'rfl)) (zcb D2)))
      t3 (list 'Exp.app '(Exp.app (Exp.var 2) (Exp.prn (Exp.var 1))) '(Exp.var 0))
      L3 (list 'Rt.rLam 'chkf D2 '(consU U.u1 (consU U.u1 (List.nil U))) 'U.u1 A3 t3 'Exp.tEmpty F3 body)
      t2 (list 'Exp.lam 'U.u1 A3 t3)
      L2 (list 'Rt.rLam 'chkf D1 '(consU U.u1 (List.nil U)) 'U.u1 'Exp.tR t2 (list 'Exp.tPi 'U.u1 A3 'Exp.tEmpty) (list 'Tl.fBase 'chkf D1 'Exp.tR 'rfl) L3)
      ST (list 'List.cons 'Exp 'Exp.tSyn D0)
      FCon (list 'Tl.fPi 'chkf D0 'U.u1 'Exp.tSyn '(Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty) (list 'Tl.fBase 'chkf D0 'Exp.tSyn 'rfl)
                 (list 'Tl.fPi 'chkf ST 'U.u1 '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) 'Exp.tEmpty
                       (list 'Tl.fT 'chkf ST '(Exp.chk (Exp.var 0) (cbot)) (list 'Tl.zChk 'chkf ST '(Exp.var 0) '(cbot) (list 'Tl.zVar 'chkf ST 0 'Exp.tSyn 'rfl) (zcb ST)))
                       (list 'Tl.fBase 'chkf (list 'List.cons 'Exp '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) ST) 'Exp.tEmpty 'rfl)))
      L1 (list 'Rt.rLam 'chkf D0 '(List.nil U) 'U.u1 '(ConP) (list 'Exp.lam 'U.u1 'Exp.tR t2) '(Hcirc) FCon L2)]
  (eval (list 'lcert.formal.base/thm 'prop47 '[chkf :- (=> Code Code Bool)]
              '(Rt chkf (List.nil Exp) (List.nil U) (p47term) (Exp.tPi U.u1 (ConP) (Hcirc)))
              (list 'exact L1))))
