(ns lcert.formal.prop44paper
  "Proposition 4.4 (no uniform budget) for the paper's family, and
  Corollary 4.6′'s second bullet's instance (fidelity pass, 2026-10-15).

  prop434 proved Proposition 4.4 at ¬ᵏ⁺¹1 (negN), with a minimal certificate
  of it as a hypothesis.  ¬ʲ1 has no certificate for odd j, so at every even
  k that hypothesis cannot be met and prop44_1/2/3 say nothing.  The proofs
  never use the shape of ¬ᵏ⁺¹1, only that it is closed and that its code has
  more than k nodes.  Here they are restated for any such A (prop43_g,
  box_out_g, prop44g_1/2/3), with two changes:
  - the family is a parameter: hcl (closed) and hsz (k + 1 ≤ ‖⌜A⌝‖) replace
    negN_closed and negN_size;
  - minimality ranges over label-safe certificates only, the paper's codes
    (R4 §1.2).  The proofs compare v only with certificates taken from the
    model, whose values of R are label-safe; box_out_g and prop43_g now
    return that bound.
  Then prop44_paper_1/2/3 state Proposition 4.4 as the paper does, at Check
  and for every k, with A = A_{k+1} = 1 ⊸ ⋯ ⊸ 1 (arrfamily), which is
  certifiable at every k.  The generator code below is prop434's, with the
  changes listed; the kernel checks every theorem."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.unfold :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.outer :refer :all]
            [lcert.formal.lemma36 :refer :all]
            [lcert.formal.strengthen]
            [lcert.formal.conversion]
            [lcert.formal.convcase]
            [lcert.formal.prop434]
            [lcert.formal.thm46f7]
            [lcert.formal.arrfamily]))

;; prop434's surface-form helpers.
(def ^:private Cb @#'lcert.formal.prop434/Cb)
(def ^:private Cex @#'lcert.formal.prop434/Cex)
(def ^:private gets @#'lcert.formal.prop434/gets)
(def ^:private box @#'lcert.formal.prop434/box)
(def ^:private ftok @#'lcert.formal.prop434/ftok)
(def ^:private Gm @#'lcert.formal.prop434/Gm)
(def ^:private em @#'lcert.formal.prop434/em)
(def ^:private Pv @#'lcert.formal.prop434/Pv)

;; A label-safe accepted certificate of A, and minimality among them.
(defn- safe [c A] (list 'And (list 'Eq 'Bool (list 'chkf c (list 'encTy A)) 'Bool.true)
                        (list 'Eq 'Bool (list 'lblOk c) 'Bool.true)))

;; --- Proposition 4.3, with M a lower bound over label-safe certificates ------
(eval (list 'lcert.formal.base/thm 'prop43_g
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
    hcs :- (CheckSpec chkf dec encTy), hes :- (TokSize chkf dec),
    A :- Exp, cA :- Exp, hcA :- (Eq (Option Code) (codeOf cA) (Option.some Code (encTy A))),
    hbc :- (Eq Bool (closedTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA))) Bool.true),
    w :- Code, hw :- (Eq Bool (chkf w (encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA)))) Bool.true),
    M :- Nat, hM :- (forall [v Code] (=> (Eq Bool (chkf v (encTy A)) Bool.true) (Eq Bool (lblOk v) Bool.true) (LE.le M (cnodes v))))]
  '(LT.lt (+ M M) (cnodes w))
  (list 'have 'X1 (Cex 'w (list 'encTy box)) (list '(And.left hcs) 'w (list 'encTy box) 'hw))
  '(refine' (exT Nat _ _ X1 _)) '(intro mm hmm) '(refine' (exT Exp _ _ hmm _)) '(intro tt htt) '(refine' (exT Exp _ _ htt _)) '(intro AA hAA)
  (list 'have 'q (Cb 'w (list 'encTy box) 'mm 'tt 'AA) 'hAA)
  (list 'have 'eB (list 'Eq 'Exp 'AA box) (list '(And.right (And.right (And.right hcs))) 'AA box (gets 'q 3) 'hbc (gets 'q 4)))
  (list 'have 'hRt (list 'Rt 'chkf '(thetaD mm) '(thetaU mm) 'tt box)
        (list 'Eq.mp (list 'congrArg '(fn [Z :- Exp] (Rt chkf (thetaD mm) (thetaU mm) tt Z)) 'eB) (gets 'q 1)))
  (list 'have 'hsz (list 'LT.lt (list '+ ftok ftok) '(cnodes w))
        (list 'hes 'w (list 'encTy box) 'mm 'tt 'AA 'hw (gets 'q 0)))
  (list 'have 'hfle (list 'LE.le ftok 'mm) '(cnt_mask_le mm (freshF tt)))
  (list 'have 'hsnd (list 'Sound 'chkf 'dec 'encTy 'mm '(thetaD mm) '(maskUF (thetaU mm) (freshF tt)) 'tt box)
        (list 'lemma36 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) 'mm '(thetaD mm) '(maskUF (thetaU mm) (freshF tt)) 'tt box
              (list 'rt_mask 'chkf '(thetaD mm) '(thetaU mm) 'tt box 'hRt '(freshF tt) '(fn [j :- Nat, h :- (Eq Bool ((freshF tt) j) Bool.true)] h))
              '(wf_theta chkf mm)))
  (list 'have 'hv (list 'Exists (list 'fn '[j :- Nat] (list 'And (list 'Nat.le 'j ftok)
                     (list 'And (list 'V 'chkf 'dec 'encTy 'mm 'Exp.tR Gm em 'j 'Sk.cert (list 'Prod.fst Pv))
                                (list 'V 'chkf 'dec 'encTy 'mm '(chkT (Exp.var 0) cA) (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em)
                                      (list '- ftok 'j) 'Sk.unit (list 'Prod.snd Pv))))))
        (list 'hsnd em ftok 'hfle '(tok_sat_mask chkf dec encTy mm mm (freshF tt))))
  '(refine' (exN _ _ hv _)) '(intro j hj)
  (list 'have 'hch (list 'Eq 'Bool (list 'chkf (list 'den 'chkf 'dec 'encTy 'mm '(Exp.var 0) (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.cert (list 'Prod.mk (list 'Prod.fst Pv) em))
                                       (list 'den 'chkf 'dec 'encTy 'mm 'cA (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.syn (list 'Prod.mk (list 'Prod.fst Pv) em))) 'Bool.true)
        (list 'chk_true 'chkf 'dec 'encTy 'mm (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em) (list '- ftok 'j) '(Exp.var 0) 'cA (list 'Prod.snd Pv) '(And.right (And.right hj))))
  (list 'have 'hacc (list 'Eq 'Bool (list 'chkf (list 'Prod.fst Pv) '(encTy A)) 'Bool.true)
        (list 'Eq.trans (list 'Eq.symm (list 'Eq.trans
              (list 'congrArg (list 'fn '[x :- Code] (list 'chkf 'x (list 'den 'chkf 'dec 'encTy 'mm 'cA (list 'List.cons 'Sk 'Sk.cert Gm) 'Sk.syn (list 'Prod.mk (list 'Prod.fst Pv) em))))
                    (list 'den_var0c 'chkf 'dec 'encTy 'mm Gm em (list 'Prod.fst Pv)))
              (list 'congrArg (list 'fn '[y :- Code] (list 'chkf (list 'Prod.fst Pv) 'y))
                    (list 'den_codeOf 'chkf 'dec 'encTy 'mm (list 'List.cons 'Sk 'Sk.cert Gm) (list 'Prod.mk (list 'Prod.fst Pv) em) 'cA '(encTy A) 'hcA))))
              'hch))
  (list 'exact (list 'p43_arith 'M (list 'cnodes (list 'Prod.fst Pv)) 'j ftok '(cnodes w)
                     (list 'hM (list 'Prod.fst Pv) 'hacc '(And.right (And.left (And.right hj)))) '(And.left (And.left (And.right hj))) '(And.left hj) 'hsz))))


;; --- Proposition 4.4 (1): no uniform budget -----------------------------------------------

;; cTerm c: the canonical code term of c (codeOf (cTerm c) = c); ¬ʲ1 (negN j),

;; --- Proposition 4.4 (1), for any closed A with a code above k nodes ---------
(def ^:private A44 'A)
(def ^:private Gk '(List.cons Sk Sk.cert (skels (thetaD k))))
(def ^:private env44 '(Prod.mk v (tokEnvD k)))
(defn- w44 [n] (list 'den 'chkf 'dec 'encTy n 'q Gk 'Sk.cert env44))
(def ^:private boxA (list 'Exp.tSig 'U.u1 'Exp.tR (list 'chkT '(Exp.var 0) (list 'cTerm (list 'encTy A44)))))
(eval (list 'lcert.formal.base/thm 'prop44g_1
  ['chkf :- '(=> Code Code Bool), 'dec :- '(=> Code (Option (Prod Nat (Prod Exp Exp)))), 'encTy :- '(=> Exp Code),
   'hcs :- '(CheckSpec chkf dec encTy), 'hes :- '(TokSize chkf dec), 'hts :- '(TypeSize chkf dec encTy), 'k :- 'Nat, 'A :- 'Exp, 'hcl :- '(Eq Bool (closedTy A) Bool.true), 'hbig :- '(LE.le (+ k 1) (cnodes (encTy A))), 'q :- 'Exp,
   'der :- '(Rt chkf (List.cons Exp Exp.tR (thetaD k)) (List.cons U U.u1 (thetaU k)) q Exp.tR),
   'hmap :- (list 'forall '[n Nat] (list 'forall '[A Exp] (list 'forall '[v Code]
              (list '=> '(Eq Bool (closedTy A) Bool.true) '(Eq Bool (chkf v (encTy A)) Bool.true) '(Eq Bool (lblOk v) Bool.true)
                    (list 'Eq 'Bool (list 'chkf (w44 'n) '(encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy A)))))) 'Bool.true)))))
   'v :- 'Code, 'hv :- (list 'Eq 'Bool (list 'chkf 'v (list 'encTy A44)) 'Bool.true), 'hvok :- '(Eq Bool (lblOk v) Bool.true),
   'hmin :- (list 'forall '[v2 Code] (list '=> (list 'Eq 'Bool (list 'chkf 'v2 (list 'encTy A44)) 'Bool.true) '(Eq Bool (lblOk v2) Bool.true) '(LE.le (cnodes v) (cnodes v2))))]
  'False
  (list 'have 'hw (list 'Eq 'Bool (list 'chkf (w44 '(+ k (cnodes v))) (list 'encTy boxA)) 'Bool.true)
        (list 'hmap '(+ k (cnodes v)) A44 'v 'hcl 'hv 'hvok))
  (list 'have 'h43 (list 'LT.lt '(+ (cnodes v) (cnodes v)) (list 'cnodes (w44 '(+ k (cnodes v)))))
        (list 'prop43_g 'chkf 'dec 'encTy 'hcs 'hes A44 (list 'cTerm (list 'encTy A44)) (list 'cterm_code (list 'encTy A44))
              (list 'box_closed (list 'encTy A44)) (w44 '(+ k (cnodes v))) 'hw '(cnodes v) 'hmin))
  (list 'have 'hsat (list 'EnvSat 'chkf 'dec 'encTy '(+ k (cnodes v)) '(List.cons Exp Exp.tR (thetaD k)) '(List.cons U U.u1 (thetaU k)) env44 '(+ (cnodes v) k))
        '(EnvSat_cons chkf dec encTy (+ k (cnodes v)) Exp.tR (thetaD k) U.u1 (thetaU k) v (tokEnvD k) (cnodes v) k
           (tok_sat chkf dec encTy (+ k (cnodes v)) k) (And.intro (Nat.le_refl (cnodes v)) hvok)))
  (list 'have 'h3 (list 'LE.le (list 'cnodes (w44 '(+ k (cnodes v)))) '(+ (cnodes v) k))
        (list 'theorem3 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) '(+ k (cnodes v)) '(List.cons Exp Exp.tR (thetaD k)) '(List.cons U U.u1 (thetaU k)) 'q 'der
              '(And.intro (Tl.fBase chkf (thetaD k) Exp.tR (Eq.refl$1 Bool.true)) (wf_theta chkf k)) env44 '(+ (cnodes v) k) '(arith6 (cnodes v) k) 'hsat))
  (list 'exact (list 'arith5 '(cnodes v) (list 'cnodes (w44 '(+ k (cnodes v)))) 'k (list 'cnodes (list 'encTy A44)) 'h43 'h3
                     'hbig
                     (list 'cert_size 'chkf 'dec 'encTy 'hcs 'hts A44 'v 'hv)))))

;; --- Proposition 4.4 (2), (3) --------------------------------------------------
(def ^:private bx (fn [A] (list 'Exp.tSig 'U.u1 'Exp.tR (list 'chkT '(Exp.var 0) (list 'cTerm (list 'encTy A))))))
(def ^:private PS '(Sk.prod Sk.cert Sk.unit))
;; out of the box: a value of □A at footprint j carries a certificate of A with at most j nodes
(eval (list 'lcert.formal.base/thm 'box_out_g
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
    G :- (List Sk), e :- (HEnv G), j :- Nat, A :- Exp, c :- (Car (Sk.prod Sk.cert Sk.unit))]
  (list '=> (list 'V 'chkf 'dec 'encTy 'n (bx 'A) 'G 'e 'j PS 'c)
        '(And (LE.le (cnodes (Prod.fst c)) j) (And (Eq Bool (chkf (Prod.fst c) (encTy A)) Bool.true) (Eq Bool (lblOk (Prod.fst c)) Bool.true))))
  '(intro h)
  (list 'have 'h2 (list 'Exists (list 'fn '[j2 :- Nat] (list 'And '(Nat.le j2 j)
                     (list 'And '(V chkf dec encTy n Exp.tR G e j2 Sk.cert (Prod.fst c))
                                (list 'V 'chkf 'dec 'encTy 'n (list 'chkT '(Exp.var 0) '(cTerm (encTy A))) '(List.cons Sk Sk.cert G) '(Prod.mk (Prod.fst c) e) '(- j j2) 'Sk.unit '(Prod.snd c)))))) 'h)
  '(refine' (exN _ _ h2 _)) '(intro j2 hj2)
  (list 'have 'hch (list 'Eq 'Bool (list 'chkf '(den chkf dec encTy n (Exp.var 0) (List.cons Sk Sk.cert G) Sk.cert (Prod.mk (Prod.fst c) e))
                                         '(den chkf dec encTy n (cTerm (encTy A)) (List.cons Sk Sk.cert G) Sk.syn (Prod.mk (Prod.fst c) e))) 'Bool.true)
        '(chk_true chkf dec encTy n (List.cons Sk Sk.cert G) (Prod.mk (Prod.fst c) e) (- j j2) (Exp.var 0) (cTerm (encTy A)) (Prod.snd c) (And.right (And.right hj2))))
  '(constructor)
  '(exact (Nat.le_trans (And.left (And.left (And.right hj2))) (And.left hj2)))
  '(constructor)
  '(exact (Eq.trans (Eq.symm (Eq.trans
            (congrArg (fn [x :- Code] (chkf x (den chkf dec encTy n (cTerm (encTy A)) (List.cons Sk Sk.cert G) Sk.syn (Prod.mk (Prod.fst c) e))))
                      (den_var0c chkf dec encTy n G e (Prod.fst c)))
            (congrArg (fn [y :- Code] (chkf (Prod.fst c) y))
                      (den_codeOf chkf dec encTy n (List.cons Sk Sk.cert G) (Prod.mk (Prod.fst c) e) (cTerm (encTy A)) (encTy A) (cterm_code (encTy A))))))
           hch))
  '(exact (And.right (And.left (And.right hj2))))))

(def ^:private A44x 'A)
(def ^:private bA (bx A44x))
(def ^:private T3 (list 'Exp.tPi 'U.u1 bA (list 'Exp.tSig 'U.u1 bA bA)))
(def ^:private P2 (list 'Sk.prod PS PS))
(def ^:private fq (list 'den 'chkf 'dec 'encTy '(+ k (cnodes v)) 'q '(skels (thetaD k)) (list 'Sk.arr PS P2) '(tokEnvD k)))
(eval (list 'lcert.formal.base/thm 'prop44g_3
  ['chkf :- '(=> Code Code Bool), 'dec :- '(=> Code (Option (Prod Nat (Prod Exp Exp)))), 'encTy :- '(=> Exp Code),
   'hcs :- '(CheckSpec chkf dec encTy), 'hts :- '(TypeSize chkf dec encTy), 'k :- 'Nat, 'A :- 'Exp, 'hbig :- '(LE.le (+ k 1) (cnodes (encTy A))), 'q :- 'Exp,
   'der :- (list 'Rt 'chkf '(thetaD k) '(thetaU k) 'q T3),
   'v :- 'Code, 'hv :- (list 'Eq 'Bool (list 'chkf 'v (list 'encTy A44x)) 'Bool.true), 'hvok :- '(Eq Bool (lblOk v) Bool.true),
   'hmin :- (list 'forall '[v2 Code] (list '=> (list 'Eq 'Bool (list 'chkf 'v2 (list 'encTy A44x)) 'Bool.true) '(Eq Bool (lblOk v2) Bool.true) '(LE.le (cnodes v) (cnodes v2))))]
  'False
  (list 'have 'hs (list 'forall '[j Nat] (list '=> '(Nat.le (+ k j) (+ k (cnodes v))) (list 'forall (vector 'a (list 'Car PS))
            (list '=> (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bA '(skels (thetaD k)) '(tokEnvD k) 'j PS 'a)
                  (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) (list 'Exp.tSig 'U.u1 bA bA) (list 'List.cons 'Sk PS '(skels (thetaD k))) '(Prod.mk a (tokEnvD k)) '(+ k j) P2 (list fq 'a))))))
        (list 'lemma36 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) '(+ k (cnodes v)) '(thetaD k) '(thetaU k) 'q T3 'der '(wf_theta chkf k)
              '(tokEnvD k) 'k '(Nat.le_add_right k (cnodes v)) '(tok_sat chkf dec encTy (+ k (cnodes v)) k)))
  (list 'have 'hout (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) (list 'Exp.tSig 'U.u1 bA bA) (list 'List.cons 'Sk PS '(skels (thetaD k))) '(Prod.mk (Prod.mk v Unit.unit) (tokEnvD k)) '(+ k (cnodes v)) P2 (list fq '(Prod.mk v Unit.unit)))
        (list 'hs '(cnodes v) '(Nat.le_refl (+ k (cnodes v))) '(Prod.mk v Unit.unit)
              (list 'box_in 'chkf 'dec 'encTy '(+ k (cnodes v)) '(skels (thetaD k)) '(tokEnvD k) A44x 'v 'hvok 'hv)))
  (list 'have 'hx (list 'Exists (list 'fn '[j :- Nat] (list 'And '(Nat.le j (+ k (cnodes v)))
                     (list 'And (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bA (list 'List.cons 'Sk PS '(skels (thetaD k))) '(Prod.mk (Prod.mk v Unit.unit) (tokEnvD k)) 'j PS (list 'Prod.fst (list fq '(Prod.mk v Unit.unit))))
                                (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bA (list 'List.cons 'Sk PS (list 'List.cons 'Sk PS '(skels (thetaD k))))
                                      (list 'Prod.mk (list 'Prod.fst (list fq '(Prod.mk v Unit.unit))) '(Prod.mk (Prod.mk v Unit.unit) (tokEnvD k)))
                                      '(- (+ k (cnodes v)) j) PS (list 'Prod.snd (list fq '(Prod.mk v Unit.unit)))))))) 'hout)
  '(refine' (exN _ _ hx _)) '(intro j hj)
  (list 'refine' (list 'arith7 '(cnodes v) 'j '(+ k (cnodes v)) 'k (list 'cnodes (list 'encTy A44x))
     (list 'Nat.le_trans (list 'hmin '_ (list 'And.left (list 'And.right (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ 'j A44x '_ '(And.left (And.right hj))))) (list 'And.right (list 'And.right (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ 'j A44x '_ '(And.left (And.right hj)))))) (list 'And.left (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ 'j A44x '_ '(And.left (And.right hj)))))
     (list 'Nat.le_trans (list 'hmin '_ (list 'And.left (list 'And.right (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ '(- (+ k (cnodes v)) j) A44x '_ '(And.right (And.right hj))))) (list 'And.right (list 'And.right (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ '(- (+ k (cnodes v)) j) A44x '_ '(And.right (And.right hj)))))) (list 'And.left (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ '(- (+ k (cnodes v)) j) A44x '_ '(And.right (And.right hj)))))
     '(And.left hj) '(Eq.refl$1 (+ k (cnodes v)))
     'hbig
     (list 'cert_size 'chkf 'dec 'encTy 'hcs 'hts A44x 'v 'hv)))))

;; (2): no q_A with Θₖ ⊢ q_A :¹ □A ⊸ □□A for every certifiable A — the output's
;; certificate of □A has at most k + ‖v‖ nodes but needs more than 2‖v‖
;; (Proposition 4.3).

(def ^:private bbA (bx bA))
(def ^:private T2 (list 'Exp.tPi 'U.u1 bA bbA))
(def ^:private fq2 (list 'den 'chkf 'dec 'encTy '(+ k (cnodes v)) 'q '(skels (thetaD k)) (list 'Sk.arr PS PS) '(tokEnvD k)))
(eval (list 'lcert.formal.base/thm 'prop44g_2
  ['chkf :- '(=> Code Code Bool), 'dec :- '(=> Code (Option (Prod Nat (Prod Exp Exp)))), 'encTy :- '(=> Exp Code),
   'hcs :- '(CheckSpec chkf dec encTy), 'hes :- '(TokSize chkf dec), 'hts :- '(TypeSize chkf dec encTy), 'k :- 'Nat, 'A :- 'Exp, 'hbig :- '(LE.le (+ k 1) (cnodes (encTy A))), 'q :- 'Exp,
   'der :- (list 'Rt 'chkf '(thetaD k) '(thetaU k) 'q T2),
   'v :- 'Code, 'hv :- (list 'Eq 'Bool (list 'chkf 'v (list 'encTy A44x)) 'Bool.true), 'hvok :- '(Eq Bool (lblOk v) Bool.true),
   'hmin :- (list 'forall '[v2 Code] (list '=> (list 'Eq 'Bool (list 'chkf 'v2 (list 'encTy A44x)) 'Bool.true) '(Eq Bool (lblOk v2) Bool.true) '(LE.le (cnodes v) (cnodes v2))))]
  'False
  (list 'have 'hs (list 'forall '[j Nat] (list '=> '(Nat.le (+ k j) (+ k (cnodes v))) (list 'forall (vector 'a (list 'Car PS))
            (list '=> (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bA '(skels (thetaD k)) '(tokEnvD k) 'j PS 'a)
                  (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bbA (list 'List.cons 'Sk PS '(skels (thetaD k))) '(Prod.mk a (tokEnvD k)) '(+ k j) PS (list fq2 'a))))))
        (list 'lemma36 'chkf 'dec 'encTy 'hcs '(conv_all chkf dec encTy hcs) '(+ k (cnodes v)) '(thetaD k) '(thetaU k) 'q T2 'der '(wf_theta chkf k)
              '(tokEnvD k) 'k '(Nat.le_add_right k (cnodes v)) '(tok_sat chkf dec encTy (+ k (cnodes v)) k)))
  (list 'have 'hout (list 'V 'chkf 'dec 'encTy '(+ k (cnodes v)) bbA (list 'List.cons 'Sk PS '(skels (thetaD k))) '(Prod.mk (Prod.mk v Unit.unit) (tokEnvD k)) '(+ k (cnodes v)) PS (list fq2 '(Prod.mk v Unit.unit)))
        (list 'hs '(cnodes v) '(Nat.le_refl (+ k (cnodes v))) '(Prod.mk v Unit.unit)
              (list 'box_in 'chkf 'dec 'encTy '(+ k (cnodes v)) '(skels (thetaD k)) '(tokEnvD k) A44x 'v 'hvok 'hv)))
  (list 'refine' (list 'arith8 '(cnodes v) (list 'cnodes (list 'Prod.fst (list fq2 '(Prod.mk v Unit.unit)))) 'k (list 'cnodes (list 'encTy A44x))
     (list 'prop43_g 'chkf 'dec 'encTy 'hcs 'hes A44x (list 'cTerm (list 'encTy A44x)) (list 'cterm_code (list 'encTy A44x))
           (list 'box_closed (list 'encTy A44x)) (list 'Prod.fst (list fq2 '(Prod.mk v Unit.unit)))
           (list 'And.left (list 'And.right (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ '(+ k (cnodes v)) bA '_ 'hout)))
           '(cnodes v) 'hmin)
     (list 'And.left (list 'box_out_g 'chkf 'dec 'encTy '(+ k (cnodes v)) '_ '_ '(+ k (cnodes v)) bA '_ 'hout))
     'hbig
     (list 'cert_size 'chkf 'dec 'encTy 'hcs 'hts A44x 'v 'hv)))))

;; --- Proposition 4.4 as R4 states it, at Check ------------------------------
;; □A = Σ(r :₁ R). T(chk′ (print r) ⌜A⌝).  A_{k+1} = arrN (k + 1) is closed,
;; certifiable, and its code has at least k + 1 nodes; a least label-safe
;; certificate of it (least_cert_witness) is minimal among label-safe ones.
(defn- boxF [A] (list 'Exp.tSig 'U.u1 'Exp.tR (list 'chkT '(Exp.var 0) (list 'cTerm (list 'encE A)))))
(def ^:private Ak '(arrN (+ k 1)))
(def ^:private hmin-of-witness
  ;; From hv : least_cert_witness's conclusion for A_{k+1} at v.
  (list 'fn ['v2 :- 'Code
             'h1 :- (list 'Eq 'Bool (list 'Check 'decCert 'v2 (list 'encE Ak)) 'Bool.true)
             'h2 :- '(Eq Bool (lblOk v2) Bool.true)]
    '(Eq.mpr (congrArg (fn [x :- Nat] (LE.le x (cnodes v2))) (And.left (And.right hv)))
             ((And.right (And.right hv)) v2 (And.intro h2 h1)))))

;; (2) No family (q_A) with Θₖ ⊢ q_A :¹ □A ⊸ □□A for every certifiable A.
(eval (list 'lcert.formal.base/thm 'prop44_paper_2 '[k :- Nat]
  (list 'Not (list 'forall '[A Exp]
    (list '=> '(Eq Bool (closedTy A) Bool.true) '(Certifiable A)
      (list 'Exists (list 'fn '[q :- Exp]
        (list 'Rt '(Check decCert) '(thetaD k) '(thetaU k) 'q
              (list 'Exp.tPi 'U.u1 (boxF 'A) (boxF (boxF 'A)))))))))
  '(intro H)
  (list 'have 'hc (list 'Certifiable Ak) '(arrN_certifiable (+ k 1)))
  (list 'refine' (list 'exT 'Exp '_ '_ (list 'H Ak '(arrN_closed (+ k 1)) 'hc) '_))
  '(intro q hq)
  (list 'refine' (list 'exT 'Code '_ '_ (list 'least_cert_witness Ak 'hc) '_))
  '(intro v hv)
  (list 'exact (list 'prop44g_2 '(Check decCert) '(decOf decCert) 'encE '(check_spec decCert)
                     '(check_toksize decCert) '(check_typesize decCert) 'k Ak '(arrN_size (+ k 1))
                     'q 'hq 'v '(And.right (And.left hv)) '(And.left (And.left hv)) hmin-of-witness))))

;; (3) No family (c_A) with Θₖ ⊢ c_A :¹ □A ⊸ □A ⊗ □A for every certifiable A.
(eval (list 'lcert.formal.base/thm 'prop44_paper_3 '[k :- Nat]
  (list 'Not (list 'forall '[A Exp]
    (list '=> '(Eq Bool (closedTy A) Bool.true) '(Certifiable A)
      (list 'Exists (list 'fn '[q :- Exp]
        (list 'Rt '(Check decCert) '(thetaD k) '(thetaU k) 'q
              (list 'Exp.tPi 'U.u1 (boxF 'A) (list 'Exp.tSig 'U.u1 (boxF 'A) (boxF 'A)))))))))
  '(intro H)
  (list 'have 'hc (list 'Certifiable Ak) '(arrN_certifiable (+ k 1)))
  (list 'refine' (list 'exT 'Exp '_ '_ (list 'H Ak '(arrN_closed (+ k 1)) 'hc) '_))
  '(intro q hq)
  (list 'refine' (list 'exT 'Code '_ '_ (list 'least_cert_witness Ak 'hc) '_))
  '(intro v hv)
  (list 'exact (list 'prop44g_3 '(Check decCert) '(decOf decCert) 'encE '(check_spec decCert)
                     '(check_typesize decCert) 'k Ak '(arrN_size (+ k 1))
                     'q 'hq 'v '(And.right (And.left hv)) '(And.left (And.left hv)) hmin-of-witness))))

;; The corollary's arithmetic, over plain variables (omega must not unfold
;; certificate sizes).
(thm cor46_no_uniform_arith [k :- Nat, a :- Nat, b :- Nat, m :- Nat,
                             hle :- (LE.le m k), hsz :- (LT.lt a b),
                             hbig :- (LE.le (+ k 1) a), he :- (Eq Nat b m)]
  False
  (omega))

;; --- Corollary 4.6′, second bullet, as R4 states it -------------------------
;; "No budget serves every such A and B: take A = 1 and B = A_j, with j > k."
;; At every k the instance A = 1, B = A_{k+1} qualifies (1, 1 ⊸ A_{k+1} =
;; A_{k+2} and A_{k+1} are certifiable, and B ≠ A), and no term of
;; □(A ⊸ B) ⊗ □A ⊸ □B exists at Θₖ: one would give μ_L(B) ≤ k
;; (cor46_prime_mu_F7), while every certificate of B exceeds B's code
;; (cert_size), which has more than k nodes (arrN_size).
(def ^:private D2T-k
  '(Exp.tPi U.u1
     (Exp.tSig U.u1 (boxTy (codeTerm (encE (Exp.tPi U.u1 Exp.tUnit (arrN (+ k 1))))))
                    (boxTy (codeTerm (encE Exp.tUnit))))
     (boxTy (codeTerm (encE (arrN (+ k 1)))))))
(eval (list 'lcert.formal.base/thm 'cor46_prime_no_uniform '[k :- Nat]
  (list 'And '(Certifiable (Exp.tPi U.u1 Exp.tUnit (arrN (+ k 1))))
    (list 'And '(Certifiable Exp.tUnit)
      (list 'And '(Certifiable (arrN (+ k 1)))
        (list 'And '(Not (Eq Exp (arrN (+ k 1)) Exp.tUnit))
          (list 'Not (list 'Exists (list 'fn '[tt :- Exp]
            (list 'Rt '(Check decCert) '(thetaD k) '(thetaU k) 'tt D2T-k))))))))
  '(have hne (=> (Eq Exp (arrN (+ k 1)) Exp.tUnit) False)
     (fn [e :- (Eq Exp (arrN (+ k 1)) Exp.tUnit)] (Exp.noConfusion e)))
  '(have hc (Certifiable (arrN (+ k 1))) (arrN_certifiable (+ k 1)))
  '(have hc0 (Certifiable (Exp.tPi U.u1 Exp.tUnit (arrN (+ k 1)))) (arrN_certifiable (+ k 2)))
  '(have hc1 (Certifiable Exp.tUnit) (arrN_certifiable 0))
  '(constructor) '(exact hc0)
  '(constructor) '(exact hc1)
  '(constructor) '(exact hc)
  '(constructor) '(exact hne)
  '(intro hex)
  '(refine' (exT Code _ _ hc0 _)) '(intro c0 s0)
  '(refine' (exT Code _ _ hc1 _)) '(intro c1 s1)
  (list 'have 'iff (list 'forall '[kk Nat]
                     (list 'Iff (list 'Exists (list 'fn '[tt :- Exp]
                                  (list 'Rt '(Check decCert) '(thetaD kk) '(thetaU kk) 'tt D2T-k)))
                           '(LE.le (muCert (arrN (+ k 1)) hc) kk)))
    '(cor46_prime_mu_F7 Exp.tUnit (arrN (+ k 1)) (arrN_closed (+ k 1)) hne
       c0 (And.right s0) (And.left s0) c1 (And.right s1) (And.left s1) hc))
  '(have hle (LE.le (muCert (arrN (+ k 1)) hc) k) (Iff.mp (iff k) hex))
  '(refine' (exT Code _ _ (mu_attained (arrN (+ k 1)) hc) _)) '(intro v hv)
  '(have hsz (LT.lt (cnodes (encE (arrN (+ k 1)))) (cnodes v))
     (cert_size (Check decCert) (decOf decCert) encE (check_spec decCert)
       (check_typesize decCert) (arrN (+ k 1)) v (And.right (And.left hv))))
  '(have hbig (LE.le (+ k 1) (cnodes (encE (arrN (+ k 1))))) (arrN_size (+ k 1)))
  '(have he (Eq Nat (cnodes v) (muCert (arrN (+ k 1)) hc)) (And.right hv))
  '(exact (cor46_no_uniform_arith k (cnodes (encE (arrN (+ k 1)))) (cnodes v)
            (muCert (arrN (+ k 1)) hc) hle hsz hbig he))))

;; (1) No q with Θₖ, x :₁ R ⊢ q :¹ R maps every certificate of every closed A
;; to a certificate of □A (in the model, at every cap n).
(def ^:private hmap-F7
  (list 'forall '[n Nat] (list 'forall '[A Exp] (list 'forall '[v Code]
    (list '=> '(Eq Bool (closedTy A) Bool.true) '(Eq Bool (Check decCert v (encE A)) Bool.true)
          '(Eq Bool (lblOk v) Bool.true)
          (list 'Eq 'Bool
                (list 'Check 'decCert
                      '(den (Check decCert) (decOf decCert) encE n q
                            (List.cons Sk Sk.cert (skels (thetaD k))) Sk.cert (Prod.mk v (tokEnvD k)))
                      '(encE (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encE A))))))
                'Bool.true))))))
(eval (list 'lcert.formal.base/thm 'prop44_paper_1
  '[k :- Nat, q :- Exp,
    der :- (Rt (Check decCert) (List.cons Exp Exp.tR (thetaD k)) (List.cons U U.u1 (thetaU k)) q Exp.tR)]
  (list 'Not hmap-F7)
  '(intro hmap)
  (list 'have 'hc (list 'Certifiable Ak) '(arrN_certifiable (+ k 1)))
  (list 'refine' (list 'exT 'Code '_ '_ (list 'least_cert_witness Ak 'hc) '_))
  '(intro v hv)
  (list 'exact (list 'prop44g_1 '(Check decCert) '(decOf decCert) 'encE '(check_spec decCert)
                     '(check_toksize decCert) '(check_typesize decCert) 'k Ak '(arrN_closed (+ k 1))
                     '(arrN_size (+ k 1)) 'q 'der 'hmap
                     'v '(And.right (And.left hv)) '(And.left (And.left hv)) hmin-of-witness))))
