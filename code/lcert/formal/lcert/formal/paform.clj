(ns lcert.formal.paform
  "F6.3c — formation of R4 §6.2's translations, at the meta level.

  Good D rho asks that every image of rho look up a Nat entry in D; it
  permits aliases and arbitrary other entries. allW and allZ have exactly
  D's length. trT_tl, trT_rt and trF_form prove, for every Good environment,
  respectively the type-level Nat typing, runtime Nat typing at allW, and
  formation of every translated formula. The inductions generalize D and
  rho, since hypothesis binders shift rho and quantifiers extend it.

  Closed constants are weakened from nil with Lemma 2.1. Their runtime
  vector is allZ, so applying a nonzero-usage constant to an allW argument
  consumes allW. No usage subsumption rule is added."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.patyp :as pt]
            [lcert.formal.dtgen :as g]
            [lcert.formal.derivations]))

(kdef Good (=> (List Exp) (=> Nat Nat) Prop)
  (fn [D :- (List Exp), ρ :- (=> Nat Nat)]
    (forall [i Nat] (Eq (Option Exp) (nthE D (ρ i)) (Option.some Exp Exp.tNat)))))

(doseq [[nm u] '[[allW U.uw] [allZ U.u0]]]
  (b/kdef! nm '(=> (List Exp) (List U))
    (list 'fn '[D :- (List Exp)]
      (list 'List.rec$1$0 'Exp '(fn [_ :- (List Exp)] (List U)) '(List.nil U)
        (list 'fn '[A :- Exp, tail :- (List Exp), ih :- (List U)]
          (list 'List.cons 'U u 'ih)) 'D))))

;; Good is preserved by the two binders in trF. nthE is well-founded, so
;; use its proved equations rather than assuming that it reduces on D.
(thm paGood_suc [D :- (List Exp), ρ :- (=> Nat Nat), h :- (Good D ρ), A :- Exp]
  (Good (List.cons Exp A D) (trSucR ρ))
  (intro i) (exact (Eq.trans (nthE.eq_3 A D (ρ i)) (h i))))
(thm paGood_up [D :- (List Exp), ρ :- (=> Nat Nat), h :- (Good D ρ)]
  (Good (List.cons Exp Exp.tNat D) (trUpR ρ))
  (intro i) (cases i)
  (exact (nthE.eq_2 Exp.tNat D))
  (exact (Eq.trans (nthE.eq_3 Exp.tNat D (ρ n)) (h n))))

(thm paAllW_len [D :- (List Exp)] (Eq Nat (lenU (allW D)) (lenE D))
  (induction D) (rfl) (exact (congrArg Nat.succ ih_tail)))
(thm paAllW_nth [D :- (List Exp)]
  (forall [i Nat] (forall [A Exp]
    (=> (Eq (Option Exp) (nthE D i) (Option.some Exp A))
        (Eq (Option U) (nthU (allW D) i) (Option.some U U.uw)))))
  (induction D)
  (intro i A h)
  (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  (intro i A) (cases i)
  (intro h) (exact (nthU.eq_2 U.uw (allW tail)))
  (intro h)
  (exact (Eq.trans (nthU.eq_3 U.uw (allW tail) n)
           (ih_tail n A (Eq.trans (Eq.symm (nthE.eq_3 head tail n)) h)))))

(thm paW_mul [r :- U, h :- (Eq Bool (nonzero r) Bool.true)]
  (Eq U (umul r U.uw) U.uw)
  (cases r) (exact (Bool.noConfusion h)) (rfl) (rfl))
(thm paAllW_add [D :- (List Exp)]
  (Eq (List U) (vadd (allW D) (allW D)) (allW D))
  (induction D) (rfl)
  (have e (Eq (List U) (List.cons U U.uw (vadd (allW tail) (allW tail)))
                        (List.cons U U.uw (allW tail)))
    (congrArg (fn [xs :- (List U)] (List.cons U U.uw xs)) ih_tail))
  (exact e))
(thm paAllW_scale [D :- (List Exp), r :- U, h :- (Eq Bool (nonzero r) Bool.true)]
  (Eq (List U) (vscale r (allW D)) (allW D))
  (induction D) (rfl)
  (exact (Eq.trans
    (congrArg (fn [u :- U] (List.cons U u (vscale r (allW tail)))) (paW_mul r h))
    (congrArg (fn [xs :- (List U)] (List.cons U U.uw xs)) ih_tail))))
(thm paZeroW_add [D :- (List Exp)]
  (Eq (List U) (vadd (allZ D) (allW D)) (allW D))
  (induction D) (rfl)
  (have e (Eq (List U) (List.cons U U.uw (vadd (allZ tail) (allW tail)))
                        (List.cons U U.uw (allW tail)))
    (congrArg (fn [xs :- (List U)] (List.cons U U.uw xs)) ih_tail))
  (exact e))
(thm paAllW_app [D :- (List Exp), r :- U, h :- (Eq Bool (nonzero r) Bool.true)]
  (Eq (List U) (vadd (allW D) (vscale r (allW D))) (allW D))
  (exact (Eq.trans (congrArg (vadd (allW D)) (paAllW_scale D r h)) (paAllW_add D))))
(thm paZeroW_app [D :- (List Exp), r :- U, h :- (Eq Bool (nonzero r) Bool.true)]
  (Eq (List U) (vadd (allZ D) (vscale r (allW D))) (allW D))
  (exact (Eq.trans (congrArg (vadd (allZ D)) (paAllW_scale D r h)) (paZeroW_add D))))

;; Iterated weakening of a closed term and type, characterized here by
;; being fixed by lift 1 0. Theorems below instantiate this with patr's
;; arithmetic constants; no well-formedness of unused entries is needed.
(thm paClosed_tl [chkf :- (=> Code Code Bool), w :- Bool, e :- Exp, A :- Exp,
                  he :- (Eq Exp (lift 1 0 e) e), hA :- (Eq Exp (lift 1 0 A) A),
                  h :- (Tl chkf w (List.nil Exp) e A), D :- (List Exp)]
  (Tl chkf w D e A)
  (induction D) (exact h)
  (exact (tl_cast3 chkf w (List.cons Exp head tail) (lift 1 0 e) e (lift 1 0 A) A
           (tl_weaken chkf w tail e A ih_tail 0 head) he hA)))
(thm paClosed_rt [chkf :- (=> Code Code Bool), e :- Exp, A :- Exp,
                  he :- (Eq Exp (lift 1 0 e) e), hA :- (Eq Exp (lift 1 0 A) A),
                  h :- (Rt chkf (List.nil Exp) (List.nil U) e A), D :- (List Exp)]
  (Rt chkf D (allZ D) e A)
  (induction D) (exact h)
  (exact (rt_cast_t chkf (List.cons Exp head tail) (List.cons U U.u0 (allZ tail))
           (lift 1 0 e) e (lift 1 0 A) A
           (rt_weaken chkf tail (allZ tail) e A ih_tail 0 head) he hA)))

;; Typed copies of EQ, PLUS and TIMES in any telescope. Their types are
;; closed, so both lift equations are reflexivity proofs.
(doseq [[nm form] (drop 2 pt/signatures)
        mode [:rt :tl]]
  (let [e (symbol (str "tr" nm)), A (g/sx (g/compile-term pt/constants form))
        closed (symbol (str e (if (= mode :rt) "_rt" "_tl")))
        pn (symbol (str closed "_at"))
        stmt (if (= mode :rt) (list 'Rt 'chkf 'D '(allZ D) e A)
                 (list 'Tl 'chkf 'Bool.false 'D e A))
        pf (if (= mode :rt)
             (list 'paClosed_rt 'chkf e A 'rfl 'rfl (list closed 'chkf) 'D)
             (list 'paClosed_tl 'chkf 'Bool.false e A 'rfl 'rfl (list closed 'chkf) 'D))]
    (a/prove-theorem pn '[chkf :- (=> Code Code Bool), D :- (List Exp)] stmt
      [(list 'exact pf)])))

;; Nondependent binary applications of the three arithmetic constants.
;; Keeping Nat/Bool concrete lets the kernel reduce every substitution in
;; the function type, without conversion lemmas about schematic types.
(doseq [[nm r X] '[[trPLUS U.u1 Exp.tNat] [trTIMES U.uw Exp.tNat] [trEQ U.uw Exp.tBool]]
        mode [:tl :rt]]
  (let [params '[chkf :- (=> Code Code Bool), D :- (List Exp), x :- Exp, y :- Exp]
        nat 'Exp.tNat
        C (list 'Exp.tPi 'U.uw nat X)
        appx (list 'Exp.app nm 'x)
        term (list 'Exp.app appx 'y)
        form (fn [D A] (list 'Tl.fBase 'chkf D A '(Eq.refl Bool.true)))
        formC (list 'Tl.fPi 'chkf '(List.cons Exp Exp.tNat D) 'U.uw nat X
                    (form '(List.cons Exp Exp.tNat D) nat)
                    (form '(List.cons Exp Exp.tNat (List.cons Exp Exp.tNat D)) X))
        natD (form 'D nat)
        XD (form '(List.cons Exp Exp.tNat D) X)
        typ (fn [e A] (if (= mode :tl) (list 'Tl 'chkf 'Bool.false 'D e A)
                         (list 'Rt 'chkf 'D '(allW D) e A)))
        pn (symbol (str nm (if (= mode :tl) "_tl_app2" "_rt_app2")))
        constpf (list (symbol (str nm (if (= mode :tl) "_tl_at" "_rt_at"))) 'chkf 'D)
        cast (fn [us e A h eq]
               (list 'rt_cast 'chkf 'D us '(allW D) e A A h eq 'rfl))
        one (if (= mode :tl)
              (list 'Tl.zApp 'chkf 'D r nm 'x nat C constpf 'hx natD formC)
              (cast (list 'vadd '(allZ D) (list 'vscale r '(allW D))) appx C
                (list 'Rt.rApp 'chkf 'D '(allZ D) '(allW D) r nm 'x nat C
                      '(Eq.refl Bool.true) constpf 'hx natD formC)
                (list 'paZeroW_app 'D r '(Eq.refl Bool.true))))
        two (if (= mode :tl)
              (list 'Tl.zApp 'chkf 'D 'U.uw appx 'y nat X one 'hy natD XD)
              (cast '(vadd (allW D) (vscale U.uw (allW D))) term X
                (list 'Rt.rApp 'chkf 'D '(allW D) '(allW D) 'U.uw appx 'y nat X
                      '(Eq.refl Bool.true) one 'hy natD XD)
                '(paAllW_app D U.uw (Eq.refl Bool.true))))]
    (a/prove-theorem pn (into params ['hx :- (typ 'x nat) 'hy :- (typ 'y nat)])
      (typ term X) [(list 'exact two)])))

;; R4's term-formation induction. A Good lookup has stored type Nat, which
;; remains Nat after the variable rule lifts it by rho(i)+1.
(thm trT_tl [chkf :- (=> Code Code Bool), t :- PT]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)]
    (=> (Good D ρ) (Tl chkf Bool.false D (trT t ρ) Exp.tNat))))
  (induction t)
  (intro D ρ h) (exact (Tl.zVar chkf D (ρ i) Exp.tNat (h i)))
  (intro D ρ h) (exact (Tl.zConst chkf D Exp.zero Exp.tNat (Eq.refl Bool.true)))
  (intro D ρ h) (exact (Tl.zSucc chkf D (trT t ρ) (ih_t D ρ h)))
  (intro D ρ h) (exact (trPLUS_tl_app2 chkf D (trT t ρ) (trT u ρ) (ih_t D ρ h) (ih_u D ρ h)))
  (intro D ρ h) (exact (trTIMES_tl_app2 chkf D (trT t ρ) (trT u ρ) (ih_t D ρ h) (ih_u D ρ h))))
(thm trT_rt [chkf :- (=> Code Code Bool), t :- PT]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)]
    (=> (Good D ρ) (Rt chkf D (allW D) (trT t ρ) Exp.tNat))))
  (induction t)
  (intro D ρ h)
  (exact (Rt.rVar chkf D (allW D) (ρ i) Exp.tNat U.uw (paAllW_len D)
           (h i) (paAllW_nth D (ρ i) Exp.tNat (h i)) (Eq.refl Bool.true)))
  (intro D ρ h) (exact (Rt.rConst chkf D (allW D) Exp.zero Exp.tNat (paAllW_len D) (Eq.refl Bool.true)))
  (intro D ρ h) (exact (Rt.rSucc chkf D (allW D) (trT t ρ) (ih_t D ρ h)))
  (intro D ρ h) (exact (trPLUS_rt_app2 chkf D (trT t ρ) (trT u ρ) (ih_t D ρ h) (ih_u D ρ h)))
  (intro D ρ h) (exact (trTIMES_rt_app2 chkf D (trT t ρ) (trT u ρ) (ih_t D ρ h) (ih_u D ρ h))))

;; R4's formula-formation induction. An implication extends the telescope
;; by a proof variable; a universal quantifier adds a Nat variable.
(thm trF_form [chkf :- (=> Code Code Bool), φ :- PF]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)]
    (=> (Good D ρ) (Tl chkf Bool.true D (trF φ ρ) Exp.tUnit))))
  (induction φ)
  (intro D ρ h)
  (exact (Tl.fT chkf D (Exp.app (Exp.app trEQ (trT t ρ)) (trT u ρ))
           (trEQ_tl_app2 chkf D (trT t ρ) (trT u ρ) (trT_tl chkf t D ρ h) (trT_tl chkf u D ρ h))))
  (intro D ρ h) (exact (Tl.fBase chkf D Exp.tEmpty (Eq.refl Bool.true)))
  (intro D ρ h)
  (exact (Tl.fPi chkf D U.uw (trF p ρ) (trF q (trSucR ρ)) (ih_p D ρ h)
           (ih_q (List.cons Exp (trF p ρ) D) (trSucR ρ) (paGood_suc D ρ h (trF p ρ)))))
  (intro D ρ h)
  (exact (Tl.fPi chkf D U.uw Exp.tNat (trF p (trUpR ρ))
           (Tl.fBase chkf D Exp.tNat (Eq.refl Bool.true))
           (ih_p (List.cons Exp Exp.tNat D) (trUpR ρ) (paGood_up D ρ h)))))
