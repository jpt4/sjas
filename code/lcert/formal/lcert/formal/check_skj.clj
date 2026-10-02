(ns lcert.formal.check-skj
  "F7 — skeleton derivation trees and their Boolean checker (R4 §1.5–1.6).

  SkDT has one data constructor for each of SkJ's 40 rules. It records
  hidden skeletons (application's domain, let's product components, and
  branch-list codomains) and premise trees. skOf is intentionally partial
  on branch lists, so inferring all annotations with skOf would restrict
  the existing SkJ relation. No such restriction is made here.

  Contexts are supplied to the checker, rather than redundantly stored in
  each node. Each premise is checked in exactly its rule's context, including
  the one-, two-, and five-variable extensions. A record is untrusted data:
  its mode, expression, skeleton, side conditions and every premise are
  checked. Soundness is induction on SkDT, generated one lemma per rule.
  The generator installs no axioms and does not alter SkJ or skOf."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]))

;; Reduce skeleton equality to the already proved expression equality by
;; a retraction into types. This is only a checker representation, not a
;; change to skeletons or to the paper's expression encoding.
(kdef f7SkExp (=> Sk Exp)
  (fn [s :- Sk]
    (Sk.rec$1 (fn [_ :- Sk] Exp)
      Exp.tUnit Exp.tBool Exp.tNat Exp.tLbl Exp.tSyn Exp.tDia Exp.tR
      (fn [x :- Sk, y :- Sk, ix :- Exp, iy :- Exp] (Exp.tPi U.u1 ix iy))
      (fn [x :- Sk, y :- Sk, ix :- Exp, iy :- Exp] (Exp.tSig U.u1 ix iy)) s)))
(thm f7SkExp_skel [s :- Sk] (Eq Sk (skel (f7SkExp s)) s)
  (induction s)
  (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl)
  (exact (Eq.trans (congrArg (fn [v :- Sk] (Sk.arr v (skel (f7SkExp t)))) ih_s)
                  (congrArg (fn [v :- Sk] (Sk.arr s v)) ih_t)))
  (exact (Eq.trans (congrArg (fn [v :- Sk] (Sk.prod v (skel (f7SkExp t)))) ih_s)
                  (congrArg (fn [v :- Sk] (Sk.prod s v)) ih_t))))
(kdef f7SkEq (=> Sk Sk Bool)
  (fn [s :- Sk, t :- Sk] (expEq (f7SkExp s) (f7SkExp t))))
(thm f7SkEq_sound [s :- Sk, t :- Sk, h :- (Eq Bool (f7SkEq s t) Bool.true)]
  (Eq Sk s t)
  (exact (Eq.trans (Eq.symm (f7SkExp_skel s))
    (Eq.trans (congrArg skel (expEq_sound (f7SkExp s) (f7SkExp t) h)) (f7SkExp_skel t)))))

(kdef f7BoolEq (=> Bool Bool Bool)
  (fn [x :- Bool, y :- Bool]
    (Bool.rec$1 (fn [_ :- Bool] Bool) (Bool.not y) y x)))
(thm f7BoolEq_sound [x :- Bool, y :- Bool]
  (=> (Eq Bool (f7BoolEq x y) Bool.true) (Eq Bool x y))
  (cases x) (cases y)
  (intro h) (rfl) (intro h) (exact (Bool.noConfusion h))
  (cases y) (intro h) (exact (Bool.noConfusion h)) (intro h) (rfl))

(kdef f7OptSk (=> (Option Sk) Sk Bool)
  (fn [o :- (Option Sk), x :- Sk]
    (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] Bool) Bool.false
      (fn [v :- Sk] (f7SkEq v x)) o)))
(thm f7OptSk_sound [o :- (Option Sk), x :- Sk]
  (=> (Eq Bool (f7OptSk o x) Bool.true) (Eq (Option Sk) o (Option.some Sk x)))
  (cases o) (intro h) (exact (Bool.noConfusion h))
  (intro h) (exact (congrArg (fn [v :- Sk] (Option.some Sk v)) (f7SkEq_sound _ x h))))

;; SkJ's 40 signatures, in constructor order. The only changes made when
;; generating SkDT are removal of G and proof-valued side conditions, and
;; replacement of premise derivations by data trees. Soundness applies the
;; original SkJ constructors; the kernel checks every generated case.
(def skj-rules
  '[(wEmpty [G (List Sk)] :where [Bool.true G Exp.tEmpty Sk.unit])
  (wUnit [G (List Sk)] :where [Bool.true G Exp.tUnit Sk.unit])
  (wBool [G (List Sk)] :where [Bool.true G Exp.tBool Sk.unit])
  (wNat [G (List Sk)] :where [Bool.true G Exp.tNat Sk.unit])
  (wLbl [G (List Sk)] :where [Bool.true G Exp.tLbl Sk.unit])
  (wSyn [G (List Sk)] :where [Bool.true G Exp.tSyn Sk.unit])
  (wDia [G (List Sk)] :where [Bool.true G Exp.tDia Sk.unit])
  (wR [G (List Sk)] :where [Bool.true G Exp.tR Sk.unit])
  (wT [G (List Sk)] [b Exp] [hb (SkJ Bool.false G b Sk.bool)] :where [Bool.true G (Exp.tT b) Sk.unit])
  (wPi [G (List Sk)] [r U] [A Exp] [B Exp] [hA (SkJ Bool.true G A Sk.unit)]
       [hB (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit)] :where [Bool.true G (Exp.tPi r A B) Sk.unit])
  (wSig [G (List Sk)] [r U] [A Exp] [B Exp] [hA (SkJ Bool.true G A Sk.unit)]
        [hB (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit)] :where [Bool.true G (Exp.tSig r A B) Sk.unit])
  ;; terms
  (sVar [G (List Sk)] [i Nat] [s Sk] [h (Eq (Option Sk) (nthS G i) (Option.some Sk s))] :where [Bool.false G (Exp.var i) s])
  (sStar [G (List Sk)] :where [Bool.false G Exp.star Sk.unit])
  (sAbort [G (List Sk)] [A Exp] [t Exp] [hA (SkJ Bool.true G A Sk.unit)] [ht (SkJ Bool.false G t Sk.unit)]
          :where [Bool.false G (Exp.abort A t) (skel A)])
  (sTT [G (List Sk)] :where [Bool.false G Exp.tt Sk.bool])
  (sFF [G (List Sk)] :where [Bool.false G Exp.ff Sk.bool])
  (sIte [G (List Sk)] [b Exp] [t Exp] [e Exp] [s Sk] [hb (SkJ Bool.false G b Sk.bool)] [ht (SkJ Bool.false G t s)]
        [he (SkJ Bool.false G e s)] :where [Bool.false G (Exp.ite b t e) s])
  (sElimB [G (List Sk)] [P Exp] [b Exp] [t Exp] [e Exp] [hP (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit)]
          [hb (SkJ Bool.false G b Sk.bool)] [ht (SkJ Bool.false G t (skel P))] [he (SkJ Bool.false G e (skel P))]
          :where [Bool.false G (Exp.elimB P b t e) (skel P)])
  (sZero [G (List Sk)] :where [Bool.false G Exp.zero Sk.nat])
  (sSucc [G (List Sk)] [n Exp] [h (SkJ Bool.false G n Sk.nat)] :where [Bool.false G (Exp.succ n) Sk.nat])
  (sRecN [G (List Sk)] [P Exp] [z Exp] [st Exp] [n Exp] [hP (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit)]
         [hz (SkJ Bool.false G z (skel P))] [hs (SkJ Bool.false (sk2 (skel P) Sk.nat G) st (skel P))]
         [hn (SkJ Bool.false G n Sk.nat)] :where [Bool.false G (Exp.recN P z st n) (skel P)])
  (sLbl [G (List Sk)] [l Nat] :where [Bool.false G (Exp.lbl l) Sk.lbl])
  (sCaseL [G (List Sk)] [P Exp] [x Exp] [bs Exp] [hP (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit)]
          [hx (SkJ Bool.false G x Sk.lbl)] [hb (SkJ Bool.false G bs (Sk.arr Sk.lbl (skel P)))] :where [Bool.false G (Exp.caseL P x bs) (skel P)])
  (sBnil [G (List Sk)] [s Sk] :where [Bool.false G Exp.bnil (Sk.arr Sk.lbl s)])
  (sBcons [G (List Sk)] [h Exp] [t Exp] [s Sk] [hh (SkJ Bool.false G h s)] [ht (SkJ Bool.false G t (Sk.arr Sk.lbl s))]
          :where [Bool.false G (Exp.bcons h t) (Sk.arr Sk.lbl s)])
  (sSleaf [G (List Sk)] [x Exp] [h (SkJ Bool.false G x Sk.lbl)] :where [Bool.false G (Exp.sleaf x) Sk.syn])
  (sSnode [G (List Sk)] [x Exp] [c1 Exp] [c2 Exp] [hx (SkJ Bool.false G x Sk.lbl)] [h1 (SkJ Bool.false G c1 Sk.syn)]
          [h2 (SkJ Bool.false G c2 Sk.syn)] :where [Bool.false G (Exp.snode x c1 c2) Sk.syn])
  (sRecS [G (List Sk)] [P Exp] [tl Exp] [tn Exp] [c Exp] [hP (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
         [hl (SkJ Bool.false (List.cons Sk Sk.lbl G) tl (skel P))]
         [hn (SkJ Bool.false (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn (skel P))]
         [hc (SkJ Bool.false G c Sk.syn)] :where [Bool.false G (Exp.recS P tl tn c) (skel P)])
  (sLeaf [G (List Sk)] [x Exp] [h (SkJ Bool.false G x Sk.lbl)] :where [Bool.false G (Exp.leaf x) Sk.cert])
  (sNode [G (List Sk)] [d Exp] [x Exp] [r1 Exp] [r2 Exp] [hd (SkJ Bool.false G d Sk.dia)] [hx (SkJ Bool.false G x Sk.lbl)]
         [h1 (SkJ Bool.false G r1 Sk.cert)] [h2 (SkJ Bool.false G r2 Sk.cert)] :where [Bool.false G (Exp.node d x r1 r2) Sk.cert])
  (sItR [G (List Sk)] [X Exp] [g Exp] [h Exp] [r Exp] [hX (SkJ Bool.true G X Sk.unit)]
        [hg (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))]
        [hh (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))]
        [hr (SkJ Bool.false G r Sk.cert)] :where [Bool.false G (Exp.itR X g h r) (skel X)])
  (sPrn [G (List Sk)] [r Exp] [h (SkJ Bool.false G r Sk.cert)] :where [Bool.false G (Exp.prn r) Sk.syn])
  (sLam [G (List Sk)] [r U] [A Exp] [t Exp] [s Sk] [hA (SkJ Bool.true G A Sk.unit)]
        [ht (SkJ Bool.false (List.cons Sk (skel A) G) t s)] :where [Bool.false G (Exp.lam r A t) (Sk.arr (skel A) s)])
  (sApp [G (List Sk)] [f Exp] [u Exp] [s Sk] [t Sk] [hf (SkJ Bool.false G f (Sk.arr s t))] [hu (SkJ Bool.false G u s)]
        :where [Bool.false G (Exp.app f u) t])
  (sPair [G (List Sk)] [r U] [A Exp] [B Exp] [x Exp] [y Exp] [hS (SkJ Bool.true G (Exp.tSig r A B) Sk.unit)]
         [hx (SkJ Bool.false G x (skel A))] [hy (SkJ Bool.false G y (skel B))]
         :where [Bool.false G (Exp.pair (Exp.tSig r A B) x y) (Sk.prod (skel A) (skel B))])
  (sLetp [G (List Sk)] [C Exp] [p Exp] [t Exp] [s1 Sk] [s2 Sk] [hC (SkJ Bool.true G C Sk.unit)]
         [hp (SkJ Bool.false G p (Sk.prod s1 s2))] [ht (SkJ Bool.false (sk2 s2 s1 G) t (skel C))]
         :where [Bool.false G (Exp.letp C p t) (skel C)])
  (sChk [G (List Sk)] [c Exp] [d Exp] [hc (SkJ Bool.false G c Sk.syn)] [hd (SkJ Bool.false G d Sk.syn)]
        :where [Bool.false G (Exp.chk c d) Sk.bool])
  (sH1 [G (List Sk)] [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp] [hr (SkJ Bool.false G r Sk.cert)]
       [hs (SkJ Bool.false G s Sk.cert)] [hc (SkJ Bool.false G c Sk.syn)] [h1 (SkJ Bool.false G e1 Sk.unit)]
       [h2 (SkJ Bool.false G e2 Sk.unit)] :where [Bool.false G (Exp.h1 r s c e1 e2) Sk.unit])
  (sRefl [G (List Sk)] [D Exp] [r Exp] [e Exp] [hD (SkJ Bool.true G D Sk.unit)]
         [hb (Eq Bool (isBaseTy D) Bool.true)] [hr (SkJ Bool.false G r Sk.cert)]
         [he (SkJ Bool.false G e Sk.unit)] :where [Bool.false G (Exp.refl D r e) (skel D)])
  (sInsp [G (List Sk)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp] [hX (SkJ Bool.true G X Sk.unit)]
         [hr (SkJ Bool.false G r Sk.cert)] [hc (SkJ Bool.false G c Sk.syn)]
         [h1 (SkJ Bool.false (sk2 Sk.unit Sk.cert G) t1 (skel X))] [h2 (SkJ Bool.false (sk2 Sk.unit Sk.cert G) t2 (skel X))]
         :where [Bool.false G (Exp.insp X r c t1 t2) (skel X)])])

(defn- premise? [[_ ty]] (and (seq? ty) (= 'SkJ (first ty))))
(defn- tree-fields [rule]
  (vec (for [[x ty :as field] (h/f7-fields rule)
             :when (and (not= x 'G) (not (h/f7-side? field)))]
         [x (if (premise? field) 'SkDT ty)])))
(defn- premises [rule] (vec (filter premise? (h/f7-fields rule))))
(defn- sides [rule] (vec (filter h/f7-side? (h/f7-fields rule))))
(defn- ih-name [x] (symbol (str "ih_" x)))
(defn- side-check [[_ [_ ty lhs rhs] :as field]]
  (if (= ty '(Option Sk)) (list 'f7OptSk lhs (last rhs)) (h/f7-side-check field)))
(defn- side-proof [[_ [_ ty lhs rhs] :as field] accepted]
  (if (= ty '(Option Sk)) (list 'f7OptSk_sound lhs (last rhs) accepted)
    (h/f7-side-proof field accepted)))

(eval (list* 'a/inductive 'SkDT []
        (for [rule skj-rules] (list* (first rule) (tree-fields rule)))))

(def ^:private check-ty '(=> Bool (List Sk) Exp Sk Bool))
(defn- rule-checks [rule premise-call]
  (let [[mode _ e s] (last rule)]
    (vec (concat [(list 'f7BoolEq 'ww mode) (list 'expEq 'ee e) (list 'f7SkEq 'ss s)]
           (map side-check (sides rule))
           (for [[x [_ & args]] (premises rule)] (premise-call x args))))))

;; Recurse on data in Type, returning a checking function. a/defn over a
;; large inductive can generate enormous equation-lemma families; explicit
;; recursors keep reduction structural and the kernel equations definitional.
(b/kdef! 'skjCheckF '(=> SkDT Bool (List Sk) Exp Sk Bool)
  (list 'fn '[tree :- SkDT]
    (apply list 'SkDT.rec$1 (list 'fn '[_ :- SkDT] check-ty)
      (concat
        (for [rule skj-rules]
          (let [fields (concat (tree-fields rule) (for [[x _] (premises rule)] [(ih-name x) check-ty]))
                body (list 'fn '[ww :- Bool, G :- (List Sk), ee :- Exp, ss :- Sk]
                       (h/f7-and (rule-checks rule (fn [x args] (apply list (ih-name x) args)))))]
            (if (seq fields) (list 'fn (h/f7-params fields) body) body))) ['tree]))))
(kdef skjCheck (=> Bool (List Sk) Exp Sk SkDT Bool)
  (fn [w :- Bool, G :- (List Sk), e :- Exp, s :- Sk, tree :- SkDT]
    (skjCheckF tree w G e s)))

(thm f7SkJ_transport [w :- Bool, w2 :- Bool, G :- (List Sk), e :- Exp, e2 :- Exp,
                      s :- Sk, s2 :- Sk, hw :- (Eq Bool w w2), he :- (Eq Exp e e2),
                      hs :- (Eq Sk s s2), der :- (SkJ w2 G e2 s2)]
  (SkJ w G e s)
  (exact (Eq.mp (congrArg (fn [v :- Bool] (SkJ v G e s)) (Eq.symm hw))
    (Eq.mp (congrArg (fn [v :- Exp] (SkJ w2 G v s)) (Eq.symm he))
      (Eq.mp (congrArg (fn [v :- Sk] (SkJ w2 G e2 v)) (Eq.symm hs)) der)))))

(defn- sound-motive [tree]
  (list 'forall '[ww Bool] (list 'forall '[G (List Sk)] (list 'forall '[ee Exp] (list 'forall '[ss Sk]
    (list '=> (list 'Eq 'Bool (list 'skjCheck 'ww 'G 'ee 'ss tree) 'Bool.true)
      '(SkJ ww G ee ss)))))))

;; A lemma per original SkJ rule. Side conditions are extracted from the
;; Boolean conjunction, premise IHs supply actual SkJ derivations, then
;; expression/skeleton/mode equalities transport the conclusion. In
;; particular no unchecked skeleton inference or context coercion is used.
(doseq [rule skj-rules]
  (let [nm (first rule), fields (tree-fields rule), prems (premises rule), side (sides rule)
        tree (h/f7-app (symbol (str "SkDT." nm)) (map first fields))
        checks (rule-checks rule (fn [x args] (apply list 'skjCheck (concat args [x]))))
        proofs (merge
          (into {} (map-indexed (fn [i field]
            [(first field) (side-proof field (h/f7-projection checks (+ 3 i) 'accepted))]) side))
          (into {} (map-indexed (fn [i [x [_ & args]]]
            [x (apply list (ih-name x) (concat args [(h/f7-projection checks (+ 3 (count side) i) 'accepted)]))]) prems)))
        [mode _ e s] (last rule)
        proof (apply list (symbol (str "SkJ." nm))
                (for [[x _ :as field] (h/f7-fields rule)]
                  (if (or (premise? field) (h/f7-side? field)) (proofs x) x)))]
    (a/prove-theorem (symbol (str "skjCheck_" nm "_sound"))
      (lv (h/f7-params (concat fields (for [[x _] prems] [(ih-name x) (sound-motive x)])
        [['ww 'Bool] ['G '(List Sk)] ['ee 'Exp] ['ss 'Sk]
         ['accepted (list 'Eq 'Bool (list 'skjCheck 'ww 'G 'ee 'ss tree) 'Bool.true)]])))
      '(SkJ ww G ee ss)
      (lv [(list 'exact (list 'f7SkJ_transport 'ww mode 'G 'ee e 'ss s
                (list 'f7BoolEq_sound 'ww mode (h/f7-projection checks 0 'accepted))
                (list 'expEq_sound 'ee e (h/f7-projection checks 1 'accepted))
                (list 'f7SkEq_sound 'ss s (h/f7-projection checks 2 'accepted)) proof))]))))

(a/prove-theorem 'skjCheck_sound '[tree :- SkDT] (sound-motive 'tree)
  (into ['(induction tree)]
    (mapcat (fn [rule]
      ['(intro ww G ee ss accepted)
       (list 'exact (apply list (symbol (str "skjCheck_" (first rule) "_sound"))
          (concat (map first (tree-fields rule)) (map (comp ih-name first) (premises rule))
                  '[ww G ee ss accepted])))]) skj-rules)))
