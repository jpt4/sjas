(ns lcert.formal.check-hd
  "F7 — head-step certificates and checking a step at a recorded path.

  R4 §1.5–1.6, ADR-0006 F7 steps 2–3. HdDT is ordinary inductive data,
  with one constructor per Hd rule and no proof fields. It records the
  rule's explicit arguments, including the branch selected by caseLbl and
  the two claimed decoded codes for delta. Those claims are checked using
  nthB/codeOf, never trusted. beta and the recursors compute subst1/substL.

  hdCheck compares both endpoints with expEq and checks every side
  condition. stepCheck additionally checks getP and the whole result of
  setP, so an invalid path or a change outside the path cannot be accepted.
  All soundness statements quantify over an arbitrary chkf; no CheckSpec
  hypothesis or completeness result is used. Per-rule proofs below are
  generated as terms and independently checked against the existing Hd."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check]))

;; Option-to-value comparisons used for partial computations. In particular,
;; none always rejects. Eliminate the Option before introducing its equality
;; hypothesis: cases on some = some is unsafe in the surface tactic layer.
(kdef f7OptExp (=> (Option Exp) Exp Bool)
  (fn [o :- (Option Exp), x :- Exp]
    (Option.rec$1$0 Exp (fn [_ :- (Option Exp)] Bool) Bool.false
      (fn [v :- Exp] (expEq v x)) o)))
(thm f7OptExp_sound [o :- (Option Exp), x :- Exp]
  (=> (Eq Bool (f7OptExp o x) Bool.true) (Eq (Option Exp) o (Option.some Exp x)))
  (cases o) (intro h) (exact (Bool.noConfusion h))
  (intro h) (exact (congrArg (fn [v :- Exp] (Option.some Exp v)) (expEq_sound _ x h))))

(kdef f7OptCode (=> (Option Code) Code Bool)
  (fn [o :- (Option Code), x :- Code]
    (Option.rec$1$0 Code (fn [_ :- (Option Code)] Bool) Bool.false
      (fn [v :- Code] (codeEq v x)) o)))
(thm f7OptCode_sound [o :- (Option Code), x :- Code]
  (=> (Eq Bool (f7OptCode o x) Bool.true) (Eq (Option Code) o (Option.some Code x)))
  (cases o) (intro h) (exact (Bool.noConfusion h))
  (intro h) (exact (congrArg (fn [v :- Code] (Option.some Code v)) (codeEq_sound _ x h))))

;; The following helpers generate surface syntax only. They do not install
;; unchecked constants. Shared with the skeleton-tree checker.
(defn f7-params [fields] (vec (mapcat (fn [[x ty]] [x ':- ty]) fields)))
(defn f7-app [f args] (if (seq args) (apply list f args) f))
(defn f7-and [xs]
  (if (seq xs) (list 'Bool.and (first xs) (f7-and (rest xs))) 'Bool.true))
(defn f7-projection [xs i h]
  (let [tail (f7-and (rest xs))]
    (if (zero? i) (list 'band_left (first xs) tail h)
      (f7-projection (rest xs) (dec i) (list 'band_right (first xs) tail h)))))
(defn f7-fields [rule] (vec (take-while vector? (rest rule))))
(defn f7-side? [[_ ty]] (and (seq? ty) (= 'Eq (first ty))))
(defn f7-side-check [[_ [_ ty lhs rhs]]]
  (cond
    (and (= ty 'Bool) (= rhs 'Bool.true)) lhs
    (= ty '(Option Exp)) (list 'f7OptExp lhs (last rhs))
    (= ty '(Option Code)) (list 'f7OptCode lhs (last rhs))
    :else (throw (ex-info "Unsupported checker side condition" {:type ty :lhs lhs :rhs rhs}))))
(defn f7-side-proof [[_ [_ ty lhs rhs]] h]
  (cond
    (= ty 'Bool) h
    (= ty '(Option Exp)) (list 'f7OptExp_sound lhs (last rhs) h)
    (= ty '(Option Code)) (list 'f7OptCode_sound lhs (last rhs) h)
    :else (throw (ex-info "Unsupported side-condition proof" {:type ty}))))

;; The 18 constructor signatures of Hd (conv.clj), copied in order. Eq
;; fields become Boolean tests, and all other fields become ordinary data.
;; The soundness terms apply Hd's original constructors, so a discrepancy
;; in this table cannot silently extend the accepted reduction relation.
(def hd-rules
  '[(beta [r U] [A Exp] [t Exp] [u Exp] :where [(Exp.app (Exp.lam r A t) u) (subst1 u t)])
  (betaLet [C Exp] [S Exp] [x Exp] [y Exp] [t Exp]
    :where [(Exp.letp C (Exp.pair S x y) t) (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)])
  (iteT [t Exp] [e Exp] :where [(Exp.ite Exp.tt t e) t])
  (iteF [t Exp] [e Exp] :where [(Exp.ite Exp.ff t e) e])
  (elimT [P Exp] [t Exp] [e Exp] :where [(Exp.elimB P Exp.tt t e) t])
  (elimF [P Exp] [t Exp] [e Exp] :where [(Exp.elimB P Exp.ff t e) e])
  (recNZ [P Exp] [z Exp] [s Exp] :where [(Exp.recN P z s Exp.zero) z])
  (recNS [P Exp] [z Exp] [s Exp] [n Exp]
    :where [(Exp.recN P z s (Exp.succ n)) (substL (List.cons Exp (Exp.recN P z s n) (List.cons Exp n (List.nil Exp))) s)])
  (caseLb [P Exp] [l Nat] [bs Exp] [b Exp] [h (Eq (Option Exp) (nthB bs l) (Option.some Exp b))]
    :where [(Exp.caseL P (Exp.lbl l) bs) b])
  (recSL [P Exp] [tl Exp] [tn Exp] [x Exp] :where [(Exp.recS P tl tn (Exp.sleaf x)) (subst1 x tl)])
  (recSN [P Exp] [tl Exp] [tn Exp] [x Exp] [c1 Exp] [c2 Exp]
    :where [(Exp.recS P tl tn (Exp.snode x c1 c2))
            (substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
                     (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn)])
  (itRL [X Exp] [g Exp] [h Exp] [x Exp] :where [(Exp.itR X g h (Exp.leaf x)) (Exp.app g x)])
  (itRN [X Exp] [g Exp] [h Exp] [d Exp] [x Exp] [r1 Exp] [r2 Exp]
    :where [(Exp.itR X g h (Exp.node d x r1 r2))
            (Exp.app (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Exp.itR X g h r2))])
  (prnL [x Exp] :where [(Exp.prn (Exp.leaf x)) (Exp.sleaf x)])
  (prnN [d Exp] [x Exp] [r1 Exp] [r2 Exp]
    :where [(Exp.prn (Exp.node d x r1 r2)) (Exp.snode x (Exp.prn r1) (Exp.prn r2))])
  (delta [c Exp] [d Exp] [cc Code] [dc Code] [hc (Eq (Option Code) (codeOf c) (Option.some Code cc))] [hd (Eq (Option Code) (codeOf d) (Option.some Code dc))]
    :where [(Exp.chk c d) (boolExp (chkf cc dc))])
  (tTT :where [(Exp.tT Exp.tt) Exp.tUnit])
  (tTF :where [(Exp.tT Exp.ff) Exp.tEmpty])])

(defn- hd-data [rule] (vec (remove f7-side? (f7-fields rule))))
(defn- hd-sides [rule] (vec (filter f7-side? (f7-fields rule))))

(eval (list* 'a/inductive 'HdDT []
        (for [rule hd-rules] (list* (first rule) (hd-data rule)))))

;; A recorded rule always has a syntactic source and target. Validity is
;; separate: a caseLb record can name an absent branch, and a delta record
;; can lie about codeOf. hdSide rejects either sort of malformed record.
(doseq [[nm ty extra select] [['hdSource '(=> HdDT Exp) [] #(first (last %))]
                              ['hdTarget '(=> (=> Code Code Bool) HdDT Exp)
                               '[chkf :- (=> Code Code Bool)] #(second (last %))]
                              ['hdSide '(=> HdDT Bool) [] #(f7-and (map f7-side-check (hd-sides %)))]]]
  (b/kdef! nm ty
    (list 'fn (into extra '[tree :- HdDT])
      (apply list 'HdDT.rec$1 (list 'fn '[_ :- HdDT] (if (= nm 'hdSide) 'Bool 'Exp))
        (concat (for [rule hd-rules]
                  (if (seq (hd-data rule))
                    (list 'fn (f7-params (hd-data rule)) (select rule))
                    (select rule))) ['tree])))))

(kdef hdCheck (=> (=> Code Code Bool) HdDT Exp Exp Bool)
  (fn [chkf :- (=> Code Code Bool), tree :- HdDT, e :- Exp, e2 :- Exp]
    (Bool.and (expEq e (hdSource tree))
      (Bool.and (expEq e2 (hdTarget chkf tree)) (hdSide tree)))))

;; Transport only after expEq has produced actual expression equalities.
(thm f7Hd_transport [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp,
                     r :- Exp, r2 :- Exp, hx :- (Eq Exp e r), hy :- (Eq Exp e2 r2),
                     hr :- (Hd chkf r r2)]
  (Hd chkf e e2)
  (exact (Eq.mp (congrArg (fn [v :- Exp] (Hd chkf v e2)) (Eq.symm hx))
    (Eq.mp (congrArg (fn [v :- Exp] (Hd chkf r v)) (Eq.symm hy)) hr))))

(doseq [rule hd-rules]
  (let [nm (first rule), fields (hd-data rule), sides (hd-sides rule)
        tree (f7-app (symbol (str "HdDT." nm)) (map first fields))
        [src dst] (last rule)
        checks (vec (concat [(list 'expEq 'ee src) (list 'expEq 'ee2 dst)]
                            (map f7-side-check sides)))
        ;; hdCheck's final conjunct is hdSide, whose right-nested chain
        ;; ends in true, matching f7-and for the whole list.
        premise-proofs (into {} (map-indexed
          (fn [i field] [(first field) (f7-side-proof field (f7-projection checks (+ i 2) 'accepted))]) sides))
        hd-proof (apply list (symbol (str "Hd." nm)) 'chkf
                   (map (fn [[x _ :as field]] (if (f7-side? field) (premise-proofs x) x)) (f7-fields rule)))]
    (a/prove-theorem (symbol (str "hdCheck_" nm "_sound"))
      (lv (into '[chkf :- (=> Code Code Bool)] (concat (f7-params fields)
             ['ee ':- 'Exp 'ee2 ':- 'Exp 'accepted ':- (list 'Eq 'Bool (list 'hdCheck 'chkf tree 'ee 'ee2) 'Bool.true)])))
      '(Hd chkf ee ee2)
      (lv [(list 'exact (list 'f7Hd_transport 'chkf 'ee 'ee2 src dst
               (list 'expEq_sound 'ee src (f7-projection checks 0 'accepted))
               (list 'expEq_sound 'ee2 dst (f7-projection checks 1 'accepted)) hd-proof))]))))

(a/prove-theorem 'hdCheck_sound '[chkf :- (=> Code Code Bool), tree :- HdDT]
  '(forall [ee Exp] (forall [ee2 Exp]
     (=> (Eq Bool (hdCheck chkf tree ee ee2) Bool.true) (Hd chkf ee ee2))))
  (into ['(cases tree)]
    (mapcat (fn [rule]
      ['(intro ee ee2 accepted)
       (list 'exact (apply list (symbol (str "hdCheck_" (first rule) "_sound"))
                     'chkf (concat (map first (hd-data rule)) '[ee ee2 accepted])))]) hd-rules)))

;; The path certificate is the path plus its HdDT. Reconstruct the redex
;; and contractum from that record; check both the lookup and the complete
;; replaced expression. No inversion theorem for setP is needed.
(kdef stepCheck (=> (=> Code Code Bool) (List Nat) HdDT Exp Exp Bool)
  (fn [chkf :- (=> Code Code Bool), p :- (List Nat), tree :- HdDT, e :- Exp, e2 :- Exp]
    (Bool.and (f7OptExp (getP p e) (hdSource tree))
      (Bool.and (hdCheck chkf tree (hdSource tree) (hdTarget chkf tree))
        (expEq e2 (setP p e (hdTarget chkf tree)))))))

(thm stepCheck_sound [chkf :- (=> Code Code Bool), p :- (List Nat), tree :- HdDT,
                      e :- Exp, e2 :- Exp, h :- (Eq Bool (stepCheck chkf p tree e e2) Bool.true)]
  (Step chkf e e2)
  (have hp (Eq (Option Exp) (getP p e) (Option.some Exp (hdSource tree)))
    (f7OptExp_sound (getP p e) (hdSource tree) (band_left _ _ h)))
  (have hs (Hd chkf (hdSource tree) (hdTarget chkf tree))
    (hdCheck_sound chkf tree (hdSource tree) (hdTarget chkf tree) (band_left _ _ (band_right _ _ h))))
  (have he (Eq Exp e2 (setP p e (hdTarget chkf tree)))
    (expEq_sound e2 (setP p e (hdTarget chkf tree)) (band_right _ _ (band_right _ _ h))))
  (constructor) (exact p) (constructor) (exact (hdSource tree))
  (constructor) (exact (hdTarget chkf tree))
  (exact (And.intro hp (And.intro hs he))))
