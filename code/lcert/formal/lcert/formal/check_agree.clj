(ns lcert.formal.check-agree
  "F7 step 5a — the checker transport for derivation data (ADR-0006).

  Checking a derivation tree consults the checker chkf only at δ-steps: a
  HdDT.delta record's contractum is boolExp (chkf cc dc), where the record
  stores the two codes cc and dc. So two checkers that agree on every first
  code smaller than a bound read off the tree check that tree alike:

    dtCheck_agree: AgreeF (dtB t) chk1 chk2 → dtCheck chk1 t J = dtCheck chk2 t J

  dtB t is the sum over t's δ-records of ‖cc‖ + 1 (a sum, not a maximum, so
  each premise's bound is below the node's by Nat.le_add_left/right).
  AgreeF n chk1 chk2 asks agreement on *first* codes with fewer than n
  nodes — the certificate's position — unlike prop410's Agree, which bounds
  the second code and only shows that some bound exists. The concrete
  checker (F7 5b) is defined by recursion on the certificate, so its δ-steps
  see only smaller certificates; this lemma moves an accepted tree from that
  restricted checker to the checker itself."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.check-der :as der]))

;; ---------------------------------------------------------------------------
;; Bounds: a δ-record bounds its first code; a tree sums its records' bounds.

(def ^:private hd-data @#'h/hd-data)
(def ^:private dt-fields @#'der/dt-fields)
(def ^:private checks-of @#'der/checks-of)
(defn- ih-name [x] (symbol (str "ih_" x)))

(b/kdef! 'hdB '(=> HdDT Nat)
  (list 'fn '[tree :- HdDT]
    (concat (list 'HdDT.rec$1 '(fn [_ :- HdDT] Nat))
      (for [rule h/hd-rules]
        (let [v (if (= (first rule) 'delta) '(+ (cnodes cc) 1) 0)]
          (if (seq (hd-data rule)) (list 'fn (h/f7-params (hd-data rule)) v) v)))
      ['tree])))

(kdef stepDTB (=> StepDT Nat)
  (fn [d :- StepDT]
    (StepDT.rec$1 (fn [_ :- StepDT] Nat)
      (fn [p :- (List Nat), head :- HdDT, e :- Exp, e2 :- Exp] (hdB head)) d)))

(defn- bound-terms
  "A rule's bound summands, in field order: each step tree's bound and each
  premise tree's (by premise-bound, the recursor's ih or dtB)."
  [rule premise-bound]
  (vec (for [[x ty] (dt-fields rule) :when (#{'DT 'StepDT} ty)]
         (if (= ty 'StepDT) (list 'stepDTB x) (premise-bound x)))))

(defn- sum-terms [ts] (reduce (fn [acc t] (list '+ t acc)) 0 (reverse ts)))

(b/kdef! 'dtB '(=> DT Nat)
  (list 'fn '[tree :- DT]
    (concat (list 'DT.rec$1 '(fn [_ :- DT] Nat))
      (for [[_ rule] dt/dt-rules]
        (let [fields (dt-fields rule)
              ihs (for [[x ty] fields :when (= ty 'DT)] [(ih-name x) 'Nat])
              v (sum-terms (bound-terms rule ih-name))]
          (if (seq (concat fields ihs)) (list 'fn (h/f7-params (concat fields ihs)) v) v)))
      ['tree])))

;; ---------------------------------------------------------------------------
;; Agreement below a bound, on the first code.

(kdef AgreeF (=> Nat (=> Code Code Bool) (=> Code Code Bool) Prop)
  (fn [n :- Nat, chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool)]
    (forall [c Code] (forall [d Code] (=> (Nat.lt (cnodes c) n) (Eq Bool (chk1 c d) (chk2 c d)))))))

(thm agreeF_mono [m :- Nat, n :- Nat, chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool),
                  hag :- (AgreeF n chk1 chk2), hle :- (LE.le m n)]
  (AgreeF m chk1 chk2)
  (exact (fn [c :- Code, d :- Code, hc :- (Nat.lt (cnodes c) m)] (hag c d (Nat.lt_of_lt_of_le hc hle)))))

;; Below 0 there is nothing to agree on (a tree without δ-steps has bound 0).
(thm agreeF_zero [chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool)]
  (AgreeF 0 chk1 chk2)
  (intro c d hc) (exact (absurd hc (Nat.not_lt_zero (cnodes c)))))

;; ---------------------------------------------------------------------------
;; Head steps, steps, step trees.

;; Only δ's contractum mentions the checker; every other record's is equal
;; by computation.
(a/prove-theorem 'hdTarget_agree '[chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), tree :- HdDT]
  '(=> (AgreeF (hdB tree) chk1 chk2) (Eq Exp (hdTarget chk1 tree) (hdTarget chk2 tree)))
  (vec (concat ['(cases tree)]
         (mapcat (fn [rule]
                   (if (= (first rule) 'delta)
                     ['(intro hag) '(exact (congrArg boolExp (hag cc dc (Nat.lt_succ_self (cnodes cc)))))]
                     ['(intro hag) '(rfl)]))
                 h/hd-rules))))

;; hdCheck and stepCheck depend on the checker only through hdTarget: each
;; is a function K of hdTarget chkf tree, so congrArg K moves the equality.
(thm hdCheck_agree [chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), tree :- HdDT,
                    e :- Exp, e2 :- Exp, hag :- (AgreeF (hdB tree) chk1 chk2)]
  (Eq Bool (hdCheck chk1 tree e e2) (hdCheck chk2 tree e e2))
  (exact (congrArg (fn [v :- Exp] (Bool.and (expEq e (hdSource tree)) (Bool.and (expEq e2 v) (hdSide tree))))
           (hdTarget_agree chk1 chk2 tree hag))))

(thm stepCheck_agree [chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), p :- (List Nat),
                      tree :- HdDT, e :- Exp, e2 :- Exp, hag :- (AgreeF (hdB tree) chk1 chk2)]
  (Eq Bool (stepCheck chk1 p tree e e2) (stepCheck chk2 p tree e e2))
  (exact (congrArg (fn [v :- Exp]
                     (Bool.and (f7OptExp (getP p e) (hdSource tree))
                       (Bool.and (Bool.and (expEq (hdSource tree) (hdSource tree))
                                           (Bool.and (expEq v v) (hdSide tree)))
                                 (expEq e2 (setP p e v)))))
           (hdTarget_agree chk1 chk2 tree hag))))

(thm stepDTCheck_agree [chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), d :- StepDT]
  (=> (AgreeF (stepDTB d) chk1 chk2) (Eq Bool (stepDTCheck chk1 d) (stepDTCheck chk2 d)))
  (cases d) (intro hag) (exact (stepCheck_agree chk1 chk2 p head e e2 hag)))

;; ---------------------------------------------------------------------------
;; Derivation trees.

(defn- le-in-sum
  "A proof that summand i of ts is at most their sum (as sum-terms builds it):
  b_i ≤ b_i + S_(i+1), then b_i ≤ b_j + S_(j+1) for j = i-1 … 0."
  [ts i]
  (let [S (fn [k] (sum-terms (drop k ts)))]
    (reduce (fn [prev j] (list 'Nat.le_trans prev (list 'Nat.le_add_left (S (inc j)) (nth ts j))))
            (list 'Nat.le_add_right (nth ts i) (S (inc i)))
            (range (dec i) -1 -1))))

(defn- and-congr
  "A proof that the right-nested conjunctions of ts1 and ts2 (h/f7-and) are
  equal, from per-conjunct proofs eqs (nil where the two conjuncts are the
  same checker-free term)."
  [ts1 eqs]
  (reduce (fn [rest [t e]] (list 'congr (list 'congrArg 'Bool.and (or e (list 'Eq.refl t))) rest))
          '(Eq.refl Bool.true)
          (reverse (map vector ts1 eqs))))

(defn- agree-motive [tree]
  (list '=> (list 'AgreeF (list 'dtB tree) 'chk1 'chk2)
    (list 'forall '[JJ DTJ] (list 'Eq 'Bool (list 'dtCheck 'chk1 tree 'JJ) (list 'dtCheck 'chk2 tree 'JJ)))))

(defn- prove-agree!
  "dtCheck_<rule>_agree: the rule's checks are equal at chk1 and chk2."
  [family rule]
  (let [nm (first rule)
        fields (dt-fields rule)
        tree (h/f7-app (symbol (str "DT." nm)) (map first fields))
        at (fn [chk] (fn [x j] (list 'dtCheck chk x j)))
        checks1 (walk/postwalk-replace {'chkf 'chk1} (checks-of family rule (at 'chkf)))
        terms1 (mapv first checks1)
        terms2 (mapv first (walk/postwalk-replace {'chkf 'chk2} (checks-of family rule (at 'chkf))))
        bounds (bound-terms rule (fn [x] (list 'dtB x)))
        bound-of (fn [x ty] (list (if (= ty 'StepDT) 'stepDTB 'dtB) x))
        agree-at (fn [x ty]
                   (let [i (.indexOf ^java.util.List bounds (bound-of x ty))]
                     (list 'agreeF_mono (bound-of x ty) (list 'dtB tree) 'chk1 'chk2 'hag (le-in-sum bounds i))))
        ;; a premise check (dtCheck chk1 x j) is equal by its induction
        ;; hypothesis at j; a step check by stepDTCheck_agree; every other
        ;; check does not mention the checker
        eqs (for [[t role [x]] checks1]
              (case role
                :prem (list (ih-name x) (agree-at x 'DT) (last t))
                :step (list 'stepDTCheck_agree 'chk1 'chk2 x (agree-at x 'StepDT))
                nil))]
    (a/prove-theorem (symbol (str "dtCheck_" nm "_agree"))
      (lv (h/f7-params (concat [['chk1 '(=> Code Code Bool)] ['chk2 '(=> Code Code Bool)]] fields
                               (for [[x ty] fields :when (= ty 'DT)] [(ih-name x) (agree-motive x)])
                               [['hag (list 'AgreeF (list 'dtB tree) 'chk1 'chk2)] ['JJ 'DTJ]])))
      (list 'Eq 'Bool (list 'dtCheck 'chk1 tree 'JJ) (list 'dtCheck 'chk2 tree 'JJ))
      ;; state the equality of the explicit conjunctions first (agree_eq: no
      ;; rule field has that name — zConv's Cv premise is hc): exact
      ;; against the folded goal would unify congr's f a with dtCheck chk1 t JJ
      (lv [(list 'have 'agree_eq (list 'Eq 'Bool (h/f7-and terms1) (h/f7-and terms2)) (and-congr terms1 eqs))
           '(exact agree_eq)]))))

(doseq [[family rule] dt/dt-rules] (prove-agree! family rule))

(a/prove-theorem 'dtCheck_agree '[chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), tree :- DT]
  (agree-motive 'tree)
  (into ['(induction tree)]
    (mapcat (fn [[_ rule]]
      (let [fields (dt-fields rule)]
        ['(intro hag JJ)
         (list 'exact (apply list (symbol (str "dtCheck_" (first rule) "_agree"))
                        'chk1 'chk2 (concat (map first fields)
                                            (for [[x ty] fields :when (= ty 'DT)] (ih-name x))
                                            '[hag JJ])))]))
      dt/dt-rules)))
