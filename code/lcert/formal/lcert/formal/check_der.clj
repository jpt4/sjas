(ns lcert.formal.check-der
  "F7 step 3 — checking whole derivation trees (ADR-0006, F7 plan).

  check_dt.clj gives the data: DT, one constructor per Tl, Rt and Cv rule,
  with premise derivations replaced by trees (DT for Tl/Rt/Cv premises,
  SkDT for skeleton typing, StepDT for steps) and proof-valued side
  conditions dropped. This namespace checks such a tree against a judgment.

  dtCheck chkf t J : Bool takes the judgment J to check as an input, as
  check_skj's skjCheck does: at a node it decides that J is the rule's
  conclusion (djEq), checks the side conditions, and checks each premise
  tree against the judgment that premise must have — so a premise is never
  trusted to conclude what it claims. Skeleton and step premises use the
  proved checkers of check_skj.clj and check_hd.clj.

  Soundness (dtCheck_sound): dtCheck chkf t J = true → Holds chkf J, where
  Holds reads a judgment as the proposition Tl, Rt or Cv at chkf — for an
  arbitrary chkf, with no hypothesis. Per rule the proof applies the
  rule's original constructor (judgment.clj, conv.clj), so a slip in the
  rule tables cannot enlarge what is accepted; the kernel checks each case.
  Completeness is not needed (CheckSpec asks only that accepted
  certificates be derivations) and is not proved."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.check-skj]
            [lcert.formal.check-dt :as dt]))

;; ---------------------------------------------------------------------------
;; Decided equality of usages, of lists, and of judgments.

;; Usages: three constructors, compared by nested case analysis.
(kdef f7UEq (=> U U Bool)
  (fn [x :- U, y :- U]
    (U.rec$1 (fn [_ :- U] Bool)
      (U.rec$1 (fn [_ :- U] Bool) Bool.true Bool.false Bool.false y)
      (U.rec$1 (fn [_ :- U] Bool) Bool.false Bool.true Bool.false y)
      (U.rec$1 (fn [_ :- U] Bool) Bool.false Bool.false Bool.true y) x)))

;; Nine cases: the three on the diagonal are rfl, the six off it have a
;; hypothesis false = true.
(a/prove-theorem 'f7UEq_sound '[x :- U, y :- U]
  '(=> (Eq Bool (f7UEq x y) Bool.true) (Eq U x y))
  (vec (concat ['(cases x)]
         (apply concat
           (for [i (range 3)]
             (concat ['(cases y)]
               (apply concat
                 (for [j (range 3)]
                   (if (= i j) ['(intro hu) '(rfl)] ['(intro hu) '(exact (Bool.noConfusion hu))])))))))))

;; An optional usage against a value (none rejects), as check_skj's f7OptSk.
(kdef f7OptU (=> (Option U) U Bool)
  (fn [o :- (Option U), x :- U]
    (Option.rec$1$0 U (fn [_ :- (Option U)] Bool) Bool.false
      (fn [v :- U] (f7UEq v x)) o)))
(thm f7OptU_sound [o :- (Option U), x :- U]
  (=> (Eq Bool (f7OptU o x) Bool.true) (Eq (Option U) o (Option.some U x)))
  (cases o) (intro hu) (exact (Bool.noConfusion hu))
  (intro hu) (exact (congrArg (fn [v :- U] (Option.some U v)) (f7UEq_sound _ x hu))))

;; Lists of expressions, usages and skeletons, each from its element
;; equality. One generator, three monomorphic copies: element type, its
;; decider, and the decider's soundness (all of the form (s a b h) : a = b).
(def ^:private list-eqs
  '[[listEqE Exp expEq expEq_sound]
    [listEqU U f7UEq f7UEq_sound]
    [listEqS Sk f7SkEq f7SkEq_sound]])

(doseq [[nm T eq eq-sound] list-eqs]
  (let [LT (list 'List T)
        cons-lemma (symbol (str nm "_cons"))
        sound (symbol (str nm "_sound"))]
    ;; listEq l1 l2: recursion on l1 returning a function of l2.
    (b/kdef! nm (list '=> LT LT 'Bool)
      (lv (list 'fn ['l1 :- LT]
        (list 'List.rec$1$0 T (list 'fn ['_ :- LT] (list '=> LT 'Bool))
          (list 'fn ['l2 :- LT]
            (list 'List.rec$1$0 T (list 'fn ['_ :- LT] 'Bool) 'Bool.true
              (list 'fn ['y :- T 'ys :- LT '_ :- 'Bool] 'Bool.false) 'l2))
          (list 'fn ['x :- T 'xs :- LT 'ih :- (list '=> LT 'Bool)]
            (list 'fn ['l2 :- LT]
              (list 'List.rec$1$0 T (list 'fn ['_ :- LT] 'Bool) 'Bool.false
                (list 'fn ['y :- T 'ys :- LT '_ :- 'Bool]
                  (list 'Bool.and (list eq 'x 'y) (list 'ih 'ys))) 'l2)))
          'l1))))
    ;; The cons–cons case, with clean names (cases would rename the second
    ;; list's fields, which clash with the induction's).
    (a/prove-theorem cons-lemma
      (lv ['x :- T 'xs :- LT 'y :- T 'ys :- LT
           'ih :- (list 'forall ['l2 LT] (list '=> (list 'Eq 'Bool (list nm 'xs 'l2) 'Bool.true) (list 'Eq LT 'xs 'l2)))
           'hl :- (list 'Eq 'Bool (list nm (list 'List.cons T 'x 'xs) (list 'List.cons T 'y 'ys)) 'Bool.true)])
      (lv (list 'Eq LT (list 'List.cons T 'x 'xs) (list 'List.cons T 'y 'ys)))
      (lv [(list 'exact
             (list 'Eq.trans
               (list 'congrArg (list 'fn ['v :- T] (list 'List.cons T 'v 'xs))
                     (list eq-sound 'x 'y (list 'band_left (list eq 'x 'y) (list nm 'xs 'ys) 'hl)))
               (list 'congrArg (list 'fn ['v :- LT] (list 'List.cons T 'y 'v))
                     (list 'ih 'ys (list 'band_right (list eq 'x 'y) (list nm 'xs 'ys) 'hl)))))]))
    (a/prove-theorem sound ['l1 :- LT]
      (lv (list 'forall ['l2 LT] (list '=> (list 'Eq 'Bool (list nm 'l1 'l2) 'Bool.true) (list 'Eq LT 'l1 'l2))))
      (lv ['(induction l1)
           ;; nil against nil, and nil against cons
           '(intro l2) '(cases l2) '(intro hl) '(rfl) '(intro hl) '(exact (Bool.noConfusion hl))
           ;; cons against nil, and cons against cons
           '(intro l2) '(cases l2) '(intro hl) '(exact (Bool.noConfusion hl))
           '(intro hl) (list 'exact (list cons-lemma '_ '_ '_ '_ 'ih_tail 'hl))]))))

;; Judgments. Each constructor's fields, with their deciders and soundness.
(def ^:private djs
  '[[tl [[w Bool f7BoolEq f7BoolEq_sound] [D (List Exp) listEqE listEqE_sound]
         [e Exp expEq expEq_sound] [A Exp expEq expEq_sound]]]
    [rt [[D (List Exp) listEqE listEqE_sound] [us (List U) listEqU listEqU_sound]
         [e Exp expEq expEq_sound] [A Exp expEq expEq_sound]]]
    [cv [[G (List Sk) listEqS listEqS_sound] [A Exp expEq expEq_sound] [B Exp expEq expEq_sound]]]])

(defn- primed [x] (symbol (str x "2")))
(defn- dj-checks [fields] (vec (for [[x _ eq] fields] (list eq x (primed x)))))
(defn- dj-binders [fields f] (vec (mapcat (fn [[x T]] [(f x) :- T]) fields)))

;; djEq J K: case on J, then on K; equal tags compare every field.
(b/kdef! 'djEq '(=> DTJ DTJ Bool)
  (lv (list 'fn '[J :- DTJ, K :- DTJ]
    (concat (list 'DTJ.rec$1 '(fn [_ :- DTJ] Bool))
      (for [[tag fields] djs]
        (list 'fn (dj-binders fields identity)
          (concat (list 'DTJ.rec$1 '(fn [_ :- DTJ] Bool))
            (for [[tag2 fields2] djs]
              (list 'fn (dj-binders fields2 primed)
                (if (= tag tag2) (h/f7-and (dj-checks fields)) 'Bool.false)))
            ['K])))
      ['J]))))

;; The equal-tag cases, one lemma each with clean names: every field is
;; equal by its decider's soundness, and the constructor applications are
;; joined field by field with congrArg.
(doseq [[tag fields] djs]
  (let [ctor (symbol (str "DTJ." tag))
        checks (dj-checks fields)
        n (count fields)
        ;; step i rewrites field i, the earlier ones already primed
        step (fn [i]
               (let [[x T _ sound] (nth fields i)
                     args (for [[j [y]] (map-indexed vector fields)]
                            (cond (< j i) (primed y) (= j i) 'v :else y))]
                 (list 'congrArg (list 'fn ['v :- T] (apply list ctor args))
                       (list sound x (primed x) (h/f7-projection checks i 'hj)))))]
    (a/prove-theorem (symbol (str "djEq_" tag))
      (lv (vec (concat (dj-binders fields identity) (dj-binders fields primed)
                 ['hj :- (list 'Eq 'Bool (list 'djEq (apply list ctor (map first fields))
                                                 (apply list ctor (map (comp primed first) fields))) 'Bool.true)])))
      (lv (list 'Eq 'DTJ (apply list ctor (map first fields)) (apply list ctor (map (comp primed first) fields))))
      (lv [(list 'exact (reduce (fn [acc i] (list 'Eq.trans acc (step i))) (step 0) (range 1 n)))]))))

(a/prove-theorem 'djEq_sound '[J :- DTJ, K :- DTJ]
  '(=> (Eq Bool (djEq J K) Bool.true) (Eq DTJ J K))
  (vec (concat ['(cases J)]
         (apply concat
           (for [[tag fields] djs]
             (concat ['(cases K)]
               (apply concat
                 (for [[tag2] djs]
                   (if (= tag tag2)
                     ['(intro hj) (list 'exact (apply list (symbol (str "djEq_" tag))
                                                 (concat (repeat (* 2 (count fields)) '_) ['hj])))]
                     ['(intro hj) '(exact (Bool.noConfusion hj))])))))))))

;; A judgment, read as the proposition it asserts at chkf.
(kdef Holds (=> (=> Code Code Bool) DTJ Prop)
  (fn [chkf :- (=> Code Code Bool), J :- DTJ]
    (DTJ.rec$1 (fn [_ :- DTJ] Prop)
      (fn [w :- Bool, D :- (List Exp), e :- Exp, A :- Exp] (Tl chkf w D e A))
      (fn [D :- (List Exp), us :- (List U), e :- Exp, A :- Exp] (Rt chkf D us e A))
      (fn [G :- (List Sk), A :- Exp, B :- Exp] (Cv chkf G A B)) J)))

;; ---------------------------------------------------------------------------
;; The checker.

;; Transports along decided equalities (as check_hd's f7Hd_transport).
(thm f7Holds_tr [chkf :- (=> Code Code Bool), J :- DTJ, K :- DTJ,
                 hj :- (Eq DTJ J K), p :- (Holds chkf K)]
  (Holds chkf J)
  (exact (Eq.mp (congrArg (fn [v :- DTJ] (Holds chkf v)) (Eq.symm hj)) p)))
(thm f7Step_tr [chkf :- (=> Code Code Bool), B :- Exp, C :- Exp, s :- Exp, t :- Exp,
                hs :- (Eq Exp s B), ht :- (Eq Exp t C), st :- (Step chkf s t)]
  (Step chkf B C)
  (exact (Eq.mp (congrArg (fn [v :- Exp] (Step chkf v C)) hs)
           (Eq.mp (congrArg (fn [v :- Exp] (Step chkf s v)) ht) st))))

(def ^:private dt-fields @#'dt/dt-fields)
(def ^:private judgment-tag '{Tl DTJ.tl, Rt DTJ.rt, Cv DTJ.cv})
(defn- ih-name [x] (symbol (str "ih_" x)))

(defn- concl-of
  "The judgment a rule concludes, as a DTJ term (Cv's context G is a field)."
  [family rule]
  (apply list (judgment-tag family) (if (= family 'Cv) (cons 'G (last rule)) (last rule))))

(defn- premise-judgment
  "A Tl/Rt/Cv premise type, as the DTJ term its tree must conclude."
  [[fam _chkf & args]]
  (apply list (judgment-tag fam) args))

(defn- checks-of
  "The rule's checks, in order, each [boolean-term role field]. Roles:
  :concl (the judgment is the rule's conclusion), :side, :prem (a Tl/Rt/Cv
  tree), :skj (a skeleton tree), and :step / :src / :tgt (a step tree and
  its two endpoints). premise-call builds a Tl/Rt/Cv premise's check from
  the field and the judgment: the recursor's ih in the definition, and the
  (definitionally equal) dtCheck call in the soundness lemmas, where ih_x
  names the induction hypothesis instead."
  [family rule premise-call]
  (vec (concat
    [[(list 'djEq 'JJ (concl-of family rule)) :concl nil]]
    (for [[x ty :as field] (h/f7-fields rule) :when (h/f7-side? field)
          :let [[_ T lhs rhs] ty]]
      [(cond (= T 'Nat) (list 'Nat.beq lhs rhs)
             (= T '(Option U)) (list 'f7OptU lhs (last rhs))
             :else (h/f7-side-check field))
       :side field])
    (apply concat
      (for [[x ty :as field] (h/f7-fields rule) :when (and (seq? ty) (#{'Tl 'Rt 'Cv 'SkJ 'Step} (first ty)))]
        (case (first ty)
          (Tl Rt Cv) [[(premise-call x (premise-judgment ty)) :prem field]]
          SkJ [[(apply list 'skjCheck (concat (rest ty) [x])) :skj field]]
          Step (let [[_ _ B C] ty]
                 [[(list 'stepDTCheck 'chkf x) :step field]
                  [(list 'expEq (list 'stepDTSrc x) B) :src field]
                  [(list 'expEq (list 'stepDTTgt x) C) :tgt field]])))))))

;; dtCheck chkf tree : DTJ → Bool, by the recursor of DT (explicit, as
;; check_dt's concl: a/defn's equation lemmas would be enormous here).
(b/kdef! 'dtCheck '(=> (=> Code Code Bool) DT DTJ Bool)
  (list 'fn '[chkf :- (=> Code Code Bool), tree :- DT]
    (concat (list 'DT.rec$1 '(fn [_ :- DT] (=> DTJ Bool)))
      (for [[family rule] dt/dt-rules]
        (let [fields (dt-fields rule)
              ihs (for [[x ty] fields :when (= ty 'DT)] [(ih-name x) '(=> DTJ Bool)])]
          (list 'fn (h/f7-params (concat fields ihs))
            (list 'fn '[JJ :- DTJ] (h/f7-and (map first (checks-of family rule (fn [x j] (list (ih-name x) j)))))))))
      ['tree])))

(defn- sound-motive [tree]
  (list 'forall '[JJ DTJ]
    (list '=> (list 'Eq 'Bool (list 'dtCheck 'chkf tree 'JJ) 'Bool.true) '(Holds chkf JJ))))

;; One soundness lemma per rule: the accepted conjunction gives, by
;; projection, every side condition and every premise; the rule's own
;; constructor then proves its conclusion, and djEq moves that to JJ.
(doseq [[family rule] dt/dt-rules]
  (let [nm (first rule)
        fields (dt-fields rule)
        tree (h/f7-app (symbol (str "DT." nm)) (map first fields))
        checks (checks-of family rule (fn [x j] (list 'dtCheck 'chkf x j)))
        terms (mapv first checks)
        proj (fn [i] (h/f7-projection terms i 'accepted))
        idx (fn [role field] (first (keep-indexed (fn [i [_ r f]] (when (and (= r role) (= f field)) i)) checks)))
        arg (fn [[x ty :as field]]
              (cond
                (h/f7-side? field)
                (let [[_ T lhs rhs] ty, p (proj (idx :side field))]
                  (cond (= T 'Nat) (list 'Nat.eq_of_beq_eq_true p)
                        (= T '(Option U)) (list 'f7OptU_sound lhs (last rhs) p)
                        :else (h/f7-side-proof field p)))
                (and (seq? ty) (#{'Tl 'Rt 'Cv} (first ty)))
                (list (ih-name x) (premise-judgment ty) (proj (idx :prem field)))
                (and (seq? ty) (= 'SkJ (first ty)))
                (apply list 'skjCheck_sound x (concat (rest ty) [(proj (idx :skj field))]))
                (and (seq? ty) (= 'Step (first ty)))
                (let [[_ _ B C] ty]
                  (list 'f7Step_tr 'chkf B C (list 'stepDTSrc x) (list 'stepDTTgt x)
                        (list 'expEq_sound (list 'stepDTSrc x) B (proj (idx :src field)))
                        (list 'expEq_sound (list 'stepDTTgt x) C (proj (idx :tgt field)))
                        (list 'stepDTCheck_sound 'chkf x (proj (idx :step field)))))
                :else x))
        ctor-proof (apply list (symbol (str family "." nm)) 'chkf (map arg (h/f7-fields rule)))
        cj (concl-of family rule)]
    (a/prove-theorem (symbol (str "dtCheck_" nm "_sound"))
      (lv (h/f7-params (concat [['chkf '(=> Code Code Bool)]] fields
                               (for [[x ty] fields :when (= ty 'DT)] [(ih-name x) (sound-motive x)])
                               [['JJ 'DTJ] ['accepted (list 'Eq 'Bool (list 'dtCheck 'chkf tree 'JJ) 'Bool.true)]])))
      '(Holds chkf JJ)
      (lv [(list 'exact (list 'f7Holds_tr 'chkf 'JJ cj (list 'djEq_sound 'JJ cj (proj 0)) ctor-proof))]))))

;; Soundness: an accepted tree proves the judgment it was checked against.
(a/prove-theorem 'dtCheck_sound '[chkf :- (=> Code Code Bool), tree :- DT] (sound-motive 'tree)
  (into ['(induction tree)]
    (mapcat (fn [[_ rule]]
      (let [fields (dt-fields rule)]
        ['(intro JJ accepted)
         (list 'exact (apply list (symbol (str "dtCheck_" (first rule) "_sound"))
                        'chkf (concat (map first fields)
                                      (for [[x ty] fields :when (= ty 'DT)] (ih-name x))
                                      '[JJ accepted])))]))
      dt/dt-rules)))
