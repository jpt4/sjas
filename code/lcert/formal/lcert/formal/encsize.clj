(ns lcert.formal.encsize
  "F7 — size and label facts about the expression encoding encE, for
  certificates without padding (ADR-0006; design note
  nachlass/docs-f7-enc46-design.md §3).

  The paper derives its size facts from the encoding (R4-metatheory.md §1.6,
  E2–E4) rather than testing them.  F7's first checker tested them, and
  completeness padded each certificate until it passed the tests.  That
  padding is what let an accepted certificate carry another accepted
  certificate for free (enc46f7.clj).  The unpadded format (certcanon.clj)
  needs the facts below instead.

  - tokc m g: the tokens of Θₘ that a freshness mask g marks used
    (cntU (maskUF (thetaU m) g)), as in TokSize.  tok_true, tok_and,
    tok_congr, tok_var: how it counts.  (E3's half of TokSize, at most the
    m tokens of Θₘ, is prop434.clj's cnt_mask_le.)
  - tok_encE (E4, the occurrence half of TokSize): the tokens free in t are
    at most the internal nodes of ⌜t⌝.  Each used token has a variable
    occurrence in t, and each variable occurrence is an internal node of
    ⌜t⌝ (var i = sn 14 ⌜i⌝ (sl 0)).  Generated one constructor at a time from
    syntactic.clj's binder table and encE's own clauses (read from
    encode.clj), so it cannot drift from either.
  - cnodes_encNat: a unary number n has n internal nodes (E3's budget half:
    the budget is recorded with one node per context entry).
  - Raw codes as literal terms.  A certificate writes a raw code c (a
    δ-record's codes) as its canonical code term, encE (codeTerm c), as the
    paper writes a code literal (E4, E6).  code_enc_gt: that has more
    internal nodes than c.  lbl_codeTerm: its label constants are c's labels.
  - nlb nb c: every *internal* node of c has a label below nb (leaves are
    free: they carry data, such as label constants).  nlb_encE: every
    internal label of an expression's encoding is below 96.  So label 96,
    which certcanon.clj reserves for certificate roots, never labels an
    internal node of an encoded expression, type, or raw code (F7's form of
    E6)."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            ;; ucnt, cntU
            [lcert.formal.lemma36]
            ;; band_left, band_right
            [lcert.formal.skof]
            ;; bool_case
            [lcert.formal.outer]
            ;; the binder-depth table
            [lcert.formal.syntactic]
            ;; freshF, maskUF
            [lcert.formal.strengthen]
            ;; codeTerm, codeTerm_of
            [lcert.formal.section4b]
            ;; encE, encNat, uLbl
            [lcert.formal.encode]
            ;; lblBelow, lblsE, band_tt2
            [lcert.formal.enclabels]))

;; ---------------------------------------------------------------------------
;; Generator inputs: Exp's constructors, encE's clauses, binder depths.

;; As in enclabels.clj: Exp's constructors and fields read from syntax.clj,
;; and encE's clauses from encode.clj, so the generated statements follow the
;; definitions exactly.
(defn- read-forms [f] (read-string (str "[" (slurp (io/resource f)) "]")))
(def ^:private exp-ctors
  (let [form (first (filter #(and (seq? %) (= 'a/inductive (first %)) (= 'Exp (second %)))
                            (read-forms "lcert/formal/syntax.clj")))]
    (vec (for [c (drop 3 form)] [(first c) (vec (map vec (rest c)))]))))
(def ^:private enc-clauses
  (let [form (first (filter #(and (seq? %) (= 'kdef (first %)) (= 'encE (second %)))
                            (read-forms "lcert/formal/encode.clj")))
        rec (nth (nth form 3) 2)]
    (vec (butlast (drop 2 rec)))))
;; syntactic.clj's table: each field with its binder depth (freshF reads an
;; Exp field under k binders at cutoff + k).
(def ^:private depth-table @#'lcert.formal.syntactic/exp-fields)

(defn- exp-fields [fields] (vec (for [[x T] fields :when (= T 'Exp)] x)))

;; The depth of each Exp field, checked against syntax.clj's names.
(defn- depths [ctor fields]
  (let [row (some (fn [[c fs]] (when (= c ctor) fs)) depth-table)]
    (when-not (= (map first row) (map first fields))
      (throw (ex-info "binder table and Exp disagree" {:ctor ctor :table row :fields fields})))
    (into {} (for [[x T k] row :when (= T 'Exp)] [x k]))))

(defn- clause-body
  "encE's clause for a constructor, each child's code i_x replaced by (encE x)."
  [[ctor fields] clause]
  (if (and (seq? clause) (= 'fn (first clause)))
    (walk/postwalk-replace (into {} (for [x (exp-fields fields)] [(symbol (str "i_" x)) (list 'encE x)]))
                           (nth clause 2))
    clause))

(defn- nodes-expr
  "cnodes of a piece of a clause body, as an arithmetic term over the
  children's cnodes: a leaf has none, a node one more than its children."
  [c]
  (cond
    (and (seq? c) (= 'Code.sl (first c))) 0
    (and (seq? c) (= 'Code.sn (first c))) (list '+ 1 (list '+ (nodes-expr (nth c 2)) (nodes-expr (nth c 3))))
    (and (seq? c) (#{'encNat 'encE} (first c))) (list 'cnodes c)
    :else (throw (ex-info "unexpected piece of an encE clause" {:piece c}))))

(def ^:private ctor-clauses (map vector exp-ctors enc-clauses))

;; ---------------------------------------------------------------------------
;; Token counting.

;; tokc m g: the tokens of Θₘ that the mask g leaves in use — thetaU m with
;; every position j where g j = true (fresh: unused) set to usage 0, counted.
;; TokSize's f is tokc m (freshF t).
(kdef tokc (=> Nat (=> Nat Bool) Nat)
  (fn [m :- Nat, g :- (=> Nat Bool)] (cntU (maskUF (thetaU m) g))))

;; One more token: position 0 counts when g 0 = false; the rest is the mask
;; shifted by one.
(kdef selU (=> Bool U) (fn [bb :- Bool] (Bool.rec$1 (fn [_ :- Bool] U) U.u1 U.u0 bb)))
(thm tokc_succ [m :- Nat, g :- (=> Nat Bool)]
  (Eq Nat (tokc (+ m 1) g) (+ (ucnt (selU (g 0))) (tokc m (fn [j :- Nat] (g (+ j 1))))))
  (rfl))

;; A mask that marks every token fresh leaves none.
(thm tok_true [m :- Nat] (Eq Nat (tokc m (fn [j :- Nat] Bool.true)) 0)
  (induction m)
  (rfl)
  (exact (Eq.trans (tokc_succ n (fn [j :- Nat] Bool.true)) (congrArg (fn [y :- Nat] (+ 0 y)) ih_n))))

;; Pointwise-equal masks count alike.
(thm tok_congr [m :- Nat]
  (forall [g (=> Nat Bool)] (forall [g2 (=> Nat Bool)]
    (=> (forall [j Nat] (Eq Bool (g j) (g2 j))) (Eq Nat (tokc m g) (tokc m g2)))))
  (induction m)
  (intro g g2 he) (rfl)
  (intro g g2 he)
  (have e1 (Eq Nat (+ (ucnt (selU (g 0))) (tokc n (fn [j :- Nat] (g (+ j 1)))))
                   (+ (ucnt (selU (g2 0))) (tokc n (fn [j :- Nat] (g2 (+ j 1))))))
    (congr (congrArg (fn [x :- Nat] (fn [y :- Nat] (+ x y))) (congrArg (fn [bb :- Bool] (ucnt (selU bb))) (he 0)))
           (ih_n (fn [j :- Nat] (g (+ j 1))) (fn [j :- Nat] (g2 (+ j 1))) (fn [j :- Nat] (he (+ j 1))))))
  (exact (Eq.trans (tokc_succ n g) (Eq.trans e1 (Eq.symm (tokc_succ n g2))))))

;; A token used by a conjunction of masks is used by one of them.
(thm ucnt_and [x :- Bool, y :- Bool]
  (LE.le (ucnt (selU (Bool.and x y))) (+ (ucnt (selU x)) (ucnt (selU y))))
  (cases x)
  (cases y)
  (exact (Nat.le_of_ble_eq_true (Eq.refl$1 Bool.true)))
  (exact (Nat.le_of_ble_eq_true (Eq.refl$1 Bool.true)))
  (cases y)
  (exact (Nat.le_of_ble_eq_true (Eq.refl$1 Bool.true)))
  (exact (Nat.le_of_ble_eq_true (Eq.refl$1 Bool.true))))
(thm add4_le [a :- Nat, bb :- Nat, a1 :- Nat, a2 :- Nat, b1 :- Nat, b2 :- Nat,
              ha :- (LE.le a (+ a1 a2)), hb :- (LE.le bb (+ b1 b2))]
  (LE.le (+ a bb) (+ (+ a1 b1) (+ a2 b2)))
  (omega))
(thm tok_and [m :- Nat]
  (forall [g (=> Nat Bool)] (forall [g2 (=> Nat Bool)]
    (LE.le (tokc m (fn [j :- Nat] (Bool.and (g j) (g2 j)))) (+ (tokc m g) (tokc m g2)))))
  (induction m)
  (intro g g2) (exact (Nat.zero_le 0))
  (intro g g2)
  (have h1 (LE.le (+ (ucnt (selU (Bool.and (g 0) (g2 0)))) (tokc n (fn [j :- Nat] (Bool.and (g (+ j 1)) (g2 (+ j 1))))))
                  (+ (+ (ucnt (selU (g 0))) (tokc n (fn [j :- Nat] (g (+ j 1)))))
                     (+ (ucnt (selU (g2 0))) (tokc n (fn [j :- Nat] (g2 (+ j 1)))))))
    (add4_le (ucnt (selU (Bool.and (g 0) (g2 0)))) (tokc n (fn [j :- Nat] (Bool.and (g (+ j 1)) (g2 (+ j 1)))))
             (ucnt (selU (g 0))) (ucnt (selU (g2 0))) (tokc n (fn [j :- Nat] (g (+ j 1)))) (tokc n (fn [j :- Nat] (g2 (+ j 1))))
             (ucnt_and (g 0) (g2 0)) (ih_n (fn [j :- Nat] (g (+ j 1))) (fn [j :- Nat] (g2 (+ j 1))))))
  (exact h1))

;; A variable occurrence var i, read at cutoffs cut, cut+1, …: at most one
;; position j has j + cut = i.
(thm beq_lt_ff [i :- Nat, j :- Nat, cut :- Nat, hi :- (LT.lt i cut)]
  (Eq Bool (Bool.not (Nat.beq i (+ j cut))) Bool.true)
  (exact (bool_case (fn [bb :- Bool] (=> (Eq Bool (Nat.beq i (+ j cut)) bb) (Eq Bool (Bool.not bb) Bool.true)))
           (Nat.beq i (+ j cut))
           (fn [ht :- (Eq Bool (Nat.beq i (+ j cut)) Bool.true), he :- (Eq Bool (Nat.beq i (+ j cut)) Bool.true)]
             (absurd (Nat.eq_of_beq_eq_true he) (Nat.ne_of_lt (Nat.lt_of_lt_of_le hi (Nat.le_add_left cut j)))))
           (fn [hf :- (Eq Bool (Nat.beq i (+ j cut)) Bool.false), he :- (Eq Bool (Nat.beq i (+ j cut)) Bool.false)]
             (Eq.refl$1 Bool.true))
           (Eq.refl$1 (Nat.beq i (+ j cut))))))
;; Below the occurrence's cutoff no position matches.
(thm tok_var_none [i :- Nat, cut :- Nat, hi :- (LT.lt i cut), m :- Nat]
  (Eq Nat (tokc m (fn [j :- Nat] (Bool.not (Nat.beq i (+ j cut))))) 0)
  (exact (Eq.trans (tok_congr m (fn [j :- Nat] (Bool.not (Nat.beq i (+ j cut)))) (fn [j :- Nat] Bool.true)
                     (fn [j :- Nat] (beq_lt_ff i j cut hi)))
                   (tok_true m))))
;; Position j + 1 at cutoff cut is position j at cutoff cut + 1.
(thm succ_add_cut [j :- Nat, cut :- Nat] (Eq Nat (+ (+ j 1) cut) (+ j (+ cut 1))) (omega))
(thm tok_var_shift [i :- Nat, cut :- Nat, m :- Nat]
  (Eq Nat (tokc m (fn [j :- Nat] (Bool.not (Nat.beq i (+ (+ j 1) cut)))))
          (tokc m (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1)))))))
  (exact (tok_congr m (fn [j :- Nat] (Bool.not (Nat.beq i (+ (+ j 1) cut)))) (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1)))))
           (fn [j :- Nat] (congrArg (fn [x :- Nat] (Bool.not (Nat.beq i x))) (succ_add_cut j cut))))))
(thm lt_succ_of_eq0 [i :- Nat, cut :- Nat, h :- (Eq Nat i (+ 0 cut))] (LT.lt i (+ cut 1)) (omega))
(thm le1_of [x :- Nat, y :- Nat, hx :- (LE.le x 1), hy :- (Eq Nat y 0)] (LE.le (+ x y) 1) (omega))
(thm le1_of2 [x :- Nat, y :- Nat, hx :- (Eq Nat x 0), hy :- (LE.le y 1)] (LE.le (+ x y) 1) (omega))
;; The step, by the value of the test at position 0 (in the goal, so cases
;; sees it).
(thm tok_var_step [i :- Nat, n :- Nat, cut :- Nat,
                   ih :- (LE.le (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1)))))) 1)]
  (forall [bb Bool] (=> (Eq Bool (Nat.beq i (+ 0 cut)) bb)
    (LE.le (+ (ucnt (selU (Bool.not bb))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1))))))) 1)))
  (intro bb) (cases bb)
  (intro hb)
  (exact (le1_of2 (ucnt (selU (Bool.not Bool.false))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1)))))) (Eq.refl$1 0) ih))
  (intro hb)
  (exact (le1_of (ucnt (selU (Bool.not Bool.true))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1))))))
           (Nat.le_refl 1)
           (tok_var_none i (+ cut 1) (lt_succ_of_eq0 i cut (Nat.eq_of_beq_eq_true hb)) n))))
;; A variable occurrence uses at most one token.
(thm tok_var [i :- Nat, m :- Nat]
  (forall [cut Nat] (LE.le (tokc m (fn [j :- Nat] (Bool.not (Nat.beq i (+ j cut))))) 1))
  (induction m)
  (intro cut) (exact (Nat.zero_le 1))
  (intro cut)
  (have e0 (Eq Nat (tokc (+ n 1) (fn [j :- Nat] (Bool.not (Nat.beq i (+ j cut)))))
                   (+ (ucnt (selU (Bool.not (Nat.beq i (+ 0 cut))))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ (+ j 1) cut)))))))
    (rfl))
  (have e1 (Eq Nat (+ (ucnt (selU (Bool.not (Nat.beq i (+ 0 cut))))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ (+ j 1) cut))))))
                   (+ (ucnt (selU (Bool.not (Nat.beq i (+ 0 cut))))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1))))))))
    (congrArg (fn [y :- Nat] (+ (ucnt (selU (Bool.not (Nat.beq i (+ 0 cut))))) y)) (tok_var_shift i cut n)))
  (have h2 (LE.le (+ (ucnt (selU (Bool.not (Nat.beq i (+ 0 cut))))) (tokc n (fn [j :- Nat] (Bool.not (Nat.beq i (+ j (+ cut 1))))))) 1)
    (tok_var_step i n cut (ih_n (+ cut 1)) (Nat.beq i (+ 0 cut)) (Eq.refl$1 (Nat.beq i (+ 0 cut)))))
  (exact (Nat.le_trans (Nat.le_of_eq (Eq.trans e0 e1)) h2)))

;; ---------------------------------------------------------------------------
;; E4 for tokens: tokc m (freshF t) ≤ ‖⌜t⌝‖, by induction on t, at every cutoff.

;; The motive: at cutoff cut (variables read as j + cut), the tokens t uses
;; are at most ⌜t⌝'s internal nodes.  The cutoff is added on the right, so
;; that a binder's cut + k is, definitionally, the field's own cutoff.
(defn- tok-motive [x]
  (list 'forall '[cu Nat] (list 'forall '[mm Nat]
    (list 'LE.le (list 'tokc 'mm (list 'fn '[jj :- Nat] (list (list 'freshF x) '(+ jj cu)))) (list 'cnodes (list 'encE x))))))

(defn- sum-r [ts] (reduce (fn [acc t] (list '+ t acc)) (last ts) (reverse (butlast ts))))
(defn- and-r [ts] (reduce (fn [acc t] (list 'Bool.and t acc)) (last ts) (reverse (butlast ts))))

(thm var_nodes_pos [i :- Nat] (LE.le 1 (cnodes (encE (Exp.var i))))
  (have e (Eq Nat (cnodes (encE (Exp.var i))) (+ 1 (+ (cnodes (encNat i)) 0))) (rfl))
  (omega))

(doseq [[[ctor fields :as c] clause] ctor-clauses]
  (let [xs (exp-fields fields)
        dep (depths ctor fields)
        v (h/f7-app (symbol (str "Exp." ctor)) (map first fields))
        goal (list 'LE.le (list 'tokc 'mm (list 'fn '[jj :- Nat] (list (list 'freshF v) '(+ jj cu)))) (list 'cnodes (list 'encE v)))
        params (h/f7-params (concat fields (for [x xs] [(symbol (str "ih_" x)) (tok-motive x)]) [['cu 'Nat] ['mm 'Nat]]))
        ;; the field's reading inside freshF: (freshF x) at (jj + cu) + k
        at (fn [x] (let [k (dep x)] (if (zero? k) '(+ jj cu) (list '+ '(+ jj cu) k))))
        cut-of (fn [x] (let [k (dep x)] (if (zero? k) 'cu (list '+ 'cu k))))
        reading (fn [x] (list (list 'freshF x) (at x)))
        tk (fn [x] (list 'tokc 'mm (list 'fn '[jj :- Nat] (reading x))))
        chain-bound (fn chain-bound [ys]
                      (if (= 1 (count ys))
                        (list 'Nat.le_refl (tk (first ys)))
                        (list 'Nat.le_trans
                              (list 'tok_and 'mm (list 'fn '[jj :- Nat] (reading (first ys)))
                                    (list 'fn '[jj :- Nat] (and-r (map reading (rest ys)))))
                              (list 'Nat.add_le_add_left (chain-bound (rest ys)) (tk (first ys))))))
        ih-bound (fn [x] (list (symbol (str "ih_" x)) (cut-of x) 'mm))
        add-bound (fn add-bound [ys]
                    (if (= 1 (count ys)) (ih-bound (first ys))
                        (list 'Nat.add_le_add (ih-bound (first ys)) (add-bound (rest ys)))))
        sumC (when (seq xs) (sum-r (map #(list 'cnodes (list 'encE %)) xs)))
        tactics (cond
                  (= ctor 'var)
                  ['(exact (Nat.le_trans (tok_var i mm cu) (var_nodes_pos i)))]
                  (empty? xs)
                  [(list 'exact (list 'Nat.le_trans '(Nat.le_of_eq (tok_true mm)) (list 'Nat.zero_le (list 'cnodes (list 'encE v)))))]
                  :else
                  ;; the tokens of v (stated at the goal's own reading, so
                  ;; omega sees one atom) are at most the fields' nodes,
                  ;; which ⌜v⌝'s nodes include
                  [(list 'have 'q_ec (list 'Eq 'Nat (list 'cnodes (list 'encE v)) (nodes-expr (clause-body c clause))) '(rfl))
                   (list 'have 'q_s1 (list 'LE.le (list 'tokc 'mm (list 'fn '[jj :- Nat] (and-r (map reading xs)))) (sum-r (map tk xs)))
                         (chain-bound xs))
                   (list 'have 'q_s2 (list 'LE.le (sum-r (map tk xs)) sumC) (add-bound xs))
                   (list 'have 'q_s12 (list 'LE.le (list 'tokc 'mm (list 'fn '[jj :- Nat] (list (list 'freshF v) '(+ jj cu)))) sumC)
                         '(Nat.le_trans q_s1 q_s2))
                   '(omega)])]
    (a/prove-theorem (symbol (str "tokE_" ctor)) (lv params) (lv goal) (lv tactics))))

(a/prove-theorem 'tok_encE_cut '[ex :- Exp] (tok-motive 'ex)
  (lv (into ['(induction ex)]
            (map (fn [[ctor fields]]
                   (list 'exact (apply list (symbol (str "tokE_" ctor))
                                       (concat (map first fields) (map #(symbol (str "ih_" %)) (exp-fields fields))))))
                 exp-ctors))))

;; E4 for tokens, as TokSize reads it: the tokens of Θₘ free in t are at most
;; the internal nodes of ⌜t⌝.
(thm tok_encE [t :- Exp, m :- Nat]
  (LE.le (cntU (maskUF (thetaU m) (freshF t))) (cnodes (encE t)))
  (exact (tok_encE_cut t 0 m)))

;; ---------------------------------------------------------------------------
;; Unary numbers.

(thm encNat_nodes_succ [n :- Nat] (Eq Nat (cnodes (encNat (+ n 1))) (+ 1 (+ (cnodes (encNat n)) 0))) (rfl))
(thm succ_arith_e [x :- Nat, n :- Nat, h :- (Eq Nat x n)] (Eq Nat (+ 1 (+ x 0)) (+ n 1)) (omega))
;; A unary number n has exactly n internal nodes.
(thm cnodes_encNat [n :- Nat] (Eq Nat (cnodes (encNat n)) n)
  (induction n)
  (rfl)
  (exact (Eq.trans (encNat_nodes_succ n) (succ_arith_e (cnodes (encNat n)) n ih_n))))

;; ---------------------------------------------------------------------------
;; Raw codes written as literal terms.

(thm code_enc_sl [l :- Nat] (LT.lt (cnodes (Code.sl l)) (cnodes (encE (codeTerm (Code.sl l)))))
  (have e1 (Eq Nat (cnodes (Code.sl l)) 0) (rfl))
  (have e2 (Eq Nat (cnodes (encE (codeTerm (Code.sl l)))) (+ 1 (+ (+ 1 (+ 0 0)) 0))) (rfl))
  (omega))
(thm code_enc_sn [l :- Nat, a :- Code, b :- Code,
                  ha :- (LT.lt (cnodes a) (cnodes (encE (codeTerm a)))), hb :- (LT.lt (cnodes b) (cnodes (encE (codeTerm b))))]
  (LT.lt (cnodes (Code.sn l a b)) (cnodes (encE (codeTerm (Code.sn l a b)))))
  (have e1 (Eq Nat (cnodes (Code.sn l a b)) (+ 1 (+ (cnodes a) (cnodes b)))) (rfl))
  (have e2 (Eq Nat (cnodes (encE (codeTerm (Code.sn l a b))))
                   (+ 1 (+ (+ 1 (+ 0 0)) (+ 1 (+ (cnodes (encE (codeTerm a))) (cnodes (encE (codeTerm b))))))))
    (rfl))
  (omega))
;; A code written as its literal term has more internal nodes than the code
;; (E4: every node becomes an snode, every label a node under lblc).
(thm code_enc_gt [c :- Code] (LT.lt (cnodes c) (cnodes (encE (codeTerm c))))
  (induction c)
  (exact (code_enc_sl l))
  (exact (code_enc_sn l a b ih_a ih_b)))

;; The label constants of a code's literal term are the code's labels.
(thm lbl_codeTerm_sn [nb :- Nat, l :- Nat, a :- Code, b :- Code,
                      ia :- (=> (Eq Bool (lblBelow nb a) Bool.true) (Eq Bool (lblsE nb (codeTerm a)) Bool.true)),
                      ib :- (=> (Eq Bool (lblBelow nb b) Bool.true) (Eq Bool (lblsE nb (codeTerm b)) Bool.true)),
                      h :- (Eq Bool (lblBelow nb (Code.sn l a b)) Bool.true)]
  (Eq Bool (lblsE nb (codeTerm (Code.sn l a b))) Bool.true)
  (have hl (Eq Bool (Nat.blt l nb) Bool.true) (band_left (Nat.blt l nb) (Bool.and (lblBelow nb a) (lblBelow nb b)) h))
  (have hab (Eq Bool (Bool.and (lblBelow nb a) (lblBelow nb b)) Bool.true)
    (band_right (Nat.blt l nb) (Bool.and (lblBelow nb a) (lblBelow nb b)) h))
  (have q (Eq Bool (Bool.and (Nat.blt l nb)
                             (Bool.and (lblsE nb (codeTerm a)) (Bool.and (lblsE nb (codeTerm b)) Bool.true))) Bool.true)
    (band_tt2 (Nat.blt l nb) (Bool.and (lblsE nb (codeTerm a)) (Bool.and (lblsE nb (codeTerm b)) Bool.true))
      hl
      (band_tt2 (lblsE nb (codeTerm a)) (Bool.and (lblsE nb (codeTerm b)) Bool.true)
        (ia (band_left (lblBelow nb a) (lblBelow nb b) hab))
        (band_tt2 (lblsE nb (codeTerm b)) Bool.true (ib (band_right (lblBelow nb a) (lblBelow nb b) hab)) (Eq.refl$1 Bool.true)))))
  (exact q))
(thm lbl_codeTerm [nb :- Nat, c :- Code]
  (=> (Eq Bool (lblBelow nb c) Bool.true) (Eq Bool (lblsE nb (codeTerm c)) Bool.true))
  (induction c)
  (intro h) (exact (band_tt2 (Nat.blt l nb) Bool.true h (Eq.refl$1 Bool.true)))
  (intro h) (exact (lbl_codeTerm_sn nb l a b ih_a ih_b h)))

;; ---------------------------------------------------------------------------
;; Internal labels.

;; nlb nb c: every internal node of c is labelled below nb; leaves are free.
(kdef nlb (=> Nat Code Bool)
  (fn [nb :- Nat, c :- Code]
    (Code.rec$1 (fn [_ :- Code] Bool)
      (fn [l :- Nat] Bool.true)
      (fn [l :- Nat, a :- Code, b :- Code, ia :- Bool, ib :- Bool] (Bool.and (Nat.blt l nb) (Bool.and ia ib)))
      c)))

;; uLbl r L0 (the three usages of Π, Σ, λ) is below 96 when L0 + 3 ≤ 96.
(thm uLbl_lt96 [r :- U, L0 :- Nat, hL :- (Eq Bool (Nat.ble (+ L0 3) 96) Bool.true)]
  (Eq Bool (Nat.blt (uLbl r L0) 96) Bool.true)
  (have h3 (LE.le (+ L0 3) 96) (Nat.le_of_ble_eq_true hL))
  (cases r)
  (change (Eq Bool (Nat.blt L0 96) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega)
  (change (Eq Bool (Nat.blt (+ L0 1) 96) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega)
  (change (Eq Bool (Nat.blt (+ L0 2) 96) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega))

(defn nlb-proof
  "A proof that nlb 96 c = true, for c built from leaves, nodes with a
  literal (or uLbl) label, and pieces whose proof (child piece) supplies.
  Shared with certcanon.clj."
  [c child]
  (cond
    (and (seq? c) (= 'Code.sl (first c))) '(Eq.refl$1 Bool.true)
    (and (seq? c) (= 'Code.sn (first c)))
    (let [[_ L x y] c]
      (list 'band_tt2 (list 'Nat.blt L 96) (list 'Bool.and (list 'nlb 96 x) (list 'nlb 96 y))
            (cond (integer? L) '(Eq.refl$1 Bool.true)
                  (and (seq? L) (= 'uLbl (first L))) (list 'uLbl_lt96 (nth L 1) (nth L 2) '(Eq.refl$1 Bool.true))
                  :else (throw (ex-info "unexpected label" {:label L})))
            (list 'band_tt2 (list 'nlb 96 x) (list 'nlb 96 y) (nlb-proof x child) (nlb-proof y child))))
    :else (child c)))

;; Unary numbers: internal label 3 only.
(thm nlb_encNat [j :- Nat] (Eq Bool (nlb 96 (encNat j)) Bool.true)
  (induction j)
  (rfl)
  (have q (Eq Bool (nlb 96 (Code.sn 3 (encNat n) (Code.sl 0))) Bool.true)
    (band_tt2 (Nat.blt 3 96) (Bool.and (nlb 96 (encNat n)) (nlb 96 (Code.sl 0))) (Eq.refl$1 Bool.true)
      (band_tt2 (nlb 96 (encNat n)) (nlb 96 (Code.sl 0)) ih_n (Eq.refl$1 Bool.true))))
  (exact q))

;; Expressions: every internal label of encE's clauses is below 96, one
;; lemma per constructor from the clause itself.
(doseq [[[ctor fields :as c] clause] ctor-clauses]
  (let [xs (exp-fields fields)
        v (h/f7-app (symbol (str "Exp." ctor)) (map first fields))
        body (clause-body c clause)
        child (fn [p] (cond (and (seq? p) (= 'encE (first p))) (symbol (str "ih_" (second p)))
                            (and (seq? p) (= 'encNat (first p))) (list 'nlb_encNat (second p))
                            :else (throw (ex-info "unexpected piece" {:piece p}))))]
    (a/prove-theorem (symbol (str "nlbE_" ctor))
      (lv (h/f7-params (concat fields (for [x xs] [(symbol (str "ih_" x)) (list 'Eq 'Bool (list 'nlb 96 (list 'encE x)) 'Bool.true)]))))
      (lv (list 'Eq 'Bool (list 'nlb 96 (list 'encE v)) 'Bool.true))
      (lv [(list 'have 'q_nlb (list 'Eq 'Bool (list 'nlb 96 body) 'Bool.true) (nlb-proof body child))
           '(exact q_nlb)]))))
(a/prove-theorem 'nlb_encE '[ex :- Exp] '(Eq Bool (nlb 96 (encE ex)) Bool.true)
  (lv (into ['(induction ex)]
            (map (fn [[ctor fields]]
                   (list 'exact (apply list (symbol (str "nlbE_" ctor))
                                       (concat (map first fields) (map #(symbol (str "ih_" %)) (exp-fields fields))))))
                 exp-ctors))))
