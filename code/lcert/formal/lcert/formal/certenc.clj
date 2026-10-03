(ns lcert.formal.certenc
  "F7 step 5c — certificates as codes: the encoding of derivation trees, its
  decoder and round trip, and completeness of the concrete checker
  (ADR-0006, F7 plan).

  check_spec.clj proves CheckSpec, TokSize and TypeSize for Check decD with
  *any* decoder — the decoder that decodes nothing included. What makes
  Check the checker of the certificates is this namespace: an encoding of
  certificates whose decoder decCert inverts it, and
    check_complete: if Check decCert accepts a typing tree and a formation
    tree as derivations, it accepts their encoding (suitably padded) as a
    certificate.

  The encoding. Every value is a code (skel.clj's Code: sl l | sn l a b).
  Nat n is sl n; Bool, U and Sk constructors are numbered leaves (Sk's two
  binary constructors are sn 7 / sn 8 of their children); Code is itself;
  Exp is encE (encode.clj, decoded through decT, round trip `roundtrip`).
  Lists: nil = sl 0, cons x r = sn 1 ⌜x⌝ ⌜r⌝. A constructor with fields
  f1 … fk (HdDT, StepDT, SkDT, DT) is sn i ⌜f1⌝ (sn 0 ⌜f2⌝ (… (sn 0 ⌜fk⌝
  (sl 0)))), i its position; without fields it is sl i. Field j of such a
  node N is chHead (chTail^j N).

  Decoding. Base types decode by structural recursion on the code. SkDT and
  DT nodes keep their fields in a chain, not as direct children, so they
  decode with fuel (decSkDT k, decDT k); a certificate records the fuel its
  trees need. Round trips are proved per constructor: an equation lemma by
  computation, then rewriting with the fields' round trips.

  A certificate is sn 0 ⌜(fuel, m, t, A, typing tree, formation tree)⌝ pad:
  the padding (prop410's padC) lets completeness make a certificate as large
  as the checker's size tests require."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.check-skj]
            [lcert.formal.check-dt :as dt]
            [lcert.formal.prop410]
            [lcert.formal.check-spec]))

;; ---------------------------------------------------------------------------
;; Generator helpers (surface syntax only; every result is kernel-checked).

(defn- chain-enc
  "The code of constructor i with field codes encs."
  [i encs]
  (if (empty? encs)
    (list 'Code.sl i)
    (list 'Code.sn i (first encs)
          (reduce (fn [acc e] (list 'Code.sn 0 e acc)) '(Code.sl 0) (reverse (rest encs))))))

(defn- field-at
  "Field j of the node code N: chHead (chTail^j N)."
  [N j]
  (list 'chHead (nth (iterate #(list 'chTail %) N) j)))

(defn- dispatch
  "Option T by the label l: the case whose index Nat.beq-matches, else none."
  [l T cases]
  (reduce (fn [acc [i e]] (list 'Bool.rec$1 (list 'fn '[_ :- Bool] (list 'Option T)) acc e (list 'Nat.beq l i)))
          (list 'Option.none T) (reverse cases)))

(defn- ob-chain
  "Decode fields in order — [[F dec-term] …] — binding v0, v1, …, then
  (Option.some T (k [v0 v1 …]))."
  [T fields k]
  (let [vs (mapv #(symbol (str "v" %)) (range (count fields)))]
    (reduce (fn [acc [i [F d]]] (list 'ob F T d (list 'fn [(vs i) :- F] acc)))
            (list 'Option.some T (k vs))
            (reverse (map-indexed vector fields)))))

;; Field heads and tails of a node; a leaf has neither (sl 0 stands in).
(kdef chHead (=> Code Code)
  (fn [c :- Code] (Code.rec$1 (fn [_ :- Code] Code) (fn [l :- Nat] (Code.sl 0))
                    (fn [l :- Nat, a :- Code, b :- Code, ia :- Code, ib :- Code] a) c)))
(kdef chTail (=> Code Code)
  (fn [c :- Code] (Code.rec$1 (fn [_ :- Code] Code) (fn [l :- Nat] (Code.sl 0))
                    (fn [l :- Nat, a :- Code, b :- Code, ia :- Code, ib :- Code] b) c)))

;; ---------------------------------------------------------------------------
;; Base types.

(kdef encN (=> Nat Code) (fn [n :- Nat] (Code.sl n)))
(kdef decN (=> Code (Option Nat))
  (fn [c :- Code] (Code.rec$1 (fn [_ :- Code] (Option Nat)) (fn [l :- Nat] (Option.some Nat l))
                    (fn [l :- Nat, a :- Code, b :- Code, ia :- (Option Nat), ib :- (Option Nat)] (Option.none Nat)) c)))
(thm rtN [n :- Nat] (Eq (Option Nat) (decN (encN n)) (Option.some Nat n)) (rfl))

(kdef encC (=> Code Code) (fn [c :- Code] c))
(kdef decC (=> Code (Option Code)) (fn [c :- Code] (Option.some Code c)))
(thm rtC [c :- Code] (Eq (Option Code) (decC (encC c)) (Option.some Code c)) (rfl))

(kdef decE (=> Code (Option Exp)) (fn [c :- Code] (Prod.fst (decT c))))
(thm rtE [e :- Exp] (Eq (Option Exp) (decE (encE e)) (Option.some Exp e))
  (exact (congrArg (fn [tp :- Tup] (Prod.fst tp)) (roundtrip e))))

;; Enumerations: Bool and U as numbered leaves.
(doseq [[T enc dec rt ctors] [['Bool 'encB 'decB 'rtB ['Bool.false 'Bool.true]]
                              ['U 'encU 'decU 'rtU ['U.u0 'U.u1 'U.uw]]]]
  (b/kdef! enc (list '=> T 'Code)
    (list 'fn ['v :- T] (concat (list (symbol (str T ".rec$1")) (list 'fn ['_ :- T] 'Code))
                                (map-indexed (fn [i _] (list 'Code.sl i)) ctors) ['v])))
  (b/kdef! dec (list '=> 'Code (list 'Option T))
    (list 'fn '[c :- Code]
      (list 'Code.rec$1 (list 'fn '[_ :- Code] (list 'Option T))
        (list 'fn '[l :- Nat] (dispatch 'l T (map-indexed (fn [i k] [i (list 'Option.some T k)]) ctors)))
        (list 'fn ['l :- 'Nat 'a :- 'Code 'b :- 'Code 'ia :- (list 'Option T) 'ib :- (list 'Option T)] (list 'Option.none T))
        'c)))
  (a/prove-theorem rt (lv ['v :- T]) (lv (list 'Eq (list 'Option T) (list dec (list enc 'v)) (list 'Option.some T 'v)))
    (lv (into ['(cases v)] (repeat (count ctors) '(rfl))))))

;; Skeletons: seven numbered leaves; arr and prod as sn 7 / sn 8 of their children.
(def ^:private sk-consts '[Sk.unit Sk.bool Sk.nat Sk.lbl Sk.syn Sk.dia Sk.cert])
(b/kdef! 'encS '(=> Sk Code)
  (list 'fn '[s :- Sk]
    (concat (list 'Sk.rec$1 '(fn [_ :- Sk] Code)) (map-indexed (fn [i _] (list 'Code.sl i)) sk-consts)
      ['(fn [s1 :- Sk, s2 :- Sk, i1 :- Code, i2 :- Code] (Code.sn 7 i1 i2))
       '(fn [s1 :- Sk, s2 :- Sk, i1 :- Code, i2 :- Code] (Code.sn 8 i1 i2)) 's])))
(b/kdef! 'decS '(=> Code (Option Sk))
  (list 'fn '[c :- Code]
    (list 'Code.rec$1 '(fn [_ :- Code] (Option Sk))
      (list 'fn '[l :- Nat] (dispatch 'l 'Sk (map-indexed (fn [i k] [i (list 'Option.some 'Sk k)]) sk-consts)))
      (list 'fn '[l :- Nat, a :- Code, b :- Code, ia :- (Option Sk), ib :- (Option Sk)]
        (dispatch 'l 'Sk [[7 '(ob Sk Sk ia (fn [x :- Sk] (om Sk Sk (fn [y :- Sk] (Sk.arr x y)) ib)))]
                          [8 '(ob Sk Sk ia (fn [x :- Sk] (om Sk Sk (fn [y :- Sk] (Sk.prod x y)) ib)))]]))
      'c)))
;; Each inductive case is its own lemma: inside the induction, have + rw
;; leaves the other case in front, and the next rw meets the wrong goal.
(doseq [[nm ctor] [['rtS_arr 'Sk.arr] ['rtS_prod 'Sk.prod]]]
  (a/prove-theorem nm
    (lv '[s :- Sk, t :- Sk, hs :- (Eq (Option Sk) (decS (encS s)) (Option.some Sk s)),
          ht :- (Eq (Option Sk) (decS (encS t)) (Option.some Sk t))])
    (lv (list 'Eq '(Option Sk) (list 'decS (list 'encS (list ctor 's 't))) (list 'Option.some 'Sk (list ctor 's 't))))
    (lv [(list 'have 'e0 (list 'Eq '(Option Sk) (list 'decS (list 'encS (list ctor 's 't)))
                               (list 'ob 'Sk 'Sk '(decS (encS s))
                                     (list 'fn '[x :- Sk] (list 'om 'Sk 'Sk (list 'fn '[y :- Sk] (list ctor 'x 'y)) '(decS (encS t))))))
               (list 'Eq.refl (list 'decS (list 'encS (list ctor 's 't)))))
         '(rw [e0]) '(rw [hs]) '(rw [ht])])))
(thm rtS [sk :- Sk] (Eq (Option Sk) (decS (encS sk)) (Option.some Sk sk))
  (induction sk) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl)
  (exact (rtS_arr s t ih_s ih_t)) (exact (rtS_prod s t ih_s ih_t)))

;; The field types a certificate's trees hold, with encoder, decoder and
;; round trip (rt v : dec (enc v) = some v). Lists are added below.
(def ^:private ty-info
  (atom '{Nat [encN decN rtN], Bool [encB decB rtB], U [encU decU rtU], Code [encC decC rtC],
          Exp [encE decE rtE], Sk [encS decS rtS]}))

;; Lists: nil = sl 0, cons x r = sn 1 ⌜x⌝ ⌜r⌝, structural.
(doseq [[suffix T] '[[N Nat] [E Exp] [U U] [S Sk]]]
  (let [[encT decT rtT] (@ty-info T)
        LT (list 'List T)
        enc (symbol (str "encL" suffix)) dec (symbol (str "decL" suffix)) rt (symbol (str "rtL" suffix))]
    (b/kdef! enc (list '=> LT 'Code)
      (list 'fn ['l :- LT]
        (list 'List.rec$1$0 T (list 'fn ['_ :- LT] 'Code) '(Code.sl 0)
              (list 'fn ['x :- T 'xs :- LT 'ih :- 'Code] (list 'Code.sn 1 (list encT 'x) 'ih)) 'l)))
    (b/kdef! dec (list '=> 'Code (list 'Option LT))
      (list 'fn '[c :- Code]
        (list 'Code.rec$1 (list 'fn '[_ :- Code] (list 'Option LT))
          (list 'fn '[l :- Nat] (dispatch 'l LT [[0 (list 'Option.some LT (list 'List.nil T))]]))
          (list 'fn ['l :- 'Nat 'a :- 'Code 'b :- 'Code 'ia :- (list 'Option LT) 'ib :- (list 'Option LT)]
            (dispatch 'l LT [[1 (list 'ob T LT (list decT 'a)
                                      (list 'fn ['x :- T] (list 'om LT LT (list 'fn ['r :- LT] (list 'List.cons T 'x 'r)) 'ib)))]]))
          'c)))
    (a/prove-theorem (symbol (str rt "_cons"))
      (lv ['x :- T 'xs :- LT 'ih :- (list 'Eq (list 'Option LT) (list dec (list enc 'xs)) (list 'Option.some LT 'xs))])
      (lv (list 'Eq (list 'Option LT) (list dec (list enc (list 'List.cons T 'x 'xs))) (list 'Option.some LT (list 'List.cons T 'x 'xs))))
      (lv [(list 'have 'e0 (list 'Eq (list 'Option LT) (list dec (list enc (list 'List.cons T 'x 'xs)))
                                 (list 'ob T LT (list decT (list encT 'x))
                                       (list 'fn ['y :- T] (list 'om LT LT (list 'fn ['r :- LT] (list 'List.cons T 'y 'r))
                                                                 (list dec (list enc 'xs))))))
                 (list 'Eq.refl (list dec (list enc (list 'List.cons T 'x 'xs)))))
           (list 'have 'e1 (list 'Eq (list 'Option T) (list decT (list encT 'x)) (list 'Option.some T 'x)) (list rtT 'x))
           '(rw [e0]) '(rw [e1]) '(rw [ih])]))
    (a/prove-theorem rt (lv ['l :- LT]) (lv (list 'Eq (list 'Option LT) (list dec (list enc 'l)) (list 'Option.some LT 'l)))
      (lv ['(induction l) '(rfl) (list 'exact (list (symbol (str rt "_cons")) 'head 'tail 'ih_tail))]))
    (swap! ty-info assoc LT [enc dec rt])))

;; ---------------------------------------------------------------------------
;; Constructors with fields, non-recursive: head steps and steps.

(defn- define-fielded!
  "Encoder, decoder and round trip for a non-recursive inductive T whose
  constructors are ctors = [[name [[field type] …]] …], every field type in
  ty-info. Then T joins ty-info."
  [T enc dec rt ctors]
  (let [info @ty-info
        indexed (map-indexed vector ctors)]
    (b/kdef! enc (list '=> T 'Code)
      (list 'fn ['v :- T]
        (concat (list (symbol (str T ".rec$1")) (list 'fn ['_ :- T] 'Code))
          (for [[i [_ fields]] indexed]
            (let [body (chain-enc i (for [[x F] fields] (list (first (info F)) x)))]
              (if (seq fields) (list 'fn (h/f7-params fields) body) body)))
          ['v])))
    (b/kdef! dec (list '=> 'Code (list 'Option T))
      (list 'fn '[c :- Code]
        (list 'Code.rec$1 (list 'fn '[_ :- Code] (list 'Option T))
          (list 'fn '[l :- Nat]
            (dispatch 'l T (for [[i [nm fields]] indexed :when (empty? fields)]
                             [i (list 'Option.some T (symbol (str T "." nm)))])))
          (list 'fn ['l :- 'Nat 'a :- 'Code 'b :- 'Code 'ia :- (list 'Option T) 'ib :- (list 'Option T)]
            (dispatch 'l T (for [[i [nm fields]] indexed :when (seq fields)]
                             [i (ob-chain T (map-indexed (fn [j [_ F]] [F (list (second (info F)) (field-at '(Code.sn l a b) j))]) fields)
                                          (fn [vs] (apply list (symbol (str T "." nm)) vs)))])))
          'c)))
    ;; one lemma per constructor, then cases
    (doseq [[i [nm fields]] indexed]
      (let [v (h/f7-app (symbol (str T "." nm)) (map first fields))
            goal (list 'Eq (list 'Option T) (list dec (list enc v)) (list 'Option.some T v))]
        (a/prove-theorem (symbol (str rt "_" nm)) (lv (h/f7-params fields)) (lv goal)
          (lv (if (empty? fields)
                ['(rfl)]
                (concat
                  [(list 'have 'q_rt0 (list 'Eq (list 'Option T) (list dec (list enc v))
                                         (ob-chain T (for [[x F] fields] [F (list (second (info F)) (list (first (info F)) x))])
                                                   (fn [vs] (apply list (symbol (str T "." nm)) vs))))
                         (list 'Eq.refl (list dec (list enc v))))]
                  (for [[j [x F]] (map-indexed vector fields)]
                    (list 'have (symbol (str "q_rt" (inc j)))
                          (list 'Eq (list 'Option F) (list (second (info F)) (list (first (info F)) x)) (list 'Option.some F x))
                          (list (nth (info F) 2) x)))
                    ;; (named q_rt…: a field may be called e2, as StepDT's is)
                  ;; one rewrite per equation: a combined rw [e0 e1 …] fails here
                  ;; and try: rw closes the goal by rfl as soon as the rest computes
                  ;; (a Nat or Code field decodes by computation alone)
                  (for [j (range (inc (count fields)))] (list 'try (list 'rw [(symbol (str "q_rt" j))])))))))))
    (a/prove-theorem rt (lv ['v :- T]) (lv (list 'Eq (list 'Option T) (list dec (list enc 'v)) (list 'Option.some T 'v)))
      (lv (into ['(cases v)]
                (for [[_ [nm fields]] indexed]
                  (list 'exact (apply list (symbol (str rt "_" nm)) (map first fields)))))))
    (swap! ty-info assoc T [enc dec rt])))

(def ^:private hd-data @#'h/hd-data)
(define-fielded! 'HdDT 'encHd 'decHd 'rtHd
  (vec (for [rule h/hd-rules] [(first rule) (hd-data rule)])))
(define-fielded! 'StepDT 'encSt 'decSt 'rtSt
  '[[at [[p (List Nat)] [head HdDT] [e Exp] [e2 Exp]]]])

;; ---------------------------------------------------------------------------
;; Recursive trees, decoded with fuel: skeleton trees, then derivation trees.

(defn- sum-terms [ts] (reduce (fn [acc t] (list '+ t acc)) 0 (reverse ts)))

(defn- le-in-sum
  "A proof that summand i of ts is at most their sum (sum-terms):
  b_i ≤ b_i + S_(i+1), then b_i ≤ b_j + S_(j+1) for j = i-1 … 0."
  [ts i]
  (let [S (fn [k] (sum-terms (drop k ts)))]
    (reduce (fn [prev j] (list 'Nat.le_trans prev (list 'Nat.le_add_left (S (inc j)) (nth ts j))))
            (list 'Nat.le_add_right (nth ts i) (S (inc i)))
            (range (dec i) -1 -1))))

(def ^:private fuel-info
  "Fuel-decoded types: encoder, fueled decoder (dec k c), height, round trip
  (rt v k (lt : ht v < k) : dec k (enc v) = some v)."
  (atom {}))

(defn- define-fueled!
  "Encoder, height, fueled decoder and round trip for a recursive inductive T
  with constructors ctors = [[name [[field type] …]] …]. A field's type is
  T (recursive), an earlier fuel-decoded type, or in ty-info."
  [T enc dec ht rt ctors]
  (let [info @ty-info
        finfo @fuel-info
        indexed (map-indexed vector ctors)
        rec? (fn [F] (= F T))
        fuel? (fn [F] (contains? finfo F))
        ih (fn [x] (symbol (str "ih_" x)))
        ;; summands of a node's height: recursive and fuel-typed fields
        heights (fn [fields ih-or-ht]
                  (vec (for [[x F] fields :when (or (rec? F) (fuel? F))]
                         (if (rec? F) (ih-or-ht x) (list (nth (finfo F) 2) x)))))
        node-ht (fn [fields ih-or-ht] (list '+ (sum-terms (heights fields ih-or-ht)) 1))
        ihs-of (fn [fields ty] (for [[x F] fields :when (rec? F)] [(ih x) ty]))]
    (b/kdef! enc (list '=> T 'Code)
      (list 'fn ['v :- T]
        (concat (list (symbol (str T ".rec$1")) (list 'fn ['_ :- T] 'Code))
          (for [[i [_ fields]] indexed]
            (let [body (chain-enc i (for [[x F] fields]
                                      (cond (rec? F) (ih x) (fuel? F) (list (first (finfo F)) x) :else (list (first (info F)) x))))
                  binders (concat fields (ihs-of fields 'Code))]
              (if (seq binders) (list 'fn (h/f7-params binders) body) body)))
          ['v])))
    (b/kdef! ht (list '=> T 'Nat)
      (list 'fn ['v :- T]
        (concat (list (symbol (str T ".rec$1")) (list 'fn ['_ :- T] 'Nat))
          (for [[_ [_ fields]] indexed]
            (let [body (node-ht fields ih) binders (concat fields (ihs-of fields 'Nat))]
              (if (seq binders) (list 'fn (h/f7-params binders) body) body)))
          ['v])))
    ;; dec 0 rejects; dec (k2+1) reads the node, its recursive fields at r = dec k2
    (b/kdef! dec (list '=> 'Nat 'Code (list 'Option T))
      (list 'fn '[k :- Nat]
        (list 'Nat.rec$1 (list 'fn '[_ :- Nat] (list '=> 'Code (list 'Option T)))
          (list 'fn '[c :- Code] (list 'Option.none T))
          (list 'fn ['k2 :- 'Nat 'r :- (list '=> 'Code (list 'Option T))]
            (list 'fn '[c :- Code]
              (list 'Code.rec$1 (list 'fn '[_ :- Code] (list 'Option T))
                (list 'fn '[l :- Nat]
                  (dispatch 'l T (for [[i [nm fields]] indexed :when (empty? fields)]
                                   [i (list 'Option.some T (symbol (str T "." nm)))])))
                (list 'fn ['l :- 'Nat 'a :- 'Code 'b :- 'Code 'ia :- (list 'Option T) 'ib :- (list 'Option T)]
                  (dispatch 'l T (for [[i [nm fields]] indexed :when (seq fields)]
                                   [i (ob-chain T (map-indexed (fn [j [_ F]]
                                                                 (let [fld (field-at '(Code.sn l a b) j)]
                                                                   [F (cond (rec? F) (list 'r fld)
                                                                            (fuel? F) (list (second (finfo F)) 'k2 fld)
                                                                            :else (list (second (info F)) fld))]))
                                                               fields)
                                                (fn [vs] (apply list (symbol (str T "." nm)) vs)))])))
                'c)))
          'k)))
    (let [motive (fn [x] (list 'forall '[kk Nat]
                           (list '=> (list 'Nat.lt (list ht x) 'kk)
                                 (list 'Eq (list 'Option T) (list dec 'kk (list enc x)) (list 'Option.some T x)))))]
      (doseq [[i [nm fields]] indexed]
        (let [v (h/f7-app (symbol (str T "." nm)) (map first fields))
              hts (heights fields (fn [x] (list ht x)))
              succ-nm (symbol (str rt "_" nm "_succ"))
              ;; field x's height is below k2: from hk : ht v < succ k2
              below (fn [x F]
                      (let [hx (if (rec? F) (list ht x) (list (nth (finfo F) 2) x))
                            i (.indexOf ^java.util.List hts hx)]
                        (list 'Nat.lt_of_le_of_lt (le-in-sum hts i)
                              (list 'AT_Nat.lt_of_succ_lt_succ (sum-terms hts) 'k2 'hk))))]
          ;; the work, at fuel succ k2
          (a/prove-theorem succ-nm
            (lv (h/f7-params (concat fields (for [[x F] fields :when (rec? F)] [(ih x) (motive x)]) [['k2 'Nat] ['hk (list 'Nat.lt (list ht v) '(Nat.succ k2))]])))
            (lv (list 'Eq (list 'Option T) (list dec '(Nat.succ k2) (list enc v)) (list 'Option.some T v)))
            (lv (concat
                  [(list 'have 'q_rt0 (list 'Eq (list 'Option T) (list dec '(Nat.succ k2) (list enc v))
                                            (ob-chain T (for [[x F] fields]
                                                          [F (cond (rec? F) (list dec 'k2 (list enc x))
                                                                   (fuel? F) (list (second (finfo F)) 'k2 (list (first (finfo F)) x))
                                                                   :else (list (second (info F)) (list (first (info F)) x)))])
                                                      (fn [vs] (apply list (symbol (str T "." nm)) vs))))
                         (list 'Eq.refl (list dec '(Nat.succ k2) (list enc v))))]
                  (for [[j [x F]] (map-indexed vector fields)]
                    (list 'have (symbol (str "q_rt" (inc j)))
                          (cond (rec? F) (list 'Eq (list 'Option T) (list dec 'k2 (list enc x)) (list 'Option.some T x))
                                (fuel? F) (list 'Eq (list 'Option F) (list (second (finfo F)) 'k2 (list (first (finfo F)) x)) (list 'Option.some F x))
                                :else (list 'Eq (list 'Option F) (list (second (info F)) (list (first (info F)) x)) (list 'Option.some F x)))
                          (cond (rec? F) (list (ih x) 'k2 (below x F))
                                (fuel? F) (list (nth (finfo F) 3) x 'k2 (below x F))
                                :else (list (nth (info F) 2) x))))
                  (for [j (range (inc (count fields)))] (list 'try (list 'rw [(symbol (str "q_rt" j))]))))))
          ;; every fuel: none at 0 (ht v ≥ 1), the work at a successor.
          ;; (Fields may be named n, Nat.succ's own field: the successor's
          ;; variable is never named, only unified.)
          (a/prove-theorem (symbol (str rt "_" nm))
            (lv (h/f7-params (concat fields (for [[x F] fields :when (rec? F)] [(ih x) (motive x)]))))
            (lv (motive v))
            (lv ['(intro kk) '(cases kk)
                 '(intro hk) (list 'exact (list 'absurd 'hk (list 'Nat.not_lt_zero (list ht v))))
                 '(intro hk) (list 'exact (apply list succ-nm (concat (map first fields) (for [[x F] fields :when (rec? F)] (ih x)) '[_ hk])))]))))
      (a/prove-theorem rt (lv ['v :- T]) (lv (motive 'v))
        (lv (into ['(induction v)]
                  (for [[_ [nm fields]] indexed]
                    (list 'exact (apply list (symbol (str rt "_" nm))
                                        (concat (map first fields) (for [[x F] fields :when (rec? F)] (ih x)))))))))
      (swap! fuel-info assoc T [enc dec ht rt]))))

(def ^:private skj-tree-fields @#'lcert.formal.check-skj/tree-fields)
(define-fueled! 'SkDT 'encSkDT 'decSkDT 'htSkDT 'rtSkDT
  (vec (for [rule lcert.formal.check-skj/skj-rules] [(first rule) (skj-tree-fields rule)])))

(def ^:private dt-fields @#'lcert.formal.check-dt/dt-fields)
(define-fueled! 'DT 'encDT 'decDT 'htDT 'rtDT
  (vec (for [[_ rule] dt/dt-rules] [(first rule) (dt-fields rule)])))

;; ---------------------------------------------------------------------------
;; Certificates.

(def ^:private CD '(Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
(defn- cd-tuple [m t A T1 T2]
  (list 'Prod.mk 'Nat '(Prod Exp (Prod Exp (Prod DT DT))) m
    (list 'Prod.mk 'Exp '(Prod Exp (Prod DT DT)) t
      (list 'Prod.mk 'Exp '(Prod DT DT) A (list 'Prod.mk 'DT 'DT T1 T2)))))

;; sn 0 ⌜(fuel, m, t, A, T1, T2)⌝ pad.
(b/kdef! 'encCert '(=> Nat Nat Exp Exp DT DT Code Code)
  (list 'fn '[f :- Nat, m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT, pad :- Code]
    (list 'Code.sn 0 (chain-enc 0 '[(encN f) (encN m) (encE t) (encE A) (encDT T1) (encDT T2)]) 'pad)))

;; Read the fuel first; decode both trees with it.
(b/kdef! 'decCert (list '=> 'Code (list 'Option CD))
  (list 'fn '[c :- Code]
    (let [X '(chHead c)]
      (ob-chain CD [['Nat (list 'decN (field-at X 0))] ['Nat (list 'decN (field-at X 1))]
                    ['Exp (list 'decE (field-at X 2))] ['Exp (list 'decE (field-at X 3))]
                    ['DT (list 'decDT 'v0 (field-at X 4))] ['DT (list 'decDT 'v0 (field-at X 5))]]
                (fn [[_ m t A T1 T2]] (cd-tuple m t A T1 T2))))))

(a/prove-theorem 'decCert_enc
  (lv '[f :- Nat, m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT, pad :- Code,
        h1 :- (Nat.lt (htDT T1) f), h2 :- (Nat.lt (htDT T2) f)])
  (lv (list 'Eq (list 'Option CD) '(decCert (encCert f m t A T1 T2 pad)) (list 'Option.some CD (cd-tuple 'm 't 'A 'T1 'T2))))
  (lv [(list 'have 'q_c0 (list 'Eq (list 'Option CD) '(decCert (encCert f m t A T1 T2 pad))
                               (ob-chain CD [['Exp '(decE (encE t))] ['Exp '(decE (encE A))]
                                             ['DT '(decDT f (encDT T1))] ['DT '(decDT f (encDT T2))]]
                                         (fn [[t2 A2 T1b T2b]] (cd-tuple 'm t2 A2 T1b T2b))))
             '(Eq.refl (decCert (encCert f m t A T1 T2 pad))))
       '(have q_c1 (Eq (Option Exp) (decE (encE t)) (Option.some Exp t)) (rtE t))
       '(have q_c2 (Eq (Option Exp) (decE (encE A)) (Option.some Exp A)) (rtE A))
       '(have q_c3 (Eq (Option DT) (decDT f (encDT T1)) (Option.some DT T1)) (rtDT T1 f h1))
       '(have q_c4 (Eq (Option DT) (decDT f (encDT T2)) (Option.some DT T2)) (rtDT T2 f h2))
       '(rw [q_c0]) '(try (rw [q_c1])) '(try (rw [q_c2])) '(try (rw [q_c3])) '(try (rw [q_c4]))]))

;; ---------------------------------------------------------------------------
;; Completeness.

;; Arithmetic for the padding: each of five summands is below the size of a
;; code holding all of them in its padding.
(doseq [i (range 5)]
  (a/prove-theorem (symbol (str "pad_le" i))
    (lv '[x0 :- Nat, x1 :- Nat, x2 :- Nat, x3 :- Nat, x4 :- Nat, X :- Nat, C :- Nat,
          hC :- (Eq Nat C (+ 1 (+ X (+ (+ x0 (+ x1 (+ x2 (+ x3 x4)))) 1))))])
    (lv (list 'LE.le (symbol (str "x" i)) 'C))
    '[(omega)]))
(thm lt_fuel_l [a :- Nat, b :- Nat] (Nat.lt a (+ (+ a b) 1)) (omega))
(thm lt_fuel_r [a :- Nat, b :- Nat] (Nat.lt b (+ (+ a b) 1)) (omega))
(thm band_tt [x :- Bool, y :- Bool, hx :- (Eq Bool x Bool.true), hy :- (Eq Bool y Bool.true)]
  (Eq Bool (Bool.and x y) Bool.true)
  (exact (Eq.trans (congrArg (fn [z :- Bool] (Bool.and z y)) hx) hy)))

;; If Check decCert accepts a typing tree and a formation tree as derivations
;; (and the type is closed), it accepts their certificate: fuel above both
;; trees' heights, padding above every size the checker tests.
(let [ff '(cntU (maskUF (thetaU m) (freshF t)))
      xs [(list '+ 'm 1) (list '+ (list '+ ff ff) 1) '(+ (cnodes (encE A)) 1) '(dtB T1) '(dtB T2)]
      N (list '+ (xs 0) (list '+ (xs 1) (list '+ (xs 2) (list '+ (xs 3) (xs 4)))))
      F '(+ (+ (htDT T1) (htDT T2)) 1)
      X (chain-enc 0 (list (list 'encN F) '(encN m) '(encE t) '(encE A) '(encDT T1) '(encDT T2)))
      cert (list 'encCert F 'm 't 'A 'T1 'T2 (list 'padC N))
      checks (walk/postwalk-replace {'r '(Check decCert) 'c cert 'd '(encE A)} @#'lcert.formal.check-spec/body-checks)
      le (fn [i] (list (symbol (str "pad_le" i)) (xs 0) (xs 1) (xs 2) (xs 3) (xs 4) (list 'cnodes X) (list 'cnodes cert) 'hC))
      tree-ok (fn [T J bound-i h]
                (list 'Eq.trans
                  (list 'dtCheck_agree (list 'restrC '(Check decCert) cert) '(Check decCert) T
                        (list 'restr_self_agree '(Check decCert) cert (list 'dtB T) (le bound-i)) J)
                  h))
      proofs [(tree-ok 'T1 '(DTJ.rt (thetaD m) (thetaU m) t A) 3 'h1)
              (tree-ok 'T2 '(DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit) 4 'h2)
              'hA
              '(codeEq_refl (encE A))
              (list 'Nat.ble_eq_true_of_le (le 0))
              (list 'Nat.ble_eq_true_of_le (le 1))
              (list 'Nat.ble_eq_true_of_le (le 2))
              (list 'Nat.ble_eq_true_of_le (le 3))
              (list 'Nat.ble_eq_true_of_le (le 4))]
      ;; the body's conjunction is true: band_tt t_i (rest_i) p_i (rest's proof),
      ;; built from the inside out
      conj-true (let [n (count checks)]
                  (reduce (fn [acc i] (list 'band_tt (nth checks i) (h/f7-and (drop (inc i) checks)) (nth proofs i) acc))
                          '(Eq.refl Bool.true) (range (dec n) -1 -1)))]
  (a/prove-theorem 'check_complete
    (lv '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
          h1 :- (Eq Bool (dtCheck (Check decCert) T1 (DTJ.rt (thetaD m) (thetaU m) t A)) Bool.true),
          h2 :- (Eq Bool (dtCheck (Check decCert) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit)) Bool.true),
          hA :- (Eq Bool (closedTy A) Bool.true)])
    (lv '(Exists (fn [c :- Code] (Eq Bool (Check decCert c (encE A)) Bool.true))))
    (lv [(list 'have 'hC (list 'Eq 'Nat (list 'cnodes cert) (list '+ 1 (list '+ (list 'cnodes X) (list '+ N 1))))
               (list 'congrArg (list 'fn '[p :- Nat] (list '+ 1 (list '+ (list 'cnodes X) 'p))) (list 'pad_nodes N)))
         (list 'have 'hdec (list 'Eq (list 'Option CD) (list 'decCert cert) (list 'Option.some CD (cd-tuple 'm 't 'A 'T1 'T2)))
               (list 'decCert_enc F 'm 't 'A 'T1 'T2 (list 'padC N) '(lt_fuel_l (htDT T1) (htDT T2)) '(lt_fuel_r (htDT T1) (htDT T2))))
         (list 'have 'hbody (list 'Eq 'Bool (h/f7-and checks) 'Bool.true) conj-true)
         (list 'have 'hopt (list 'Eq 'Bool (list 'bodyO '(Check decCert) cert '(encE A) (list 'Option.some CD (cd-tuple 'm 't 'A 'T1 'T2))) 'Bool.true)
               'hbody)
         (list 'have 'hchk (list 'Eq 'Bool (list 'Check 'decCert cert '(encE A)) 'Bool.true)
               (list 'Eq.trans (list 'check_fix 'decCert cert '(encE A))
                     (list 'Eq.trans (list 'congrArg (list 'fn ['o :- (list 'Option CD)] (list 'bodyO '(Check decCert) cert '(encE A) 'o)) 'hdec)
                           'hopt)))
         '(constructor) (list 'exact cert) '(exact hchk)])))
