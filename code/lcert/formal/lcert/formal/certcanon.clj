(ns lcert.formal.certcanon
  "F7 — the canonical certificate format: no padding, the size facts derived
  from the encoding, completeness, and the shape of every accepted code
  (ADR-0006; design note nachlass/docs-f7-enc46-design.md §3).

  F7's first format (certenc.clj's encCertPad) padded a certificate until it
  passed the checker's size tests, and its decoder read only part of the
  code.  So an accepted certificate could carry another accepted certificate
  for free (enc46f7.clj), which refutes Theorem 4.6's encoding fact Enc46.
  This format closes every free position:

    encCert m t A T₁ T₂ = sn 96 X (sl 0),
    X = ⌜(F, m, t, A, T₁, T₂)⌝,  F = htDT T₁ + htDT T₂ + 1 (the fuel),

  with X certenc.clj's constructor chain and raw codes written as literal
  terms (certenc.clj's encC).  decCert runs the padded decoder and accepts
  its result y only if the code is exactly encCertY y (guardC): an accepted
  code is the encoding of its own data (decCert_canon).

  - The certificate label 96 is an encoding label (below 97, so in L) that no
    component encoder puts on an internal node: nlb 96 holds of every
    encoder's output (nlbN … nlbDT, generated from certenc.clj's constructor
    tables; nlb_encE for expressions is encsize.clj's).  This is F7's form of
    E6: label 96 marks an internal node only at a certificate's root.
  - The size facts are consequences of the encoding (the paper's E2–E4),
    not of padding: cert_size0 … cert_size4 show that every canonical
    certificate passes each of Check's size tests (the budget is unary, the
    tokens are at most the budget and at most the term's nodes, the type's
    code is a subtree, and each δ-record holds its code as a larger literal
    term, dtB_le).  check_spec.clj's tests stay in the checker, generic over
    the decoder, but they are now redundant: budget_canon, toksize_canon and
    typesize_canon derive the three facts CheckSpec, TokSize and TypeSize
    read from canonicity alone.
  - check_complete, check_complete_lbl: completeness with no padding.
  - acc_shape, acc_encCert, lblOk_certType: what an accepted code looks
    like, for thm46f7.clj (no accepted code nests inside another, so Enc46
    holds there)."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            ;; codeEq, codeEq_refl, codeEq_sound
            [lcert.formal.check]
            ;; dtB, hdB, stepDTB
            [lcert.formal.check-agree]
            ;; Check, bodyO, check_fix, bodyO_true, decOf, tup_eq, restrC, restr_self_agree
            [lcert.formal.check-spec]
            ;; cnt_mask_le (E3: the tokens in use are at most the budget)
            [lcert.formal.prop434]
            ;; lblBelow, lblOk_lblBelow, band_tt2, blt_lit
            [lcert.formal.enclabels]
            ;; nlb, nlb-proof, nlb_encNat, nlb_encE, cnodes_encNat, tok_encE, code_enc_gt
            [lcert.formal.encsize]
            ;; the encoders, decCertPad, decCertPad_enc, lt_fuel_l/r, band_tt, the label lemmas
            [lcert.formal.certenc]
            ;; exT
            [lcert.formal.outer]))

;; ---------------------------------------------------------------------------
;; Generator inputs (surface syntax only; every result is kernel-checked).

;; certenc.clj's tables: encoders by type, constructor tables, the chain shape.
(def ^:private ty-info @@#'lcert.formal.certenc/ty-info)
(def ^:private fuel-info @@#'lcert.formal.certenc/fuel-info)
(def ^:private hd-ctors @#'lcert.formal.certenc/hd-ctors)
(def ^:private st-ctors @#'lcert.formal.certenc/st-ctors)
(def ^:private skdt-ctors @#'lcert.formal.certenc/skdt-ctors)
(def ^:private dt-ctors @#'lcert.formal.certenc/dt-ctors)
(def ^:private chain-enc @#'lcert.formal.certenc/chain-enc)
(def ^:private code-proof @#'lcert.formal.certenc/code-proof)
(def ^:private cd-tuple @#'lcert.formal.certenc/cd-tuple)
(def ^:private body-checks @#'lcert.formal.check-spec/body-checks)
(def ^:private nlb-proof lcert.formal.encsize/nlb-proof)

;; The decoded certificate (check_spec.clj's CD), expanded in the forms below.
(def ^:private CD '(Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))))
(defn- x [form] (lv (walk/postwalk-replace {'CD CD} form)))
(defmacro ^:private kdef* [nm ty body] `(b/kdef! '~nm (x '~ty) (x '~body)))
(defmacro ^:private thm* [nm params goal & tactics]
  `(a/prove-theorem '~nm (x '~params) (x '~goal) (x '~(vec tactics))))

(defn- enc-of [T] (first (or (ty-info T) (fuel-info T))))
(defn- sum-terms [ts] (reduce (fn [acc t] (list '+ t acc)) 0 (reverse ts)))

(defn- nodes-of
  "cnodes of a code expression, as arithmetic over its pieces' cnodes: a leaf
  has none, a node one more than its children; any other piece p is (cnodes p)."
  [c]
  (cond
    (and (seq? c) (= 'Code.sl (first c))) 0
    (and (seq? c) (= 'Code.sn (first c))) (list '+ 1 (list '+ (nodes-of (nth c 2)) (nodes-of (nth c 3))))
    :else (list 'cnodes c)))

;; ---------------------------------------------------------------------------
;; F7's E6: no encoder puts label 96 on an internal node.
;;
;; nlbX v : nlb 96 (encX v) = true, for every type a certificate holds.  The
;; encoders' own internal labels are constructor positions (below 70), the
;; chain and list labels 0 and 1, Sk's 7 and 8, the unary number's 3, and
;; the expression encoding's (below 58, nlb_encE); raw codes are literal
;; terms, so their labels are leaves.

(def ^:private nlb-info (atom '{Exp nlb_encE}))

(thm nlbN [n :- Nat] (Eq Bool (nlb 96 (encN n)) Bool.true) (exact (nlb_encNat n)))
(thm nlbC [c :- Code] (Eq Bool (nlb 96 (encC c)) Bool.true) (exact (nlb_encE (codeTerm c))))
(swap! nlb-info assoc 'Nat 'nlbN 'Code 'nlbC)

;; Enumerations: numbered leaves.
(doseq [[T lem n] '[[Bool nlbB 2] [U nlbU 3]]]
  (a/prove-theorem lem (lv ['v :- T]) (lv (list 'Eq 'Bool (list 'nlb 96 (list (enc-of T) 'v)) 'Bool.true))
    (lv (into ['(cases v)] (repeat n '(rfl)))))
  (swap! nlb-info assoc T lem))

;; Skeletons: leaves, and sn 7 / sn 8 of the children.
(doseq [[nm ctor L] [['nlbS_arr 'Sk.arr 7] ['nlbS_prod 'Sk.prod 8]]]
  (a/prove-theorem nm
    (lv '[s :- Sk, t :- Sk, is :- (Eq Bool (nlb 96 (encS s)) Bool.true), it :- (Eq Bool (nlb 96 (encS t)) Bool.true)])
    (lv (list 'Eq 'Bool (list 'nlb 96 (list 'encS (list ctor 's 't))) 'Bool.true))
    (lv [(list 'have 'q_nlb (list 'Eq 'Bool (list 'nlb 96 (list 'Code.sn L '(encS s) '(encS t))) 'Bool.true)
               (nlb-proof (list 'Code.sn L '(encS s) '(encS t)) {'(encS s) 'is '(encS t) 'it}))
         '(exact q_nlb)])))
(thm nlbS [sk :- Sk] (Eq Bool (nlb 96 (encS sk)) Bool.true)
  (induction sk) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl)
  (exact (nlbS_arr s t ih_s ih_t)) (exact (nlbS_prod s t ih_s ih_t)))
(swap! nlb-info assoc 'Sk 'nlbS)

;; Lists: nil = sl 0, cons = sn 1.
(doseq [[suffix T] '[[N Nat] [E Exp] [U U] [S Sk]]]
  (let [LT (list 'List T)
        enc (enc-of LT) encT (enc-of T) elem (@nlb-info T)
        lem (symbol (str "nlbL" suffix)) cons-lem (symbol (str "nlbL" suffix "_cons"))
        code (list 'Code.sn 1 (list encT 'x) (list enc 'xs))]
    (a/prove-theorem cons-lem
      (lv ['x :- T 'xs :- LT 'ih :- (list 'Eq 'Bool (list 'nlb 96 (list enc 'xs)) 'Bool.true)])
      (lv (list 'Eq 'Bool (list 'nlb 96 (list enc (list 'List.cons T 'x 'xs))) 'Bool.true))
      (lv [(list 'have 'q_nlb (list 'Eq 'Bool (list 'nlb 96 code) 'Bool.true)
                 (nlb-proof code {(list encT 'x) (list elem 'x) (list enc 'xs) 'ih}))
           '(exact q_nlb)]))
    (a/prove-theorem lem (lv ['l :- LT]) (lv (list 'Eq 'Bool (list 'nlb 96 (list enc 'l)) 'Bool.true))
      (lv ['(induction l) '(rfl) (list 'exact (list cons-lem 'head 'tail 'ih_tail))]))
    (swap! nlb-info assoc LT lem)))

;; Head steps, steps, skeleton trees, derivation trees: a constructor's chain
;; sn i ⌜f₁⌝ (sn 0 ⌜f₂⌝ …), one lemma per constructor, then cases or induction.
(defn- define-nlb!
  [T lem ctors]
  (let [indexed (map-indexed vector ctors)
        enc (enc-of T)
        rec? (fn [F] (= F T))
        ih (fn [x] (symbol (str "ih_" x)))
        recursive (some (fn [[_ fs]] (some (fn [[_ F]] (rec? F)) fs)) ctors)]
    (doseq [[i [nm fields]] indexed]
      (let [v (h/f7-app (symbol (str T "." nm)) (map first fields))
            code (chain-enc i (for [[x F] fields] (list (enc-of F) x)))
            child (into {} (for [[x F] fields]
                             [(list (enc-of F) x) (if (rec? F) (ih x) (list (@nlb-info F) x))]))]
        (a/prove-theorem (symbol (str lem "_" nm))
          (lv (h/f7-params (concat fields (for [[x F] fields :when (rec? F)]
                                            [(ih x) (list 'Eq 'Bool (list 'nlb 96 (list enc x)) 'Bool.true)]))))
          (lv (list 'Eq 'Bool (list 'nlb 96 (list enc v)) 'Bool.true))
          (lv [(list 'have 'q_nlb (list 'Eq 'Bool (list 'nlb 96 code) 'Bool.true) (nlb-proof code child))
               '(exact q_nlb)]))))
    (a/prove-theorem lem (lv ['v :- T]) (lv (list 'Eq 'Bool (list 'nlb 96 (list enc 'v)) 'Bool.true))
      (lv (into [(if recursive '(induction v) '(cases v))]
                (for [[_ [nm fields]] indexed]
                  (list 'exact (apply list (symbol (str lem "_" nm))
                                      (concat (map first fields) (for [[x F] fields :when (rec? F)] (ih x)))))))))
    (swap! nlb-info assoc T lem)))

(define-nlb! 'HdDT 'nlbHd hd-ctors)
(define-nlb! 'StepDT 'nlbSt st-ctors)
(define-nlb! 'SkDT 'nlbSkDT skdt-ctors)
(define-nlb! 'DT 'nlbDT dt-ctors)

;; ---------------------------------------------------------------------------
;; Counting nodes without arithmetic by computation.
;;
;; cnodes (encDT v) = 1 + (‖f₁‖ + (1 + …)) holds by rfl, but the kernel's Nat
;; reduction re-reduces the nested sums at every level, so checking it costs
;; time exponential in the chain's length (a ten-field constructor did not
;; finish).  So the node count of a code expression is built one node at a
;; time (cn_sl, cn_sn_eq: cnodes-eq), and a bound below a chain is built
;; field by field (le_cn_b, le_cn_u: chain-bound); only the unfolding of an
;; encoder to its code (no arithmetic) is left to rfl.

(thm cn_sl [l :- Nat] (Eq Nat (cnodes (Code.sl l)) 0) (rfl))
(thm cn_sn_eq [l :- Nat, a :- Code, b :- Code, xa :- Nat, xb :- Nat, ha :- (Eq Nat (cnodes a) xa), hb :- (Eq Nat (cnodes b) xb)]
  (Eq Nat (cnodes (Code.sn l a b)) (+ 1 (+ xa xb)))
  (have q (Eq Nat (cnodes (Code.sn l a b)) (+ 1 (+ (cnodes a) (cnodes b)))) (rfl))
  (omega))
;; A bound b ≤ ‖e‖ on a field, and B ≤ ‖R‖ on the rest of the chain, give
;; b + B ≤ ‖sn l e R‖; an unbounded field passes B on.
(thm le_cn_b [l :- Nat, e :- Code, R :- Code, b :- Nat, B :- Nat, hb :- (LE.le b (cnodes e)), hB :- (LE.le B (cnodes R))]
  (LE.le (+ b B) (cnodes (Code.sn l e R)))
  (have q (Eq Nat (cnodes (Code.sn l e R)) (+ 1 (+ (cnodes e) (cnodes R)))) (rfl))
  (omega))
(thm le_cn_u [l :- Nat, e :- Code, R :- Code, B :- Nat, hB :- (LE.le B (cnodes R))]
  (LE.le B (cnodes (Code.sn l e R)))
  (have q (Eq Nat (cnodes (Code.sn l e R)) (+ 1 (+ (cnodes e) (cnodes R)))) (rfl))
  (omega))

(defn- cnodes-eq
  "A proof of cnodes c = (nodes-of c), node by node."
  [c]
  (cond
    (and (seq? c) (= 'Code.sl (first c))) (list 'cn_sl (second c))
    (and (seq? c) (= 'Code.sn (first c)))
    (let [[_ L a b] c] (list 'cn_sn_eq L a b (nodes-of a) (nodes-of b) (cnodes-eq a) (cnodes-eq b)))
    :else (list 'Eq.refl (list 'cnodes c))))

(defn- chain-bound
  "For a chain code sn L₀ e₀ (sn 0 e₁ (… (sl l))) and, per position, nil or
  [bound proof] (proof : bound ≤ cnodes eⱼ): [B, a proof of B ≤ cnodes code],
  B the sum of the bounds in order, ending in 0 (as sum-terms writes it)."
  [code per-field]
  (if (and (seq? code) (= 'Code.sn (first code)))
    (let [[_ L e R] code
          [B-rest p-rest] (chain-bound R (rest per-field))
          f (first per-field)]
      (if f
        (let [[b hb] f] [(list '+ b B-rest) (list 'le_cn_b L e R b B-rest hb p-rest)])
        [B-rest (list 'le_cn_u L e R B-rest p-rest)]))
    [0 (list 'Nat.zero_le (list 'cnodes code))]))

;; ---------------------------------------------------------------------------
;; The δ-bound is paid for in the tree's own nodes: dtB T ≤ ‖⌜T⌝‖.
;;
;; dtB T (check_agree.clj) sums ‖cc‖ + 1 over T's δ-records; each record
;; holds cc as its literal term encC cc, which has more nodes than cc
;; (code_enc_gt), inside a chain node of its own.

(doseq [[i [nm fields]] (map-indexed vector hd-ctors)]
  (let [v (h/f7-app (symbol (str "HdDT." nm)) (map first fields))
        code (chain-enc i (for [[x F] fields] (list (enc-of F) x)))
        goal (list 'LE.le (list 'hdB v) (list 'cnodes (list 'encHd v)))]
    (a/prove-theorem (symbol (str "hdB_le_" nm)) (lv (h/f7-params fields)) (lv goal)
      (lv (if (= nm 'delta)
            [(list 'have 'q_b (list 'Eq 'Nat (list 'hdB v) '(+ (cnodes cc) 1)) '(rfl))
             (list 'have 'q_n (list 'Eq 'Nat (list 'cnodes (list 'encHd v)) (nodes-of code)) '(rfl))
             '(have q_g (LT.lt (cnodes cc) (cnodes (encC cc))) (code_enc_gt cc))
             '(omega)]
            [(list 'have 'q_b (list 'Eq 'Nat (list 'hdB v) 0) '(rfl))
             '(omega)])))))
(a/prove-theorem 'hdB_le '[hdv :- HdDT] '(LE.le (hdB hdv) (cnodes (encHd hdv)))
  (lv (into ['(cases hdv)]
            (for [[nm fields] hd-ctors]
              (list 'exact (apply list (symbol (str "hdB_le_" nm)) (map first fields)))))))

(thm stepB_le_at [p :- (List Nat), head :- HdDT, e :- Exp, e2 :- Exp]
  (LE.le (stepDTB (StepDT.at p head e e2)) (cnodes (encSt (StepDT.at p head e e2))))
  (have q_b (Eq Nat (stepDTB (StepDT.at p head e e2)) (hdB head)) (rfl))
  (have q_n (Eq Nat (cnodes (encSt (StepDT.at p head e e2)))
                (+ 1 (+ (cnodes (encLN p)) (+ 1 (+ (cnodes (encHd head)) (+ 1 (+ (cnodes (encE e)) (+ 1 (+ (cnodes (encE e2)) 0))))))))) (rfl))
  (have q_h (LE.le (hdB head) (cnodes (encHd head))) (hdB_le head))
  (omega))
(thm stepB_le [s :- StepDT] (LE.le (stepDTB s) (cnodes (encSt s)))
  (cases s) (exact (stepB_le_at p head e e2)))

(doseq [[i [nm fields]] (map-indexed vector dt-ctors)]
  (let [v (h/f7-app (symbol (str "DT." nm)) (map first fields))
        ih (fn [x] (symbol (str "ih_" x)))
        code (chain-enc i (for [[x F] fields] (list (enc-of F) x)))
        ;; per field: its bound and the bound's proof, for the step and premise fields
        per-field (for [[x F] fields]
                    (case F
                      StepDT [(list 'stepDTB x) (list 'stepB_le x)]
                      DT [(list 'dtB x) (ih x)]
                      nil))
        [B pf] (chain-bound code per-field)]
    (a/prove-theorem (symbol (str "dtB_le_" nm))
      (lv (h/f7-params (concat fields (for [[x F] fields :when (= F 'DT)]
                                        [(ih x) (list 'LE.le (list 'dtB x) (list 'cnodes (list 'encDT x)))]))))
      (lv (list 'LE.le (list 'dtB v) (list 'cnodes (list 'encDT v))))
      (lv [(list 'have 'q_e (list 'Eq 'Code (list 'encDT v) code) '(rfl))
           (list 'have 'q_b (list 'Eq 'Nat (list 'dtB v) B) '(rfl))
           (list 'have 'q_le (list 'LE.le B (list 'cnodes code)) pf)
           ;; (the binders are q_z: a field may be called z, as recN's is)
           (list 'exact (list 'Eq.mpr (list 'congrArg (list 'fn '[q_z :- Nat] (list 'LE.le 'q_z (list 'cnodes (list 'encDT v)))) 'q_b)
                              (list 'Eq.mpr (list 'congrArg (list 'fn '[q_z :- Code] (list 'LE.le B '(cnodes q_z))) 'q_e) 'q_le)))]))))
(a/prove-theorem 'dtB_le '[trv :- DT] '(LE.le (dtB trv) (cnodes (encDT trv)))
  (lv (into ['(induction trv)]
            (for [[nm fields] dt-ctors]
              (list 'exact (apply list (symbol (str "dtB_le_" nm))
                                  (concat (map first fields) (for [[x F] fields :when (= F 'DT)] (symbol (str "ih_" x))))))))))

;; ---------------------------------------------------------------------------
;; The certificate.

(def ^:private cparams '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT])
;; The fuel: one more than the larger tree's height (their sum, here).
(def ^:private F '(+ (+ (htDT T1) (htDT T2)) 1))
;; X = ⌜(F, m, t, A, T₁, T₂)⌝, the padded format's left child.
(def ^:private X-expr (chain-enc 0 [(list 'encN F) '(encN m) '(encE t) '(encE A) '(encDT T1) '(encDT T2)]))
(def ^:private cert-expr (list 'Code.sn 96 X-expr '(Code.sl 0)))

(b/kdef! 'certX '(=> Nat Exp Exp DT DT Code) (list 'fn cparams X-expr))
;; The certificate of Θₘ ⊢ t :¹ A with typing tree T₁ and formation tree T₂.
(kdef encCert (=> Nat Exp Exp DT DT Code)
  (fn [m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT] (Code.sn 96 (certX m t A T1 T2) (Code.sl 0))))
;; The certificate of a decoded tuple.
(kdef* encCertY (=> CD Code)
  (fn [y :- CD] (encCert (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                         (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y)))))))

;; Canonicity: a decoding y of c stands only if c is exactly encCertY y.
(kdef* guardC (=> Code (Option CD) (Option CD))
  (fn [c :- Code, o :- (Option CD)]
    (Option.rec$1$0 CD (fn [_ :- (Option CD)] (Option CD)) (Option.none CD)
      (fn [y :- CD] (Bool.rec$1 (fn [_ :- Bool] (Option CD)) (Option.none CD) (Option.some CD y) (codeEq c (encCertY y))))
      o)))
;; The decoder Check reads: the padded format's (which reads the root's left
;; child), guarded.
(kdef* decCert (=> Code (Option CD)) (fn [c :- Code] (guardC c (decCertPad c))))

;; The certificate decodes to its data.  The padded decoder reads X, which is
;; the padded certificate's left child too (decCertPad_enc, with fuel F); the
;; guard then compares the certificate with itself.
(let [Y (cd-tuple 'm 't 'A 'T1 'T2)
      E '(encCert m t A T1 T2)
      brec (fn [b] (x (list 'Bool.rec$1 '(fn [_ :- Bool] (Option CD)) '(Option.none CD) (list 'Option.some 'CD Y) b)))]
  (a/prove-theorem 'decCert_enc (lv cparams)
    (x (list 'Eq '(Option CD) (list 'decCert E) (list 'Option.some 'CD Y)))
    (x [(list 'have 'e0 (list 'Eq '(Option CD) (list 'decCertPad E) (list 'Option.some 'CD Y))
              (list 'decCertPad_enc F 'm 't 'A 'T1 'T2 '(Code.sl 0) '(lt_fuel_l (htDT T1) (htDT T2)) '(lt_fuel_r (htDT T1) (htDT T2))))
        (list 'have 'e1 (list 'Eq 'Bool (list 'codeEq E (list 'encCertY Y)) 'Bool.true) (list 'codeEq_refl E))
        (list 'have 'e2 (list 'Eq '(Option CD) (list 'decCert E) (list 'guardC E (list 'Option.some 'CD Y)))
              (list 'congrArg (list 'guardC E) 'e0))
        (list 'have 'e3 (list 'Eq '(Option CD) (list 'guardC E (list 'Option.some 'CD Y)) (brec (list 'codeEq E (list 'encCertY Y))))
              '(rfl))
        (list 'have 'e4 (list 'Eq '(Option CD) (brec (list 'codeEq E (list 'encCertY Y))) (list 'Option.some 'CD Y))
              (list 'congrArg (list 'fn '[bb :- Bool] (brec 'bb)) 'e1))
        '(exact (Eq.trans e2 (Eq.trans e3 e4)))])))

;; An accepted decoding is the certificate's own data.
(thm* none_ne_some_cd [y :- CD, h :- (Eq (Option CD) (Option.none CD) (Option.some CD y))] False
  ;; (cases h can close such a goal with a term the kernel rejects: project to Bool)
  (exact (Bool.noConfusion (congrArg (fn [o :- (Option CD)] (Option.rec$1$0 CD (fn [_ :- (Option CD)] Bool) Bool.false (fn [z :- CD] Bool.true) o)) h))))
(thm* guard_bool [c :- Code, y0 :- CD, y :- CD]
  (forall [bb Bool] (=> (Eq Bool (codeEq c (encCertY y0)) bb)
                        (Eq (Option CD) (Bool.rec$1 (fn [_ :- Bool] (Option CD)) (Option.none CD) (Option.some CD y0) bb) (Option.some CD y))
                        (Eq Code c (encCertY y))))
  (intro bb) (cases bb)
  (intro hb hs) (exact (False.elim (none_ne_some_cd y hs)))
  (intro hb hs)
  (have hc (Eq Code c (encCertY y0)) (codeEq_sound c (encCertY y0) hb))
  (have hy (Eq CD y0 y) (Option.some.inj hs))
  (exact (Eq.trans hc (congrArg encCertY hy))))
(thm* guard_canon [c :- Code, y :- CD, o :- (Option CD)]
  (=> (Eq (Option CD) (guardC c o) (Option.some CD y)) (Eq Code c (encCertY y)))
  (cases o)
  (intro hs) (exact (False.elim (none_ne_some_cd y hs)))
  (intro hs) (exact (guard_bool c val y (codeEq c (encCertY val)) (Eq.refl$1 (codeEq c (encCertY val))) hs)))
;; Canonicity: decCert c = some y only for c = encCertY y.
(thm* decCert_canon [c :- Code, y :- CD, h :- (Eq (Option CD) (decCert c) (Option.some CD y))] (Eq Code c (encCertY y))
  (exact (guard_canon c y (decCertPad c) h)))

;; ---------------------------------------------------------------------------
;; The size facts follow from the encoding (E2–E4).

;; ‖encCert m t A T₁ T₂‖, as a sum over its pieces.
(a/prove-theorem 'cert_nodes (lv cparams) (lv (list 'Eq 'Nat '(cnodes (encCert m t A T1 T2)) (nodes-of cert-expr)))
  (lv [(list 'have 'q_e (list 'Eq 'Code '(encCert m t A T1 T2) cert-expr) '(rfl))
       (list 'exact (list 'Eq.trans '(congrArg cnodes q_e) (cnodes-eq cert-expr)))]))

(def ^:private ff '(cntU (maskUF (thetaU m) (freshF t))))
(let [hC (list 'have 'q_c (list 'Eq 'Nat '(cnodes (encCert m t A T1 T2)) (nodes-of cert-expr)) '(cert_nodes m t A T1 T2))
      em '(have q_m (Eq Nat (cnodes (encN m)) m) (cnodes_encNat m))
      size (fn [i bound tactics]
             (a/prove-theorem (symbol (str "cert_size" i)) (lv cparams) (lv (list 'LE.le bound '(cnodes (encCert m t A T1 T2))))
               (lv (concat [hC] tactics ['(omega)]))))]
  ;; m < ‖c‖: the budget is unary (E3).
  (size 0 '(+ m 1) [em])
  ;; 2f < ‖c‖: the tokens free in t are at most m (E3) and at most ‖⌜t⌝‖ (E4),
  ;; and ⌜m⌝ and ⌜t⌝ are disjoint subtrees (E2).
  (size 1 (list '+ (list '+ ff ff) 1)
        [em (list 'have 'q_t1 (list 'LE.le ff '(cnodes (encE t))) '(tok_encE t m))
            (list 'have 'q_t2 (list 'LE.le ff 'm) '(cnt_mask_le m (freshF t)))])
  ;; ‖⌜A⌝‖ < ‖c‖: ⌜A⌝ is a proper subtree.
  (size 2 '(+ (cnodes (encE A)) 1) [])
  ;; dtB Tᵢ ≤ ‖c‖: each δ-record pays for its code (dtB_le).
  (size 3 '(dtB T1) ['(have q_d (LE.le (dtB T1) (cnodes (encDT T1))) (dtB_le T1))])
  (size 4 '(dtB T2) ['(have q_d (LE.le (dtB T2) (cnodes (encDT T2))) (dtB_le T2))]))

;; The three size facts CheckSpec, TokSize and TypeSize read, from
;; canonicity alone (no size test): an accepted decoding (m, t, A) of c has
;; m < ‖c‖, 2f < ‖c‖ and ‖⌜A⌝‖ < ‖c‖.
(defn- y-args [] '[(Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                   (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))])
(doseq [[nm i fact] [['budget_canon 0 '(LT.lt (Prod.fst y) (cnodes c))]
                     ['toksize_canon 1 '(LT.lt (+ (cntU (maskUF (thetaU (Prod.fst y)) (freshF (Prod.fst (Prod.snd y)))))
                                                  (cntU (maskUF (thetaU (Prod.fst y)) (freshF (Prod.fst (Prod.snd y))))))
                                               (cnodes c))]
                     ['typesize_canon 2 '(LT.lt (cnodes (encE (Prod.fst (Prod.snd (Prod.snd y))))) (cnodes c))]]]
  (let [[m t A] (y-args)
        bound (case i
                0 (list '+ m 1)
                1 (let [f (list 'cntU (list 'maskUF (list 'thetaU m) (list 'freshF t)))] (list '+ (list '+ f f) 1))
                2 (list '+ (list 'cnodes (list 'encE A)) 1))]
    (a/prove-theorem nm (x '[c :- Code, y :- CD, h :- (Eq (Option CD) (decCert c) (Option.some CD y))]) (x fact)
      (x [(list 'have 'q_s (list 'LE.le bound '(cnodes (encCertY y))) (apply list (symbol (str "cert_size" i)) (y-args)))
          '(have q_e (Eq Nat (cnodes c) (cnodes (encCertY y))) (congrArg cnodes (decCert_canon c y h)))
          '(omega)]))))

;; A certificate's labels: the root's 96 and the chain's, the unary numbers',
;; and its data's (the term's and type's label constants, the trees' data).
(a/prove-theorem 'lblBelow_encCert
  (lv (into ['nb :- 'Nat 'hk :- '(LE.le 97 nb)]
            (into cparams '[ht :- (Eq Bool (lblsE nb t) Bool.true), hA :- (Eq Bool (lblsE nb A) Bool.true),
                            h1 :- (Eq Bool (dataOkDT nb T1) Bool.true), h2 :- (Eq Bool (dataOkDT nb T2) Bool.true)])))
  '(Eq Bool (lblBelow nb (encCert m t A T1 T2)) Bool.true)
  (lv [(list 'have 'q_l (list 'Eq 'Bool (list 'lblBelow 'nb cert-expr) 'Bool.true)
             (code-proof cert-expr {(list 'encN F) (list 'lblN 'nb 'hk F '(Eq.refl Bool.true)) '(encN m) '(lblN nb hk m (Eq.refl Bool.true))
                                    '(encE t) '(lblBelow_encE nb hk t ht) '(encE A) '(lblBelow_encE nb hk A hA)
                                    '(encDT T1) '(lblDT nb hk T1 h1) '(encDT T2) '(lblDT nb hk T2 h2)}))
       '(exact q_l)]))

;; ---------------------------------------------------------------------------
;; Completeness, with no padding.

;; If Check decCert accepts a typing tree and a formation tree as derivations
;; (and the type is closed), it accepts their certificate: it decodes to them
;; (decCert_enc), and passes every size test (cert_size0 … cert_size4).
(let [cert '(encCert m t A T1 T2)
      checks (walk/postwalk-replace {'r '(Check decCert) 'c cert 'd '(encE A)} body-checks)
      le (fn [i] (list (symbol (str "cert_size" i)) 'm 't 'A 'T1 'T2))
      tree-ok (fn [T J bound-i hyp]
                (list 'Eq.trans
                  (list 'dtCheck_agree (list 'restrC '(Check decCert) cert) '(Check decCert) T
                        (list 'restr_self_agree '(Check decCert) cert (list 'dtB T) (le bound-i)) J)
                  hyp))
      proofs [(tree-ok 'T1 '(DTJ.rt (thetaD m) (thetaU m) t A) 3 'h1)
              (tree-ok 'T2 '(DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit) 4 'h2)
              'hA
              '(codeEq_refl (encE A))
              (list 'Nat.ble_eq_true_of_le (le 0))
              (list 'Nat.ble_eq_true_of_le (le 1))
              (list 'Nat.ble_eq_true_of_le (le 2))
              (list 'Nat.ble_eq_true_of_le (le 3))
              (list 'Nat.ble_eq_true_of_le (le 4))]
      cc-params '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
                  h1 :- (Eq Bool (dtCheck (Check decCert) T1 (DTJ.rt (thetaD m) (thetaU m) t A)) Bool.true),
                  h2 :- (Eq Bool (dtCheck (Check decCert) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit)) Bool.true),
                  hA :- (Eq Bool (closedTy A) Bool.true)]
      Y (cd-tuple 'm 't 'A 'T1 'T2)
      conj-true (let [n (count checks)]
                  (reduce (fn [acc i] (list 'band_tt (nth checks i) (h/f7-and (drop (inc i) checks)) (nth proofs i) acc))
                          '(Eq.refl Bool.true) (range (dec n) -1 -1)))]
  (a/prove-theorem 'check_cert_ok
    (lv cc-params)
    (lv (list 'Eq 'Bool (list 'Check 'decCert cert '(encE A)) 'Bool.true))
    (x [(list 'have 'hdec (list 'Eq '(Option CD) (list 'decCert cert) (list 'Option.some 'CD Y)) '(decCert_enc m t A T1 T2))
        (list 'have 'hbody (list 'Eq 'Bool (h/f7-and checks) 'Bool.true) conj-true)
        (list 'have 'hopt (list 'Eq 'Bool (list 'bodyO '(Check decCert) cert '(encE A) (list 'Option.some 'CD Y)) 'Bool.true) 'hbody)
        (list 'have 'hchk (list 'Eq 'Bool (list 'Check 'decCert cert '(encE A)) 'Bool.true)
              (list 'Eq.trans (list 'check_fix 'decCert cert '(encE A))
                    (list 'Eq.trans (list 'congrArg (list 'fn '[o :- (Option CD)] (list 'bodyO '(Check decCert) cert '(encE A) 'o)) 'hdec)
                          'hopt)))
        '(exact hchk)]))
  (a/prove-theorem 'check_complete (lv cc-params)
    (lv '(Exists (fn [c :- Code] (Eq Bool (Check decCert c (encE A)) Bool.true))))
    (lv ['(constructor) (list 'exact cert) '(exact (check_cert_ok m t A T1 T2 h1 h2 hA))]))
  ;; The same certificate has every label below NL = 100 (lblOk), so a program
  ;; can hold it as an R value — given that the trees' and the type's data
  ;; (label constants, raw codes) are below 100, as typing already asks.
  (a/prove-theorem 'check_complete_lbl
    (lv (into cc-params '[lt :- (Eq Bool (lblsE 100 t) Bool.true), lA :- (Eq Bool (lblsE 100 A) Bool.true),
                          d1 :- (Eq Bool (dataOkDT 100 T1) Bool.true), d2 :- (Eq Bool (dataOkDT 100 T2) Bool.true)]))
    (lv '(Exists (fn [c :- Code] (And (Eq Bool (lblOk c) Bool.true) (Eq Bool (Check decCert c (encE A)) Bool.true)))))
    (lv ['(constructor) (list 'exact cert)
         '(exact (And.intro (Eq.trans (lblOk_lblBelow (encCert m t A T1 T2))
                                      (lblBelow_encCert 100 (Nat.le_of_ble_eq_true (Eq.refl Bool.true)) m t A T1 T2 lt lA d1 d2))
                            (check_cert_ok m t A T1 T2 h1 h2 hA)))])))

;; ---------------------------------------------------------------------------
;; Labels.

;; Subtrees of a code whose labels are below nb have labels below nb.
(thm lbl_sub_l [nb :- Nat, l :- Nat, a :- Code, b :- Code, h :- (Eq Bool (lblBelow nb (Code.sn l a b)) Bool.true)]
  (Eq Bool (lblBelow nb a) Bool.true)
  (exact (band_left (lblBelow nb a) (lblBelow nb b) (band_right (Nat.blt l nb) (Bool.and (lblBelow nb a) (lblBelow nb b)) h))))
(thm lbl_sub_r [nb :- Nat, l :- Nat, a :- Code, b :- Code, h :- (Eq Bool (lblBelow nb (Code.sn l a b)) Bool.true)]
  (Eq Bool (lblBelow nb b) Bool.true)
  (exact (band_right (lblBelow nb a) (lblBelow nb b) (band_right (Nat.blt l nb) (Bool.and (lblBelow nb a) (lblBelow nb b)) h))))

;; A certificate with labels in L has a type code with labels in L: ⌜A⌝ is a
;; subtree (the fourth field of X).
(let [[_ _ eF X1] X-expr
      [_ _ em X2] X1
      [_ _ et X3] X2
      [_ _ eA X4] X3]
  (a/prove-theorem 'lblOk_certType
    (lv (conj cparams 'h :- '(Eq Bool (lblOk (encCert m t A T1 T2)) Bool.true)))
    '(Eq Bool (lblOk (encE A)) Bool.true)
    (lv [(list 'have 'q_0 (list 'Eq 'Bool (list 'lblBelow 100 cert-expr) 'Bool.true)
               '(Eq.trans (Eq.symm (lblOk_lblBelow (encCert m t A T1 T2))) h))
         (list 'have 'q_x (list 'Eq 'Bool (list 'lblBelow 100 X-expr) 'Bool.true) (list 'lbl_sub_l 100 96 X-expr '(Code.sl 0) 'q_0))
         (list 'have 'q_1 (list 'Eq 'Bool (list 'lblBelow 100 X1) 'Bool.true) (list 'lbl_sub_r 100 0 eF X1 'q_x))
         (list 'have 'q_2 (list 'Eq 'Bool (list 'lblBelow 100 X2) 'Bool.true) (list 'lbl_sub_r 100 0 em X2 'q_1))
         (list 'have 'q_3 (list 'Eq 'Bool (list 'lblBelow 100 X3) 'Bool.true) (list 'lbl_sub_r 100 0 et X3 'q_2))
         (list 'have 'q_A (list 'Eq 'Bool (list 'lblBelow 100 eA) 'Bool.true) (list 'lbl_sub_l 100 0 eA X4 'q_3))
         '(exact (Eq.trans (lblOk_lblBelow (encE A)) q_A))])))

;; ---------------------------------------------------------------------------
;; The shape of an accepted code.

;; X has every internal label below 96.
(a/prove-theorem 'nlb_certX (lv cparams) '(Eq Bool (nlb 96 (certX m t A T1 T2)) Bool.true)
  (lv [(list 'have 'q_nlb (list 'Eq 'Bool (list 'nlb 96 X-expr) 'Bool.true)
             (nlb-proof X-expr {(list 'encN F) (list 'nlbN F) '(encN m) '(nlbN m) '(encE t) '(nlb_encE t) '(encE A) '(nlb_encE A)
                                '(encDT T1) '(nlbDT T1) '(encDT T2) '(nlbDT T2)}))
       '(exact q_nlb)]))

;; An accepted code is sn 96 X (sl 0) with X's internal labels below 96: it
;; decodes (check_fix, bodyO_true) and is canonical (decCert_canon).
(thm* acc_shape [c :- Code, d :- Code, h :- (Eq Bool (Check decCert c d) Bool.true)]
  (Exists (fn [X :- Code] (And (Eq Code c (Code.sn 96 X (Code.sl 0))) (Eq Bool (nlb 96 X) Bool.true))))
  (have hb (Eq Bool (bodyO (Check decCert) c d (decCert c)) Bool.true) (Eq.trans (Eq.symm (check_fix decCert c d)) h))
  (refine' (exT _ _ _ (bodyO_true (Check decCert) c d (decCert c) hb) _))
  (intro y hy)
  (constructor)
  (exact (certX (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))))
  (exact (And.intro (decCert_canon c y (And.left hy))
                    (nlb_certX (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))
                               (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))))))

(thm encCert_congr [m :- Nat, m2 :- Nat, t :- Exp, t2 :- Exp, A :- Exp, A2 :- Exp, T1 :- DT, T2 :- DT,
                    hm :- (Eq Nat m m2), ht :- (Eq Exp t t2), hA :- (Eq Exp A A2)]
  (Eq Code (encCert m t A T1 T2) (encCert m2 t2 A2 T1 T2))
  (rw [hm]) (rw [ht]) (rw [hA]))

;; An accepted code with decoding (m, t, A) is the certificate encCert m t A T₁ T₂
;; of some trees.
(thm* acc_encCert [v :- Code, d :- Code, m :- Nat, t :- Exp, A :- Exp, h :- (Eq Bool (Check decCert v d) Bool.true),
                   hdec :- (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCert v)
                               (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t A))))]
  (Exists (fn [T1 :- DT] (Exists (fn [T2 :- DT] (Eq Code v (encCert m t A T1 T2))))))
  (have hb (Eq Bool (bodyO (Check decCert) v d (decCert v)) Bool.true) (Eq.trans (Eq.symm (check_fix decCert v d)) h))
  (refine' (exT _ _ _ (bodyO_true (Check decCert) v d (decCert v) hb) _))
  (intro y hy)
  (have hdo (Eq (Option (Prod Nat (Prod Exp Exp))) (decOf decCert v)
                (Option.some (Prod Nat (Prod Exp Exp))
                  (Prod.mk Nat (Prod Exp Exp) (Prod.fst y) (Prod.mk Exp Exp (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y)))))))
    (congrArg (fn [o :- (Option CD)]
                (Option.rec$1$0 CD (fn [_ :- (Option CD)] (Option (Prod Nat (Prod Exp Exp))))
                  (Option.none (Prod Nat (Prod Exp Exp)))
                  (fn [z :- CD] (Option.some (Prod Nat (Prod Exp Exp))
                                  (Prod.mk Nat (Prod Exp Exp) (Prod.fst z)
                                    (Prod.mk Exp Exp (Prod.fst (Prod.snd z)) (Prod.fst (Prod.snd (Prod.snd z)))))))
                  o))
              (And.left hy)))
  (have heq (And (Eq Nat m (Prod.fst y)) (And (Eq Exp t (Prod.fst (Prod.snd y))) (Eq Exp A (Prod.fst (Prod.snd (Prod.snd y))))))
    (tup_eq m t A (Prod.fst y) (Prod.fst (Prod.snd y)) (Prod.fst (Prod.snd (Prod.snd y))) (Eq.trans (Eq.symm hdec) hdo)))
  (constructor) (exact (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))))
  (constructor) (exact (Prod.snd (Prod.snd (Prod.snd (Prod.snd y)))))
  (exact (Eq.trans (decCert_canon v y (And.left hy))
                   (encCert_congr (Prod.fst y) m (Prod.fst (Prod.snd y)) t (Prod.fst (Prod.snd (Prod.snd y))) A
                                  (Prod.fst (Prod.snd (Prod.snd (Prod.snd y)))) (Prod.snd (Prod.snd (Prod.snd (Prod.snd y))))
                                  (Eq.symm (And.left heq)) (Eq.symm (And.left (And.right heq))) (Eq.symm (And.right (And.right heq)))))))
