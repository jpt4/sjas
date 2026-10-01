(ns lcert.formal.prop410
  "F4 — Proposition 4.10′ (H does not give H₁), R4-metatheory.md §4.9.

  In λᶜᵉʳᵗ₀ without the constant H₁, no budget derives a closed inhabitant of
  H₁°.  The proof interprets a derivation in a model whose checker χ agrees
  with Check except at two arguments the derivation never uses, and whose
  reflect returns defaults.

  Here:
  - noH1: H₁ does not occur in a term (generated, with noh_<ctor> splitting);
  - F_refl0 and lemma36_noH1: Lemma 3.6 for H₁-free terms in that model,
    from any χ that rejects every code against ⌜0⌝ (refl0_empty);
  - rt_tr: a derivation under one checker is a derivation under any checker
    agreeing with it below some code size (only δ-steps consult it);
  - prop410_prime: the separation, with χ accepting every large code."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
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
            [lcert.formal.skof]
            [lcert.formal.convcase]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(defn- ren [f] (if (= f 'c) 'cx f))
(defn- nh-terms [fields] (for [[f ty _] fields :when (= ty 'Exp)] (list 'noH1 (ren f))))
(defn- and-chain [xs] (if (= 1 (count xs)) (first xs) (list 'Bool.and (first xs) (and-chain (rest xs)))))
(defn- clause [[ctor fields]]
  (let [pat (if (seq fields) (apply list ctor (map (comp ren first) fields)) ctor)
        body (cond (= ctor 'h1) 'false
                   (empty? (nh-terms fields)) 'true
                   :else (and-chain (nh-terms fields)))]
    [pat body]))
;; noH1 e: the constant H₁ does not occur in e
(eval (list 'a/defn 'noH1 '[e :- Exp] 'Bool (apply list 'match 'e (map (comp vec clause) exp-fields))))
(defn- split [xs h]
  (if (= 1 (count xs)) h
      (let [x0 (first xs) R (and-chain (rest xs))]
        (list 'And.intro (list 'band_left x0 R h) (split (rest xs) (list 'band_right x0 R h))))))
(defn- and-props [xs] (if (= 1 (count xs)) (list 'Eq 'Bool (first xs) 'Bool.true) (list 'And (list 'Eq 'Bool (first xs) 'Bool.true) (and-props (rest xs)))))
(doseq [[ctor fields] exp-fields :let [xs (nh-terms fields)] :when (and (>= (count xs) 2) (not= ctor 'h1))]
  (let [fs (map (comp ren first) fields)
        term (apply list (symbol (str "Exp." ctor)) fs)]
    (a/prove-theorem (symbol (str "noh_" ctor))
      (lv (into (vec (mapcat (fn [[f ty _]] [(ren f) :- ty]) fields)) ['hnh :- (list 'Eq 'Bool (list 'noH1 term) 'Bool.true)]))
      (lv (and-props xs))
      (lv [(list 'exact (split xs 'hnh))]))))


;; --- the χ-model's Reflect case ------------------------------------------------------

;; With dec the constant none, ⟦reflect_X r e⟧ is the default of skel X
;; (den_refl_dec0); the default lies in V(X) for every base X but 0
;; (refl0_val); at 0 the evidence gives χ(⟦r⟧, ⌜0⌝) = tt, which the hypothesis
;; on χ refutes (refl0_empty).

(thm refl0_val [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                  G :- (List Sk), en :- (HEnv G), k :- Nat, X :- Exp, cd :- Exp]
  (=> (Eq (Option Exp) (baseCode X) (Option.some Exp cd)) (=> (Eq Exp X Exp.tEmpty) False)
      (V chkf dec encTy n X G en k (skel X) (dflt (skel X))))
  (cases X) (intro hb hne) (exact (False.elim (hne (Eq.refl$1 Exp.tEmpty)))) (intro hb hne) (exact True.intro) (intro hb hne) (exact True.intro) (intro hb hne) (exact True.intro) (intro hb hne) (exact (Nat.zero_lt_succ 99)) (intro hb hne) (exact (Eq.refl$1 Bool.true)) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (And.intro (Nat.zero_le k) (Eq.refl$1 Bool.true))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))) (intro hb hne) (exact (False.elim$0 (none_ne_someE cd hb))))

(thm bool_rec_eq [α :- Type, x :- α, y :- α, b :- Bool, h :- (Eq α y x)] (Eq α (Bool.rec$1 (fn [_ :- Bool] α) x y b) x)
  (subst h) (cases b) (rfl) (rfl))
(def ^:private dec0 '(fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))))
(eval (list 'lcert.formal.base/thm 'den_refl_dec0
  '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), n :- Nat, X :- Exp, r :- Exp, e :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G)]
  (list 'Eq '(Car sk) (list 'den 'chkf dec0 'encTy 'n '(Exp.refl X r e) 'G 'sk 'en) '(dflt sk))
  (list 'rw [(list 'den_refl_at 'chkf dec0 'encTy 'n 'X 'r 'e 'G 'sk 'en)])
  '(refine' (bool_rec_eq _ _ _ _ _))
  '(rfl)))

(defn- ES0 [us kk] (list 'EnvSat 'chkf dec0 'encTy 'n 'D us 'en kk))
(defn- split0 [h a b K ka kb hp]
  (let [body (list 'And (list 'Nat.le (list '+ ka kb) K) (list 'And (ES0 a ka) (ES0 b kb)))
        ex (list 'Exists (list 'fn [ka :- 'Nat] (list 'Exists (list 'fn [kb :- 'Nat] body))))
        hx (symbol (str hp "_ex")) hq (symbol (str hp "_q")) hr (symbol (str hp "_r"))]
    [(list 'have hx ex (list 'EnvSat_split 'chkf dec0 'encTy 'n 'D a b 'en K h))
     (list 'refine' (list 'exN '_ '_ hx '_)) (list 'intro ka hq)
     (list 'refine' (list 'exN '_ '_ hq '_)) (list 'intro kb hr)
     (list 'have hp body hr)]))
(eval (list 'lcert.formal.base/thm 'refl0_empty
  '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G), r :- Exp, cd :- Exp, X :- Exp,
    hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)), hX :- (Eq Exp X Exp.tEmpty),
    hchi :- (forall [c Code] (Eq Bool (chkf c (Code.sl 15)) Bool.false))]
  (list '=> (list 'Eq 'Bool (list 'chkf (list 'den 'chkf dec0 'encTy 'n 'r 'G 'Sk.cert 'en) (list 'den 'chkf dec0 'encTy 'n 'cd 'G 'Sk.syn 'en)) 'Bool.true) 'False)
  '(intro hct)
  '(have e1 (Eq Exp cd (cLeaf 15)) (Eq.trans (base_leaf X cd hb) (congrArg (fn [Z :- Exp] (cLeaf (leafOf Z))) hX)))
  (list 'have 'e2 (list 'Eq 'Code (list 'den 'chkf dec0 'encTy 'n 'cd 'G 'Sk.syn 'en) '(Code.sl 15))
        (list 'Eq.trans (list 'congrArg (list 'fn '[Z :- Exp] (list 'den 'chkf dec0 'encTy 'n 'Z 'G 'Sk.syn 'en)) 'e1)
              (list 'den_cleaf 'chkf dec0 'encTy 'n 'G 'en 15)))
  (list 'have 'h3 (list 'Eq 'Bool (list 'chkf (list 'den 'chkf dec0 'encTy 'n 'r 'G 'Sk.cert 'en) '(Code.sl 15)) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf (list 'den 'chkf dec0 'encTy 'n 'r 'G 'Sk.cert 'en) 'q) 'Bool.true)) 'e2) 'hct))
  (list 'exact (list 'Bool.noConfusion (list 'Eq.trans (list 'Eq.symm (list 'hchi (list 'den 'chkf dec0 'encTy 'n 'r 'G 'Sk.cert 'en))) 'h3)))))

(eval (concat (list 'lcert.formal.base/thm 'F_refl0
  '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U),
    X :- Exp, cd :- Exp, r :- Exp, e :- Exp, hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
    hchi :- (forall [c Code] (Eq Bool (chkf c (Code.sl 15)) Bool.false)),
    ihe :- (Sound chkf (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) encTy n D us2 e (chkT r cd))]
  (list 'Sound 'chkf dec0 'encTy 'n 'D '(vadd us1 us2) '(Exp.refl X r e) 'X))
  (concat
   ['(intro en k hk hs)
    '(rw [(den_refl_dec0 chkf encTy n X r e (skels D) (skel X) en)])]
   (split0 'hs 'us1 'us2 'k 'k1 'k2 'p)
   ['(have hk2 (Nat.le k2 n) (Nat.le_trans (Nat.le_trans (Nat.le_add_left k2 k1) (And.left p)) hk))
    (list 'have 've (list 'V 'chkf dec0 'encTy 'n '(chkT r cd) '(skels D) 'en 'k2 'Sk.unit (list 'den 'chkf dec0 'encTy 'n 'e '(skels D) 'Sk.unit 'en))
          '(ihe en k2 hk2 (And.right (And.right p))))
    (list 'exact (list 'refl0_val 'chkf dec0 'encTy 'n '(skels D) 'en 'k 'X 'cd 'hb
                       (list 'fn '[hX :- (Eq Exp X Exp.tEmpty)]
                             (list 'refl0_empty 'chkf 'encTy 'n '(skels D) 'en 'r 'cd 'X 'hb 'hX 'hchi
                                   (list 'chk_true 'chkf dec0 'encTy 'n '(skels D) 'en 'k2 'r 'cd (list 'den 'chkf dec0 'encTy 'n 'e '(skels D) 'Sk.unit 'en) 've)))))])))


;; --- Lemma 3.6 for H₁-free terms, in the χ-model ---------------------------------------

;; lemma36_noH1: lemma36_step's table (lemma36.clj) with dec := none, each IH
;; given its subterm's noH1 (split from the conclusion's by noh_<ctor>), Reflect
;; by F_refl0, Conv by F_conv (convcase.clj; any checker), and H₁ vacuous (its
;; term is not H₁-free).  No CheckSpec and no outer induction: the decoded
;; programs never run.

(def ^:private l36cases @#'lcert.formal.lemma36/case-terms)
(def ^:private noh-map
  {2 {'ih_ht ['noh_lam '[r A t] 1 2]} 3 {'ih_hf ['noh_app '[f u] 0 2]}
   4 {'ih_hf ['noh_app '[f u] 0 2] 'ih_hu ['noh_app '[f u] 1 2]}
   5 {'ih_hy ['noh_pair '[(Exp.tSig U.u0 A B) x y] 2 3]}
   6 {'ih_hx ['noh_pair '[(Exp.tSig r A B) x y] 1 3] 'ih_hy ['noh_pair '[(Exp.tSig r A B) x y] 2 3]}
   7 {'ih_hp ['noh_letp '[C p t] 1 3] 'ih_ht ['noh_letp '[C p t] 2 3]}
   8 {'ih_ht ['noh_abort '[A t] 1 2]} 9 {'ih_ht :same}
   10 {'ih_hb ['noh_ite '[b t e] 0 3] 'ih_ht ['noh_ite '[b t e] 1 3] 'ih_he ['noh_ite '[b t e] 2 3]}
   11 {'ih_ht ['noh_elimB '[P b t e] 2 4] 'ih_he ['noh_elimB '[P b t e] 3 4]}
   13 {'ih_hz ['noh_recN '[P z s n] 1 4] 'ih_hs ['noh_recN '[P z s n] 2 4]}
   14 {'ih_hx ['noh_caseL '[P x bs] 1 3] 'ih_hb ['noh_caseL '[P x bs] 2 3]}
   16 {'ih_hh ['noh_bcons '[h t] 0 2] 'ih_ht ['noh_bcons '[h t] 1 2]}
   17 {'ih_h :same}
   18 {'ih_hx ['noh_snode '[x c1 c2] 0 3] 'ih_h1 ['noh_snode '[x c1 c2] 1 3] 'ih_h2 ['noh_snode '[x c1 c2] 2 3]}
   19 {'ih_hl ['noh_recS '[P tl tn c] 1 4] 'ih_hn ['noh_recS '[P tl tn c] 2 4] 'ih_hc ['noh_recS '[P tl tn c] 3 4]}
   20 {'ih_h :same}
   21 {'ih_hd ['noh_node '[d x r1 r2] 0 4] 'ih_hx ['noh_node '[d x r1 r2] 1 4] 'ih_h1 ['noh_node '[d x r1 r2] 2 4] 'ih_h2 ['noh_node '[d x r1 r2] 3 4]}
   22 {'ih_hg ['noh_itR '[X g h r] 1 4] 'ih_hh ['noh_itR '[X g h r] 2 4] 'ih_hr ['noh_itR '[X g h r] 3 4]}
   23 {'ih_h :same}
   27 {'ih_hr ['noh_insp '[X r c t1 t2] 1 5] 'ih_h1 ['noh_insp '[X r c t1 t2] 3 5] 'ih_h2 ['noh_insp '[X r c t1 t2] 4 5]}})
(defn- conj-i [base i n]
  (let [r (reduce (fn [t _] (list 'And.right t)) base (range i))] (if (= i (dec n)) r (list 'And.left r))))
(defn- noh-proof [spec]
  (if (= spec :same) 'hnh (let [[lem args i n] spec] (conj-i (concat (list lem) args (list 'hnh)) i n))))
(defn- add-noh [idx form]
  (walk/prewalk
    (fn [x] (if (and (seq? x) (symbol? (first x)) (.startsWith (name (first x)) "ih_") (= 2 (count x)))
              (let [spec (get-in noh-map [idx (first x)])]
                (assert spec [idx (first x)])
                (list (first x) (second x) (noh-proof spec)))
              x))
    form))
(def ^:private ncases
  (vec (map-indexed
        (fn [idx s]
          (let [f (walk/postwalk-replace {'dec dec0} (read-string s))]
            (cond
              (= idx 9) (list (list 'F_conv 'chkf dec0 'encTy 'cap) 'D 'us 't 'A 'B 'ht 'hB 'hc '(ih_ht hw hnh))
              (= idx 25) '(Bool.noConfusion hnh)
              (= idx 26) '(F_refl0 chkf encTy cap D us1 us2 X cd r e hb hchi (ih_he hw (And.right (And.right (noh_refl X r e hnh)))))
              :else (add-noh idx f))))
        l36cases)))
(eval (list 'a/prove-theorem ''lemma36_noH1
  (list 'lv (list 'quote ['chkf :- '(=> Code Code Bool) 'encTy :- '(=> Exp Code) 'cap :- 'Nat
                          'hchi :- '(forall [c Code] (Eq Bool (chkf c (Code.sl 15)) Bool.false))
                          'D0 :- '(List Exp) 'us0 :- '(List U) 't0 :- 'Exp 'A0 :- 'Exp 'der :- '(Rt chkf D0 us0 t0 A0)]))
  (list 'quote (list '=> '(WFCtx chkf D0) '(Eq Bool (noH1 t0) Bool.true) (list 'Sound 'chkf dec0 'encTy 'cap 'D0 'us0 't0 'A0)))
  (list 'lv (list 'quote (into ['(induction der)] (mapcat (fn [c] ['(intro hw hnh) (list 'refine' c)]) ncases))))))


;; --- the checker transport ---------------------------------------------------------------

;; Agree N chk1 chk2: the checkers agree whenever the second code has fewer
;; than N internal nodes.  A derivation under chk1 is one under every chk2
;; that agrees with it below some N: only δ-steps consult the checker, at
;; codes the derivation records.  hd_tr (δ: N = the code's size + 1), step_tr,
;; cv_tr, then tl_tr and rt_tr, generated from the rule tables (each premise's
;; bound summed; trI introduces the existential).

(kdef Agree (=> Nat (=> Code Code Bool) (=> Code Code Bool) Prop)
  (fn [N :- Nat, chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool)]
    (forall [c Code] (forall [d Code] (=> (Nat.lt (cnodes d) N) (Eq Bool (chk2 c d) (chk1 c d)))))))
(thm agree_mono [M :- Nat, N :- Nat, chk1 :- (=> Code Code Bool), chk2 :- (=> Code Code Bool), hag :- (Agree N chk1 chk2), hle :- (LE.le M N)]
  (Agree M chk1 chk2)
  (exact (fn [c :- Code, d :- Code, h :- (Nat.lt (cnodes d) M)] (hag c d (Nat.lt_of_lt_of_le h hle)))))

(defn- read-inductive [file nm]
  (let [s (slurp (clojure.java.io/resource file)) i (.indexOf s (str "(a/inductive " nm " "))]
    (read-string (subs s i))))
(def ^:private hd-spec (read-inductive "lcert/formal/conv.clj" "Hd"))
(defn- ctors [spec] (filter seq? (drop 3 spec)))
(defn- fields [ctor] (vec (filter vector? (take-while #(not= % :where) (rest ctor)))))
(def ^:private hd-script
  (into ['(induction der)]
        (mapcat (fn [ctor]
                  (let [nm (first ctor) fs (map first (fields ctor))
                        built (apply list (symbol (str "Hd." nm)) 'chk2 fs)]
                    (if (= nm 'delta)
                      ['(constructor) '(exact (+ (cnodes dc) 1)) '(intro chk2 hag)
                       (list 'exact (list 'Eq.mp '(congrArg (fn [b :- Bool] (Hd chk2 (Exp.chk c d) (boolExp b))) (hag cc dc (Nat.lt_succ_self (cnodes dc)))) built))]
                      ['(constructor) '(exact 0) '(intro chk2 hag) (list 'exact built)])))
                (ctors hd-spec))))
(a/prove-theorem 'hd_tr '[chk1 :- (=> Code Code Bool), e0 :- Exp, e20 :- Exp, der :- (Hd chk1 e0 e20)]
  '(Exists (fn [N :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (Hd chk2 e0 e20)))))
  (lv hd-script))

(thm trI [chk1 :- (=> Code Code Bool), P :- (=> (=> Code Code Bool) Prop), N :- Nat,
            h :- (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (P chk2)))]
  (Exists (fn [M :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree M chk1 chk2) (P chk2)))))
  (exact (Exists.intro$1 N h)))

(thm step_tr [chk1 :- (=> Code Code Bool), A :- Exp, B :- Exp, hs :- (Step chk1 A B)]
  (Exists (fn [N :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (Step chk2 A B)))))
  (have hs2 (Exists (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
              (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chk1 r r2) (Eq Exp B (setP p A r2)))))))))) hs)
  (refine' (exT (List Nat) _ _ hs2 _)) (intro p hp) (refine' (exT Exp _ _ hp _)) (intro r hr) (refine' (exT Exp _ _ hr _)) (intro r2 hq)
  (have hd (Hd chk1 r r2) (And.left (And.right hq)))
  (refine' (exN _ _ (hd_tr chk1 r r2 hd) _)) (intro N hN)
  (refine' (trI chk1 _ N _)) (intro chk2 hag)
  (constructor) (exact p) (constructor) (exact r) (constructor) (exact r2)
  (exact (And.intro (And.left hq) (And.intro (hN chk2 hag) (And.right (And.right hq))))))
(a/prove-theorem 'cv_tr '[chk1 :- (=> Code Code Bool), G :- (List Sk), A0 :- Exp, B0 :- Exp, der :- (Cv chk1 G A0 B0)]
  '(Exists (fn [N :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (Cv chk2 G A0 B0)))))
  (lv '[(induction der)
        (refine' (trI chk1 _ 0 _)) (intro chk2 hag) (exact (Cv.cvRefl chk2 G A h hn))
        (refine' (exN _ _ ih_hab _)) (intro N1 h1) (refine' (exN _ _ (step_tr chk1 B C hs) _)) (intro N2 h2)
        (refine' (trI chk1 _ (+ N1 N2) _)) (intro chk2 hag)
        (exact (Cv.cvFwd chk2 G A B C (h1 chk2 (agree_mono N1 (+ N1 N2) chk1 chk2 hag (Nat.le_add_right N1 N2)))
                 (h2 chk2 (agree_mono N2 (+ N1 N2) chk1 chk2 hag (Nat.le_add_left N2 N1))) hc hn))
        (refine' (exN _ _ ih_hab _)) (intro N1 h1) (refine' (exN _ _ (step_tr chk1 C B hs) _)) (intro N2 h2)
        (refine' (trI chk1 _ (+ N1 N2) _)) (intro chk2 hag)
        (exact (Cv.cvBwd chk2 G A B C (h1 chk2 (agree_mono N1 (+ N1 N2) chk1 chk2 hag (Nat.le_add_right N1 N2)))
                 (h2 chk2 (agree_mono N2 (+ N1 N2) chk1 chk2 hag (Nat.le_add_left N2 N1))) hc hn))]))

(defn- sumN [ns] (if (empty? ns) 0 (if (= 1 (count ns)) (first ns) (list '+ (first ns) (sumN (rest ns))))))
(doseq [k (range 2 9) i (range k)]
  (let [ns (vec (for [j (range k)] (symbol (str "n" j))))]
    (eval (list 'lcert.formal.base/thm (symbol (str "lsum_" k "_" i)) (vec (mapcat (fn [n] [n :- 'Nat]) ns))
                (list 'LE.le (nth ns i) (sumN ns)) '(omega)))))
(defn- le-proof [k i ns] (if (= k 1) (list 'Nat.le_refl (first ns)) (apply list (symbol (str "lsum_" k "_" i)) ns)))
(def ^:private tl-spec (read-inductive "lcert/formal/judgment.clj" "Tl"))
(def ^:private rt-spec (read-inductive "lcert/formal/judgment.clj" "Rt"))
(defn- prem-kind [self ty]
  (when (seq? ty)
    (cond (and (= (first ty) self) (= (second ty) 'chkf)) :ih
          (and (= (first ty) 'Tl) (= (second ty) 'chkf)) :tl
          (and (= (first ty) 'Cv) (= (second ty) 'chkf)) :cv
          :else nil)))
(defn- tr-script [self ind ctor-list]
  (into ['(induction der)]
    (mapcat
      (fn [ctor]
        (let [nm (first ctor) fs (fields ctor)
              eps (vec (keep-indexed (fn [i [f ty]] (when-let [kd (prem-kind self ty)] [i f kd])) fs))
              k (count eps) Ns (vec (for [j (range k)] (symbol (str "NT" j)))) hs (vec (for [j (range k)] (symbol (str "hT" j))))
              SUM (sumN Ns)
              destr (mapcat (fn [j [_ f kd]]
                              [(list 'refine' (list 'exN '_ '_ (case kd :ih (symbol (str "ih_" f)) :tl (list 'tl_tr 'chk1 '_ '_ '_ '_ f) :cv (list 'cv_tr 'chk1 '_ '_ '_ f)) '_))
                               (list 'intro (Ns j) (hs j))])
                            (range) eps)
              eidx (into {} (map-indexed (fn [j [i _ _]] [i j]) eps))
              args (map-indexed (fn [i [f _]]
                                  (if-let [j (eidx i)]
                                    (list (hs j) 'chk2 (list 'agree_mono (Ns j) SUM 'chk1 'chk2 'hag (le-proof k j Ns)))
                                    f))
                                fs)]
          (concat destr
                  [(list 'refine' (list 'trI 'chk1 '_ SUM '_)) '(intro chk2 hag)
                   (list 'exact (apply list (symbol (str ind "." nm)) 'chk2 args))])))
      ctor-list)))
(a/prove-theorem 'tl_tr '[chk1 :- (=> Code Code Bool), w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp, der :- (Tl chk1 w0 D0 t0 A0)]
  '(Exists (fn [N :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (Tl chk2 w0 D0 t0 A0)))))
  (lv (tr-script 'Tl "Tl" (ctors tl-spec))))

(a/prove-theorem 'rt_tr '[chk1 :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chk1 D0 us0 t0 A0)]
  '(Exists (fn [N :- Nat] (forall [chk2 (=> Code Code Bool)] (=> (Agree N chk1 chk2) (Rt chk2 D0 us0 t0 A0)))))
  (lv (tr-script 'Rt "Rt" (ctors rt-spec))))


;; --- the modified checker χ and the separation ----------------------------------------------

;; χ (chiF chkf N) accepts every pair whose second code has more than N nodes
;; and is chkf otherwise — a simpler choice than the paper's (which changes
;; Check at two pairs only), with the same effect: it agrees with Check below
;; N, it is Check at ⌜0⌝ (no nodes), and it accepts the evidence at a code D
;; (padC N) larger than any the derivation uses.

;; χ accepts every pair whose second code has more than N nodes, and otherwise is chkf
(kdef chiF (=> (=> Code Code Bool) Nat Code Code Bool)
  (fn [chkf :- (=> Code Code Bool), N :- Nat, c :- Code, d :- Code] (Bool.or (Nat.blt N (cnodes d)) (chkf c d))))

(thm blt_false [N :- Nat, m :- Nat, h :- (Nat.lt m N)] (Eq Bool (Nat.blt N m) Bool.false)
  (exact (bfalse (Nat.blt N m) (fn [h2 :- (Eq Bool (Nat.blt N m) Bool.true)]
           (Nat.lt_irrefl N (Nat.lt_trans (Eq.mp (Nat.blt_eq N m) h2) h))))))
(thm chi_agree [chkf :- (=> Code Code Bool), N :- Nat] (Agree N chkf (chiF chkf N))
  (intro c d h)
  (change (Eq Bool (Bool.or (Nat.blt N (cnodes d)) (chkf c d)) (chkf c d)))
  (rw [(blt_false N (cnodes d) h)]))
;; a code with N + 1 internal nodes, all labels 0
(kdef padC (=> Nat Code) (fn [k :- Nat] (Nat.rec$1 (fn [_ :- Nat] Code) (Code.sn 0 (Code.sl 0) (Code.sl 0)) (fn [j :- Nat, p :- Code] (Code.sn 0 p (Code.sl 0))) k)))

(thm arith1 [a :- Nat, n :- Nat, h :- (Eq Nat a (+ n 1))] (Eq Nat (+ 1 (+ a 0)) (+ (+ n 1) 1)) (omega))
(thm pad_nodes [k :- Nat] (Eq Nat (cnodes (padC k)) (+ k 1))
  (induction k) (rfl)
  (exact (arith1 (cnodes (padC n)) n ih_n)))

(thm pad_ok [k :- Nat] (Eq Bool (lblOk (padC k)) Bool.true)
  (induction k) (rfl)
  (exact (Eq.trans (lblOk_sn 0 (padC n) (Code.sl 0)) (congrArg (fn [q :- Bool] (Bool.and (Nat.blt 0 100) (Bool.and q (lblOk (Code.sl 0))))) ih_n))))
(thm chi_big [chkf :- (=> Code Code Bool), N :- Nat, c :- Code, d :- Code, h :- (Nat.lt N (cnodes d))]
  (Eq Bool (chiF chkf N c d) Bool.true)
  (change (Eq Bool (Bool.or (Nat.blt N (cnodes d)) (chkf c d)) Bool.true))
  (rw [(Eq.mpr (Nat.blt_eq N (cnodes d)) h)]))

(def ^:private chi '(chiF chkf N))
(def ^:private D '(padC N))
(defn- dn [t G s e] (list 'den chi dec0 'encTy 'k t G s e))
(def ^:private G3 '(List.cons Sk Sk.syn (List.cons Sk Sk.cert (List.cons Sk Sk.cert G))))
(def ^:private E3 (list 'Prod.mk D '(Prod.mk (Code.sl 0) (Prod.mk (Code.sl 0) en))))
(def ^:private G4 (list 'List.cons 'Sk 'Sk.unit G3))
(def ^:private E4 (list 'Prod.mk 'Unit.unit E3))
(thm big1 [N :- Nat] (Nat.lt N (cnodes (padC N))) (rw [(pad_nodes N)]) (exact (Nat.lt_succ_self N)))

(eval (list 'lcert.formal.base/thm 'ev4
  '[chkf :- (=> Code Code Bool), N :- Nat, encTy :- (=> Exp Code), k :- Nat, G :- (List Sk), en :- (HEnv G)]
  (list 'Eq 'Bool (dn '(Exp.chk (Exp.prn (Exp.var 2)) (Exp.var 0)) G3 'Sk.bool E3) 'Bool.true)
  (list 'rw [(list 'den_chk_val chi dec0 'encTy 'k '(Exp.prn (Exp.var 2)) '(Exp.var 0) G3 E3)])
  (list 'rw [(list 'den_prn_val chi dec0 'encTy 'k '(Exp.var 2) G3 E3)])
  (list 'rw [(list 'den_var_at chi dec0 'encTy 'k 2 G3 'Sk.cert E3)])
  (list 'rw [(list 'den_var_at chi dec0 'encTy 'k 0 G3 'Sk.syn E3)])
  (list 'exact (list 'chi_big 'chkf 'N '(Code.sl 0) D '(big1 N)))))

(thm arith2 [N :- Nat, a :- Nat, h :- (LT.lt N a)] (LT.lt N (+ 1 (+ a 0))) (omega))
(eval (list 'lcert.formal.base/thm 'ev5
  '[chkf :- (=> Code Code Bool), N :- Nat, encTy :- (=> Exp Code), k :- Nat, G :- (List Sk), en :- (HEnv G)]
  (list 'Eq 'Bool (dn '(Exp.chk (Exp.prn (Exp.var 2)) (negT (Exp.var 1))) G4 'Sk.bool E4) 'Bool.true)
  (list 'rw [(list 'den_chk_val chi dec0 'encTy 'k '(Exp.prn (Exp.var 2)) '(negT (Exp.var 1)) G4 E4)])
  (list 'rw [(list 'den_prn_val chi dec0 'encTy 'k '(Exp.var 2) G4 E4)])
  (list 'rw [(list 'den_negT chi dec0 'encTy 'k '(Exp.var 1) G4 E4)])
  (list 'rw [(list 'den_var_at chi dec0 'encTy 'k 2 G4 'Sk.cert E4)])
  (list 'rw [(list 'den_var_at chi dec0 'encTy 'k 1 G4 'Sk.syn E4)])
  (list 'exact (list 'chi_big 'chkf 'N '(Code.sl 0) (list 'Code.sn 25 D '(Code.sl 15)) (list 'arith2 'N (list 'cnodes D) '(big1 N))))))

(def ^:private Gk '(skels (thetaD k)))
(def ^:private ek '(tokEnvD k))
(defn- Vc [A G e j s v] (list 'V chi dec0 'encTy 'k A G e j s v))
(defn- cons-sk [s G] (list 'List.cons 'Sk s G))
(def ^:private fH (list 'den chi dec0 'encTy 'k 't Gk '(Sk.arr Sk.cert (Sk.arr Sk.cert (Sk.arr Sk.syn (Sk.arr Sk.unit (Sk.arr Sk.unit Sk.unit))))) ek))
(def ^:private A4 '(chkT (Exp.var 2) (Exp.var 0)))
(def ^:private A5 '(chkT (Exp.var 2) (negT (Exp.var 1))))

;; levels: [usage dom-type dom-skel]; builds V(Π…) unfolded through n levels, then V of the rest at the rest's skeleton
(defn- unfold-pi [levels rest-ty rest-sk G e K f]
  (if (empty? levels)
    (Vc rest-ty G e K rest-sk f)
    (let [[[u A sx] & more] levels
          d (count levels)
          a (symbol (str "a" d)) j (symbol (str "j" d))
          G2 (cons-sk sx G) e2 (list 'Prod.mk a e)]
      (if (= u :w)
        (list 'forall [a (list 'Car sx)] (list '=> (Vc A G e 0 sx a) (unfold-pi more rest-ty rest-sk G2 e2 K (list f a))))
        (let [K2 (list '+ K j)]
          (list 'forall [j 'Nat] (list '=> (list 'Nat.le K2 'k)
            (list 'forall [a (list 'Car sx)] (list '=> (Vc A G e j sx a) (unfold-pi more rest-ty rest-sk G2 e2 K2 (list f a)))))))))))
(def ^:private lv5 [[:1 'Exp.tR 'Sk.cert] [:1 'Exp.tR 'Sk.cert] [:w 'Exp.tSyn 'Sk.syn] [:1 A4 'Sk.unit] [:1 A5 'Sk.unit]])

(def ^:private hv-type (unfold-pi lv5 'Exp.tEmpty 'Sk.unit Gk ek 'k fH))

;; The contradiction: ⟦t⟧ at two leaves (no nodes), D (labels 0), and ⋆ twice —
;; χ accepts both pieces of evidence (ev4, ev5) — lies in V(0) = ∅.

(eval (list 'lcert.formal.base/thm 'p410_core
  '[chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), k :- Nat, t :- Exp, N :- Nat,
    hnh :- (Eq Bool (noH1 t) Bool.true),
    hchi :- (forall [c Code] (Eq Bool ((chiF chkf N) c (Code.sl 15)) Bool.false)),
    der2 :- (Rt (chiF chkf N) (thetaD k) (thetaU k) t (H1circ))]
  'False
  (list 'have 'hs (list 'Sound chi dec0 'encTy 'k '(thetaD k) '(thetaU k) 't '(H1circ))
        (list 'lemma36_noH1 chi 'encTy 'k 'hchi '(thetaD k) '(thetaU k) 't '(H1circ) 'der2 (list 'wf_theta chi 'k) 'hnh))
  (list 'have 'hv hv-type (list 'hs ek 'k '(Nat.le_refl k) (list 'tok_sat chi dec0 'encTy 'k 'k)))
  (list 'exact (list 'hv 0 '(Nat.le_refl k) '(Code.sl 0) '(And.intro (Nat.le_refl 0) (Eq.refl$1 Bool.true))
                        0 '(Nat.le_refl k) '(Code.sl 0) '(And.intro (Nat.le_refl 0) (Eq.refl$1 Bool.true))
                        D '(pad_ok N)
                        0 '(Nat.le_refl k) 'Unit.unit (list 'ev4 'chkf 'N 'encTy 'k Gk ek)
                        0 '(Nat.le_refl k) 'Unit.unit (list 'ev5 'chkf 'N 'encTy 'k Gk ek)))))

;; Proposition 4.10′: for every checker satisfying CheckSpec, no H₁-free term
;; derives H₁° in any Θₖ.

(thm prop410_prime [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                 hcs :- (CheckSpec chkf dec encTy), k :- Nat, t :- Exp, hnh :- (Eq Bool (noH1 t) Bool.true),
                 der :- (Rt chkf (thetaD k) (thetaU k) t (H1circ))]
  False
  (refine' (exN _ _ (rt_tr chkf (thetaD k) (thetaU k) t (H1circ) der) _)) (intro N hN)
  (have e15 (Eq Code (Code.sl 15) (encTy Exp.tEmpty))
    (someC_inj (Code.sl 15) (encTy Exp.tEmpty) ((And.left (And.right hcs)) Exp.tEmpty (cLeaf 15) (Eq.refl$1 (Option.some Exp (cLeaf 15))))))
  (exact (p410_core chkf encTy k t N hnh
           (fn [c :- Code] (Eq.mpr (congrArg (fn [q :- Code] (Eq Bool (chkf c q) Bool.false)) e15)
                                   (cor37_refutation chkf dec encTy hcs (conv_all chkf dec encTy hcs) c)))
           (hN (chiF chkf N) (chi_agree chkf N)))))
