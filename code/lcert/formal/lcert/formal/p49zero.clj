(ns lcert.formal.p49zero
  "Proposition 4.9 at k = 0, including the closed inhabitant.

  recSyn splits a code into a leaf or a node. The leaf arm uses caseLbl
  (all 100 labels), and each canonical leaf uses Corollary 3.7 and delta.
  The node arm uses the depth hypothesis, which computes to T(ff).

  The small Clojure builders below emit explicit rule applications for
  formation and fixed-context typing. Every emitted theorem is kernel
  checked; the builders are outside the trust base. They do not construct
  higher-depth case trees. No code from the f63 dtgen namespace is copied."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.section4c]
            [lcert.formal.convcase]
            [lcert.formal.certcanon]))

;; One compatible step under T. Unlike a conversion oracle this records
;; the exact path [0], redex, contractum and supplied head-step proof.
(thm p49z_step_T [chkf :- (=> Code Code Bool), x :- Exp, y :- Exp,
                 hh :- (Hd chkf x y)]
  (Step chkf (Exp.tT x) (Exp.tT y))
  (unfold Step)
  (apply (Exists.intro (List.cons Nat 0 (List.nil Nat))))
  (apply (Exists.intro x)) (apply (Exists.intro y))
  (exact (And.intro rfl (And.intro hh rfl))))

;; The generic delta certificate: no evaluation of an abstract chkf is
;; attempted. Its supplied value equation transports Hd.delta's target.
(thm p49z_delta [chkf :- (=> Code Code Bool), c :- Exp, cc :- Code,
                hc :- (Eq (Option Code) (codeOf c) (Option.some Code cc)),
                hf :- (Eq Bool (chkf cc (Code.sl 15)) Bool.false)]
  (Hd chkf (Exp.chk c (cbot)) Exp.ff)
  (exact (hd_cast chkf (Exp.chk c (cbot)) (boolExp (chkf cc (Code.sl 15))) Exp.ff
           (Hd.delta chkf c (cbot) cc (Code.sl 15) hc rfl)
           (congrArg boolExp hf))))

(kdef p49z_depth (=> Exp Exp)
  (fn [c :- Exp] (Exp.recS Exp.tBool Exp.tt Exp.ff c)))
(kdef p49z_motive Exp
  (Exp.tPi U.u1 (Exp.tT (p49z_depth (Exp.var 0)))
    (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 1) (cbot))) Exp.tEmpty)))
(kdef P49zero Exp (Exp.tPi U.uw Exp.tSyn p49z_motive))
(thm p49z_depth_agrees []
  (Eq Exp (Exp.lam U.uw Exp.tSyn (p49z_depth (Exp.var 0))) (sh4_depth0)) (rfl))

;; Builders use the same constructor/binder table as the substitution
;; proofs, rather than inventing a separate convention for the five recSyn
;; binders. Expressions below are the actual Ansatz surface forms.
(def ^:private fields
  (into {} (map (fn [[c fs]] [(symbol (str "Exp." c)) fs])
               @#'lcert.formal.syntactic/exp-fields)))
(defn- emap [f e]
  (if (seq? e)
    (apply list (first e)
      (map (fn [[_ ty depth] x] (if (= ty 'Exp) (f x depth) x))
           (fields (first e)) (rest e))) e))
(defn- shift [n cut e]
  (if (and (seq? e) (= 'Exp.var (first e)))
    (list 'Exp.var (if (< (second e) cut) (second e) (+ n (second e))))
    (emap (fn [x d] (shift n (+ cut d) x)) e)))
(defn- sub [sigma e]
  (if (and (seq? e) (= 'Exp.var (first e))) (sigma (second e))
    (emap (fn [x d]
      (sub (fn [i] (if (< i d) (list 'Exp.var i) (shift d 0 (sigma (- i d))))) x)) e)))
(defn- sub1 [x e] (sub (fn [i] (if (zero? i) x (list 'Exp.var (dec i)))) e))
(defn- ls [ty xs] (reduce (fn [acc x] (list 'List.cons ty x acc)) (list 'List.nil ty) (reverse xs)))
(defn- ds [D] (ls 'Exp D))
(defn- us [u] (ls 'U u))
(defn- v [i] (list 'Exp.var i))
(defn- depth [c] (list 'Exp.recS 'Exp.tBool 'Exp.tt 'Exp.ff c))
(def ^:private bot '(Exp.sleaf (Exp.lbl 15)))
(defn- evidence [c] (list 'Exp.tT (list 'Exp.chk c bot)))
(defn- motive [c]
  (list 'Exp.tPi 'U.u1 (list 'Exp.tT (depth c))
    (list 'Exp.tPi 'U.u1 (evidence (shift 1 0 c)) 'Exp.tEmpty)))
(def ^:private P (motive (v 0)))
(defn- leaf-type [P] (sub (fn [i] (if (zero? i) (list 'Exp.sleaf (v 0)) (v i))) P))
(defn- node-type [P] (sub (fn [i] (if (zero? i) (list 'Exp.snode (v 4) (v 3) (v 2)) (v (+ i 4)))) P))
(defn- at-type [P k n] (sub (fn [i] (if (zero? i) (v k) (v (+ (dec i) n)))) P))

;; Lookup proofs explicitly follow the well-founded nthE/nthU equations;
;; reflexivity on those definitions is not assumed.
(defn- lookup [kind D i]
  (let [ty ({'nthE 'Exp 'nthU 'U} kind) [x & xs] D]
    (if (zero? i) (list (symbol (str kind ".eq_2")) x (ls ty xs))
      (list 'Eq.trans (list (symbol (str kind ".eq_3")) x (ls ty xs) (dec i))
        (lookup kind xs (dec i))))))

(declare form tl)
(defn- form [D A]
  (let [Dx (ds D)]
    (cond
      (symbol? A) (list 'Tl.fBase 'chkf Dx A 'rfl)
      (= 'Exp.tT (first A)) (list 'Tl.fT 'chkf Dx (second A) (tl D (second A)))
      (= 'Exp.tPi (first A))
      (let [[_ r X Y] A]
        (list 'Tl.fPi 'chkf Dx r X Y (form D X) (form (into [X] D) Y)))
      :else (throw (ex-info "p49 formation outside fragment" {:A A})))))
(defn- tl [D e]
  (let [Dx (ds D) [tag & xs] (if (seq? e) e [e])]
    (case tag
      Exp.var (let [i (first xs)] (list 'Tl.zVar 'chkf Dx i (nth D i) (lookup 'nthE D i)))
      (Exp.tt Exp.ff Exp.lbl)
      (list 'Tl.zConst 'chkf Dx e (if (= tag 'Exp.lbl) 'Exp.tLbl 'Exp.tBool)
        (if (= e '(Exp.lbl l)) 'hlt 'rfl))
      Exp.sleaf (list 'Tl.zSleaf 'chkf Dx (first xs) (tl D (first xs)))
      Exp.snode (let [[x c1 c2] xs] (list 'Tl.zSnode 'chkf Dx x c1 c2 (tl D x) (tl D c1) (tl D c2)))
      Exp.chk (let [[c d] xs] (list 'Tl.zChk 'chkf Dx c d (tl D c) (tl D d)))
      Exp.recS
      (let [[M l n c] xs
            Dn (into [(at-type M 1 4) (at-type M 1 3) 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl] D)]
        (list 'Tl.zRecS 'chkf Dx M l n c (tl D c) (form (into ['Exp.tSyn] D) M)
          (tl (into ['Exp.tLbl] D) l) (tl Dn n)))
      (throw (ex-info "p49 type-level term outside fragment" {:e e})))))

;; Convert T(b) to 0 using precisely one Boolean head step and T(ff).
;; Formation supplies the source skeleton; every intermediate also has a
;; skeleton certificate and nbr = true.
(defn- cv-empty [D A head-proof]
  (let [G (list 'skels (ds D)) B '(Exp.tT Exp.ff)]
    (list 'Cv.cvFwd 'chkf G A B 'Exp.tEmpty
      (list 'Cv.cvFwd 'chkf G A A B
        (list 'Cv.cvRefl 'chkf G A
          (list 'lemma25_tl_type 'chkf (ds D) A (form D A)) 'rfl)
        (list 'p49z_step_T 'chkf (second A) 'Exp.ff head-proof)
        (list 'SkJ.wT G 'Exp.ff (list 'SkJ.sFF G)) 'rfl)
      (list 'sh4_step_hd 'chkf B 'Exp.tEmpty (list 'Hd.tTF 'chkf))
      (list 'SkJ.wEmpty G) 'rfl)))

(defn- var-proof [D u i]
  (list 'Rt.rVar 'chkf (ds D) (us u) i (nth D i) (nth u i)
    'rfl (lookup 'nthE D i) (lookup 'nthU u i) 'rfl))
(defn- var-empty [D u i hh]
  (let [A (shift (inc i) 0 (nth D i))]
    (list 'Rt.rConv 'chkf (ds D) (us u) (v i) A 'Exp.tEmpty
      (var-proof D u i) (form D 'Exp.tEmpty) (cv-empty D A hh))))
(defn- lambdas [D u t A body-proof]
  (if (and (seq? t) (= 'Exp.lam (first t)))
    (let [[_ r X b] t [_ _ _ B] A]
      (list 'Rt.rLam 'chkf (ds D) (us u) r X b B (form D X)
        (lambdas (into [X] D) (into [r] u) b B body-proof)))
    (body-proof D u)))
(defn- arm [c i]
  (let [[_ _ A [_ _ B _]] (motive c)]
    (list 'Exp.lam 'U.u1 A (list 'Exp.lam 'U.u1 B (v i)))))
(def ^:private leaf-code '(Exp.sleaf (Exp.lbl l)))
(def ^:private leaf-arm (arm leaf-code 0))
(def ^:private node-code '(Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)))
(def ^:private node-arm (arm node-code 1))
(def ^:private Dleaf ['Exp.tLbl 'Exp.tSyn])
(def ^:private Dnode (into [(at-type P 1 4) (at-type P 1 3) 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl] ['Exp.tSyn]))
(def ^:private params
  '[chkf :- (=> Code Code Bool),
    hff :- (forall [c Code] (Eq Bool (chkf c (Code.sl 15)) Bool.false))])

;; A single parametric leaf theorem supplies all 100 caseLbl arms. The
;; bound hlt is a real premise: type-level label formation requires l < NL.
(b/kdef! 'p49z_leaf '(=> Nat Exp) (list 'fn '[l :- Nat] leaf-arm))
(a/prove-theorem 'p49z_leaf_arm
  (into params '[l :- Nat, hlt :- (Eq Bool (Nat.blt l 100) Bool.true)])
  (list 'Rt 'chkf (ds Dleaf) (us ['U.u0 'U.u0]) (list 'p49z_leaf 'l) (motive leaf-code))
  [(list 'exact
     (lambdas Dleaf ['U.u0 'U.u0] leaf-arm (motive leaf-code)
       (fn [D u] (var-empty D u 0
         '(p49z_delta chkf (Exp.sleaf (Exp.lbl l)) (Code.sl l) rfl (hff (Code.sl l)))))))])

;; The node arm keeps both recSyn recursive-result variables in its usage
;; vector, even though the final variable axiom does not inspect them.
;; It consumes the depth evidence (index 1), not the checker evidence.
(b/kdef! 'p49z_node 'Exp node-arm)
(a/prove-theorem 'p49z_node_arm params
  (list 'Rt 'chkf (ds Dnode) (us ['U.u1 'U.u1 'U.uw 'U.uw 'U.uw 'U.u0]) 'p49z_node (node-type P))
  [(list 'exact
     (lambdas Dnode ['U.u1 'U.u1 'U.uw 'U.uw 'U.uw 'U.u0] node-arm (node-type P)
       (fn [D u] (var-empty D u 1
         '(Hd.recSN chkf Exp.tBool Exp.tt Exp.ff (Exp.var 6) (Exp.var 5) (Exp.var 4))))))])

;; A finite branch list for labels 0 through 99. Sharing each suffix keeps
;; elaboration linear in NL. This is only the k=0 label split, not a
;; purported construction or proof for arbitrary k.
(def ^:private Q (motive '(Exp.sleaf (Exp.var 0))))
(defn- br-name [i] (symbol (str "p49z_branches_" i)))
(defn- br-thm [i] (symbol (str "p49z_branches_typed_" i)))
(doseq [i (range 100 -1 -1)]
  (let [nm (br-name i)
        t (if (= i 100) 'Exp.bnil (list 'Exp.bcons (list 'p49z_leaf i) (br-name (inc i))))]
    (b/kdef! nm 'Exp t)
    (a/prove-theorem (br-thm i) params
      (list 'Rt 'chkf (ds Dleaf) (us ['U.u0 'U.u0]) nm (list 'Exp.tBrs Q i))
      [(list 'exact
         (if (= i 100)
           (list 'Rt.rBnil 'chkf (ds Dleaf) (us ['U.u0 'U.u0]) Q 'rfl)
           (list 'Rt.rBcons 'chkf (ds Dleaf) (us ['U.u0 'U.u0]) Q i
             (list 'p49z_leaf i) (br-name (inc i))
             (list 'p49z_leaf_arm 'chkf 'hff i 'rfl)
             (list (br-thm (inc i)) 'chkf 'hff)
             (form (into ['Exp.tLbl] Dleaf) Q))))])))

(b/kdef! 'p49z_leaf_case 'Exp (list 'Exp.caseL Q (v 0) (br-name 0)))
(a/prove-theorem 'p49z_leaf_case_typed params
  (list 'Rt 'chkf (ds Dleaf) (us ['U.uw 'U.u0]) 'p49z_leaf_case (leaf-type P))
  [(list 'exact
     (list 'Rt.rCaseL 'chkf (ds Dleaf) (us ['U.uw 'U.u0]) (us ['U.u0 'U.u0])
       Q (v 0) (br-name 0) (var-proof Dleaf ['U.uw 'U.u0] 0)
       (form (into ['Exp.tLbl] Dleaf) Q)
       (list (br-thm 0) 'chkf 'hff)))])

(kdef p49z_term Exp
  (Exp.lam U.uw Exp.tSyn
    (Exp.recS p49z_motive p49z_leaf_case p49z_node (Exp.var 0))))

;; Assembly of recSyn and the outer lambda. All recursive-result type
;; formation premises of rRecS are supplied explicitly.
(a/prove-theorem 'p49z_typed params
  '(Rt chkf (List.nil Exp) (List.nil U) p49z_term P49zero)
  [(list 'exact
    (list 'Rt.rLam 'chkf (ds []) (us []) 'U.uw 'Exp.tSyn
      '(Exp.recS p49z_motive p49z_leaf_case p49z_node (Exp.var 0)) 'p49z_motive
      (form [] 'Exp.tSyn)
      (list 'Rt.rRecS 'chkf (ds ['Exp.tSyn]) (us ['U.uw]) (us ['U.u0]) (us ['U.u0])
        'p49z_motive 'p49z_leaf_case 'p49z_node (v 0)
        (var-proof ['Exp.tSyn] ['U.uw] 0)
        (form ['Exp.tSyn 'Exp.tSyn] P)
        '(p49z_leaf_case_typed chkf hff) '(p49z_node_arm chkf hff)
        (form ['Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn] (at-type P 1 3))
        (form (into [(at-type P 1 3)] ['Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn]) (at-type P 1 4)))))])

;; CheckSpec fixes the literal code of 0. Corollary 3.7 with the proved
;; conversion case then discharges the only checker hypothesis in p49z_typed.
(thm p49z_checker_empty [chkf :- (=> Code Code Bool),
                       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                       encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (Eq Code (encTy Exp.tEmpty) (Code.sl 15))
  (exact (Eq.symm (Option.some.inj
           ((And.left (And.right hcs)) Exp.tEmpty (cbot) rfl)))))
(thm prop49_0 [chkf :- (=> Code Code Bool),
               dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
               encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (Rt chkf (List.nil Exp) (List.nil U) p49z_term P49zero)
  (apply (p49z_typed chkf)) (intro c)
  (exact (Eq.trans
           (congrArg (fn [d :- Code] (chkf c d)) (Eq.symm (p49z_checker_empty chkf dec encTy hcs)))
           (cor37_refutation chkf dec encTy hcs (conv_all chkf dec encTy hcs) c))))

;; Hypothesis-free concrete checker instance (relative to the Ansatz kernel).
(thm prop49_0_concrete []
  (Rt (Check decCert) (List.nil Exp) (List.nil U) p49z_term P49zero)
  (exact (prop49_0 (Check decCert) (decOf decCert) encE (check_spec decCert))))
