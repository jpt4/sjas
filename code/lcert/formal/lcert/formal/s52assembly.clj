(ns lcert.formal.s52assembly
  "Theorem 5.2: context extension and the derivation induction (R4 §5)."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52fund :refer [prove! pars ps ctx den rel trace result]]
            [lcert.formal.s52bind]
            [lcert.formal.s52prod]
            [lcert.formal.s52branch]
            [lcert.formal.s52recn]
            [lcert.formal.s52recs]
            [lcert.formal.s52itr]
            [lcert.formal.s52inspect]
            [lcert.formal.s52reflect]
            [lcert.formal.s52reg]))

;; Dispatch constTyped explicitly: labels retain the finite-carrier bound.
;; Formation judgments are not executions; only the five term constants
;; appear here. The same proof serves both evaluators and both term modes.
(prove! 's52_const ctx
  (list 'forall '[t Exp] (list 'forall '[A Exp]
    (list '=> '(Eq Bool (constTyped t A) Bool.true) (result 't 'A 't))))
  (let [fields @#'lcert.formal.syntactic/exp-fields
        valid '{star tUnit, tt tBool, ff tBool, zero tNat, lbl tLbl}]
    (into '[(intro t) (cases t)]
      (mapcat (fn [[c _]]
        (if-let [ty (valid c)]
          (into '[(intro A) (cases A)]
            (mapcat (fn [[ac _]]
              ['(intro hct)
               (if (= ac ty)
                 (list 'exact (apply list (symbol (str "s52_" c))
                   (concat ps '[G en rho]
                     (when (= c 'lbl) '[l (Eq.mp (Nat.blt_eq l (NL)) hct)]))))
                 '(exact (Bool.noConfusion hct)))]) fields))
          '[(intro A hct) (exact (Bool.noConfusion hct))])) fields))))

;; Type-level RecSyn does not record hY1/hY2. Their skeleton formation
;; follows from its motive by the existing, checked substitution maps.
(thm s52_wfs_y1 [G :- (List Sk), P :- Exp,
                 hp :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
  (SkJ Bool.true (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
    (y1Ty P) Sk.unit)
  (exact (skj_subst Bool.true (List.cons Sk Sk.syn G) P Sk.unit hp
    (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
    (fn [j :- Nat] (sAt 1 3 j)) (subOK_sAt13 G))))

(thm s52_wfs_y2 [G :- (List Sk), P :- Exp,
                 hp :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
  (SkJ Bool.true (List.cons Sk (skel (y1Ty P))
    (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) (y2Ty P) Sk.unit)
  (exact (skj_subst Bool.true (List.cons Sk Sk.syn G) P Sk.unit hp
    (List.cons Sk (skel (y1Ty P)) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
    (fn [j :- Nat] (sAt 1 4 j)) (subOK_sAt14 (skel (y1Ty P)) G))))

;; Inspect's two evidence types are formed from the code's skeleton
;; judgment. This discharges the context premise for the type-level rule,
;; which does not record the runtime rule's hF1/hF2 fields.
(thm s52_wfs_insp1 [G :- (List Sk), c :- Exp, hc :- (SkJ Bool.false G c Sk.syn)]
  (SkJ Bool.true (List.cons Sk Sk.cert G) (chkT (Exp.var 0) (lift 1 0 c)) Sk.unit)
  (exact (SkJ.wT (List.cons Sk Sk.cert G) (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))
    (SkJ.sChk (List.cons Sk Sk.cert G) (Exp.prn (Exp.var 0)) (lift 1 0 c)
      (SkJ.sPrn (List.cons Sk Sk.cert G) (Exp.var 0)
        (SkJ.sVar (List.cons Sk Sk.cert G) 0 Sk.cert (nthS.eq_2 Sk.cert G)))
      (skj_weaken Bool.false G c Sk.syn hc 0 Sk.cert)))))

(thm s52_wfs_insp2 [G :- (List Sk), c :- Exp, hc :- (SkJ Bool.false G c Sk.syn)]
  (SkJ Bool.true (List.cons Sk Sk.cert G)
    (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) Sk.unit)
  (exact (SkJ.wT (List.cons Sk Sk.cert G)
    (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))
    (SkJ.sIte (List.cons Sk Sk.cert G) (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)) Exp.ff Exp.tt Sk.bool
      (SkJ.sChk (List.cons Sk Sk.cert G) (Exp.prn (Exp.var 0)) (lift 1 0 c)
        (SkJ.sPrn (List.cons Sk Sk.cert G) (Exp.var 0)
          (SkJ.sVar (List.cons Sk Sk.cert G) 0 Sk.cert (nthS.eq_2 Sk.cert G)))
        (skj_weaken Bool.false G c Sk.syn hc 0 Sk.cert))
      (SkJ.sFF (List.cons Sk Sk.cert G)) (SkJ.sTT (List.cons Sk Sk.cert G))))))

;; ---------------------------------------------------------------------------
;; Support for the derivation inductions (R4 §5, the fundamental property).
;;
;; The non-erasing evaluator ignores usages: Entry52 at erasing = false is S
;; itself, at every usage. The induction motives for evalₙ therefore quantify
;; over an arbitrary usage vector ws, of which only the length is read (the
;; variable case); premises are judged in the conclusion's very environment
;; and need no restriction. Tl derivations carry no usages at all.

;; Evidence that a runtime value at a base type is related to its carrier
;; value, read off S's base clauses. They serve every entry the inductions
;; add to an environment (numerals, labels, codes, certificates).
(prove! 's52_ent_lbl
  '[G :- (List Sk), en :- (HEnv G), l :- Nat, hl :- (LT.lt l (NL))]
  '(S52 chkf dec encTy cap erasing Exp.tLbl G en Sk.lbl (RV.lbl l) l)
  '[(constructor) (rfl) (exact hl)])

(prove! 's52_ent_nat '[G :- (List Sk), en :- (HEnv G), i :- Nat]
  '(S52 chkf dec encTy cap erasing Exp.tNat G en Sk.nat (RV.nat i) i)
  '[(rfl)])

(prove! 's52_ent_syn
  '[G :- (List Sk), en :- (HEnv G), a :- Code, ha :- (Eq Bool (lblOk a) Bool.true)]
  '(S52 chkf dec encTy cap erasing Exp.tSyn G en Sk.syn (RV.code a) a)
  '[(constructor) (rfl) (exact ha)])

(prove! 's52_ent_cert
  '[G :- (List Sk), en :- (HEnv G), a :- Code, ha :- (Eq Bool (lblOk a) Bool.true)]
  '(S52 chkf dec encTy cap erasing Exp.tR G en Sk.cert (RV.cert a) a)
  '[(constructor) (rfl) (exact ha)])

;; A related environment's usage vector has the context's length.
(prove! 'env52_len '[D :- (List Exp)]
  '(forall [us (List U)] (forall [rho (List RV)] (forall [en (HEnv (skels D))]
     (=> (Env52 chkf dec encTy cap erasing D us rho en) (Eq Nat (lenU us) (lenE D))))))
  '[(induction D)
    (intro us rho en h)
    (have e (Eq (List U) us (List.nil U)) (And.left h))
    (subst e) (rfl)
    (intro us rho en h)
    (refine' (exT U _ _ h _)) (intro r0 h0)
    (refine' (exT (List U) _ _ h0 _)) (intro us0 h1)
    (refine' (exT RV _ _ h1 _)) (intro v0 h2)
    (refine' (exT (List RV) _ _ h2 _)) (intro rho0 hp)
    (have hu (Eq (List U) us (List.cons U r0 us0)) (And.left hp))
    (have ht (Env52 chkf dec encTy cap erasing tail us0 rho0 (Prod.snd en))
      (And.left (And.right (And.right hp))))
    (have hl (Eq Nat (lenU us0) (lenE tail)) (ih_tail us0 rho0 (Prod.snd en) ht))
    (exact (Eq.mpr (congrArg (fn [q :- (List U)] (Eq Nat (lenU q) (lenE (List.cons Exp head tail)))) hu)
      (congrArg Nat.succ hl)))])

;; A usage vector as long as the context has an entry wherever the context
;; does. The cons step is its own theorem, so that cases on the vector name
;; its fields cleanly (they would clash with the context's).
(prove! 's52_nth_cc
  '[A :- Exp, D :- (List Exp),
    ih :- (forall [us (List U)] (=> (Eq Nat (lenU us) (lenE D))
      (forall [i Nat] (forall [B Exp] (=> (Eq (Option Exp) (nthE D i) (Option.some Exp B))
        (Exists (fn [r :- U] (Eq (Option U) (nthU us i) (Option.some U r)))))))))]
  '(forall [us (List U)] (=> (Eq Nat (lenU us) (lenE (List.cons Exp A D)))
      (forall [i Nat] (forall [B Exp]
        (=> (Eq (Option Exp) (nthE (List.cons Exp A D) i) (Option.some Exp B))
          (Exists (fn [r :- U] (Eq (Option U) (nthU us i) (Option.some U r)))))))))
  '[(intro us) (cases us)
    (intro hl) (exact (absurd (Eq.symm hl) (Nat.succ_ne_zero (lenE D))))
    (intro hl i) (cases i)
    (intro B h) (constructor) (exact head) (exact (nthU.eq_2 head tail))
    (intro B h)
    (have h2 (Eq (Option Exp) (nthE D n) (Option.some Exp B))
      (Eq.trans (Eq.symm (nthE.eq_3 A D n)) h))
    (refine' (exT U _ _ (ih tail (succ_eq (lenU tail) (lenE D) hl) n B h2) _))
    (intro r hr) (constructor) (exact r)
    (exact (Eq.trans (nthU.eq_3 head tail n) hr))])

(prove! 's52_nth_ex '[D :- (List Exp)]
  '(forall [us (List U)] (=> (Eq Nat (lenU us) (lenE D))
      (forall [i Nat] (forall [B Exp]
        (=> (Eq (Option Exp) (nthE D i) (Option.some Exp B))
          (Exists (fn [r :- U] (Eq (Option U) (nthU us i) (Option.some U r)))))))))
  '[(induction D)
    (intro us hl i B h)
    (exact (False.elim$0 (none_ne_someE B (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
    (exact (s52_nth_cc chkf dec encTy cap erasing head tail ih_tail))])

;; The same variable lemma for evalₙ: no usage hypothesis is needed, since
;; Entry52 ignores usages there. Any usage vector of the context's length has
;; an entry at i, and the environment's own vector supplies it.
(defn prove-n! [nm params goal tactics]
  (a/prove-theorem nm (lv (vec (concat (take 12 pars) params))) (lv goal) (lv tactics)))

(prove-n! 's52_var_free
  '[D :- (List Exp), i :- Nat, A :- Exp, hw :- (WFS D),
    hA :- (Eq (Option Exp) (nthE D i) (Option.some Exp A)),
    ws :- (List U), rho :- (List RV), en :- (HEnv (skels D)),
    he :- (Env52 chkf dec encTy cap Bool.false D ws rho en)]
  '(Result52 chkf dec encTy cap Bool.false (skels D) en rho (Exp.var i) (lift (+ i 1) 0 A) (Exp.var i))
  '[(refine' (exT U _ _ (s52_nth_ex chkf dec encTy cap Bool.false D ws
        (env52_len chkf dec encTy cap Bool.false D ws rho en he) i A hA) _))
    (intro r hu)
    (exact (s52_var chkf dec encTy cap Bool.false D ws i A r hw hA hu
      (Or.inl (Eq.refl Bool.false)) rho en he))])

;; ---------------------------------------------------------------------------
;; The induction motives (non-erasing evaluator).
;;
;; S52JudgR: a runtime derivation, in every environment related at every
;; usage vector (the vector is not read) of a well-formed context, evaluates
;; safely. S52JudgT is the same for type-level derivations; Tl's index w
;; separates formation (w = true, nothing to evaluate) from typing.

(def NH '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
          encTy :- (=> Exp Code), cap :- Nat])
(defn pi* [params body]
  (reduce (fn [q [x _ ty]] (list 'forall [x ty] q)) body (reverse (partition 3 params))))
(defn kdef-fn! [nm params body]
  (kdef! nm (pi* params 'Prop) (list 'fn (vec params) body)))

(def MOTIVE-N
  '(forall [ws (List U)] (forall [rho (List RV)] (forall [en (HEnv (skels D))]
     (=> (WFS D) (Env52 chkf dec encTy cap Bool.false D ws rho en)
       (Result52 chkf dec encTy cap Bool.false (skels D) en rho t A t))))))

(kdef-fn! 'S52JudgR (concat NH '[D :- (List Exp), us :- (List U), t :- Exp, A :- Exp]) MOTIVE-N)
(kdef-fn! 'S52JudgT (concat NH '[w :- Bool, D :- (List Exp), t :- Exp, A :- Exp])
  (list '=> '(Eq Bool w Bool.false) '(Reg (skels D) A) MOTIVE-N))

;; ---------------------------------------------------------------------------
;; The case generator.
;;
;; One rule family (Lam, App, ...) yields the minor premise of the three
;; inductions: Tl and Rt for evalₙ, Er for evalᴱₙ. The rule's body is the
;; same call of the matching s52_* case lemma; what differs is the mode m:
;;   :E      the evaluator flag, Bool.false or Bool.true;
;;   :er?    evalᴱₙ (premises carry erasures, environments are restricted);
;;   :rec    which judgment the recursive premises are (:tl, :rt, :er);
;;   :V, :c  (Er only) the conclusion's usage vector and constructor, for
;;           the restriction of environments (as in theorem4e.clj).
;; The minor's own variables: wsE rhoE enE hwE heE are the usage vector, the
;; runtime and carrier environments, WFS D and the relatedness hypothesis.

(defn L [& xs] (apply list xs))
(defn er [m erased src] (if (:er? m) erased src))
(defn ihn [h] (symbol (str "ih_" h)))
(defn hform [D A hA] (L 'lemma25_tl_type 'chkf D A hA))
(defn hclean [D A hA] (L 'skj_clean 'Bool.true (L 'skels D) A 'Sk.unit (hform D A hA)))
(def HN (L 'Or.inl (L 'Eq.refl 'Bool.false)))
(defn hn-of [m r-nz] (if (:er? m) (L 'Or.inr r-nz) HN))
(defn S [m nm & args]
  (apply L nm 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE 'rhoE args))
(def RELA '(Eq.refl Bool.false))
(defn fnt [binders body] (L 'fn (vec binders) body))
(defn rel-of [m A G en v a] (L 'S52 'chkf 'dec 'encTy 'cap (:E m) A G en (L 'skel A) v a))

;; Skeleton evidence of a premise t : A (kind :tl or :rec, the latter being
;; the mode's own judgment; for Er it is read through er_rt).
(defn pkind [m kind] (if (= kind :rec) (:rec m) kind))
(defn sj [m kind D t A h]
  (case (pkind m kind)
    :tl (L 'lemma25_tl_term 'chkf D t A h)
    :rt (L 'lemma25_rt 'chkf D '_ t A h)
    :er (L 'lemma25_rt 'chkf D '_ t A (L 'er_rt 'chkf D '_ t A '_ h))))
(defn sk [m kind D t A h hcl]
  (case (pkind m kind)
    :tl (L 'skOf_tl 'chkf 'Bool.false D t A h RELA hcl)
    :rt (L 'skOf_rt 'chkf D '_ t A h hcl)
    :er (L 'skOf_rt 'chkf D '_ t A (L 'er_rt 'chkf D '_ t A '_ h) hcl)))

;; Restriction of the conclusion's environment to a premise's vector (Er).
(def theorem4e-priv (fn [s] @(ns-resolve 'lcert.formal.theorem4e s)))
(defn subpf [m w]
  (let [vtrees (theorem4e-priv 'vtrees) leaf-len (theorem4e-priv 'leaf-len)
        sub-pf (theorem4e-priv 'sub-pf) nz (theorem4e-priv 'nz)
        [t leaves] (vtrees (:c m))
        Lf (fn [h] (L 'er_len 'chkf '_ '_ '_ '_ '_ h))
        lp (into {} (for [[k v] leaves] [k (leaf-len Lf v)]))]
    (sub-pf w t lp nz)))
(defn restr [m w]
  (if (or (not (:er? m)) (= w (:V m))) 'heE
      (L 'env52_sub 'chkf 'dec 'encTy 'cap 'Bool.true 'D (:V m) w 'rhoE 'enE 'heE (subpf m w))))

;; The regularity (s52reg.clj) of a type-level premise's type, from the
;; rule's table (tl-regs below).
(defn reg-arg [m h]
  (or (get (:regs m) h) (throw (ex-info "no regularity proof for premise" {:premise h :ctor (:ctor m)}))))

;; A recursive premise's result in the conclusion's environment.
(defn prem [m h w]
  (case (:rec m)
    :tl (L (ihn h) RELA (reg-arg m h) 'wsE 'rhoE 'enE 'hwE 'heE)
    :rt (L (ihn h) 'wsE 'rhoE 'enE 'hwE 'heE)
    :er (L (ihn h) 'rhoE 'enE 'hwE (restr m w))))

;; Extend the environment by entries (the first is added first). An entry:
;;   :A :D :v :alpha  type, context it extends, runtime and carrier value;
;;   :r               its usage (used only by evalᴱₙ);
;;   :rel             a proof of S52 at the entry, or :entry a proof of
;;                    Entry52 (the let-bound first component);
;;   :wf              SkJ formation of the type.
(defn chain [m entries]
  (reduce
    (fn [c {:keys [A D v alpha r rel entry wf]}]
      (let [r (if (:er? m) r 'U.uw)
            rel (walk/postwalk-replace {'erasing (:E m)} rel)
            hnr (if (:er? m) (L 'Or.inr '(Eq.refl Bool.true)) HN)]
        {:D (L 'List.cons 'Exp A (:D c))
         :ws (L 'List.cons 'U r (:ws c))
         :rho (L 'List.cons 'RV v (:rho c))
         :en (L 'Prod.mk alpha (:en c))
         :hw (L 'wfs_cons A (:D c) wf (:hw c))
         :he (if entry
               (L 'env52_cons 'chkf 'dec 'encTy 'cap (:E m) A (:D c) r (:ws c) v (:rho c) alpha (:en c) entry (:he c))
               (L 'env52_cons_rel 'chkf 'dec 'encTy 'cap (:E m) A (:D c) r (:ws c) v (:rho c) alpha (:en c) hnr rel (:he c)))
         :pre (cons r (:pre c))}))
    {:D 'D :ws (if (:er? m) (:V m) 'wsE) :rho 'rhoE :en 'enE :hw 'hwE :he 'heE :pre ()}
    entries))

;; A recursive premise under binders, judged at the extended context.
(defn prem-ext [m h w entries]
  (let [c (chain m entries)]
    (case (:rec m)
      :tl (L (ihn h) RELA (reg-arg m h) (:ws c) (:rho c) (:en c) (:hw c) (:he c))
      :rt (L (ihn h) (:ws c) (:rho c) (:en c) (:hw c) (:he c))
      :er (let [cons-us (theorem4e-priv 'cons-us) cons-sub (theorem4e-priv 'cons-sub)
                pre (:pre c)]
            (L (ihn h) (:rho c) (:en c) (:hw c)
               (L 'env52_sub 'chkf 'dec 'encTy 'cap 'Bool.true (:D c) (cons-us pre (:V m)) (cons-us pre w)
                  (:rho c) (:en c) (:he c) (cons-sub pre w (:V m) (subpf m w))))))))

;; --- the rule families -----------------------------------------------------------

(def fam
  {:var
   (fn [m] (if (:er? m)
             (L 's52_var 'chkf 'dec 'encTy 'cap 'Bool.true 'D 'us 'i 'A 'r 'hwE 'hA 'hu
                (L 'Or.inr 'hr) 'rhoE 'enE 'heE)
             (L 's52_var_free 'chkf 'dec 'encTy 'cap 'D 'i 'A 'hwE (if (= (:rec m) :tl) 'h 'hA)
                'wsE 'rhoE 'enE 'heE)))

   :const (fn [m] (S m 's52_const 't 'A 'h))

   ;; Lam: non-erasing at every usage; erasing through the usage dispatcher
   ;; s52e_lam (s52erased.clj).
   :lam
   (fn [m]
     (if (:er? m)
       (L (L 's52e_lam 'chkf 'dec 'encTy 'cap 'D 'us 'A 'B 't 'te (hform 'D 'A 'hA) 'hwE 'rhoE 'enE 'heE)
          'r 'ih_ht)
       (S m 's52_lam 'r 'A 'B 't 't HN
          (fnt ['av :- 'RV, 'alpha :- '(Car (skel A)), 'ha :- (rel-of m 'A '(skels D) 'enE 'av 'alpha)]
            (prem-ext m 'ht 'us
              [{:A 'A :D 'D :v 'av :alpha 'alpha :rel 'ha :wf (hform 'D 'A 'hA)}])))))

   ;; App at a runtime argument (premise kind :rec) or a type-level one (:tl).
   :app
   (fn [m]
     (let [ukind (if (:ulogical m) :tl :rec)
           hcl (hclean 'D 'A 'hA)
           hB (L 'lemma25_tl_type 'chkf '(List.cons Exp A D) 'B 'hB)
           hu' (sj m ukind 'D 'u 'A 'hu)
           hk' (sk m ukind 'D 'u 'A 'hu hcl)
           ihu (if (:ulogical m) (:u-result m) (prem m 'hu 'us2))]
       (if (:zero m)
         (if (:er? m)
           (S m 's52_app0e 'A 'B 'f 'u 'fe 'Exp.star hB hu' hk' '(Eq.refl Bool.true)
              (prem m 'hf 'us))
           (S m 's52_app 'U.u0 'A 'B 'f 'u 'f 'u hB hu' hk' HN (prem m 'hf 'us) ihu))
         (S m 's52_app 'r 'A 'B 'f 'u (er m 'fe 'f) (er m 'ue 'u) hB hu' hk'
            (hn-of m 'hr) (prem m 'hf 'us1) ihu))))

   :pair
   (fn [m]
     (let [xkind (if (:xlogical m) :tl :rec)
           hcl (hclean 'D 'A 'hA)
           hB (L 'lemma25_tl_type 'chkf '(List.cons Exp A D) 'B 'hB)
           hx' (sj m xkind 'D 'x 'A 'hx)
           hk' (sk m xkind 'D 'x 'A 'hx hcl)
           ihx (if (:xlogical m) (:x-result m) (prem m 'hx 'us1))]
       (cond
         (and (:zero m) (:er? m))
         (S m 's52_pair0e 'A 'B 'x 'y 'Exp.star 'ye hB hx' hk' '(Eq.refl Bool.true) (prem m 'hy 'us))
         (:zero m)
         (S m 's52_pair 'U.u0 'A 'B 'x 'y 'x 'y hB hx' hk' HN ihx (prem m 'hy 'us))
         :else
         (S m 's52_pair 'r 'A 'B 'x 'y (er m 'xe 'x) (er m 'ye 'y) hB hx' hk' (hn-of m 'hr)
            ihx (prem m 'hy 'us2)))))

   :let
   (fn [m]
     (let [PD '(den chkf dec encTy cap p (skels D) (Sk.prod (skel A) (skel B)) enE)
           alpha (L 'Prod.fst PD) beta (L 'Prod.snd PD)
           hSig (L 'skj_clean 'Bool.true '(skels D) '(Exp.tSig r A B) 'Sk.unit
                   (L 'SkJ.wSig '(skels D) 'r 'A 'B (hform 'D 'A 'hA)
                      (L 'lemma25_tl_type 'chkf '(List.cons Exp A D) 'B 'hB)))
           hk' (sk m :rec 'D 'p '(Exp.tSig r A B) 'hp hSig)
           DA '(List.cons Exp A D)]
       (S m 's52_let 'r 'A 'B 'C 'p 't (er m 'pe 'p) (er m 'te 't) (hform 'D 'C 'hC) hk'
          (prem m 'hp 'us1)
          (fnt ['va :- 'RV, 'vb :- 'RV,
                'hent :- (L 'Entry52 (:E m) 'r (rel-of m 'A '(skels D) 'enE 'va alpha)),
                'hrb :- (rel-of m 'B '(List.cons Sk (skel A) (skels D)) (L 'Prod.mk alpha 'enE) 'vb beta)]
            (prem-ext m 'ht 'us2
              [{:A 'A :D 'D :v 'va :alpha alpha :r 'r :entry 'hent :wf (hform 'D 'A 'hA)}
               {:A 'B :D DA :v 'vb :alpha beta :r 'U.u1 :rel 'hrb :wf (hform DA 'B 'hB)}])))))

   ;; abort: its premise yields an element of S(0); the case is vacuous.
   :abort
   (fn [m] (S m 's52_abort 't (er m 'te 't)
             (L 'Result52 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE 'rhoE
                '(Exp.abort A t) 'A (L 'Exp.abort 'A (er m 'te 't)))
             (prem m 'ht 'us)))

   :conv (fn [m] (S m 's52_conv 't (er m 'te 't) 'A 'B 'hc (prem m 'ht 'us)))

   :ite (fn [m] (S m 's52_ite 'b 't 'e 'C (er m 'be 'b) (er m 'te 't) (er m 'ee 'e)
                   (prem m 'hb 'us1) (prem m 'ht 'us2) (prem m 'he 'us2)))

   :elimB
   (fn [m] (S m 's52_elimB 'P 'b 't 'e (er m 'be 'b) (er m 'te 't) (er m 'ee 'e)
             (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tBool D) 'P 'hP)
             (sj m :rec 'D 'b 'Exp.tBool 'hb)
             (sk m :rec 'D 'b 'Exp.tBool 'hb '(Eq.refl Bool.true))
             (prem m 'hb 'us1) (prem m 'ht 'us2) (prem m 'he 'us2)))

   :succ (fn [m] (S m 's52_succ 'n (er m 'ne 'n) (prem m 'h 'us)))

   :recN
   (fn [m]
     (let [DN '(List.cons Exp Exp.tNat D)
           hP (L 'lemma25_tl_type 'chkf DN 'P 'hP)]
       (S m 's52_recN 'P 'z 's 'n (er m 'ze 'z) (er m 'se 's) (er m 'ne 'n) hP
          (sj m :rec 'D 'n 'Exp.tNat 'hn)
          (sk m :rec 'D 'n 'Exp.tNat 'hn '(Eq.refl Bool.true))
          (prem m 'hn 'us1) (prem m 'hz 'us2)
          (fnt ['i :- 'Nat, 'av :- 'RV, 'alpha :- '(Car (skel P)),
                'ha :- (rel-of m 'P '(List.cons Sk Sk.nat (skels D)) '(Prod.mk i enE) 'av 'alpha)]
            (prem-ext m 'hs '(vscale U.uw us3)
              [{:A 'Exp.tNat :D 'D :v '(RV.nat i) :alpha 'i :r 'U.uw
                :rel '(s52_ent_nat chkf dec encTy cap erasing (skels D) enE i) :wf '(SkJ.wNat (skels D))}
               {:A 'P :D DN :v 'av :alpha 'alpha :r 'U.u1 :rel 'ha :wf hP}])))))

   :caseL
   (fn [m] (S m 's52_caseL 'P 'x 'bs (er m 'xe 'x) (er m 'bse 'bs)
             (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tLbl D) 'P 'hP)
             (sj m :rec 'D 'x 'Exp.tLbl 'hx)
             (sk m :rec 'D 'x 'Exp.tLbl 'hx '(Eq.refl Bool.true))
             (prem m 'hx 'us1) (prem m 'hb 'us2)))

   :bnil (fn [m] (S m 's52_bnil 'P))

   :bcons
   (fn [m] (S m 's52_bcons 'P 'k 'h 't (er m 'he 'h) (er m 'te 't)
             ;; Tl.zBcons does not record P's formation: it is Reg's second disjunct.
             (if (= (:rec m) :tl)
               (L 'reg_brs_inv '(skels D) 'P 'k 'hrg)
               (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tLbl D) 'P 'hP))
             (prem m 'hh 'us) (prem m 'ht 'us)))

   :sleaf (fn [m] (S m 's52_sleaf 'x (er m 'xe 'x) (prem m 'h 'us)))

   :snode (fn [m] (S m 's52_snode 'x 'c1 'c2 (er m 'xe 'x) (er m 'c1e 'c1) (er m 'c2e 'c2)
                    (prem m 'hx 'us1) (prem m 'h1 'us2) (prem m 'h2 'us3)))

   :recS
   (fn [m]
     (let [DS '(List.cons Exp Exp.tSyn D)
           hP (L 'lemma25_tl_type 'chkf DS 'P 'hP)
           DL '(List.cons Exp Exp.tLbl D)
           DB1 '(List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))
           DB2 '(List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
           DY1 '(List.cons Exp (y1Ty P) (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
           aY1 '(carTo (skel (y1Ty P)) (skel P) (skel_y1Ty P) alpha)
           aY2 '(carTo (skel (y2Ty P)) (skel P) (skel_y2Ty P) beta)
           inv (fn [c v a] (rel-of m 'P '(List.cons Sk Sk.syn (skels D)) (L 'Prod.mk c 'enE) v a))]
       (S m 's52_recS 'P 'tl 'tn 'c (er m 'tle 'tl) (er m 'tne 'tn) (er m 'ce 'c) hP
          (sj m :rec 'D 'c 'Exp.tSyn 'hc)
          (sk m :rec 'D 'c 'Exp.tSyn 'hc '(Eq.refl Bool.true))
          (prem m 'hc 'us1)
          (fnt ['l :- 'Nat, 'hlE :- '(LT.lt l (NL))]
            (prem-ext m 'hl '(vscale U.uw us2)
              [{:A 'Exp.tLbl :D 'D :v '(RV.lbl l) :alpha 'l :r 'U.uw
                :rel '(s52_ent_lbl chkf dec encTy cap erasing (skels D) enE l hlE) :wf '(SkJ.wLbl (skels D))}]))
          (fnt ['l :- 'Nat, 'a :- 'Code, 'b :- 'Code, 'ar :- 'RV, 'br :- 'RV,
                'alpha :- '(Car (skel P)), 'beta :- '(Car (skel P)),
                'hlE :- '(LT.lt l (NL)), 'haE :- '(Eq Bool (lblOk a) Bool.true),
                'hbE :- '(Eq Bool (lblOk b) Bool.true),
                'hraE :- (inv 'a 'ar 'alpha), 'hrbE :- (inv 'b 'br 'beta)]
            (prem-ext m 'hn '(vscale U.uw us3)
              (walk/postwalk-replace {'hP' hP}
              [{:A 'Exp.tLbl :D 'D :v '(RV.lbl l) :alpha 'l :r 'U.uw
                :rel '(s52_ent_lbl chkf dec encTy cap erasing (skels D) enE l hlE) :wf '(SkJ.wLbl (skels D))}
               {:A 'Exp.tSyn :D DL :v '(RV.code a) :alpha 'a :r 'U.uw
                :rel '(s52_ent_syn chkf dec encTy cap erasing (skels (List.cons Exp Exp.tLbl D)) (Prod.mk l enE) a haE)
                :wf '(SkJ.wSyn (skels (List.cons Exp Exp.tLbl D)))}
               {:A 'Exp.tSyn :D DB1 :v '(RV.code b) :alpha 'b :r 'U.uw
                :rel '(s52_ent_syn chkf dec encTy cap erasing
                        (skels (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
                        (Prod.mk a (Prod.mk l enE)) b hbE)
                :wf '(SkJ.wSyn (skels (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))}
               {:A '(y1Ty P) :D DB2 :v 'ar :alpha aY1 :r 'U.u1
                :rel '(s52_y1_val chkf dec encTy cap erasing ar (skels D) P hP' b a l enE alpha hraE)
                :wf '(s52_wfs_y1 (skels D) P hP')}
               {:A '(y2Ty P) :D DY1 :v 'br :alpha aY2 :r 'U.u1
                :rel '(s52_y2_val chkf dec encTy cap erasing br (skels D) P hP' (skel (y1Ty P))
                        (carTo (skel (y1Ty P)) (skel P) (skel_y1Ty P) alpha) b a l enE beta hrbE)
                :wf '(s52_wfs_y2 (skels D) P hP')}]))))))

   :leaf (fn [m] (S m 's52_leaf 'x (er m 'xe 'x) (prem m 'h 'us)))

   :node (fn [m] (S m 's52_node 'd 'x 'r1 'r2 (er m 'de 'd) (er m 'xe 'x) (er m 'r1e 'r1) (er m 'r2e 'r2)
                   (prem m 'hd 'us1) (prem m 'hx 'us2) (prem m 'h1 'us3) (prem m 'h2 'us4)))

   :itR (fn [m] (S m 's52_itR 'X 'g 'h 'r (er m 'ge 'g) (er m 'he 'h) (er m 're 'r)
                  (hform 'D 'X 'hX)
                  (prem m 'hg '(vscale U.uw us1)) (prem m 'hh '(vscale U.uw us2)) (prem m 'hr 'us3)))

   :prn (fn [m] (S m 's52_prn 'r (er m 're 'r) (prem m 'h 'us)))

   :chk (fn [m] (S m 's52_chk 'c 'd (er m 'ce 'c) (er m 'de 'd) (prem m 'hc 'us1) (prem m 'hd 'us2)))

   :h1 (fn [m] (S m 's52_h1 'hcs 'r 's 'c 'e1 'e2 (er m 'e1e 'e1) (er m 'e2e 'e2)
                 (L 'Result52 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE 'rhoE
                    '(Exp.h1 r s c e1 e2) 'Exp.tEmpty
                    (L 'Exp.h1 (er m 're 'r) (er m 'se 's) (er m 'ce 'c) (er m 'e1e 'e1) (er m 'e2e 'e2)))
                 (prem m 'h1 'us4) (prem m 'h2 'us5)))

   :refl (fn [m] (S m 's52_refl 'X 'cd 'r 'e (er m 're 'r) (er m 'ee 'e) 'hb 'hcs (:below m)
                   (prem m 'hr 'us1) (prem m 'he 'us2)))

   ;; inspect: the branch the checker selects, in the environment extended by
   ;; the certificate and the true evidence of that branch.
   :insp
   (fn [m]
     (let [CR '(den chkf dec encTy cap r (skels D) Sk.cert enE)
           CD '(den chkf dec encTy cap c (skels D) Sk.syn enE)
           DR '(List.cons Exp Exp.tR D)
           F1 '(chkT (Exp.var 0) (lift 1 0 c))
           F2 '(Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))
           hcS (sj m :rec 'D 'c 'Exp.tSyn 'hc)
           ent (fn [truth F ev wfl]
                 (prem-ext m (if truth 'h1 'h2) 'us2
                   [{:A 'Exp.tR :D 'D :v (L 'RV.cert CR) :alpha CR :r 'U.u1
                     :rel (L 's52_ent_cert 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE CR
                             (L 's52_valid_cert 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE 'rhoE
                                'r (er m 're 'r) (prem m 'hr 'us1)))
                     :wf '(SkJ.wR (skels D))}
                    {:A F :D DR :v 'RV.star :alpha 'Unit.unit :r 'U.u1
                     :rel (L ev 'chkf 'dec 'encTy 'cap (:E m) '(skels D) 'enE 'rhoE 'c CR hcS 'hck)
                     :wf (L wfl '(skels D) 'c hcS)}]))]
       (S m 's52_insp 'X 'r 'c 't1 't2 (er m 're 'r) (er m 'ce 'c) (er m 't1e 't1) (er m 't2e 't2)
          (hform 'D 'X 'hX)
          (prem m 'hr 'us1) (prem m 'hc '(vscale U.uw us0))
          (fnt ['hck :- (L 'Eq 'Bool (L 'chkf CR CD) 'Bool.true)]
            (ent true F1 's52_insp_true 's52_wfs_insp1))
          (fnt ['hck :- (L 'Eq 'Bool (L 'chkf CR CD) 'Bool.false)]
            (ent false F2 's52_insp_false 's52_wfs_insp2)))))})

;; --- constructor tables ------------------------------------------------------------

(def tl-fams
  '{zVar :var, zConst :const, zConv :conv, zLam :lam, zApp :app, zPair :pair, zLet :let,
    zAbort :abort, zIte :ite, zElimB :elimB, zSucc :succ, zRecN :recN, zCaseL :caseL,
    zBnil :bnil, zBcons :bcons, zSleaf :sleaf, zSnode :snode, zRecS :recS, zLeaf :leaf,
    zNode :node, zItR :itR, zPrn :prn, zChk :chk, zH1 :h1, zRefl :refl, zInsp :insp})
(def rt-fams
  '{rVar :var, rConst :const, rLam :lam, rApp0 :app, rApp :app, rPair0 :pair, rPair :pair,
    rLet :let, rAbort :abort, rConv :conv, rIte :ite, rElimB :elimB, rSucc :succ, rRecN :recN,
    rCaseL :caseL, rBnil :bnil, rBcons :bcons, rSleaf :sleaf, rSnode :snode, rRecS :recS,
    rLeaf :leaf, rNode :node, rItR :itR, rPrn :prn, rChk :chk, rH1 :h1, rRefl :refl, rInsp :insp})
(def er-fams
  '{eVar :var, eConst :const, eLam :lam, eApp0 :app, eApp :app, ePair0 :pair, ePair :pair,
    eLet :let, eAbort :abort, eConv :conv, eIte :ite, eElimB :elimB, eSucc :succ, eRecN :recN,
    eCaseL :caseL, eBnil :bnil, eBcons :bcons, eSleaf :sleaf, eSnode :snode, eRecS :recS,
    eLeaf :leaf, eNode :node, eItR :itR, ePrn :prn, eChk :chk, eH1 :h1, eRefl :refl, eInsp :insp})

;; The Tl premise of the zero-usage rules is evaluated by evalₙ through the
;; type-level induction (s52_tl_step), at the conclusion's environment.
(defn tl-result [D t A h]
  (L 's52_tl_step 'chkf 'dec 'encTy 'cap 'hcs 'belowN 'Bool.false D t A h RELA
     (L 'reg_formed '(skels D) 'A (hform 'D 'A 'hA))
     'wsE 'rhoE 'enE 'hwE 'heE))


;; --- regularity of the premises' types (Tl induction), rule by rule --------------

(def GD '(skels D))
(defn rfi [Gp A h] (L 'reg_formed Gp A h))
(defn rf [A h] (rfi GD A h))
(def hA1 (hform 'D 'A 'hA))
(def hB1 (L 'lemma25_tl_type 'chkf '(List.cons Exp A D) 'B 'hB))
(defn base-reg [ty w] (rf ty (L w GD)))
(def reg-R (base-reg 'Exp.tR 'SkJ.wR))
(def reg-Syn (base-reg 'Exp.tSyn 'SkJ.wSyn))
(def reg-Lbl (base-reg 'Exp.tLbl 'SkJ.wLbl))
(def reg-Nat (base-reg 'Exp.tNat 'SkJ.wNat))
(def reg-Bool (base-reg 'Exp.tBool 'SkJ.wBool))
(defn skj-tl [D t A h] (L 'lemma25_tl_term 'chkf D t A h))

(defn tl-regs [ctor]
  (case ctor
    zLam {'ht (L 'reg_lam GD 'r 'A 'B 'hrg)}
    zApp {'hf (rf '(Exp.tPi r A B) (L 'SkJ.wPi GD 'r 'A 'B hA1 hB1)) 'hu (rf 'A hA1)}
    zPair {'hx (rf 'A hA1)
           'hy (rf '(subst1 x B)
                   (L 'reg_subst1 GD '(skel A) 'B 'x hB1 (skj-tl 'D 'x 'A 'hx)
                      (L 'skOf_tl 'chkf 'Bool.false 'D 'x 'A 'hx RELA (hclean 'D 'A 'hA))))}
    zLet {'hp (rf '(Exp.tSig r A B) (L 'SkJ.wSig GD 'r 'A 'B hA1 hB1))
          'ht (rfi '(List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))) '(lift 2 0 C)
                   (L 'reg_lift2 GD 'C '(skel A) '(skel B) (hform 'D 'C 'hC)))}
    zAbort {'ht (rf 'Exp.tEmpty '(SkJ.wEmpty (skels D)))}
    zIte {'hb reg-Bool 'ht 'hrg 'he 'hrg}
    zElimB (let [hP (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tBool D) 'P 'hP)]
             {'hb reg-Bool
              'ht (rf '(subst1 Exp.tt P)
                      (L 'reg_subst1 GD 'Sk.bool 'P 'Exp.tt hP '(SkJ.sTT (skels D))
                         '(Eq.refl$1 (Option.some Sk Sk.bool))))
              'he (rf '(subst1 Exp.ff P)
                      (L 'reg_subst1 GD 'Sk.bool 'P 'Exp.ff hP '(SkJ.sFF (skels D))
                         '(Eq.refl$1 (Option.some Sk Sk.bool))))})
    zSucc {'h reg-Nat}
    zRecN (let [hP (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tNat D) 'P 'hP)]
            {'hn reg-Nat
             'hz (rf '(subst1 Exp.zero P)
                     (L 'reg_subst1 GD 'Sk.nat 'P 'Exp.zero hP '(SkJ.sZero (skels D))
                        '(Eq.refl$1 (Option.some Sk Sk.nat))))
             'hs (rfi '(List.cons Sk (skel P) (List.cons Sk Sk.nat (skels D))) '(stepTy P)
                      (L 'reg_stepTy GD 'P hP))})
    zCaseL {'hx reg-Lbl
            'hb (L 'reg_brs GD 'P 0 (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tLbl D) 'P 'hP))}
    zBcons (let [hPb (L 'reg_brs_inv GD 'P 'k 'hrg)]
             {'hh (rf '(subst1 (Exp.lbl k) P)
                      (L 'reg_subst1 GD 'Sk.lbl 'P '(Exp.lbl k) hPb '(SkJ.sLbl (skels D) k)
                         '(Eq.refl$1 (Option.some Sk Sk.lbl))))
              'ht (L 'reg_brs GD 'P '(+ k 1) hPb)})
    zSleaf {'h reg-Lbl}
    zSnode {'hx reg-Lbl 'h1 reg-Syn 'h2 reg-Syn}
    zRecS (let [hP (L 'lemma25_tl_type 'chkf '(List.cons Exp Exp.tSyn D) 'P 'hP)]
            {'hc reg-Syn
             'hl (rfi '(List.cons Sk Sk.lbl (skels D)) '(leafTy P) (L 'reg_leafTy GD 'P hP))
             'hn (rfi '(List.cons Sk (skel (y2Ty P)) (List.cons Sk (skel (y1Ty P))
                         (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (skels D))))))
                      '(nodeTy P) (L 'reg_nodeTy GD 'P hP))})
    zLeaf {'h reg-Lbl}
    zNode {'hd (base-reg 'Exp.tDia 'SkJ.wDia) 'hx reg-Lbl 'h1 reg-R 'h2 reg-R}
    zItR (let [hX (hform 'D 'X 'hX)]
           {'hg (rf '(gTy X) (L 'reg_gTy GD 'X hX)) 'hh (rf '(hTy X) (L 'reg_hTy GD 'X hX)) 'hr reg-R})
    zPrn {'h reg-R}
    zChk {'hc reg-Syn 'hd reg-Syn}
    zH1 (let [hrS (skj-tl 'D 'r 'Exp.tR 'hr) hsS (skj-tl 'D 's 'Exp.tR 'hs) hcS (skj-tl 'D 'c 'Exp.tSyn 'hc)]
          {'hr reg-R 'hs reg-R 'hc reg-Syn
           'h1 (rf '(chkT r c) (L 'reg_chkT GD 'r 'c hrS hcS))
           'h2 (rf '(chkT s (negT c)) (L 'reg_chkT GD 's '(negT c) hsS (L 'reg_negT GD 'c hcS)))})
    zRefl {'hr reg-R
           'he (rf '(chkT r cd)
                   (L 'reg_chkT GD 'r 'cd (skj-tl 'D 'r 'Exp.tR 'hr) (L 'reg_baseCode 'X 'cd GD 'hb)))}
    zInsp (let [hX (hform 'D 'X 'hX)
                ctx '(List.cons Sk Sk.unit (List.cons Sk Sk.cert (skels D)))
                reg2 (rfi ctx '(lift 2 0 X) (L 'reg_lift2 GD 'X 'Sk.cert 'Sk.unit hX))]
            {'hr reg-R 'hc reg-Syn 'h1 reg2 'h2 reg2})
    zConv {'ht (rf 'A (L 'cv_wf_left 'chkf GD 'A 'B 'hc))}
    {}))

(defn case-mode [kind ctor]
  (let [base (case kind
               :tl {:E 'Bool.false :er? false :rec :tl :below 'belowN}
               :rt {:E 'Bool.false :er? false :rec :rt :below 'belowN}
               :er {:E 'Bool.true :er? true :rec :er :below 'belowE :c ctor})
        zero? (#{'rApp0 'eApp0 'rPair0 'ePair0} ctor)]
    (cond-> base
      (= kind :tl) (assoc :regs (tl-regs ctor) :ctor ctor)
      zero? (assoc :zero true)
      (and zero? (#{'rApp0 'rPair0} ctor) (= kind :rt))
      (assoc (if (= ctor 'rApp0) :ulogical :xlogical) true
             (if (= ctor 'rApp0) :u-result :x-result)
             (if (= ctor 'rApp0) (tl-result 'D 'u 'A 'hu) (tl-result 'D 'x 'A 'hx)))
      (and zero? (#{'eApp0 'ePair0} ctor))
      (assoc (if (= ctor 'eApp0) :ulogical :xlogical) true))))

;; --- reading the derivation declarations ------------------------------------------

(defn decl-ctors [resource decl]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/resource resource)))]
    (loop []
      (let [f (read {:eof nil} r)]
        (cond
          (nil? f) (throw (ex-info "declaration not found" {:decl decl}))
          (and (seq? f) (= 'a/inductive (first f)) (= decl (second f)))
          (vec (filter seq? (drop 3 f)))
          :else (recur))))))

(defn thm* [nm params prop tactics]
  (a/prove-theorem nm (lv (vec params)) (lv prop) (lv (vec tactics))))

;; The recursor term. Each minor premise binds the constructor's fields, the
;; induction hypotheses of its recursive premises, and the environment data.
(defn rec-term [decl resource motive judge fams kind]
  (let [ctors (decl-ctors resource decl)
        rec-minor
        (fn [[ctor & fs]]
          (let [binders (take-while vector? fs)
                where (last fs)
                ihs (mapcat (fn [[f ty]]
                              (when (and (seq? ty) (= decl (first ty)))
                                [(ihn f) :- (apply L judge 'chkf 'dec 'encTy 'cap (drop 2 ty))]))
                            binders)
                tl? (= decl 'Tl)
                us (nth where 1)
                env (if (= kind :er)
                      ['rhoE :- '(List RV), 'enE :- '(HEnv (skels D)), 'hwE :- '(WFS D),
                       'heE :- (L 'Env52 'chkf 'dec 'encTy 'cap 'Bool.true 'D us 'rhoE 'enE)]
                      ['wsE :- '(List U), 'rhoE :- '(List RV), 'enE :- '(HEnv (skels D)), 'hwE :- '(WFS D),
                       'heE :- '(Env52 chkf dec encTy cap Bool.false D wsE rhoE enE)])
                hwf (when tl? ['hwf :- (L 'Eq 'Bool (first where) 'Bool.false)
                               'hrg :- (L 'Reg '(skels D) (nth where 3))])
                body (if-let [f (fams ctor)]
                       ((get fam f) (cond-> (case-mode kind ctor) (= kind :er) (assoc :V us)))
                       (L 's52_tf_absurd
                          (L 'Result52 'chkf 'dec 'encTy 'cap 'Bool.false '(skels D) 'enE 'rhoE
                             (nth where 2) (nth where 3) (nth where 2))
                          'hwf))]
            (L 'fn (vec (concat (mapcat (fn [[f ty]] [f :- ty]) binders) ihs hwf env)) body)))]
    (apply L (symbol (str (name decl) ".rec")) 'chkf motive
           (map rec-minor ctors))))

(thm* 's52_tf_absurd '[Q :- Prop, h :- (Eq Bool Bool.true Bool.false)] 'Q
  '[(exact (Bool.noConfusion h))])

;; --- the inductions on Tl and Rt (Theorem 5.2 for evalₙ) --------------------------
;;
;; Each step proves the fundamental property at one budget cap, given it
;; for evalₙ below cap (belowN: the closed token-context instance, used by
;; reflect). The Rt induction evaluates its type-level premises through the
;; Tl one, so s52_tl_step comes first.

(def STEP-N '[hcs :- (CheckSpec chkf dec encTy),
              belowN :- (forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m Bool.false)))])

(thm* 's52_tl_step
  (concat NH STEP-N '[w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp, der :- (Tl chkf w0 D0 t0 A0)])
  '(S52JudgT chkf dec encTy cap w0 D0 t0 A0)
  [(L 'exact (concat
     (rec-term 'Tl "lcert/formal/judgment.clj"
       '(fn [w :- Bool, D :- (List Exp), t :- Exp, A :- Exp, hd :- (Tl chkf w D t A)]
          (S52JudgT chkf dec encTy cap w D t A))
       'S52JudgT tl-fams :tl)
     '[w0 D0 t0 A0 der]))])

(thm* 's52_rt_step
  (concat NH STEP-N '[D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)])
  '(S52JudgR chkf dec encTy cap D0 us0 t0 A0)
  [(L 'exact (concat
     (rec-term 'Rt "lcert/formal/judgment.clj"
       '(fn [D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, hd :- (Rt chkf D us t A)]
          (S52JudgR chkf dec encTy cap D us t A))
       'S52JudgR rt-fams :rt)
     '[D0 us0 t0 A0 der]))])
