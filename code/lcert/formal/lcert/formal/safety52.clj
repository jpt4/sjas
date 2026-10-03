(ns lcert.formal.safety52
  "F5 — the relation S and traces for Theorem 5.2 (R4-metatheory.md §5).

  S52 is indexed by the source type, environment and carrier skeleton.
  Unlike E, it distinguishes 0 from 1 and reads the truth of T(b). Its
  function clause asks for a safe application, including the entire body
  and any nested reflect run. There is no footprint or certificate-size
  bound. Labels and code labels stay below NL, because this formalization
  represents the paper's finite label carrier by Nat and Code.

  The erasing flag selects OkE instead of Ok, ignores the first component
  of a usage-0 product, and applies usage-0 functions to star for every
  carrier argument. All other function and product clauses are independent
  of usage. This one definition supports both versions of the paper's S.

  The generated constructor clauses are ordinary kdef declarations checked
  by the kernel; the generator adds no axioms. This file first establishes
  the relation's clauses and the two consistency consequences needed by
  the H and H1 cases. The fundamental property is a separate obligation."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.erase]
            [lcert.formal.convcase]))

;; A safe trace uses the matching evaluator. Ok/OkE already trace closure
;; applications, recursor steps, and decoded programs (erase.clj).
(kdef Trace52
  (=> (=> Code Code Bool) (=> Code (Option (Prod Nat (Prod Exp Exp))))
      (=> Exp Code) Nat Bool EvSrc RV Prop)
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool,
       src :- EvSrc, v :- RV]
    (Bool.rec$1 (fn [_ :- Bool] Prop)
      (Ok chkf dec encTy cap src v) (OkE chkf dec encTy cap src v) erasing)))

(def ^:private pars
  '[chkf :- (=> Code Code Bool),
    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool])
(def ^:private ps '[chkf dec encTy cap erasing])
(def ^:private ST
  '(forall [G (List Sk)]
     (=> (HEnv G) (forall [s Sk] (=> RV (Car s) Prop)))))
(def ^:private fields @#'lcert.formal.syntactic/exp-fields)

;; Select a carrier shape with Sk.rec, as the existing semantic type V
;; does. Wrong shapes have an empty relation. rv is bound in the clause.
(defn- shape [wanted body]
  (list
    (apply list 'Sk.rec$1 '(fn [q :- Sk] (=> (Car q) Prop))
      (concat
        (for [c '[unit bool nat lbl syn dia cert]]
          (list 'fn ['a :- (list 'Car (symbol (str "Sk." c)))]
                (if (= c wanted) body 'False)))
        (for [c '[arr prod]]
          (list 'fn '[x :- Sk, y :- Sk, ix :- (=> (Car x) Prop), iy :- (=> (Car y) Prop)]
            (list 'fn ['a :- (list 'Car (list (symbol (str "Sk." c)) 'x 'y))]
                  (if (= c wanted) body 'False))))
        ['s])) 'a))

(defn- safe-result [arg aval]
  (list 'Exists (list 'fn '[w :- RV]
    (list 'And (list* 'Trace52 (concat ps [(list 'EvSrc.ap 'rv arg) 'w]))
      (list 'i_B '(List.cons Sk x G) (list 'Prod.mk aval 'en) 'y 'w (list 'a aval))))))

(def ^:private pi-normal
  (list 'forall '[av RV] (list 'forall '[alpha (Car x)]
    (list '=> '(i_A G en x av alpha) (safe-result 'av 'alpha)))))
(def ^:private pi-erased
  (list 'forall '[alpha (Car x)] (safe-result 'RV.star 'alpha)))

(def ^:private sig-normal '(i_A G en x av (Prod.fst a)))
(def ^:private sig-first
  (list 'Bool.rec$1 '(fn [_ :- Bool] Prop) sig-normal
    (list 'U.rec$1 '(fn [_ :- U] Prop) 'True sig-normal sig-normal 'r) 'erasing))

(def ^:private clauses
  {'tEmpty 'False
   'tUnit '(Eq RV rv RV.star)
   'tBool (shape 'bool '(Eq RV rv (RV.bool a)))
   'tNat (shape 'nat '(Eq RV rv (RV.nat a)))
   'tLbl (shape 'lbl '(And (Eq RV rv (RV.lbl a)) (LT.lt a (NL))))
   'tSyn (shape 'syn '(And (Eq RV rv (RV.code a)) (Eq Bool (lblOk a) Bool.true)))
   'tDia '(Eq RV rv RV.token)
   'tR (shape 'cert '(And (Eq RV rv (RV.cert a)) (Eq Bool (lblOk a) Bool.true)))
   'tT '(And (Eq RV rv RV.star) (Eq Bool (den chkf dec encTy cap b G Sk.bool en) Bool.true))
   'tPi (shape 'arr
          (list 'Bool.rec$1 '(fn [_ :- Bool] Prop) pi-normal
            (list 'U.rec$1 '(fn [_ :- U] Prop) pi-erased pi-normal pi-normal 'r) 'erasing))
   'tSig (shape 'prod
           (list 'Exists (list 'fn '[av :- RV] (list 'Exists (list 'fn '[bv :- RV]
             (list 'And '(Eq RV rv (RV.pair av bv))
               (list 'And sig-first
                 '(i_B (List.cons Sk x G) (Prod.mk (Prod.fst a) en) y bv (Prod.snd a)))))))))
   ;; A branch list's position is label minus its starting label. Only
   ;; labels in [k, NL) are demanded; bnil at k=NL is vacuous.
   'tBrs (shape 'arr
           '(forall [l Nat] (=> (LE.le k l) (LT.lt l (NL))
              (Exists (fn [w :- RV]
                (And (Trace52 chkf dec encTy cap erasing (EvSrc.ap rv (RV.lbl (- l k))) w)
                  (i_P (List.cons Sk x G) (Prod.mk (coe Sk.lbl x l) en) y w
                       (a (coe Sk.lbl x (- l k))))))))))})

;; One definition per Exp constructor keeps generated JVM methods small.
(doseq [[ctor fs] fields]
  (let [extras (vec (mapcat (fn [[f ty _]] [f :- ty]) fs))
        ihs (vec (mapcat (fn [[f ty _]] (when (= ty 'Exp) [(symbol (str "i_" f)) :- ST])) fs))
        binders (vec (concat pars extras ihs))
        ty (reduce (fn [acc [b _ t]] (list 'forall [b t] acc)) ST
                   (reverse (partition 3 binders)))]
    (kdef! (symbol (str "s52c_" ctor)) ty
      (list 'fn binders
        (list 'fn '[G :- (List Sk), en :- (HEnv G), s :- Sk, rv :- RV, a :- (Car s)]
          (get clauses ctor 'False))))))

(kdef! 'S52
  (reduce (fn [acc [b _ t]] (list 'forall [b t] acc)) (list '=> 'Exp ST)
          (reverse (partition 3 pars)))
  (list 'fn (vec (concat pars '[A :- Exp]))
    (apply list 'Exp.rec$1 (list 'fn '[_ :- Exp] ST)
      (concat (for [[ctor _] fields] (apply list (symbol (str "s52c_" ctor)) ps)) ['A]))))

;; These equations are the load-bearing distinctions from Erel.
(thm s52_empty
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, erasing :- Bool,
   G :- (List Sk), en :- (HEnv G), s :- Sk, v :- RV, a :- (Car s)]
  (Eq Prop (S52 chkf dec encTy n erasing Exp.tEmpty G en s v a) False)
  (rfl))

(thm s52_unit
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, erasing :- Bool,
   G :- (List Sk), en :- (HEnv G), s :- Sk, v :- RV, a :- (Car s)]
  (Eq Prop (S52 chkf dec encTy n erasing Exp.tUnit G en s v a) (Eq RV v RV.star))
  (rfl))

(thm s52_T
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, erasing :- Bool, b :- Exp,
   G :- (List Sk), en :- (HEnv G), s :- Sk, v :- RV, a :- (Car s)]
  (Eq Prop (S52 chkf dec encTy n erasing (Exp.tT b) G en s v a)
    (And (Eq RV v RV.star) (Eq Bool (den chkf dec encTy n b G Sk.bool en) Bool.true)))
  (rfl))

(thm s52_bool
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, erasing :- Bool,
   G :- (List Sk), en :- (HEnv G), v :- RV, a :- Bool]
  (Eq Prop (S52 chkf dec encTy n erasing Exp.tBool G en Sk.bool v a) (Eq RV v (RV.bool a)))
  (rfl))

;; Corollary 3.7 is used with its conversion premise discharged, at every
;; certificate size. These do not assume a footprint bound at cap n.
(thm s52_no_refutation
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), c :- Code]
  (Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.false)
  (exact (cor37_refutation chkf dec encTy hcs (conv_all chkf dec encTy hcs) c)))

(thm s52_no_contradiction
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), c1 :- Code, c2 :- Code, d :- Code,
   h1 :- (Eq Bool (chkf c1 d) Bool.true),
   h2 :- (Eq Bool (chkf c2 (Code.sn 25 d (Code.sl 15))) Bool.true)]
  False
  (exact (cor37_contradiction chkf dec encTy hcs (conv_all chkf dec encTy hcs) c1 c2 d h1 h2)))

;; Trace projection uses exactly the constructor fields of Ok and OkE.
;; hH is the additional root-safety check on reflect, and is forgotten.
;; Reading the constructor declarations avoids maintaining a third copy of
;; the evaluator table. The generated inductions remain kernel checked.
(defn- trace-fields [pred]
  (with-open [r (java.io.PushbackReader. (io/reader (io/resource "lcert/formal/erase.clj")))]
    (loop []
      (let [f (read {:eof nil} r)]
        (cond (nil? f) (throw (ex-info "Missing safe trace declaration" {:predicate pred}))
              (and (= 'a/inductive (first f)) (= pred (second f)))
              (filter seq? (drop 3 f))
              :else (recur))))))

(doseq [[pred evaluator nm] '[[Ok Ev ok52_eval] [OkE EvE ok52_evalE]]]
  (a/prove-theorem nm
    (lv (vec (concat (take 9 pars)
                ['n0 :- 'Nat, 'src0 :- 'EvSrc, 'v0 :- 'RV,
                 'der :- (list pred 'chkf 'dec 'encTy 'n0 'src0 'v0)])))
    (list evaluator 'chkf 'dec 'encTy 'n0 'src0 'v0)
    (lv (into ['(induction der)]
          (for [[ctor & fs] (trace-fields pred)]
            (list 'exact
              (apply list (symbol (str evaluator "." ctor)) 'chkf 'dec 'encTy
                (for [[f ty] (take-while vector? fs) :when (not= f 'hH)]
                  (if (and (seq? ty) (= pred (first ty))) (symbol (str "ih_" f)) f)))))))))
